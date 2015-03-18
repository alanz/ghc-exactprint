{-# LANGUAGE NamedFieldPuns #-}
module Language.Haskell.GHC.ExactPrint.Annotate  (annotateLHsModule, annotateAST ) where

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.RWS
import Control.Applicative
import Control.Monad.Trans.Free
import Control.Exception
import Data.Data
import Data.Generics
import Data.List

import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Utils
import Language.Haskell.GHC.ExactPrint.Common

import qualified Bag            as GHC
import qualified BasicTypes     as GHC
import qualified BooleanFormula as GHC
import qualified Class          as GHC
import qualified CoAxiom        as GHC
import qualified DynFlags       as GHC
import qualified FastString     as GHC
import qualified ForeignCall    as GHC
import qualified GHC            as GHC
import qualified Name           as GHC
import qualified NameSet        as GHC
import qualified Outputable     as GHC
import qualified RdrName        as GHC
import qualified SrcLoc         as GHC
import qualified Var            as GHC

import qualified OccName(occNameString)

import qualified Data.Map as Map


-- ---------------------------------------------------------------------

data APState = APState
             { priorEndPosition :: GHC.SrcSpan
               -- | Ordered list of comments still to be allocated
             , apComments :: [Comment]
               -- | The original GHC API Annotations
             , apAnns :: GHC.ApiAnns
             }

data StackItem = StackItem
               { -- | Current `SrcSpan`
                 curSrcSpan :: GHC.SrcSpan
                 -- |  `SrcSpan` of the immediately prior scope
               , prevSrcSpan :: GHC.SrcSpan
                 -- | The offset required to get from the prior end point to the
                 -- | The offset required to get from the prior end point to the
                 -- start of the current SrcSpan. Accessed via `getEntryDP`
               , offset     :: DeltaPos
                 -- | Offsets for the elements annotated in this `SrcSpan`
               -- | Indicates whether the contents of this SrcSpan are
               -- subject to vertical alignment layout rules
                 -- | The constructor name of the AST element, to
                 -- distinguish between nested elements that have the same
                 -- `SrcSpan` in the AST.
               , annConName :: AnnConName
               }

initialStackItem :: StackItem
initialStackItem =
  StackItem
    { curSrcSpan = GHC.noSrcSpan
    , prevSrcSpan = GHC.noSrcSpan
    , offset = DP (0,0)
    , annConName = annGetConstr ()
    }

defaultAPState :: GHC.SrcSpan -> GHC.ApiAnns -> APState
defaultAPState priorEnd ga =
  let cs = flattenedComments ga in
    APState
      { priorEndPosition = priorEnd
      , apComments = cs
      , apAnns     = ga    -- $
      }

data APWriter = APWriter
  { -- Final list of annotations
    finalAnns :: Endo (Map.Map AnnKey Annotation)
    -- Used locally to pass Keywords, delta pairs relevant to a specific
    -- subtree to the parent.
  , annKds :: [(KeywordId, DeltaPos)]
    -- Used locally to report a subtrees aderhence to haskell's layout
    -- rules.
  , layoutFlag :: LayoutFlag
  }





tellFinalAnn :: (AnnKey, Annotation) -> AP ()
tellFinalAnn (k, v) =
  tell (mempty { finalAnns = Endo (Map.insertWith (<>) k v) })

tellKd :: (KeywordId, DeltaPos) -> AP ()
tellKd kd = tell (mempty { annKds = [kd] })

setLayoutFlag' :: AP ()
setLayoutFlag' = tell (mempty {layoutFlag = LayoutRules})

instance Monoid APWriter where
  mempty = APWriter mempty mempty mempty
  (APWriter a b e) `mappend` (APWriter c d f) = APWriter (a <> c) (b <> d) (e <> f)


-- | Type used in the AP Monad. The state variables maintain
--    - the current SrcSpan and the constructor of the thing it encloses
--      as a stack to the root of the AST as it is traversed,
--    - the srcspan of the last thing annotated, to calculate delta's from
--    - extra data needing to be stored in the monad
--    - the annotations provided by GHC
type AP a = RWS StackItem APWriter APState a

runAP :: Wrapped () -> GHC.ApiAnns -> GHC.SrcSpan -> Anns
runAP action ga priorEnd =
  ($ mempty) . appEndo . finalAnns . snd
  . (\action -> execRWS action initialStackItem (defaultAPState priorEnd ga))
  . simpleInterpret $ action


simpleInterpret :: Wrapped a -> AP a
simpleInterpret = iterTM go
  where
    go :: AnnotationF (AnnKey, Annotation) (AP a) -> AP a
    go (Output w next) = next
    go (AddEofAnnotation next) = addEofAnnotation' >> next
    go (AddDeltaAnnotation kwid next) =
      addDeltaAnnotation' kwid >> next
    go (AddDeltaAnnotationsOutside akwid kwid next) = addDeltaAnnotationsOutside' akwid kwid >> next
    go (AddDeltaAnnotationsInside akwid next) = addDeltaAnnotationsInside' akwid >> next
    go (AddDeltaAnnotations akwid next) = addDeltaAnnotations' akwid >> next
    go (AddDeltaAnnotationLs akwid n next) = addDeltaAnnotationLs' akwid n >> next
    go (AddDeltaAnnotationAfter akwid next) = addDeltaAnnotationAfter' akwid >> next
    go (AddDeltaAnnotationExt ss akwid next) = addDeltaAnnotationExt' ss akwid >> next
    go (WithAST lss layoutflag prog next) =
      withAST' lss layoutflag (simpleInterpret prog) >>= next
    go (OutputKD (kwid, (_, dp)) next) = tellKd (dp, kwid) >> next
    go (CountAnnsAP kwid next) = countAnnsAP' kwid >>= next
    go (SetLayoutFlag next) = setLayoutFlag' >> next
    go (PrintAnnString akwid s next) = addDeltaAnnotation' akwid >> next
    go (PrintAnnStringExt ss akwid _ next) = addDeltaAnnotationExt' ss akwid >> next
    go (PrintAnnStringLs akwid _ n next) = addDeltaAnnotationLs' akwid n >> next


-- ---------------------------------------------------------------------

flattenedComments :: GHC.ApiAnns -> [Comment]
flattenedComments (_,cm) = map tokComment . GHC.sortLocated . concat $ Map.elems cm

-- -------------------------------------

getSrcSpanAP :: AP GHC.SrcSpan
getSrcSpanAP = asks curSrcSpan

getPriorSrcSpanAP :: AP GHC.SrcSpan
getPriorSrcSpanAP = asks prevSrcSpan

withSrcSpanAP :: Data a => (GHC.Located a) -> DeltaPos -> AP b -> AP b
withSrcSpanAP (GHC.L l a) edp =
  local (\s -> let previousSrcSpan = curSrcSpan s in
                   StackItem l previousSrcSpan edp (annGetConstr a))

getEntryDP :: AP DeltaPos
getEntryDP = asks offset

getUnallocatedComments :: AP [Comment]
getUnallocatedComments = gets apComments

putUnallocatedComments :: [Comment] -> AP ()
putUnallocatedComments cs = modify (\s -> s { apComments = cs } )

-- ---------------------------------------------------------------------

adjustDeltaForOffsetM :: DeltaPos -> AP DeltaPos
adjustDeltaForOffsetM dp = do
  colOffset <- getCurrentColOffset
  return (adjustDeltaForOffset colOffset dp)

adjustDeltaForOffset :: Int -> DeltaPos -> DeltaPos
adjustDeltaForOffset _colOffset dp@(DP (0,_)) = dp -- same line
adjustDeltaForOffset  colOffset    (DP (l,c)) = DP (l,c - colOffset)

-- ---------------------------------------------------------------------

-- | Get the current column offset
getCurrentColOffset :: AP ColOffset
getCurrentColOffset = srcSpanStartColumn <$> getSrcSpanAP

-- |Get the difference between the current and the previous
-- colOffsets, if they are on the same line
getCurrentDP :: LayoutFlag -> AP (ColOffset,LineChanged)
getCurrentDP layoutOn = do
  -- Note: the current col offsets are not needed here, any
  -- indentation should be fully nested in an AST element
  ss <- getSrcSpanAP
  ps <- getPriorSrcSpanAP
  let boolLayoutFlag = case layoutOn of { LayoutRules -> True; NoLayoutRules -> False}
      colOffset = if srcSpanStartLine ss == srcSpanStartLine ps
                    then srcSpanStartColumn ss - srcSpanStartColumn ps
                    else srcSpanStartColumn ss
      r = case ( boolLayoutFlag , srcSpanStartLine ss == srcSpanStartLine ps) of
             (True,  True) -> (colOffset, LayoutLineSame)
             (True, False) -> (colOffset, LayoutLineChanged)
             (False, True) -> (colOffset, LineSame)
             (False,False) -> (colOffset, LineChanged)
  return r
    `debug` ("getCurrentDP:layoutOn=" ++ show layoutOn)

-- ---------------------------------------------------------------------

-- |Note: assumes the prior end SrcSpan stack is nonempty
getPriorEnd :: AP GHC.SrcSpan
getPriorEnd = gets priorEndPosition

setPriorEnd :: GHC.SrcSpan -> AP ()
setPriorEnd pe = modify (\s -> s { priorEndPosition = pe })

-- -------------------------------------

getAnnotationAP :: GHC.AnnKeywordId -> AP [GHC.SrcSpan]
getAnnotationAP an = do
    ga <- gets apAnns
    ss <- getSrcSpanAP
    return $ GHC.getAnnotation ga ss an

getAndRemoveAnnotationAP :: GHC.SrcSpan -> GHC.AnnKeywordId -> AP [GHC.SrcSpan]
getAndRemoveAnnotationAP sp an = do
    ga <- gets apAnns
    let (r,ga') = GHC.getAndRemoveAnnotation ga sp an
    r <$ modify (\s -> s { apAnns = ga' })


tokComment :: GHC.Located GHC.AnnotationComment -> Comment
tokComment t@(GHC.L lt _) = Comment (ss2span lt) (ghcCommentText t)

-- -------------------------------------

-- |Add some annotation to the currently active SrcSpan
addAnnotationsAP :: Annotation -> AP ()
addAnnotationsAP ann = do
    l <- ask
    tellFinalAnn ((getAnnKey l),ann)

getAnnKey :: StackItem -> AnnKey
getAnnKey StackItem {curSrcSpan, annConName} = AnnKey curSrcSpan annConName

-- -------------------------------------

addAnnDeltaPos :: (GHC.SrcSpan,KeywordId) -> DeltaPos -> AP ()
addAnnDeltaPos (_s,kw) dp = tellKd (kw, dp)

-- -------------------------------------

-- | Enter a new AST element. Maintain SrcSpan stack
withAST' :: Data a => GHC.Located a -> LayoutFlag -> AP b -> AP b
withAST' lss layout action = do
  -- Calculate offset required to get to the start of the SrcSPan
  pe <- getPriorEnd
  let ss = (GHC.getLoc lss)
  let edp = deltaFromSrcSpans pe ss
  edp' <- adjustDeltaForOffsetM edp
  -- need to save edp', and put it in Annotation

  withSrcSpanAP lss edp' (do

    let maskWriter s = s { annKds = []
                         , layoutFlag = NoLayoutRules }

    (res, w) <-
      (censor maskWriter (listen action))

    (dp,nl)  <- getCurrentDP (layout <> layoutFlag w)
    finaledp <- getEntryDP
    let kds = annKds w
    addAnnotationsAP (Ann finaledp nl (srcSpanStartColumn ss) dp kds)
      `debug` ("leaveAST:(ss,edp,dp,kds)=" ++ show (showGhc ss,edp,dp,kds,dp))
    return res)

-- ---------------------------------------------------------------------

-- ---------------------------------------------------------------------


-- ---------------------------------------------------------------------

-- |Split the ordered list of comments into ones that occur prior to
-- the given SrcSpan and the rest
allocatePriorComments :: [Comment] -> GHC.SrcSpan -> ([Comment],[Comment])
allocatePriorComments cs ss = partition isPrior cs
  where
    (start,_) = ss2span ss
    isPrior (Comment s _)  = (fst s) < start
      `debug` ("allocatePriorComments:(s,ss,cond)=" ++ showGhc (s,ss,(fst s) < start))

-- ---------------------------------------------------------------------

addAnnotationWorker :: KeywordId -> GHC.SrcSpan -> AP ()
addAnnotationWorker ann pa = do
  if not (isPointSrcSpan pa)
    then do
      pe <- getPriorEnd
      ss <- getSrcSpanAP
      let p = deltaFromSrcSpans pe pa
      case (ann,isGoodDelta p) of
        (G GHC.AnnComma,False) -> return ()
        (G GHC.AnnSemi, False) -> return ()
        (G GHC.AnnOpen, False) -> return ()
        (G GHC.AnnClose,False) -> return ()
        _ -> do
          cs <- getUnallocatedComments
          let (allocated,cs') = allocatePriorComments cs pa
          putUnallocatedComments cs'
          return () `debug`("addAnnotationWorker:(ss,pa,allocated,cs)=" ++ showGhc (ss,pa,allocated,cs))
          mapM_ addDeltaComment allocated
          p' <- adjustDeltaForOffsetM p
          addAnnDeltaPos (ss,ann) p'
          setPriorEnd pa
              -- `debug` ("addDeltaAnnotationWorker:(ss,pe,pa,p,ann)=" ++ show (ss2span ss,ss2span pe,ss2span pa,p,ann))
    else do
      return ()
          -- `debug` ("addDeltaAnnotationWorker::point span:(ss,ma,ann)=" ++ show (ss2span pa,ann))

-- ---------------------------------------------------------------------

addDeltaComment :: Comment -> AP ()
addDeltaComment (Comment paspan str) = do
  let pa = span2ss paspan
  pe <- getPriorEnd
  ss <- getSrcSpanAP
  let p = deltaFromSrcSpans pe pa
  p' <- adjustDeltaForOffsetM p
  setPriorEnd pa
  let e = ss2deltaP (ss2posEnd pe) (snd paspan)
  e' <- adjustDeltaForOffsetM e
  addAnnDeltaPos (ss,AnnComment (DComment (p',e') str)) p'

-- ---------------------------------------------------------------------

-- | Look up and add a Delta annotation at the current position, and
-- advance the position to the end of the annotation
addDeltaAnnotation' :: GHC.AnnKeywordId -> AP ()
addDeltaAnnotation' ann = do
  ss <- getSrcSpanAP
  ma <- getAnnotationAP ann
  case nub ma of -- ++AZ++ TODO: get rid of duplicates earlier
    [] -> return () `debug` ("addDeltaAnnotation empty ma for:" ++ show ann)
    [pa] -> addAnnotationWorker (G ann) pa
    _ -> error $ "addDeltaAnnotation:(ss,ann,ma)=" ++ showGhc (ss,ann,ma)

-- | Look up and add a Delta annotation appearing beyond the current
-- SrcSpan at the current position, and advance the position to the
-- end of the annotation
addDeltaAnnotationAfter' :: GHC.AnnKeywordId -> AP ()
addDeltaAnnotationAfter' ann = do
  ss <- getSrcSpanAP
  ma <- getAnnotationAP ann
  let ma' = filter (\s -> not (GHC.isSubspanOf s ss)) ma
  case ma' of
    [] -> return () `debug` ("addDeltaAnnotation empty ma")
    [pa] -> addAnnotationWorker (G ann) pa
    _ -> error $ "addDeltaAnnotation:(ss,ann,ma)=" ++ showGhc (ss,ann,ma)

-- | Look up and add a Delta annotation at the current position, and
-- advance the position to the end of the annotation
addDeltaAnnotationLs' :: GHC.AnnKeywordId -> Int -> AP ()
addDeltaAnnotationLs' ann off = do
  ma <- getAnnotationAP ann
  case (drop off ma) of
    [] -> return ()
        -- `debug` ("addDeltaAnnotationLs:missed:(off,pe,ann,ma)=" ++ show (off,ss2span pe,ann,fmap ss2span ma))
    (pa:_) -> addAnnotationWorker (G ann) pa

-- | Look up and add possibly multiple Delta annotation at the current
-- position, and advance the position to the end of the annotations
addDeltaAnnotations' :: GHC.AnnKeywordId -> AP ()
addDeltaAnnotations' ann = do
  ma <- getAnnotationAP ann
  let do_one ap' = addAnnotationWorker (G ann) ap'
                    -- `debug` ("addDeltaAnnotations:do_one:(ap',ann)=" ++ showGhc (ap',ann))
  mapM_ do_one (sort ma)

-- | Look up and add possibly multiple Delta annotations enclosed by
-- the current SrcSpan at the current position, and advance the
-- position to the end of the annotations
addDeltaAnnotationsInside' :: GHC.AnnKeywordId -> AP ()
addDeltaAnnotationsInside' ann = do
  ss <- getSrcSpanAP
  ma <- getAnnotationAP ann
  let do_one ap' = addAnnotationWorker (G ann) ap'
                    -- `debug` ("addDeltaAnnotations:do_one:(ap',ann)=" ++ showGhc (ap',ann))
  mapM_ do_one (sort $ filter (\s -> GHC.isSubspanOf s ss) ma)

-- | Look up and add possibly multiple Delta annotations not enclosed by
-- the current SrcSpan at the current position, and advance the
-- position to the end of the annotations
addDeltaAnnotationsOutside' :: GHC.AnnKeywordId -> KeywordId -> AP ()
addDeltaAnnotationsOutside' gann ann = do
  ss <- getSrcSpanAP
  -- ma <- getAnnotationAP ss gann
  ma <- getAndRemoveAnnotationAP ss gann
  let do_one ap' = addAnnotationWorker ann ap'
                    -- `debug` ("addDeltaAnnotations:do_one:(ap',ann)=" ++ showGhc (ap',ann))
  mapM_ do_one (sort $ filter (\s -> not (GHC.isSubspanOf s ss)) ma)

-- | Add a Delta annotation at the current position, and advance the
-- position to the end of the annotation
addDeltaAnnotationExt' :: GHC.SrcSpan -> GHC.AnnKeywordId -> AP ()
addDeltaAnnotationExt' s ann = do
  addAnnotationWorker (G ann) s


addEofAnnotation' :: AP ()
addEofAnnotation' = do
  pe <- getPriorEnd
  ss <- getSrcSpanAP
  ma <- withSrcSpanAP (GHC.noLoc ()) (DP (0,0)) (getAnnotationAP GHC.AnnEofPos)
  case ma of
    [] -> return ()
    (pa:pss) -> do
      cs <- getUnallocatedComments
      mapM_ addDeltaComment cs
      let DP (r,c) = deltaFromSrcSpans pe pa
      addAnnDeltaPos (ss,G GHC.AnnEofPos) (DP (r, c - 1))
      setPriorEnd pa `warn` ("Trailing annotations after Eof: " ++ showGhc pss)


countAnnsAP' :: GHC.AnnKeywordId -> AP Int
countAnnsAP' ann = do
  ma <- getAnnotationAP ann
  return (length ma)

-- ---------------------------------------------------------------------
-- Managing lists which have been separated, e.g. Sigs and Binds


-- ---------------------------------------------------------------------
-- Start of application specific part

-- ---------------------------------------------------------------------

annotateLHsModule :: GHC.Located (GHC.HsModule GHC.RdrName) -> GHC.ApiAnns
                  -> Anns
annotateLHsModule modu@(GHC.L ss _) ghcAnns
   = runAP (annotatePC modu) ghcAnns ss

annotateAST :: GHC.Located (GHC.HsModule GHC.RdrName) -> GHC.ApiAnns -> Anns
annotateAST ast ghcAnns = annotateLHsModule ast ghcAnns

-- ---------------------------------------------------------------------

