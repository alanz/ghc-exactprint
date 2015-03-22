{-# LANGUAGE NamedFieldPuns #-}
module Language.Haskell.GHC.ExactPrint.Delta  (relativiseApiAnns) where

import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.RWS
import Control.Applicative
import Control.Monad.Trans.Free
import Data.Data
import Data.List

import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Utils
import Language.Haskell.GHC.ExactPrint.Annotate (AnnotationF(..), Annotated
                                                , markLocated, Annotate(..))

import qualified GHC            as GHC
import qualified SrcLoc         as GHC


import qualified Data.Map as Map

-- ---------------------------------------------------------------------

-- | Transform concrete annotations into relative annotations which are
-- more useful when transforming an AST.
relativiseApiAnns :: Annotate ast
                  => GHC.Located ast
                  -> GHC.ApiAnns
                  -> Anns
relativiseApiAnns modu@(GHC.L ss _) ghcAnns
   = runDelta (markLocated modu) ghcAnns ss

-- ---------------------------------------------------------------------

data DeltaState = DeltaState
             { priorEndPosition :: GHC.SrcSpan
               -- | Ordered list of comments still to be allocated
             , apComments :: [Comment]
               -- | The original GHC DeltaI Annotations
             , apAnns :: GHC.ApiAnns
             }

data DeltaStack = DeltaStack
               { -- | Current `SrcSpan`
                 curSrcSpan :: GHC.SrcSpan
                 -- | The offset required to get from the prior end point to the
                 -- | The offset required to get from the prior end point to the
                 -- start of the current SrcSpan. Accessed via `getEntryDP`
                 -- | Offsets for the elements annotated in this `SrcSpan`
               -- | Indicates whether the contents of this SrcSpan are
               -- subject to vertical alignment layout rules
                 -- | The constructor name of the AST element, to
                 -- distinguish between nested elements that have the same
                 -- `SrcSpan` in the AST.
               , annConName :: AnnConName
               }

initialDeltaStack :: DeltaStack
initialDeltaStack =
  DeltaStack
    { curSrcSpan = GHC.noSrcSpan
    , annConName = annGetConstr ()
    }

defaultDeltaState :: GHC.SrcSpan -> GHC.ApiAnns -> DeltaState
defaultDeltaState priorEnd ga =
  let cs = flattenedComments ga in
    DeltaState
      { priorEndPosition = priorEnd
      , apComments = cs
      , apAnns     = ga    -- $
      }

data DeltaWriter = DeltaWriter
  { -- Final list of annotations
    finalAnns :: Endo (Map.Map AnnKey Annotation)
    -- Used locally to pass Keywords, delta pairs relevant to a specific
    -- subtree to the parent.
  , annKds :: [(KeywordId, DeltaPos)]
    -- Used locally to report a subtrees aderhence to haskell's layout
    -- rules.
  , layoutFlag :: LayoutFlag
  }


tellFinalAnn :: (AnnKey, Annotation) -> Delta ()
tellFinalAnn (k, v) =
  tell (mempty { finalAnns = Endo (Map.insertWith (<>) k v) })

tellKd :: (KeywordId, DeltaPos) -> Delta ()
tellKd kd = tell (mempty { annKds = [kd] })

setLayoutFlag :: Delta ()
setLayoutFlag = tell (mempty {layoutFlag = LayoutRules})

instance Monoid DeltaWriter where
  mempty = DeltaWriter mempty mempty mempty
  (DeltaWriter a b e) `mappend` (DeltaWriter c d f) = DeltaWriter (a <> c) (b <> d) (e <> f)


-- | Type used in the Delta Monad. The state variables maintain
--    - the current SrcSpan and the constructor of the thing it encloses
--      as a stack to the root of the AST as it is traversed,
--    - the srcspan of the last thing annotated, to calculate delta's from
--    - extra data needing to be stored in the monad
--    - the annotations provided by GHC
type Delta a = RWS DeltaStack DeltaWriter DeltaState a

runDelta :: Annotated () -> GHC.ApiAnns -> GHC.SrcSpan -> Anns
runDelta action ga priorEnd =
  ($ mempty) . appEndo . finalAnns . snd
  . (\next -> execRWS next initialDeltaStack (defaultDeltaState priorEnd ga))
  . simpleInterpret $ action


simpleInterpret :: Annotated a -> Delta a
simpleInterpret = iterTM go
  where
    go :: AnnotationF (Delta a) -> Delta a
    go (MarkEOF next) = addEofAnnotation >> next
    go (MarkPrim kwid _ next) =
      addDeltaAnnotation kwid >> next
    go (MarkOutside akwid kwid next) =
      addDeltaAnnotationsOutside akwid kwid >> next
    go (MarkInside akwid next) =
      addDeltaAnnotationsInside akwid >> next
    go (MarkMany akwid next) = addDeltaAnnotations akwid >> next
    go (MarkOffsetPrim akwid n _ next) = addDeltaAnnotationLs akwid n >> next
    go (MarkAfter akwid next) = addDeltaAnnotationAfter akwid >> next
    go (WithAST lss layoutflag prog next) =
      withAST lss layoutflag (simpleInterpret prog) >>= next
    go (OutputKD (kwid, (_, dp)) next) = tellKd (dp, kwid) >> next
    go (CountAnns kwid next) = countAnnsDelta kwid >>= next
    go (SetLayoutFlag next) = setLayoutFlag >> next
    go (MarkExternal ss akwid _ next) = addDeltaAnnotationExt ss akwid >> next


-- ---------------------------------------------------------------------

flattenedComments :: GHC.ApiAnns -> [Comment]
flattenedComments (_,cm) = map tokComment . GHC.sortLocated . concat $ Map.elems cm

-- -------------------------------------

getSrcSpanDelta :: Delta GHC.SrcSpan
getSrcSpanDelta = asks curSrcSpan

withSrcSpanDelta :: Data a => (GHC.Located a) -> Delta b -> Delta b
withSrcSpanDelta (GHC.L l a) =
  local (\s -> s { curSrcSpan = l
                 , annConName = annGetConstr a
                 })

getUnallocatedComments :: Delta [Comment]
getUnallocatedComments = gets apComments

putUnallocatedComments :: [Comment] -> Delta ()
putUnallocatedComments cs = modify (\s -> s { apComments = cs } )

-- ---------------------------------------------------------------------

adjustDeltaForOffsetM :: DeltaPos -> Delta DeltaPos
adjustDeltaForOffsetM dp = do
  colOffset <- getCurrentColOffset
  return (adjustDeltaForOffset colOffset dp)

adjustDeltaForOffset :: Int -> DeltaPos -> DeltaPos
adjustDeltaForOffset _colOffset dp@(DP (0,_)) = dp -- same line
adjustDeltaForOffset  colOffset    (DP (l,c)) = DP (l,c - colOffset)

-- ---------------------------------------------------------------------

-- | Get the current column offset
getCurrentColOffset :: Delta ColOffset
getCurrentColOffset = srcSpanStartColumn <$> getSrcSpanDelta


-- ---------------------------------------------------------------------

-- |Note: assumes the prior end SrcSpan stack is nonempty
getPriorEnd :: Delta GHC.SrcSpan
getPriorEnd = gets priorEndPosition

setPriorEnd :: GHC.SrcSpan -> Delta ()
setPriorEnd pe = modify (\s -> s { priorEndPosition = pe })

-- -------------------------------------

getAnnotationDelta :: GHC.AnnKeywordId -> Delta [GHC.SrcSpan]
getAnnotationDelta an = do
    ga <- gets apAnns
    ss <- getSrcSpanDelta
    return $ GHC.getAnnotation ga ss an

getAndRemoveAnnotationDelta :: GHC.SrcSpan -> GHC.AnnKeywordId -> Delta [GHC.SrcSpan]
getAndRemoveAnnotationDelta sp an = do
    ga <- gets apAnns
    let (r,ga') = GHC.getAndRemoveAnnotation ga sp an
    r <$ modify (\s -> s { apAnns = ga' })


tokComment :: GHC.Located GHC.AnnotationComment -> Comment
tokComment t@(GHC.L lt _) = Comment (ss2span lt) (ghcCommentText t)

-- -------------------------------------

-- |Add some annotation to the currently active SrcSpan
addAnnotationsDelta :: Annotation -> Delta ()
addAnnotationsDelta ann = do
    l <- ask
    tellFinalAnn ((getAnnKey l),ann)

getAnnKey :: DeltaStack -> AnnKey
getAnnKey DeltaStack {curSrcSpan, annConName} = AnnKey curSrcSpan annConName

-- -------------------------------------

addAnnDeltaPos :: (GHC.SrcSpan,KeywordId) -> DeltaPos -> Delta ()
addAnnDeltaPos (_s,kw) dp = tellKd (kw, dp)

-- -------------------------------------

-- | Enter a new AST element. Maintain SrcSpan stack
withAST :: Data a => GHC.Located a -> LayoutFlag -> Delta b -> Delta b
withAST lss layout action = do
  -- Calculate offset required to get to the start of the SrcSPan
  pe <- getPriorEnd
  let ss = (GHC.getLoc lss)
  edp <- adjustDeltaForOffsetM (deltaFromSrcSpans pe ss)
  prior <- getSrcSpanDelta
  withSrcSpanDelta lss (do

    let maskWriter s = s { annKds = []
                         , layoutFlag = NoLayoutRules }

    (res, w) <-
      (censor maskWriter (listen action))

    let (dp,nl) = getCurrentDP (layout <> layoutFlag w) ss prior
    let kds = annKds w
    addAnnotationsDelta (Ann edp nl (srcSpanStartColumn ss) dp kds)
      `debug` ("leaveAST:(ss,finaledp,dp,nl,kds)=" ++ show (showGhc ss,edp,dp,nl,kds))
    return res)

-- ---------------------------------------------------------------------
-- |Get the difference between the current and the previous
-- colOffsets, if they are on the same line
getCurrentDP :: LayoutFlag -> GHC.SrcSpan -> GHC.SrcSpan -> (ColOffset,LineChanged)
getCurrentDP layoutOn ss ps =
  -- Note: the current col offsets are not needed here, any
  -- indentation should be fully nested in an AST element
  let
      colOffset = if srcSpanStartLine ss == srcSpanStartLine ps
                    then srcSpanStartColumn ss - srcSpanStartColumn ps
                    else srcSpanStartColumn ss
      r = case (layoutOn, srcSpanStartLine ss == srcSpanStartLine ps) of
             (LayoutRules,    True) -> (colOffset, LayoutLineSame)
             (LayoutRules,   False) -> (colOffset, LayoutLineChanged)
             (NoLayoutRules,  True) -> (colOffset, LineSame)
             (NoLayoutRules, False) -> (colOffset, LineChanged)
  in r
    `debug` ("getCurrentDP:(layoutOn=" ++ show layoutOn)

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

addAnnotationWorker :: KeywordId -> GHC.SrcSpan -> Delta ()
addAnnotationWorker ann pa = do
  if not (isPointSrcSpan pa)
    then do
      pe <- getPriorEnd
      ss <- getSrcSpanDelta
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

addDeltaComment :: Comment -> Delta ()
addDeltaComment (Comment paspan str) = do
  let pa = span2ss paspan
  pe <- getPriorEnd
  ss <- getSrcSpanDelta
  let p = deltaFromSrcSpans pe pa
  p' <- adjustDeltaForOffsetM p
  setPriorEnd pa
  let e = ss2deltaP (ss2posEnd pe) (snd paspan)
  e' <- adjustDeltaForOffsetM e
  addAnnDeltaPos (ss,AnnComment (DComment (p',e') str)) p'

-- ---------------------------------------------------------------------

-- | Look up and add a Delta annotation at the current position, and
-- advance the position to the end of the annotation
addDeltaAnnotation :: GHC.AnnKeywordId -> Delta ()
addDeltaAnnotation ann = do
  ss <- getSrcSpanDelta
  when (ann == GHC.AnnVal) (debugM (showGhc ss))
  ma <- getAnnotationDelta ann
  when (ann == GHC.AnnVal && null ma) (debugM "empty")
  case nub ma of -- ++AZ++ TODO: get rid of duplicates earlier
    [] -> return () `debug` ("addDeltaAnnotation empty ma for:" ++ show ann)
    [pa] -> addAnnotationWorker (G ann) pa
    _ -> error $ "addDeltaAnnotation:(ss,ann,ma)=" ++ showGhc (ss,ann,ma)

-- | Look up and add a Delta annotation appearing beyond the current
-- SrcSpan at the current position, and advance the position to the
-- end of the annotation
addDeltaAnnotationAfter :: GHC.AnnKeywordId -> Delta ()
addDeltaAnnotationAfter ann = do
  ss <- getSrcSpanDelta
  ma <- getAnnotationDelta ann
  let ma' = filter (\s -> not (GHC.isSubspanOf s ss)) ma
  case ma' of
    [] -> return () `debug` ("addDeltaAnnotation empty ma")
    [pa] -> addAnnotationWorker (G ann) pa
    _ -> error $ "addDeltaAnnotation:(ss,ann,ma)=" ++ showGhc (ss,ann,ma)

-- | Look up and add a Delta annotation at the current position, and
-- advance the position to the end of the annotation
addDeltaAnnotationLs :: GHC.AnnKeywordId -> Int -> Delta ()
addDeltaAnnotationLs ann off = do
  ma <- getAnnotationDelta ann
  case (drop off ma) of
    [] -> return ()
        -- `debug` ("addDeltaAnnotationLs:missed:(off,pe,ann,ma)=" ++ show (off,ss2span pe,ann,fmap ss2span ma))
    (pa:_) -> addAnnotationWorker (G ann) pa

-- | Look up and add possibly multiple Delta annotation at the current
-- position, and advance the position to the end of the annotations
addDeltaAnnotations :: GHC.AnnKeywordId -> Delta ()
addDeltaAnnotations ann = do
  ma <- getAnnotationDelta ann
  let do_one ap' = addAnnotationWorker (G ann) ap'
                    -- `debug` ("addDeltaAnnotations:do_one:(ap',ann)=" ++ showGhc (ap',ann))
  mapM_ do_one (sort ma)

-- | Look up and add possibly multiple Delta annotations enclosed by
-- the current SrcSpan at the current position, and advance the
-- position to the end of the annotations
addDeltaAnnotationsInside :: GHC.AnnKeywordId -> Delta ()
addDeltaAnnotationsInside ann = do
  ss <- getSrcSpanDelta
  ma <- getAnnotationDelta ann
  let do_one ap' = addAnnotationWorker (G ann) ap'
                    -- `debug` ("addDeltaAnnotations:do_one:(ap',ann)=" ++ showGhc (ap',ann))
  let filtered = (sort $ filter (\s -> GHC.isSubspanOf s ss) ma)
  mapM_ do_one filtered

-- | Look up and add possibly multiple Delta annotations not enclosed by
-- the current SrcSpan at the current position, and advance the
-- position to the end of the annotations
addDeltaAnnotationsOutside :: GHC.AnnKeywordId -> KeywordId -> Delta ()
addDeltaAnnotationsOutside gann ann = do
  ss <- getSrcSpanDelta
  if ss2span ss == ((1,1),(1,1))
    then return ()
    else do
      -- ma <- getAnnotationDelta ss gann
      ma <- getAndRemoveAnnotationDelta ss gann
      let do_one ap' = addAnnotationWorker ann ap'
      mapM_ do_one (sort $ filter (\s -> not (GHC.isSubspanOf s ss)) ma)

-- | Add a Delta annotation at the current position, and advance the
-- position to the end of the annotation
addDeltaAnnotationExt :: GHC.SrcSpan -> GHC.AnnKeywordId -> Delta ()
addDeltaAnnotationExt s ann = do
  addAnnotationWorker (G ann) s


addEofAnnotation :: Delta ()
addEofAnnotation = do
  pe <- getPriorEnd
  ss <- getSrcSpanDelta
  ma <- withSrcSpanDelta (GHC.noLoc ()) (getAnnotationDelta GHC.AnnEofPos)
  case ma of
    [] -> return ()
    (pa:pss) -> do
      cs <- getUnallocatedComments
      mapM_ addDeltaComment cs
      let DP (r,c) = deltaFromSrcSpans pe pa
      addAnnDeltaPos (ss,G GHC.AnnEofPos) (DP (r, c - 1))
      setPriorEnd pa `warn` ("Trailing annotations after Eof: " ++ showGhc pss)


countAnnsDelta :: GHC.AnnKeywordId -> Delta Int
countAnnsDelta ann = do
  ma <- getAnnotationDelta ann
  return (length ma)



