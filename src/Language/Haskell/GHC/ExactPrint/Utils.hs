{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-} -- for GHC.DataId
module Language.Haskell.GHC.ExactPrint.Utils
  (
    annotateLHsModule

  -- , organiseAnns
  -- , OrganisedAnns

  -- , ghcIsComment
  , ghcIsMultiLine

  , srcSpanStartLine
  , srcSpanEndLine
  , srcSpanStartColumn
  , srcSpanEndColumn
  , getLocalBindsSrcSpan

  , ss2span
  , ss2pos
  , ss2posEnd
  , undelta
  , undeltaComment
  , isGoodDelta
  , rdrName2String
  , isSymbolRdrName

  , isListComp

  , showGhc
  , showAnnData

  , merge

  -- * For tests
  , debug

  , runAP
  , AP(..)
  , getSrcSpanAP, pushSrcSpanAP, popSrcSpanAP
  , getAnnotationAP
  , addAnnotationsAP

  , ghead
  , glast
  , gtail
  , gfromJust

  ) where

import Control.Monad ( liftM, ap)
import Control.Exception
import Data.Data
import Data.Generics
import Data.List
import Data.Monoid

import Language.Haskell.GHC.ExactPrint.Types

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

import Debug.Trace

debug :: c -> String -> c
-- debug = flip trace
debug c _ = c

-- ---------------------------------------------------------------------

-- | Type used in the AP Monad. The state variables maintain
--    - the current SrcSpan and the constructor of the thing it encloses
--      as a stack to the root of the AST as it is traversed,
--    - the srcspan of the last thing annotated, to calculate delta's from
--    - extra data needing to be stored in the monad
--    - the annotations provided by GHC

{- -}
newtype AP x = AP ([(GHC.SrcSpan,DeltaPos,DeltaPos,[(KeywordId,DeltaPos)],AnnConName)] -> GHC.SrcSpan -> Extra -> GHC.ApiAnns
            -> (x, [(GHC.SrcSpan,DeltaPos,DeltaPos,[(KeywordId,DeltaPos)],AnnConName)],   GHC.SrcSpan,   Extra,   GHC.ApiAnns,
                   [(AnnKey,AnnValue)]
                 ))


data Extra = Extra
               {
                 e_is_infix :: Bool -- isInfix for a FunBind
                 -- TODO: AZ: Is this ^^  still needed?
               }

data Grouping = None | Primed GHC.SrcSpan | Active DeltaPos | Done DeltaPos
              deriving (Eq,Show)

instance Functor AP where
  fmap = liftM

instance Applicative AP where
  pure = return
  (<*>) = ap

instance Monad AP where
  return x = AP $ \l pe e ga -> (x, l, pe, e, ga, mempty)

  AP m >>= k = AP $ \l0 p0 e0 ga0 -> let
        (a, l1, p1, e1, ga1, s1) = m l0 p0 e0 ga0
        AP f = k a
        (b, l2, p2, e2, ga2, s2) = f l1 p1 e1 ga1
    in (b, l2, p2, e2, ga2, s1 <> s2)


runAP :: AP () -> GHC.ApiAnns -> Anns
runAP (AP f) ga
 = let (_,_,_,_,_,sa) = f [] GHC.noSrcSpan (Extra False) ga
   in (Map.fromListWith combineAnns sa)

combineAnns :: AnnValue -> AnnValue -> AnnValue
combineAnns ((Ann cs1 ed1 dp1),dps1) ((Ann cs2 _ed2 _dp2),dps2)
  = (Ann (cs1 ++ cs2) ed1 dp1,dps1++dps2)

-- -------------------------------------

-- |Note: assumes the SrcSpan stack is nonempty
getSrcSpanAP :: AP GHC.SrcSpan
getSrcSpanAP = AP (\l@((ss,_,_,_,_):_) pe e ga -> (ss,l,pe,e,ga,mempty))

getPriorSrcSpanAP :: AP GHC.SrcSpan
getPriorSrcSpanAP = AP (\l@(_:(ss,_,_,_,_):_) pe e ga -> (ss,l,pe,e,ga,mempty))

pushSrcSpanAP :: Data a => (GHC.Located a) -> DeltaPos -> AP ()
pushSrcSpanAP (GHC.L l a) edp = AP (\ls pe e ga ->
  ((),(l,DP (0,srcSpanStartColumn l),edp,[],annGetConstr a):ls,pe,e,ga,mempty))

popSrcSpanAP :: AP ()
popSrcSpanAP = AP (\(_:ls) pe e ga -> ((),ls,pe,e,ga,mempty))

getEntryDP :: AP DeltaPos
getEntryDP = AP (\l@((_,_,edp,_,_):_) pe e ga
                   -> (edp,l,pe,e,ga,mempty))


-- ---------------------------------------------------------------------

adjustDeltaForOffsetM :: DeltaPos -> AP DeltaPos
adjustDeltaForOffsetM dp = do
  DP (_,colOffset) <- getCurrentColOffset
  return (adjustDeltaForOffset colOffset dp)

adjustDeltaForOffset :: Int -> DeltaPos -> DeltaPos
adjustDeltaForOffset _colOffset dp@(DP (0,_)) = dp -- same line
adjustDeltaForOffset  colOffset    (DP (l,c)) = DP (l,c - colOffset)

-- ---------------------------------------------------------------------

-- | Get the current column offset
getCurrentColOffset :: AP DeltaPos
getCurrentColOffset = AP (\l@((_,o,_,_,_):_) pe e ga
                   -> (o,l,pe,e,ga,mempty))

-- |Get the difference between the current and the previous
-- colOffsets, if they are on the same line
getCurrentDP :: AP ColOffset
getCurrentDP = do
  -- Note: the current col offsets are not needed here, any
  -- indentation should be fully nested in an AST element
  ss <- getSrcSpanAP
  ps <- getPriorSrcSpanAP
  if srcSpanStartLine ss == srcSpanStartLine ps
     then return (srcSpanStartColumn ss - srcSpanStartColumn ps)
     -- else return (srcSpanStartColumn ss - srcSpanStartColumn ps - co)
     else return (srcSpanStartColumn ss - srcSpanStartColumn ps)

-- ---------------------------------------------------------------------

-- |Note: assumes the prior end SrcSpan stack is nonempty
getPriorEnd :: AP GHC.SrcSpan
getPriorEnd = AP (\l pe e ga -> (pe,l,pe,e,ga,mempty))

setPriorEnd :: GHC.SrcSpan -> AP ()
setPriorEnd pe = AP (\ls _ e ga  -> ((),ls,pe,e,ga,mempty))

-- -------------------------------------

getAnnotationAP :: GHC.SrcSpan -> GHC.AnnKeywordId -> AP [GHC.SrcSpan]
getAnnotationAP sp an = AP (\l pe e ga
    -> (GHC.getAnnotation ga sp an, l,pe,e,ga,mempty))


getAndRemoveAnnotationAP :: GHC.SrcSpan -> GHC.AnnKeywordId -> AP [GHC.SrcSpan]
getAndRemoveAnnotationAP sp an = AP (\l pe e ga ->
    let
      (r,ga') = GHC.getAndRemoveAnnotation ga sp an
    in (r, l,pe,e,ga',mempty))

-- -------------------------------------
-- |Retrieve the comments allocated to the current 'SrcSpan', and
-- remove them from the annotations
getAndRemoveAnnotationComments :: GHC.ApiAnns -> GHC.SrcSpan
                               -> ([GHC.Located GHC.AnnotationComment],GHC.ApiAnns)
getAndRemoveAnnotationComments (anns,canns) ss =
  (case Map.lookup ss canns of
    Just cs -> (cs,(anns,Map.delete ss canns))
    Nothing -> ([],(anns,canns)))
     -- `debug` ("getAndRemoveAnnotationComments:ss=" ++ showGhc ss)

----------------------------------------

getCommentsForSpan :: GHC.SrcSpan -> AP [Comment]
getCommentsForSpan s = AP (\l pe e ga ->
  let
    (gcs,ga1) = getAndRemoveAnnotationComments ga s
    cs = reverse $ map tokComment gcs
    tokComment :: GHC.Located GHC.AnnotationComment -> Comment
    tokComment t@(GHC.L lt _) = Comment (ghcIsMultiLine t) (ss2span lt) (ghcCommentText t)
  in (cs,l,pe,e,ga1,mempty)
      -- `debug` ("getCommentsForSpan:(s,cs)" ++ show (showGhc s,cs))
     )

-- -------------------------------------

-- |Add some annotation to the currently active SrcSpan
addAnnotationsAP :: AnnValue -> AP ()
addAnnotationsAP ann = AP (\l pe e ga ->
                       ( (),l,pe,e,ga,
                 [((getAnnKey $ ghead "addAnnotationsAP" l),ann)]))

getAnnKey :: (GHC.SrcSpan,t1,t2,t3,AnnConName) -> AnnKey
getAnnKey (l,_,_,_,t) = (l,t)

-- -------------------------------------

addAnnDeltaPos :: (GHC.SrcSpan,KeywordId) -> DeltaPos -> AP ()
addAnnDeltaPos (_s,kw) dp = AP (\((ss,d1,d2, kd,       cn):ls) pe e ga -> ( (),
                                 ((ss,d1,d2,(kw,dp):kd,cn):ls),pe,e,ga,[]))

{-
addAnnDeltaPos (s,kw) dp = AP (\l pe e ga -> ( (),
                                l,pe,e,ga,
                               ([],
                               [ ((s,kw),[dp]) ])  ))
-}

-- -------------------------------------

getKds :: AP [(KeywordId,DeltaPos)]
getKds = AP (\l@((_ss,_d1,_d2,kd,_cn):_) pe e ga
             -> (reverse kd,l,pe,e,ga,[]))

-- -------------------------------------

setFunIsInfix :: Bool -> AP ()
setFunIsInfix e = AP (\l pe (Extra _) ga -> ((),l,pe,(Extra e),ga,mempty))

getFunIsInfix :: AP Bool
getFunIsInfix = AP (\l pe ex@(Extra e) ga -> (e,l,pe,ex,ga,mempty))

-- -------------------------------------

-- | Enter a new AST element. Maintain SrcSpan stack
enterAST :: Data a => GHC.Located a -> AP ()
enterAST lss = do
  return () `debug` ("enterAST entered for " ++ show (ss2span $ GHC.getLoc lss))
  -- return () `debug` ("enterAST:currentColOffset=" ++ show (DP (0,srcSpanStartColumn $ GHC.getLoc lss)))
  -- Calculate offset required to get to the start of the SrcSPan
  pe <- getPriorEnd
  let p = deltaFromSrcSpans pe (GHC.getLoc lss)
  p' <- adjustDeltaForOffsetM p
  -- need to save p', and put it in Annotation
  pushSrcSpanAP lss p'
  return ()


-- | Pop up the SrcSpan stack, capture the annotations, and work the
-- comments in belonging to the span
-- Assumption: the annotations belong to the immediate sub elements of
-- the AST, hence relate to the current SrcSpan. They can thus be used
-- to decide which comments belong at this level,
-- The assumption is made valid by matching enterAST/leaveAST calls.
leaveAST :: AP ()
leaveAST = do
  -- Automatically add any trailing comma or semi
  addDeltaAnnotationAfter GHC.AnnComma
  ss <- getSrcSpanAP
  if ss2span ss == ((1,1),(1,1))
     then return ()
     else addDeltaAnnotationsOutside GHC.AnnSemi AnnSemiSep

  -- priorEnd <- getPriorEnd

  newCs <- getCommentsForSpan ss
  DP (_,co) <- getCurrentColOffset
  let (lcs,_) = localComments co (ss2span ss) newCs []

  -- let dp = deltaFromSrcSpans priorEnd ss
  dp <- getCurrentDP
  edp <- getEntryDP
  kds <- getKds
  addAnnotationsAP ((Ann lcs edp dp),kds)
    `debug` ("leaveAST:(ss,edp,dp,kds)=" ++ show (showGhc ss,edp,dp,kds,dp))
  popSrcSpanAP
  return () -- `debug` ("leaveAST:(ss,dp,priorEnd)=" ++ show (ss2span ss,dp,ss2span priorEnd))

-- ---------------------------------------------------------------------

class Data ast => AnnotateP ast where
  annotateP :: GHC.SrcSpan -> ast -> AP ()

-- |First move to the given location, then call exactP
annotatePC :: (AnnotateP ast) => GHC.Located ast -> AP ()
annotatePC a@(GHC.L l ast) = do
  enterAST a `debug` ("annotatePC:entering " ++ showGhc l)
  annotateP l ast
  leaveAST `debug` ("annotatePC:leaving " ++ showGhc (l))


annotateMaybe :: (AnnotateP ast) => Maybe (GHC.Located ast) -> AP ()
annotateMaybe Nothing    = return ()
annotateMaybe (Just ast) = annotatePC ast

annotateList :: (AnnotateP ast) => [GHC.Located ast] -> AP ()
annotateList xs = mapM_ annotatePC xs

-- ---------------------------------------------------------------------

isGoodDelta :: DeltaPos -> Bool
isGoodDelta (DP (ro,co)) = ro >= 0 && co >= 0

addFinalComments :: AP ()
addFinalComments = do
  return () -- `debug` ("addFinalComments:entering=")
  cs <- getCommentsForSpan GHC.noSrcSpan
  let (dcs,_) = localComments 1 ((1,1),(1,1)) cs []
  pushSrcSpanAP (GHC.L GHC.noSrcSpan ()) (DP (0,0))
  addAnnotationsAP ((Ann dcs (DP (0,0)) 0),[])
    -- `debug` ("leaveAST:dcs=" ++ show dcs)
  return () -- `debug` ("addFinalComments:dcs=" ++ show dcs)

-- ---------------------------------------------------------------------

addAnnotationWorker :: KeywordId -> GHC.SrcSpan -> AP ()
addAnnotationWorker ann pa = do
  if not (isPointSrcSpan pa)
    then do
      pe <- getPriorEnd
      ss <- getSrcSpanAP
      -- maybeAdjustColOffsetForGrouping pa
      let p = deltaFromSrcSpans pe pa
      case (ann,isGoodDelta p) of
        (G GHC.AnnComma,False) -> return ()
             -- `debug`  ("addDeltaAnnotationWorker::bad delta:(ss,ma,p,ann)=" ++ show (ss2span ss,ss2span pa,p,ann))
        (G GHC.AnnSemi,False) -> return ()
             -- `debug`  ("addDeltaAnnotationWorker::bad delta:(ss,ma,p,ann)=" ++ show (ss2span ss,ss2span pa,p,ann))
        (G GHC.AnnOpen,False) -> return ()
             -- `debug`  ("addDeltaAnnotationWorker::bad delta:(ss,ma,p,ann)=" ++ show (ss2span ss,ss2span pa,p,ann))
        (G GHC.AnnClose,False) -> return ()
             -- `debug`  ("addDeltaAnnotationWorker::bad delta:(ss,ma,p,ann)=" ++ show (ss2span ss,ss2span pa,p,ann))
        _ -> do
          p' <- adjustDeltaForOffsetM p
          addAnnDeltaPos (ss,ann) p'
          setPriorEnd pa
              -- `debug` ("addDeltaAnnotationWorker:(ss,pe,pa,p,ann)=" ++ show (ss2span ss,ss2span pe,ss2span pa,p,ann))
    else do
      return ()
          -- `debug` ("addDeltaAnnotationWorker::point span:(ss,ma,ann)=" ++ show (ss2span pa,ann))


-- | Look up and add a Delta annotation at the current position, and
-- advance the position to the end of the annotation
addDeltaAnnotation :: GHC.AnnKeywordId -> AP ()
addDeltaAnnotation ann = do
  ss <- getSrcSpanAP
  ma <- getAnnotationAP ss ann
  case nub ma of -- ++AZ++ TODO: get rid of duplicates earlier
    [] -> return () `debug` ("addDeltaAnnotation empty ma for:" ++ show ann)
    [pa] -> addAnnotationWorker (G ann) pa
    _ -> error $ "addDeltaAnnotation:(ss,ann,ma)=" ++ showGhc (ss,ann,ma)

-- | Look up and add a Delta annotation appearing beyond the current
-- SrcSpan at the current position, and advance the position to the
-- end of the annotation
addDeltaAnnotationAfter :: GHC.AnnKeywordId -> AP ()
addDeltaAnnotationAfter ann = do
  ss <- getSrcSpanAP
  ma <- getAnnotationAP ss ann
  let ma' = filter (\s -> not (GHC.isSubspanOf s ss)) ma
  case ma' of
    [] -> return () `debug` ("addDeltaAnnotation empty ma")
    [pa] -> addAnnotationWorker (G ann) pa
    _ -> error $ "addDeltaAnnotation:(ss,ann,ma)=" ++ showGhc (ss,ann,ma)

-- | Look up and add a Delta annotation at the current position, and
-- advance the position to the end of the annotation
addDeltaAnnotationLs :: GHC.AnnKeywordId -> Int -> AP ()
addDeltaAnnotationLs ann off = do
  ss <- getSrcSpanAP
  ma <- getAnnotationAP ss ann
  case (drop off ma) of
    [] -> return ()
        -- `debug` ("addDeltaAnnotationLs:missed:(off,pe,ann,ma)=" ++ show (off,ss2span pe,ann,fmap ss2span ma))
    (pa:_) -> addAnnotationWorker (G ann) pa

-- | Look up and add possibly multiple Delta annotation at the current
-- position, and advance the position to the end of the annotations
addDeltaAnnotations :: GHC.AnnKeywordId -> AP ()
addDeltaAnnotations ann = do
  ss <- getSrcSpanAP
  ma <- getAnnotationAP ss ann
  let do_one ap' = addAnnotationWorker (G ann) ap'
                    -- `debug` ("addDeltaAnnotations:do_one:(ap',ann)=" ++ showGhc (ap',ann))
  mapM_ do_one (sort ma)

-- | Look up and add possibly multiple Delta annotations enclosed by
-- the current SrcSpan at the current position, and advance the
-- position to the end of the annotations
addDeltaAnnotationsInside :: GHC.AnnKeywordId -> AP ()
addDeltaAnnotationsInside ann = do
  ss <- getSrcSpanAP
  ma <- getAnnotationAP ss ann
  let do_one ap' = addAnnotationWorker (G ann) ap'
                    -- `debug` ("addDeltaAnnotations:do_one:(ap',ann)=" ++ showGhc (ap',ann))
  mapM_ do_one (sort $ filter (\s -> GHC.isSubspanOf s ss) ma)

-- | Look up and add possibly multiple Delta annotations not enclosed by
-- the current SrcSpan at the current position, and advance the
-- position to the end of the annotations
addDeltaAnnotationsOutside :: GHC.AnnKeywordId -> KeywordId -> AP ()
addDeltaAnnotationsOutside gann ann = do
  ss <- getSrcSpanAP
  -- ma <- getAnnotationAP ss gann
  ma <- getAndRemoveAnnotationAP ss gann
  let do_one ap' = addAnnotationWorker ann ap'
                    -- `debug` ("addDeltaAnnotations:do_one:(ap',ann)=" ++ showGhc (ap',ann))
  mapM_ do_one (sort $ filter (\s -> not (GHC.isSubspanOf s ss)) ma)

-- | Add a Delta annotation at the current position, and advance the
-- position to the end of the annotation
addDeltaAnnotationExt :: GHC.SrcSpan -> GHC.AnnKeywordId -> AP ()
addDeltaAnnotationExt s ann = do
  pe <- getPriorEnd
  ss <- getSrcSpanAP
  -- maybeAdjustColOffsetForGrouping s
  let p = deltaFromSrcSpans pe s
  p' <- adjustDeltaForOffsetM p
  addAnnDeltaPos (ss,G ann) p'
  setPriorEnd s

addEofAnnotation :: AP ()
addEofAnnotation = do
  pe <- getPriorEnd
  ss <- getSrcSpanAP
  ma <- getAnnotationAP GHC.noSrcSpan GHC.AnnEofPos
  case ma of
    [] -> return ()
    [pa] -> do
      let DP (r,c) = deltaFromSrcSpans pe pa
      addAnnDeltaPos (ss,G GHC.AnnEofPos) (DP (r, c - 1))
      setPriorEnd pa

countAnnsAP :: GHC.AnnKeywordId -> AP Int
countAnnsAP ann = do
  ss <- getSrcSpanAP
  ma <- getAnnotationAP ss ann
  return (length ma)

-- ---------------------------------------------------------------------
-- Managing lists which have been separated, e.g. Sigs and Binds

prepareListAnnotation :: AnnotateP a => [GHC.Located a] -> [(GHC.SrcSpan,AP ())]
prepareListAnnotation ls = map (\b@(GHC.L l _) -> (l,annotatePC b)) ls

applyListAnnotations :: [(GHC.SrcSpan,AP ())] -> AP ()
applyListAnnotations ls
  = mapM_ (\(_,b) -> b) $ sortBy (\(a,_) (b,_) -> compare a b) ls

-- ---------------------------------------------------------------------
-- Start of application specific part

-- ---------------------------------------------------------------------

annotateLHsModule :: GHC.Located (GHC.HsModule GHC.RdrName) -> GHC.ApiAnns
                  -> Anns
annotateLHsModule modu ghcAnns
   = runAP (addFinalComments >> annotatePC modu) ghcAnns

-- ---------------------------------------------------------------------

instance AnnotateP (GHC.HsModule GHC.RdrName) where
  annotateP lm (GHC.HsModule mmn mexp imps decs mdepr _haddock) = do
    setPriorEnd lm

    addDeltaAnnotation GHC.AnnModule

    case mmn of
      Nothing -> return ()
      Just (GHC.L ln _) -> addDeltaAnnotationExt ln GHC.AnnVal

    annotateMaybe mdepr

    case mexp of
      Nothing   -> return ()
      Just expr -> annotatePC expr

    addDeltaAnnotation GHC.AnnWhere
    addDeltaAnnotation GHC.AnnOpenC -- Possible '{'
    addDeltaAnnotations GHC.AnnSemi -- possible leading semis
    mapM_ annotatePC imps

    annotateList decs

    addDeltaAnnotation GHC.AnnCloseC -- Possible '}'

    addEofAnnotation


-- ---------------------------------------------------------------------

instance AnnotateP GHC.WarningTxt where
  annotateP _ (GHC.WarningTxt (GHC.L ls _) lss) = do
    addDeltaAnnotationExt ls GHC.AnnOpen
    addDeltaAnnotation GHC.AnnOpenS
    mapM_ annotatePC lss
    addDeltaAnnotation GHC.AnnCloseS
    addDeltaAnnotation GHC.AnnClose

  annotateP _ (GHC.DeprecatedTxt (GHC.L ls _) lss) = do
    addDeltaAnnotationExt ls GHC.AnnOpen
    addDeltaAnnotation GHC.AnnOpenS
    mapM_ annotatePC lss
    addDeltaAnnotation GHC.AnnCloseS
    addDeltaAnnotation GHC.AnnClose

-- ---------------------------------------------------------------------

instance (GHC.DataId name,AnnotateP name)
  => AnnotateP [GHC.LIE name] where
   annotateP _ ls = do
     addDeltaAnnotation GHC.AnnHiding -- in an import decl
     addDeltaAnnotation GHC.AnnOpenP -- '('
     mapM_ annotatePC ls
     addDeltaAnnotation GHC.AnnCloseP -- ')'

instance (GHC.DataId name,AnnotateP name)
  => AnnotateP (GHC.IE name) where
  annotateP _ ie = do

    case ie of
        (GHC.IEVar ln) -> do
          addDeltaAnnotation GHC.AnnPattern
          addDeltaAnnotation GHC.AnnType
          annotatePC ln

        (GHC.IEThingAbs ln) -> do
          addDeltaAnnotation GHC.AnnType
          annotatePC ln

        (GHC.IEThingWith ln ns) -> do
          annotatePC ln
          addDeltaAnnotation GHC.AnnOpenP
          mapM_ annotatePC ns
          addDeltaAnnotation GHC.AnnCloseP

        (GHC.IEThingAll ln) -> do
          annotatePC ln
          addDeltaAnnotation GHC.AnnOpenP
          addDeltaAnnotation GHC.AnnDotdot
          addDeltaAnnotation GHC.AnnCloseP

        (GHC.IEModuleContents (GHC.L lm _n)) -> do
          addDeltaAnnotation GHC.AnnModule
          addDeltaAnnotationExt lm GHC.AnnVal


-- ---------------------------------------------------------------------

instance AnnotateP GHC.RdrName where
  annotateP l n = do
    case rdrName2String n of
      "[]" -> do
        addDeltaAnnotation GHC.AnnOpenS  -- '['
        addDeltaAnnotation GHC.AnnCloseS -- ']'
      "()" -> do
        addDeltaAnnotation GHC.AnnOpenP  -- '('
        addDeltaAnnotation GHC.AnnCloseP -- ')'
      "(##)" -> do
        addDeltaAnnotation GHC.AnnOpen  -- '(#'
        addDeltaAnnotation GHC.AnnClose -- '#)'
      "[::]" -> do
        addDeltaAnnotation GHC.AnnOpen  -- '[:'
        addDeltaAnnotation GHC.AnnClose -- ':]'
      _ ->  do
        addDeltaAnnotation GHC.AnnType
        addDeltaAnnotation GHC.AnnOpenP -- '('
        addDeltaAnnotationLs GHC.AnnBackquote 0
        addDeltaAnnotations GHC.AnnCommaTuple -- For '(,,,)'
        cnt <- countAnnsAP GHC.AnnVal
        cntT <- countAnnsAP GHC.AnnCommaTuple
        cntR <- countAnnsAP GHC.AnnRarrow
        case cnt of
          0 -> if cntT >0 || cntR >0 then return () else addDeltaAnnotationExt l GHC.AnnVal
          1 -> addDeltaAnnotation GHC.AnnVal
          x -> error $ "annotateP.RdrName: too many AnnVal :" ++ showGhc (l,x)
        addDeltaAnnotation GHC.AnnTildehsh
        addDeltaAnnotation GHC.AnnTilde
        addDeltaAnnotation GHC.AnnRarrow
        addDeltaAnnotationLs GHC.AnnBackquote 1
        addDeltaAnnotation GHC.AnnCloseP -- ')'

-- ---------------------------------------------------------------------

instance AnnotateP GHC.Name where
  annotateP l _n = do
    addDeltaAnnotationExt l GHC.AnnVal

-- ---------------------------------------------------------------------

instance (GHC.DataId name,AnnotateP name)
  => AnnotateP (GHC.ImportDecl name) where
 annotateP _ (GHC.ImportDecl _msrc (GHC.L ln _) _pkg _src _safe _qual _impl _as hiding) = do

   -- 'import' maybe_src maybe_safe optqualified maybe_pkg modid maybeas maybeimpspec
   addDeltaAnnotation GHC.AnnImport

   -- "{-# SOURCE" and "#-}"
   addDeltaAnnotation GHC.AnnOpen
   addDeltaAnnotation GHC.AnnClose
   addDeltaAnnotation GHC.AnnSafe
   addDeltaAnnotation GHC.AnnQualified
   addDeltaAnnotation GHC.AnnPackageName

   addDeltaAnnotationExt ln GHC.AnnVal -- modid

   addDeltaAnnotation GHC.AnnAs
   addDeltaAnnotation GHC.AnnVal -- as modid

   case hiding of
     Nothing -> return ()
     Just (_isHiding,lie) -> do
       addDeltaAnnotation GHC.AnnHiding
       annotatePC lie

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateP name)
  => AnnotateP (GHC.HsDecl name) where
  annotateP l decl = do
    case decl of
      GHC.TyClD d       -> annotateP l d
      GHC.InstD d       -> annotateP l d
      GHC.DerivD d      -> annotateP l d
      GHC.ValD d        -> annotateP l d
      GHC.SigD d        -> annotateP l d
      GHC.DefD d        -> annotateP l d
      GHC.ForD d        -> annotateP l d
      GHC.WarningD d    -> annotateP l d
      GHC.AnnD d        -> annotateP l d
      GHC.RuleD d       -> annotateP l d
      GHC.VectD d       -> annotateP l d
      GHC.SpliceD d     -> annotateP l d
      GHC.DocD d        -> annotateP l d
      GHC.QuasiQuoteD d -> annotateP l d
      GHC.RoleAnnotD d  -> annotateP l d

-- ---------------------------------------------------------------------

instance (AnnotateP name)
   => AnnotateP (GHC.RoleAnnotDecl name) where
  annotateP _ (GHC.RoleAnnotDecl ln mr) = do
    addDeltaAnnotation GHC.AnnType
    addDeltaAnnotation GHC.AnnRole
    annotatePC ln
    mapM_ annotatePC mr

instance AnnotateP (Maybe GHC.Role) where
  annotateP l _ = addDeltaAnnotationExt l GHC.AnnVal

-- ---------------------------------------------------------------------

instance (AnnotateP name)
   => AnnotateP (GHC.HsQuasiQuote name) where
  annotateP _ (GHC.HsQuasiQuote _n _ss _fs) = assert False undefined

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateP name)
   => AnnotateP (GHC.SpliceDecl name) where
  annotateP _ (GHC.SpliceDecl (GHC.L _ls (GHC.HsSplice _n e)) _flag) = do
    addDeltaAnnotation GHC.AnnOpen -- "$(" or "$$("
    annotatePC e
    addDeltaAnnotation GHC.AnnClose -- ")"

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateP name)
   => AnnotateP (GHC.VectDecl name) where
  annotateP _ (GHC.HsVect _src ln e) = do
    addDeltaAnnotation GHC.AnnOpen -- "{-# VECTORISE"
    annotatePC ln
    addDeltaAnnotation GHC.AnnEqual
    annotatePC e
    addDeltaAnnotation GHC.AnnClose -- "#-}"

  annotateP _ (GHC.HsNoVect _src ln) = do
    addDeltaAnnotation GHC.AnnOpen -- "{-# NOVECTORISE"
    annotatePC ln
    addDeltaAnnotation GHC.AnnClose -- "#-}"

  annotateP _ (GHC.HsVectTypeIn _src _b ln mln) = do
    addDeltaAnnotation GHC.AnnOpen -- "{-# VECTORISE" or "{-# VECTORISE SCALAR"
    addDeltaAnnotation GHC.AnnType
    annotatePC ln
    addDeltaAnnotation GHC.AnnEqual
    annotateMaybe mln
    addDeltaAnnotation GHC.AnnClose -- "#-}"

  annotateP _ (GHC.HsVectTypeOut {}) = error $ "annotateP.HsVectTypeOut: only valid after type checker"

  annotateP _ (GHC.HsVectClassIn _src ln) = do
    addDeltaAnnotation GHC.AnnOpen -- "{-# VECTORISE"
    addDeltaAnnotation GHC.AnnClass
    annotatePC ln
    addDeltaAnnotation GHC.AnnClose -- "#-}"

  annotateP _ (GHC.HsVectClassOut {}) = error $ "annotateP.HsVectClassOut: only valid after type checker"
  annotateP _ (GHC.HsVectInstIn {})   = error $ "annotateP.HsVectInstIn: not supported?"
  annotateP _ (GHC.HsVectInstOut {})   = error $ "annotateP.HsVectInstOut: not supported?"

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateP name)
   => AnnotateP (GHC.RuleDecls name) where
   annotateP _ (GHC.HsRules _src rules) = do
     addDeltaAnnotation GHC.AnnOpen
     mapM_ annotatePC rules
     addDeltaAnnotation GHC.AnnClose

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateP name)
   => AnnotateP (GHC.RuleDecl name) where
  annotateP _ (GHC.HsRule ln _act bndrs lhs _ rhs _) = do
    annotatePC ln
    -- activation
    addDeltaAnnotation GHC.AnnOpenS -- "["
    addDeltaAnnotation GHC.AnnTilde
    addDeltaAnnotation GHC.AnnVal
    addDeltaAnnotation GHC.AnnCloseS -- "]"

    addDeltaAnnotation GHC.AnnForall
    mapM_ annotatePC bndrs
    addDeltaAnnotation GHC.AnnDot

    annotatePC lhs
    addDeltaAnnotation GHC.AnnEqual
    annotatePC rhs

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateP name)
   => AnnotateP (GHC.RuleBndr name) where
  annotateP _ (GHC.RuleBndr ln) = annotatePC ln
  annotateP _ (GHC.RuleBndrSig ln (GHC.HsWB thing _ _ _)) = do
    addDeltaAnnotation GHC.AnnOpenP -- "("
    annotatePC ln
    addDeltaAnnotation GHC.AnnDcolon
    annotatePC thing
    addDeltaAnnotation GHC.AnnCloseP -- ")"

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateP name)
   => AnnotateP (GHC.AnnDecl name) where
   annotateP _ (GHC.HsAnnotation _src prov e) = do
     addDeltaAnnotation GHC.AnnOpen -- "{-# Ann"
     addDeltaAnnotation GHC.AnnType
     addDeltaAnnotation GHC.AnnModule
     case prov of
       (GHC.ValueAnnProvenance n) -> annotatePC n
       (GHC.TypeAnnProvenance n) -> annotatePC n
       (GHC.ModuleAnnProvenance) -> return ()

     annotatePC e
     addDeltaAnnotation GHC.AnnClose

-- ---------------------------------------------------------------------

instance AnnotateP name => AnnotateP (GHC.WarnDecls name) where
   annotateP _ (GHC.Warnings _src warns) = do
     addDeltaAnnotation GHC.AnnOpen
     mapM_ annotatePC warns
     addDeltaAnnotation GHC.AnnClose

-- ---------------------------------------------------------------------

instance (AnnotateP name)
   => AnnotateP (GHC.WarnDecl name) where
   annotateP _ (GHC.Warning lns txt) = do
     mapM_ annotatePC lns
     addDeltaAnnotation GHC.AnnOpenS -- "["
     case txt of
       GHC.WarningTxt    _src ls -> mapM_ annotatePC ls
       GHC.DeprecatedTxt _src ls -> mapM_ annotatePC ls
     addDeltaAnnotation GHC.AnnCloseS -- "]"

instance AnnotateP GHC.FastString where
  annotateP l _ = addDeltaAnnotationExt l GHC.AnnVal

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateP name)
   => AnnotateP (GHC.ForeignDecl name) where

  annotateP _ (GHC.ForeignImport ln typ _
               (GHC.CImport cconv safety@(GHC.L ll _) _mh _imp (GHC.L ls _src))) = do
    addDeltaAnnotation GHC.AnnForeign
    addDeltaAnnotation GHC.AnnImport
    annotatePC cconv
    if ll == GHC.noSrcSpan
      then return ()
      else annotatePC safety
    -- annotateMaybe mh
    addDeltaAnnotationExt ls GHC.AnnVal
    annotatePC ln
    addDeltaAnnotation GHC.AnnDcolon
    annotatePC typ


  annotateP _l (GHC.ForeignExport ln typ _ (GHC.CExport spec (GHC.L ls _src))) = do
    addDeltaAnnotation GHC.AnnForeign
    addDeltaAnnotation GHC.AnnExport
    annotatePC spec
    addDeltaAnnotationExt ls GHC.AnnVal
    annotatePC ln
    addDeltaAnnotation GHC.AnnDcolon
    annotatePC typ


-- ---------------------------------------------------------------------

instance (AnnotateP GHC.CExportSpec) where
  annotateP l (GHC.CExportStatic _ cconv) = annotateP l cconv

-- ---------------------------------------------------------------------

instance (AnnotateP GHC.CCallConv) where
  annotateP l _ = addDeltaAnnotationExt l GHC.AnnVal

-- ---------------------------------------------------------------------

instance (AnnotateP GHC.Safety) where
  annotateP l _ = addDeltaAnnotationExt l GHC.AnnVal

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateP name)
   => AnnotateP (GHC.DerivDecl name) where

  annotateP _ (GHC.DerivDecl typ mov) = do
    addDeltaAnnotation GHC.AnnDeriving
    addDeltaAnnotation GHC.AnnInstance
    annotateMaybe mov
    annotatePC typ

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateP name)
   => AnnotateP (GHC.DefaultDecl name) where

  annotateP _ (GHC.DefaultDecl typs) = do
    addDeltaAnnotation GHC.AnnDefault
    addDeltaAnnotation GHC.AnnOpenP -- '('
    mapM_ annotatePC typs
    addDeltaAnnotation GHC.AnnCloseP -- ')'

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateP name)
   => AnnotateP (GHC.InstDecl name) where

  annotateP l (GHC.ClsInstD      cid) = annotateP l  cid
  annotateP l (GHC.DataFamInstD dfid) = annotateP l dfid
  annotateP l (GHC.TyFamInstD   tfid) = annotateP l tfid

-- ---------------------------------------------------------------------

instance AnnotateP (GHC.OverlapMode) where
  annotateP _ _ = do
    addDeltaAnnotation GHC.AnnOpen
    addDeltaAnnotation GHC.AnnClose

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateP name)
   => AnnotateP (GHC.ClsInstDecl name) where

  annotateP _ (GHC.ClsInstDecl poly binds sigs tyfams datafams mov) = do
    addDeltaAnnotation GHC.AnnInstance
    annotateMaybe mov
    annotatePC poly
    addDeltaAnnotation GHC.AnnWhere
    addDeltaAnnotation GHC.AnnOpenC -- '{'
    addDeltaAnnotationsInside GHC.AnnSemi

    -- must merge all the rest
    applyListAnnotations (prepareListAnnotation (GHC.bagToList binds)
                       ++ prepareListAnnotation sigs
                       ++ prepareListAnnotation tyfams
                       ++ prepareListAnnotation datafams
                         )

    addDeltaAnnotation GHC.AnnCloseC -- '}'

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateP name)
   => AnnotateP (GHC.TyFamInstDecl name) where

  annotateP _ (GHC.TyFamInstDecl eqn _) = do
    addDeltaAnnotation GHC.AnnType
    addDeltaAnnotation GHC.AnnInstance
    annotatePC eqn

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateP name)
   => AnnotateP (GHC.DataFamInstDecl name) where

  annotateP l (GHC.DataFamInstDecl ln (GHC.HsWB pats _ _ _) defn _) = do
    addDeltaAnnotation GHC.AnnData
    addDeltaAnnotation GHC.AnnNewtype
    addDeltaAnnotation GHC.AnnInstance
    annotatePC ln
    mapM_ annotatePC pats
    addDeltaAnnotation GHC.AnnWhere
    addDeltaAnnotation GHC.AnnEqual
    annotateDataDefn l defn

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateP name) =>
                                                  AnnotateP (GHC.HsBind name) where
  annotateP _ (GHC.FunBind (GHC.L _ln _n) isInfix (GHC.MG matches _ _ _) _ _ _) = do
    setFunIsInfix isInfix
    mapM_ annotatePC matches

  annotateP _ (GHC.PatBind lhs (GHC.GRHSs grhs lb) _typ _fvs _ticks) = do
    annotatePC lhs
    addDeltaAnnotation GHC.AnnEqual
    mapM_ annotatePC grhs
    addDeltaAnnotation GHC.AnnWhere
    -- annotateHsLocalBinds lb
    annotatePC (GHC.L (getLocalBindsSrcSpan lb) lb)

  annotateP _ (GHC.VarBind _n rhse _) = do
    -- Note: this bind is introduced by the typechecker
    annotatePC rhse

  annotateP _ (GHC.PatSynBind (GHC.PSB ln _fvs args def dir)) = do
    addDeltaAnnotation GHC.AnnPattern
    annotatePC ln
    case args of
      GHC.InfixPatSyn la lb -> do
        annotatePC la
        annotatePC lb
      GHC.PrefixPatSyn ns -> do
        mapM_ annotatePC ns
    addDeltaAnnotation GHC.AnnEqual
    addDeltaAnnotation GHC.AnnLarrow
    annotatePC def
    case dir of
      GHC.Unidirectional           -> return ()
      GHC.ImplicitBidirectional    -> return ()
      GHC.ExplicitBidirectional mg -> annotateMatchGroup mg

    addDeltaAnnotation GHC.AnnWhere
    addDeltaAnnotation GHC.AnnOpenC  -- '{'
    addDeltaAnnotation GHC.AnnCloseC -- '}'

    return ()

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateP name)
    => AnnotateP (GHC.IPBind name) where
  annotateP _ (GHC.IPBind en e) = do
    case en of
      Left n -> annotatePC n
      Right _i -> error $ "annotateP.IPBind:should not happen"
    addDeltaAnnotation GHC.AnnEqual
    annotatePC e

-- ---------------------------------------------------------------------

instance AnnotateP GHC.HsIPName where
  annotateP l _ = addDeltaAnnotationExt l GHC.AnnVal

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateP name,
                                                  AnnotateP body)
  => AnnotateP (GHC.Match name (GHC.Located body)) where

  annotateP _ (GHC.Match mln pats _typ (GHC.GRHSs grhs lb)) = do
    isInfix <- getFunIsInfix
    let
      get_infix Nothing = isInfix
      get_infix (Just (_,f)) = f
    case (get_infix mln,pats) of
      (True,[a,b]) -> do
        annotatePC a
        case mln of
          Nothing -> do
            addDeltaAnnotation GHC.AnnOpen -- possible '`'
            addDeltaAnnotation GHC.AnnFunId
            addDeltaAnnotation GHC.AnnClose -- possible '`'
          Just (n,_) -> annotatePC n
        annotatePC b
      _ -> do
        case mln of
          Nothing -> addDeltaAnnotation GHC.AnnFunId
          Just (n,_) -> annotatePC n
        mapM_ annotatePC pats

    addDeltaAnnotation GHC.AnnEqual
    addDeltaAnnotation GHC.AnnRarrow -- For HsLam

    mapM_ annotatePC grhs

    addDeltaAnnotation GHC.AnnWhere
    addDeltaAnnotation GHC.AnnOpenC -- '{'
    addDeltaAnnotationsInside GHC.AnnSemi
    -- annotateHsLocalBinds lb
    annotatePC (GHC.L (getLocalBindsSrcSpan lb) lb)
    addDeltaAnnotation GHC.AnnCloseC -- '}'

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateP name,
                                                  AnnotateP body)
  => AnnotateP (GHC.GRHS name (GHC.Located body)) where
  annotateP _ (GHC.GRHS guards expr) = do

    addDeltaAnnotation GHC.AnnVbar
    mapM_ annotatePC guards
    addDeltaAnnotation GHC.AnnEqual
    addDeltaAnnotation GHC.AnnRarrow -- in case alts
    annotatePC expr

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateP name)
  => AnnotateP (GHC.Sig name) where

  annotateP _ (GHC.TypeSig lns typ _) = do
    mapM_ annotatePC lns
    addDeltaAnnotation GHC.AnnDcolon
    annotatePC typ

  annotateP _ (GHC.PatSynSig ln (_,GHC.HsQTvs _ns bndrs) ctx1 ctx2 typ) = do
    addDeltaAnnotation GHC.AnnPattern
    annotatePC ln
    addDeltaAnnotation GHC.AnnDcolon

    -- Note: The 'forall' bndrs '.' may occur multiple times
    addDeltaAnnotation GHC.AnnForall
    mapM_ annotatePC bndrs
    addDeltaAnnotation GHC.AnnDot

    annotatePC ctx1
    addDeltaAnnotationLs GHC.AnnDarrow 0
    annotatePC ctx2
    addDeltaAnnotationLs GHC.AnnDarrow 1
    annotatePC typ


  annotateP _ (GHC.GenericSig ns typ) = do
    addDeltaAnnotation GHC.AnnDefault
    mapM_ annotatePC ns
    addDeltaAnnotation GHC.AnnDcolon
    annotatePC typ

  annotateP _ (GHC.IdSig _) = return ()

  -- FixSig (FixitySig name)
  annotateP _ (GHC.FixSig (GHC.FixitySig lns (GHC.Fixity _v _fdir))) = do
    addDeltaAnnotation GHC.AnnInfix
    addDeltaAnnotation GHC.AnnVal
    mapM_ annotatePC lns

  -- InlineSig (Located name) InlinePragma
  -- '{-# INLINE' activation qvar '#-}'
  annotateP _ (GHC.InlineSig ln _inl) = do
    addDeltaAnnotation GHC.AnnOpen   -- '{-# INLINE'
    addDeltaAnnotation GHC.AnnOpenS  -- '['
    addDeltaAnnotation  GHC.AnnTilde -- ~
    addDeltaAnnotation  GHC.AnnVal   -- e.g. 34
    addDeltaAnnotation GHC.AnnCloseS -- ']'
    annotatePC ln
    addDeltaAnnotation GHC.AnnClose -- '#-}'


  annotateP _ (GHC.SpecSig ln typs _inl) = do
    addDeltaAnnotation GHC.AnnOpen  -- '{-# SPECIALISE'
    addDeltaAnnotation GHC.AnnOpenS --  '['
    addDeltaAnnotation GHC.AnnTilde -- ~
    addDeltaAnnotation GHC.AnnVal   -- e.g. 34

    addDeltaAnnotation GHC.AnnCloseS -- ']'
    annotatePC ln
    addDeltaAnnotation GHC.AnnDcolon -- '::'
    mapM_ annotatePC typs
    addDeltaAnnotation GHC.AnnClose -- '#-}'


  -- '{-# SPECIALISE' 'instance' inst_type '#-}'
  annotateP _ (GHC.SpecInstSig _ typ) = do
    addDeltaAnnotation GHC.AnnOpen -- '{-# SPECIALISE'
    addDeltaAnnotation GHC.AnnInstance
    annotatePC typ
    addDeltaAnnotation GHC.AnnClose -- '#-}'


  -- MinimalSig (BooleanFormula (Located name))
  annotateP _ (GHC.MinimalSig _ formula) = do
    addDeltaAnnotation GHC.AnnOpen -- '{-# MINIMAL'
    annotateBooleanFormula formula
    addDeltaAnnotation GHC.AnnClose -- '#-}'


-- ---------------------------------------------------------------------

annotateBooleanFormula :: GHC.BooleanFormula (GHC.Located name) -> AP ()
annotateBooleanFormula = assert False undefined

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateP name) =>
                     AnnotateP (GHC.HsTyVarBndr name) where
  annotateP l (GHC.UserTyVar _n) = do
    addDeltaAnnotationExt l GHC.AnnVal

  annotateP _ (GHC.KindedTyVar n ty) = do
    addDeltaAnnotation GHC.AnnOpenP  -- '('
    annotatePC n
    addDeltaAnnotation GHC.AnnDcolon -- '::'
    annotatePC ty
    addDeltaAnnotation GHC.AnnCloseP -- '('

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateP name)
   => AnnotateP (GHC.HsType name) where

  annotateP _ (GHC.HsForAllTy _f mwc (GHC.HsQTvs _kvs tvs) ctx@(GHC.L lc ctxs) typ) = do
    addDeltaAnnotation GHC.AnnOpenP -- "("
    addDeltaAnnotation GHC.AnnForall
    mapM_ annotatePC tvs
    addDeltaAnnotation GHC.AnnDot

    case mwc of
      Nothing -> if lc /= GHC.noSrcSpan then annotatePC ctx else return ()
      Just lwc -> annotatePC (GHC.L lc (GHC.sortLocated ((GHC.L lwc GHC.HsWildcardTy):ctxs)))

    addDeltaAnnotation GHC.AnnDarrow
    annotatePC typ
    addDeltaAnnotation GHC.AnnCloseP -- ")"

  annotateP l (GHC.HsTyVar n) = do
    addDeltaAnnotation GHC.AnnDcolon -- for HsKind, alias for HsType
    annotateP l n

  annotateP _ (GHC.HsAppTy t1 t2) = do
    addDeltaAnnotation GHC.AnnDcolon -- for HsKind, alias for HsType
    annotatePC t1
    annotatePC t2

  annotateP _ (GHC.HsFunTy t1 t2) = do
    addDeltaAnnotation GHC.AnnDcolon -- for HsKind, alias for HsType
    annotatePC t1
    addDeltaAnnotation GHC.AnnRarrow
    annotatePC t2

  annotateP _ (GHC.HsListTy t) = do
    addDeltaAnnotation GHC.AnnDcolon -- for HsKind, alias for HsType
    addDeltaAnnotation GHC.AnnOpenS -- '['
    annotatePC t
    addDeltaAnnotation GHC.AnnCloseS -- ']'

  annotateP _ (GHC.HsPArrTy t) = do
    addDeltaAnnotation GHC.AnnOpen  -- '[:'
    annotatePC t
    addDeltaAnnotation GHC.AnnClose -- ':]'

  annotateP _ (GHC.HsTupleTy _tt ts) = do
    addDeltaAnnotation GHC.AnnDcolon -- for HsKind, alias for HsType
    addDeltaAnnotation GHC.AnnOpen  -- '(#'
    addDeltaAnnotation GHC.AnnOpenP  -- '('
    mapM_ annotatePC ts
    addDeltaAnnotation GHC.AnnCloseP -- ')'
    addDeltaAnnotation GHC.AnnClose --  '#)'

  annotateP _ (GHC.HsOpTy t1 (_,lo) t2) = do
    annotatePC t1
    annotatePC lo
    annotatePC t2

  annotateP _ (GHC.HsParTy t) = do
    addDeltaAnnotation GHC.AnnDcolon -- for HsKind, alias for HsType
    addDeltaAnnotation GHC.AnnOpenP  -- '('
    annotatePC t
    addDeltaAnnotation GHC.AnnCloseP -- ')'

  annotateP _ (GHC.HsIParamTy _n t) = do
    addDeltaAnnotation GHC.AnnVal
    addDeltaAnnotation GHC.AnnDcolon
    annotatePC t

  annotateP _ (GHC.HsEqTy t1 t2) = do
    annotatePC t1
    addDeltaAnnotation GHC.AnnTilde
    annotatePC t2

  annotateP _ (GHC.HsKindSig t k) = do
    addDeltaAnnotation GHC.AnnOpenP  -- '('
    annotatePC t
    addDeltaAnnotation GHC.AnnDcolon -- '::'
    annotatePC k
    addDeltaAnnotation GHC.AnnCloseP -- ')'

  -- HsQuasiQuoteTy (HsQuasiQuote name)
  annotateP l (GHC.HsQuasiQuoteTy _qq) = do
    addDeltaAnnotationExt l GHC.AnnVal

  -- HsSpliceTy (HsSplice name) (PostTc name Kind)
  annotateP _ (GHC.HsSpliceTy (GHC.HsSplice _is e) _) = do
    addDeltaAnnotation GHC.AnnOpen  -- '$('
    annotatePC e
    addDeltaAnnotation GHC.AnnClose -- ')'

  annotateP _ (GHC.HsDocTy t ds) = do
    annotatePC t
    annotatePC ds

  annotateP _ (GHC.HsBangTy _b t) = do
    addDeltaAnnotation GHC.AnnOpen  -- '{-# UNPACK' or '{-# NOUNPACK'
    addDeltaAnnotation GHC.AnnClose -- '#-}'
    addDeltaAnnotation GHC.AnnBang  -- '!'
    annotatePC t

  -- HsRecTy [LConDeclField name]
  annotateP _ (GHC.HsRecTy cons) = do
    addDeltaAnnotation GHC.AnnOpenC  -- '{'
    mapM_ annotatePC cons
    addDeltaAnnotation GHC.AnnCloseC -- '}'

  -- HsCoreTy Type
  annotateP _ (GHC.HsCoreTy _t) = return ()

  annotateP _ (GHC.HsExplicitListTy _ ts) = do
    -- TODO: what about SIMPLEQUOTE?
    addDeltaAnnotation GHC.AnnOpen  -- "'["
    mapM_ annotatePC ts
    addDeltaAnnotation GHC.AnnCloseS -- ']'

  annotateP _ (GHC.HsExplicitTupleTy _ ts) = do
    addDeltaAnnotation GHC.AnnOpen  -- "'("
    mapM_ annotatePC ts
    addDeltaAnnotation GHC.AnnClose -- ')'

  -- HsTyLit HsTyLit
  annotateP l (GHC.HsTyLit _tl) = do
    addDeltaAnnotationExt l GHC.AnnVal

  -- HsWrapTy HsTyWrapper (HsType name)
  annotateP _ (GHC.HsWrapTy _ _) = return ()

  annotateP l (GHC.HsWildcardTy) = do
    addDeltaAnnotationExt l GHC.AnnVal
    addDeltaAnnotation GHC.AnnDarrow -- if only part of a partial type signature context

  annotateP l (GHC.HsNamedWildcardTy _n) = do
    addDeltaAnnotationExt l GHC.AnnVal

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateP name) =>
                             AnnotateP (GHC.ConDeclField name) where
  annotateP _ (GHC.ConDeclField ns ty mdoc) = do
    mapM_ annotatePC ns
    addDeltaAnnotation GHC.AnnDcolon
    annotatePC ty
    annotateMaybe mdoc

-- ---------------------------------------------------------------------

instance AnnotateP GHC.HsDocString where
  annotateP l (GHC.HsDocString _s) = do
    addDeltaAnnotationExt l GHC.AnnVal

-- ---------------------------------------------------------------------

instance (GHC.DataId name,AnnotateP name,GHC.OutputableBndr name)
  => AnnotateP (GHC.Pat name) where
  annotateP l (GHC.WildPat _) = addDeltaAnnotationExt l GHC.AnnVal
  annotateP l (GHC.VarPat _)  = addDeltaAnnotationExt l GHC.AnnVal
  annotateP _ (GHC.LazyPat p) = do
    addDeltaAnnotation GHC.AnnTilde
    annotatePC p

  annotateP _ (GHC.AsPat ln p) = do
    annotatePC ln
    addDeltaAnnotation GHC.AnnAt
    annotatePC p

  annotateP _ (GHC.ParPat p) = do
    addDeltaAnnotation GHC.AnnOpenP
    annotatePC p
    addDeltaAnnotation GHC.AnnCloseP

  annotateP _ (GHC.BangPat p) = do
    addDeltaAnnotation GHC.AnnBang
    annotatePC p

  annotateP _ (GHC.ListPat ps _ _) = do
    addDeltaAnnotation GHC.AnnOpenS
    mapM_ annotatePC ps
    addDeltaAnnotation GHC.AnnCloseS

  annotateP _ (GHC.TuplePat ps _ _) = do
    addDeltaAnnotation GHC.AnnOpen
    addDeltaAnnotation GHC.AnnOpenP
    mapM_ annotatePC ps
    addDeltaAnnotation GHC.AnnCloseP
    addDeltaAnnotation GHC.AnnClose

  annotateP _ (GHC.PArrPat ps _) = do
    addDeltaAnnotation GHC.AnnOpen
    mapM_ annotatePC ps
    addDeltaAnnotation GHC.AnnClose

  annotateP _ (GHC.ConPatIn n dets) = do
    annotateHsConPatDetails n dets

  annotateP _ (GHC.ConPatOut {}) = return ()

  -- ViewPat (LHsExpr id) (LPat id) (PostTc id Type)
  annotateP _ (GHC.ViewPat e pat _) = do
    annotatePC e
    addDeltaAnnotation GHC.AnnRarrow
    annotatePC pat

  -- SplicePat (HsSplice id)
  annotateP _ (GHC.SplicePat (GHC.HsSplice _ e)) = do
    addDeltaAnnotation GHC.AnnOpen -- '$('
    annotatePC e
    addDeltaAnnotation GHC.AnnClose -- ')'

  -- QuasiQuotePat (HsQuasiQuote id)
  annotateP l (GHC.QuasiQuotePat (GHC.HsQuasiQuote _ _ _)) = do
    addDeltaAnnotationExt l GHC.AnnVal

  -- LitPat HsLit
  annotateP l (GHC.LitPat _lp) = addDeltaAnnotationExt l GHC.AnnVal

  -- NPat (HsOverLit id) (Maybe (SyntaxExpr id)) (SyntaxExpr id)
  annotateP _ (GHC.NPat ol _ _) = do
    addDeltaAnnotation GHC.AnnMinus
    annotatePC ol

  -- NPlusKPat (Located id) (HsOverLit id) (SyntaxExpr id) (SyntaxExpr id)
  annotateP _ (GHC.NPlusKPat ln ol _ _) = do
    annotatePC ln
    addDeltaAnnotation GHC.AnnVal -- "+"
    annotatePC ol

  annotateP l (GHC.SigPatIn pat ty) = do
    annotatePC pat
    addDeltaAnnotation GHC.AnnDcolon
    annotateP l ty

  annotateP _ (GHC.SigPatOut {}) = return ()

  -- CoPat HsWrapper (Pat id) Type
  annotateP _ (GHC.CoPat {}) = return ()

-- ---------------------------------------------------------------------

annotateHsConPatDetails :: (GHC.DataId name,GHC.OutputableBndr name,AnnotateP name)
                      => GHC.Located name -> GHC.HsConPatDetails name -> AP ()
annotateHsConPatDetails ln dets = do
  case dets of
    GHC.PrefixCon args -> do
      annotatePC ln
      mapM_ annotatePC args
    GHC.RecCon (GHC.HsRecFields fs _) -> do
      annotatePC ln
      addDeltaAnnotation GHC.AnnOpenC -- '{'
      mapM_ annotatePC fs
      addDeltaAnnotation GHC.AnnDotdot
      addDeltaAnnotation GHC.AnnCloseC -- '}'
    GHC.InfixCon a1 a2 -> do
      annotatePC a1
      annotatePC ln
      annotatePC a2

annotateHsConDeclDetails :: (GHC.DataId name,GHC.OutputableBndr name,AnnotateP name)
                    =>  [GHC.Located name] -> GHC.HsConDeclDetails name -> AP ()
annotateHsConDeclDetails lns dets = do
  case dets of
    GHC.PrefixCon args -> mapM_ annotatePC args
    GHC.RecCon fs -> do
      addDeltaAnnotation GHC.AnnOpenC
      annotatePC fs
      addDeltaAnnotation GHC.AnnCloseC
    GHC.InfixCon a1 a2 -> do
      annotatePC a1
      mapM_ annotatePC lns
      annotatePC a2

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateP name)
   => AnnotateP [GHC.LConDeclField name] where
  annotateP _ fs = do
       addDeltaAnnotation GHC.AnnOpenC -- '{'
       mapM_ annotatePC fs
       addDeltaAnnotation GHC.AnnDotdot
       addDeltaAnnotation GHC.AnnCloseC -- '}'

-- ---------------------------------------------------------------------

instance (GHC.DataId name) => AnnotateP (GHC.HsOverLit name) where
  annotateP l _ol = addDeltaAnnotationExt l GHC.AnnVal

-- ---------------------------------------------------------------------

instance (GHC.DataId name,AnnotateP arg)
    => AnnotateP (GHC.HsWithBndrs name (GHC.Located arg)) where
  annotateP _ (GHC.HsWB thing _ _ _) = annotatePC thing

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateP name,AnnotateP body) =>
                            AnnotateP (GHC.Stmt name (GHC.Located body)) where

  annotateP _ (GHC.LastStmt body _) = annotatePC body

  annotateP _ (GHC.BindStmt pat body _ _) = do
    annotatePC pat
    addDeltaAnnotation GHC.AnnLarrow
    annotatePC body
    addDeltaAnnotation GHC.AnnVbar -- possible in list comprehension

  annotateP _ (GHC.BodyStmt body _ _ _) = do
    annotatePC body

  annotateP _ (GHC.LetStmt lb) = do
    -- return () `debug` ("annotateP.LetStmt entered")
    addDeltaAnnotation GHC.AnnLet
    addDeltaAnnotation GHC.AnnOpenC -- '{'
    annotatePC (GHC.L (getLocalBindsSrcSpan lb) lb)
    addDeltaAnnotation GHC.AnnCloseC -- '}'
    -- return () `debug` ("annotateP.LetStmt done")

  annotateP _ (GHC.ParStmt pbs _ _) = do
    mapM_ annotateParStmtBlock pbs

  annotateP _ (GHC.TransStmt form stmts _b using by _ _ _) = do
    mapM_ annotatePC stmts
    case form of
      GHC.ThenForm -> do
        addDeltaAnnotation GHC.AnnThen
        annotatePC using
        addDeltaAnnotation GHC.AnnBy
        case by of
          Just b -> annotatePC b
          Nothing -> return ()
      GHC.GroupForm -> do
        addDeltaAnnotation GHC.AnnThen
        addDeltaAnnotation GHC.AnnGroup
        addDeltaAnnotation GHC.AnnBy
        case by of
          Just b -> annotatePC b
          Nothing -> return ()
        addDeltaAnnotation GHC.AnnUsing
        annotatePC using

  annotateP _ (GHC.RecStmt stmts _ _ _ _ _ _ _ _) = do
    addDeltaAnnotation GHC.AnnRec
    addDeltaAnnotation GHC.AnnOpenC
    addDeltaAnnotationsInside GHC.AnnSemi
    mapM_ annotatePC stmts
    addDeltaAnnotation GHC.AnnCloseC

-- ---------------------------------------------------------------------

annotateParStmtBlock :: (GHC.DataId name,GHC.OutputableBndr name, AnnotateP name)
  =>  GHC.ParStmtBlock name name -> AP ()
annotateParStmtBlock (GHC.ParStmtBlock stmts _ns _) = do
  mapM_ annotatePC stmts

-- ---------------------------------------------------------------------

-- | Local binds need to be indented as a group, and thus need to have a
-- SrcSpan around them so they can be processed via the normal
-- annotatePC / exactPC machinery.
getLocalBindsSrcSpan :: (GHC.HsLocalBinds name) -> GHC.SrcSpan
getLocalBindsSrcSpan (GHC.HsValBinds (GHC.ValBindsIn binds sigs))
  = case spans of
      []  -> GHC.noSrcSpan
      sss -> GHC.combineSrcSpans (head sss) (last sss)
  where
    spans = sort $ (map GHC.getLoc (GHC.bagToList binds) ++ map GHC.getLoc sigs)

getLocalBindsSrcSpan (GHC.HsValBinds (GHC.ValBindsOut {}))
   = error $ "getLocalBindsSrcSpan: only valid after type checking"

getLocalBindsSrcSpan (GHC.HsIPBinds (GHC.IPBinds binds _))
  = case sort (map GHC.getLoc binds) of
      [] -> GHC.noSrcSpan
      sss -> GHC.combineSrcSpans (head sss) (last sss)

getLocalBindsSrcSpan (GHC.EmptyLocalBinds) = GHC.noSrcSpan

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateP name)
  => AnnotateP (GHC.HsLocalBinds name) where
  annotateP _ lb = annotateHsLocalBinds lb

-- ---------------------------------------------------------------------

annotateHsLocalBinds :: (GHC.DataId name,GHC.OutputableBndr name,AnnotateP name)
                     => (GHC.HsLocalBinds name) -> AP ()
annotateHsLocalBinds (GHC.HsValBinds (GHC.ValBindsIn binds sigs)) = do
    applyListAnnotations (prepareListAnnotation (GHC.bagToList binds)
                       ++ prepareListAnnotation sigs
                         )
annotateHsLocalBinds (GHC.HsValBinds (GHC.ValBindsOut {}))
   = error $ "annotateHsLocalBinds: only valid after type checking"

annotateHsLocalBinds (GHC.HsIPBinds (GHC.IPBinds binds _)) = mapM_ annotatePC binds
annotateHsLocalBinds (GHC.EmptyLocalBinds)                 = return ()

-- ---------------------------------------------------------------------

annotateMatchGroup :: (GHC.DataId name,GHC.OutputableBndr name,AnnotateP name,
                                               AnnotateP body)
                   =>   (GHC.MatchGroup name (GHC.Located body))
                   -> AP ()
annotateMatchGroup (GHC.MG matches _ _ _)
  = mapM_ annotatePC matches

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateP name)
  => AnnotateP (GHC.HsExpr name) where
  annotateP l (GHC.HsVar n)           = annotateP l n
  annotateP l (GHC.HsIPVar _)         = addDeltaAnnotationExt l GHC.AnnVal
  annotateP l (GHC.HsOverLit _ov)     = addDeltaAnnotationExt l GHC.AnnVal
  annotateP l (GHC.HsLit _)           = addDeltaAnnotationExt l GHC.AnnVal

  annotateP _ (GHC.HsLam match)       = do
    addDeltaAnnotation GHC.AnnLam
    annotateMatchGroup match

  annotateP _ (GHC.HsLamCase _ match) = annotateMatchGroup match

  annotateP _ (GHC.HsApp e1 e2) = do
    annotatePC e1
    annotatePC e2

  annotateP _ (GHC.OpApp e1 e2 _ e3) = do
    annotatePC e1
    annotatePC e2
    annotatePC e3

  annotateP _ (GHC.NegApp e _) = do
    addDeltaAnnotation GHC.AnnMinus
    annotatePC e

  annotateP _ (GHC.HsPar e) = do
    addDeltaAnnotation GHC.AnnOpenP -- '('
    annotatePC e
    addDeltaAnnotation GHC.AnnCloseP -- ')'

  annotateP _ (GHC.SectionL e1 e2) = do
    annotatePC e1
    annotatePC e2

  annotateP _ (GHC.SectionR e1 e2) = do
    annotatePC e1
    annotatePC e2

  annotateP _ (GHC.ExplicitTuple args _boxity) = do
    addDeltaAnnotation GHC.AnnOpen
    addDeltaAnnotation GHC.AnnOpenP
    mapM_ annotatePC args
    addDeltaAnnotation GHC.AnnCloseP
    addDeltaAnnotation GHC.AnnClose

  annotateP _ (GHC.HsCase e1 matches) = do
    addDeltaAnnotation GHC.AnnCase
    annotatePC e1
    addDeltaAnnotation GHC.AnnOf
    addDeltaAnnotation GHC.AnnOpenC
    addDeltaAnnotationsInside GHC.AnnSemi
    annotateMatchGroup matches
    addDeltaAnnotation GHC.AnnCloseC

  annotateP _ (GHC.HsIf _ e1 e2 e3) = do
    addDeltaAnnotation GHC.AnnIf
    annotatePC e1
    addDeltaAnnotationLs GHC.AnnSemi 0
    addDeltaAnnotation GHC.AnnThen
    annotatePC e2
    addDeltaAnnotationLs GHC.AnnSemi 1
    addDeltaAnnotation GHC.AnnElse
    annotatePC e3

  annotateP _ (GHC.HsMultiIf _ rhs) = do
    addDeltaAnnotation GHC.AnnIf
    mapM_ annotatePC rhs

  annotateP _ (GHC.HsLet binds e) = do
    addDeltaAnnotation GHC.AnnLet
    addDeltaAnnotation GHC.AnnOpenC
    addDeltaAnnotationsInside GHC.AnnSemi
    -- startGroupingOffsets
    -- annotateHsLocalBinds binds
    annotatePC (GHC.L (getLocalBindsSrcSpan binds) binds)
    -- stopGroupingOffsets
    addDeltaAnnotation GHC.AnnCloseC
    addDeltaAnnotation GHC.AnnIn
    annotatePC e

  annotateP _ (GHC.HsDo cts es _) = do
    addDeltaAnnotation GHC.AnnDo
    addDeltaAnnotation GHC.AnnOpen
    addDeltaAnnotation GHC.AnnOpenS
    addDeltaAnnotation GHC.AnnOpenC
    addDeltaAnnotationsInside GHC.AnnSemi
    if isListComp cts
      then do
        annotatePC (last es)
        addDeltaAnnotation GHC.AnnVbar
        mapM_ annotatePC (init es)
      else do
        mapM_ annotatePC es
    addDeltaAnnotation GHC.AnnCloseS
    addDeltaAnnotation GHC.AnnCloseC
    addDeltaAnnotation GHC.AnnClose

  annotateP _ (GHC.ExplicitList _ _ es) = do
    addDeltaAnnotation GHC.AnnOpenS
    mapM_ annotatePC es
    addDeltaAnnotation GHC.AnnCloseS

  annotateP _ (GHC.ExplicitPArr _ es)   = do
    addDeltaAnnotation GHC.AnnOpen
    mapM_ annotatePC es
    addDeltaAnnotation GHC.AnnClose

  annotateP _ (GHC.RecordCon n _ (GHC.HsRecFields fs _)) = do
    annotatePC n
    addDeltaAnnotation GHC.AnnOpenC
    addDeltaAnnotation GHC.AnnDotdot
    mapM_ annotatePC fs
    addDeltaAnnotation GHC.AnnCloseC

  annotateP _ (GHC.RecordUpd e (GHC.HsRecFields fs _) _cons _ _) = do
    annotatePC e
    addDeltaAnnotation GHC.AnnOpenC
    addDeltaAnnotation GHC.AnnDotdot
    mapM_ annotatePC fs
    addDeltaAnnotation GHC.AnnCloseC

  annotateP _ (GHC.ExprWithTySig e typ _) = do
    annotatePC e
    addDeltaAnnotation GHC.AnnDcolon
    annotatePC typ

  annotateP _ (GHC.ExprWithTySigOut e typ) = do
    annotatePC e
    addDeltaAnnotation GHC.AnnDcolon
    annotatePC typ

  annotateP _ (GHC.ArithSeq _ _ seqInfo) = do
    addDeltaAnnotation GHC.AnnOpenS -- '['
    case seqInfo of
        GHC.From e -> do
          annotatePC e
          addDeltaAnnotation GHC.AnnDotdot
        GHC.FromTo e1 e2 -> do
          annotatePC e1
          addDeltaAnnotation GHC.AnnDotdot
          annotatePC e2
        GHC.FromThen e1 e2 -> do
          annotatePC e1
          addDeltaAnnotation GHC.AnnComma
          annotatePC e2
          addDeltaAnnotation GHC.AnnDotdot
        GHC.FromThenTo e1 e2 e3 -> do
          annotatePC e1
          addDeltaAnnotation GHC.AnnComma
          annotatePC e2
          addDeltaAnnotation GHC.AnnDotdot
          annotatePC e3
    addDeltaAnnotation GHC.AnnCloseS -- ']'

  annotateP _ (GHC.PArrSeq _ seqInfo) = do
    addDeltaAnnotation GHC.AnnOpen -- '[:'
    case seqInfo of
        GHC.From e -> do
          annotatePC e
          addDeltaAnnotation GHC.AnnDotdot
        GHC.FromTo e1 e2 -> do
          annotatePC e1
          addDeltaAnnotation GHC.AnnDotdot
          annotatePC e2
        GHC.FromThen e1 e2 -> do
          annotatePC e1
          addDeltaAnnotation GHC.AnnComma
          annotatePC e2
          addDeltaAnnotation GHC.AnnDotdot
        GHC.FromThenTo e1 e2 e3 -> do
          annotatePC e1
          addDeltaAnnotation GHC.AnnComma
          annotatePC e2
          addDeltaAnnotation GHC.AnnDotdot
          annotatePC e3
    addDeltaAnnotation GHC.AnnClose -- ':]'

  annotateP _ (GHC.HsSCC _ _csFStr e) = do
    addDeltaAnnotation GHC.AnnOpen -- '{-# SCC'
    addDeltaAnnotation GHC.AnnVal
    addDeltaAnnotation GHC.AnnValStr
    addDeltaAnnotation GHC.AnnClose -- '#-}'
    annotatePC e

  annotateP _ (GHC.HsCoreAnn _ _csFStr e) = do
    addDeltaAnnotation GHC.AnnOpen -- '{-# CORE'
    addDeltaAnnotation GHC.AnnVal
    addDeltaAnnotation GHC.AnnClose -- '#-}'
    annotatePC e

  annotateP l (GHC.HsBracket (GHC.VarBr _ _)) = do
    addDeltaAnnotationExt l GHC.AnnVal
  annotateP _ (GHC.HsBracket (GHC.DecBrL ds)) = do
    addDeltaAnnotation GHC.AnnOpen
    addDeltaAnnotation GHC.AnnOpenC
    mapM_ annotatePC ds
    addDeltaAnnotation GHC.AnnCloseC
    addDeltaAnnotation GHC.AnnClose
  annotateP _ (GHC.HsBracket (GHC.ExpBr e)) = do
    addDeltaAnnotation GHC.AnnOpen
    annotatePC e
    addDeltaAnnotation GHC.AnnClose
  annotateP _ (GHC.HsBracket (GHC.TExpBr e)) = do
    addDeltaAnnotation GHC.AnnOpen
    annotatePC e
    addDeltaAnnotation GHC.AnnClose
  annotateP _ (GHC.HsBracket (GHC.TypBr e)) = do
    addDeltaAnnotation GHC.AnnOpen
    annotatePC e
    addDeltaAnnotation GHC.AnnClose
  annotateP _ (GHC.HsBracket (GHC.PatBr e)) = do
    addDeltaAnnotation GHC.AnnOpen
    annotatePC e
    addDeltaAnnotation GHC.AnnClose

  annotateP _ (GHC.HsRnBracketOut _ _) = return ()
  annotateP _ (GHC.HsTcBracketOut _ _) = return ()

  annotateP _ (GHC.HsSpliceE _typed (GHC.HsSplice _ e)) = do
    addDeltaAnnotation GHC.AnnOpen -- possible '$('
    annotatePC e
    addDeltaAnnotation GHC.AnnClose -- possible ')'

  annotateP l (GHC.HsQuasiQuoteE (GHC.HsQuasiQuote _ _ _)) = do
    addDeltaAnnotationExt l GHC.AnnVal

  annotateP _ (GHC.HsProc p c) = do
    addDeltaAnnotation GHC.AnnProc
    annotatePC p
    addDeltaAnnotation GHC.AnnRarrow
    annotatePC c

  annotateP _ (GHC.HsStatic e) = do
    addDeltaAnnotation GHC.AnnStatic
    annotatePC e

  annotateP _ (GHC.HsArrApp e1 e2 _ _ _) = do
    annotatePC e1
    -- only one of the next 4 will be resent
    addDeltaAnnotation GHC.Annlarrowtail
    addDeltaAnnotation GHC.Annrarrowtail
    addDeltaAnnotation GHC.AnnLarrowtail
    addDeltaAnnotation GHC.AnnRarrowtail

    annotatePC e2

  annotateP _ (GHC.HsArrForm e _ cs) = do
    addDeltaAnnotation GHC.AnnOpen -- '(|'
    annotatePC e
    mapM_ annotatePC cs
    addDeltaAnnotation GHC.AnnClose -- '|)'

  annotateP _ (GHC.HsTick _ _) = return ()
  annotateP _ (GHC.HsBinTick _ _ _) = return ()

  annotateP _ (GHC.HsTickPragma _ (_str,(_v1,_v2),(_v3,_v4)) e) = do
    -- '{-# GENERATED' STRING INTEGER ':' INTEGER '-' INTEGER ':' INTEGER '#-}'
    addDeltaAnnotation   GHC.AnnOpen     -- '{-# GENERATED'
    addDeltaAnnotationLs GHC.AnnVal   0 -- STRING
    addDeltaAnnotationLs GHC.AnnVal   1 -- INTEGER
    addDeltaAnnotationLs GHC.AnnColon 0 -- ':'
    addDeltaAnnotationLs GHC.AnnVal   2 -- INTEGER
    addDeltaAnnotation   GHC.AnnMinus   -- '-'
    addDeltaAnnotationLs GHC.AnnVal   3 -- INTEGER
    addDeltaAnnotationLs GHC.AnnColon 1 -- ':'
    addDeltaAnnotationLs GHC.AnnVal   4 -- INTEGER
    addDeltaAnnotation   GHC.AnnClose   -- '#-}'
    annotatePC e

  annotateP l (GHC.EWildPat) = do
    addDeltaAnnotationExt l GHC.AnnVal

  annotateP _ (GHC.EAsPat ln e) = do
    annotatePC ln
    addDeltaAnnotation GHC.AnnAt
    annotatePC e

  annotateP _ (GHC.EViewPat e1 e2) = do
    annotatePC e1
    addDeltaAnnotation GHC.AnnRarrow
    annotatePC e2

  annotateP _ (GHC.ELazyPat e) = do
    addDeltaAnnotation GHC.AnnTilde
    annotatePC e

  annotateP _ (GHC.HsType ty) = annotatePC ty

  annotateP _ (GHC.HsWrap _ _) = return ()
  annotateP _ (GHC.HsUnboundVar _) = return ()


-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateP name)
  => AnnotateP (GHC.HsTupArg name) where
  annotateP _ (GHC.Present e) = do
    annotatePC e

  annotateP _ (GHC.Missing _) = do
    addDeltaAnnotation GHC.AnnComma

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateP name)
  => AnnotateP (GHC.HsCmdTop name) where
  annotateP _ (GHC.HsCmdTop cmd _ _ _) = annotatePC cmd

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateP name)
   => AnnotateP (GHC.HsCmd name) where
  annotateP _ (GHC.HsCmdArrApp e1 e2 _ _ _) = do
    annotatePC e1
    -- only one of the next 4 will be resent
    addDeltaAnnotation GHC.Annlarrowtail
    addDeltaAnnotation GHC.Annrarrowtail
    addDeltaAnnotation GHC.AnnLarrowtail
    addDeltaAnnotation GHC.AnnRarrowtail

    annotatePC e2

  annotateP _ (GHC.HsCmdArrForm e _mf cs) = do
    addDeltaAnnotation GHC.AnnOpen -- '(|'
    annotatePC e
    mapM_ annotatePC cs
    addDeltaAnnotation GHC.AnnClose -- '|)'

  annotateP _ (GHC.HsCmdApp e1 e2) = do
    annotatePC e1
    annotatePC e2

  annotateP _ (GHC.HsCmdLam match) = do
    addDeltaAnnotation GHC.AnnLam
    annotateMatchGroup match

  annotateP _ (GHC.HsCmdPar e) = do
    addDeltaAnnotation GHC.AnnOpenP -- '('
    annotatePC e
    addDeltaAnnotation GHC.AnnCloseP -- ')'

  annotateP _ (GHC.HsCmdCase e1 matches) = do
    addDeltaAnnotation GHC.AnnCase
    annotatePC e1
    addDeltaAnnotation GHC.AnnOf
    addDeltaAnnotation GHC.AnnOpenC
    annotateMatchGroup matches
    addDeltaAnnotation GHC.AnnCloseC

  annotateP _ (GHC.HsCmdIf _ e1 e2 e3) = do
    addDeltaAnnotation GHC.AnnIf
    annotatePC e1
    addDeltaAnnotationLs GHC.AnnSemi 0
    addDeltaAnnotation GHC.AnnThen
    annotatePC e2
    addDeltaAnnotationLs GHC.AnnSemi 1
    addDeltaAnnotation GHC.AnnElse
    annotatePC e3

  annotateP _ (GHC.HsCmdLet binds e) = do
    addDeltaAnnotation GHC.AnnLet
    addDeltaAnnotation GHC.AnnOpenC
    -- annotateHsLocalBinds binds
    annotatePC (GHC.L (getLocalBindsSrcSpan binds) binds)
    addDeltaAnnotation GHC.AnnCloseC
    addDeltaAnnotation GHC.AnnIn
    annotatePC e

  annotateP _ (GHC.HsCmdDo es _) = do
    addDeltaAnnotation GHC.AnnDo
    addDeltaAnnotation GHC.AnnOpenC
    mapM_ annotatePC es
    addDeltaAnnotation GHC.AnnCloseC

  annotateP _ (GHC.HsCmdCast {}) = error $ "annotateP.HsCmdCast: only valid after type checker"


-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateP name)
     => AnnotateP (GHC.TyClDecl name) where

  annotateP l (GHC.FamDecl famdecl) = annotateP l famdecl

  annotateP _ (GHC.SynDecl ln (GHC.HsQTvs _ tyvars) typ _) = do
    addDeltaAnnotation GHC.AnnType
    annotatePC ln
    mapM_ annotatePC tyvars
    addDeltaAnnotation GHC.AnnEqual
    annotatePC typ

  annotateP _ (GHC.DataDecl ln (GHC.HsQTvs _ns tyVars)
                (GHC.HsDataDefn _ ctx mctyp mk cons mderivs) _) = do
    addDeltaAnnotation GHC.AnnData
    addDeltaAnnotation GHC.AnnNewtype
    annotateMaybe mctyp
    annotatePC ctx
    addDeltaAnnotation GHC.AnnDarrow
    annotateTyClass ln tyVars
    addDeltaAnnotation GHC.AnnDcolon
    annotateMaybe mk
    addDeltaAnnotation GHC.AnnEqual
    addDeltaAnnotation GHC.AnnWhere
    mapM_ annotatePC cons
    annotateMaybe mderivs

  -- -----------------------------------

  annotateP _ (GHC.ClassDecl ctx ln (GHC.HsQTvs _ns tyVars) fds
                          sigs meths ats atdefs docs _) = do
    addDeltaAnnotation GHC.AnnClass
    annotatePC ctx

    annotateTyClass ln tyVars

    addDeltaAnnotation GHC.AnnVbar
    mapM_ annotatePC fds
    addDeltaAnnotation GHC.AnnWhere
    addDeltaAnnotation GHC.AnnOpenC -- '{'
    addDeltaAnnotationsInside GHC.AnnSemi
    applyListAnnotations (prepareListAnnotation sigs
                       ++ prepareListAnnotation (GHC.bagToList meths)
                       ++ prepareListAnnotation ats
                       ++ prepareListAnnotation atdefs
                       ++ prepareListAnnotation docs
                         )
    addDeltaAnnotation GHC.AnnCloseC -- '}'

-- ---------------------------------------------------------------------

annotateTyClass :: (AnnotateP a, AnnotateP ast)
                => GHC.Located a -> [GHC.Located ast] -> AP ()
annotateTyClass ln tyVars = do
    addDeltaAnnotations GHC.AnnOpenP
    applyListAnnotations (prepareListAnnotation [ln]
                      ++ prepareListAnnotation (take 2 tyVars))
    addDeltaAnnotations GHC.AnnCloseP
    mapM_ annotatePC (drop 2 tyVars)

-- ---------------------------------------------------------------------

instance (GHC.DataId name,AnnotateP name, GHC.OutputableBndr name)
   => AnnotateP (GHC.FamilyDecl name) where
  annotateP _ (GHC.FamilyDecl info ln (GHC.HsQTvs _ tyvars) mkind) = do
    addDeltaAnnotation GHC.AnnType
    addDeltaAnnotation GHC.AnnData
    addDeltaAnnotation GHC.AnnFamily
    annotatePC ln
    mapM_ annotatePC tyvars
    addDeltaAnnotation GHC.AnnDcolon
    annotateMaybe mkind
    addDeltaAnnotation GHC.AnnWhere
    addDeltaAnnotation GHC.AnnOpenC -- {
    case info of
      GHC.ClosedTypeFamily eqns -> mapM_ annotatePC eqns
      _ -> return ()
    case info of
      GHC.ClosedTypeFamily eqns -> mapM_ annotatePC eqns
      _ -> return ()
    addDeltaAnnotation GHC.AnnCloseC -- }

-- ---------------------------------------------------------------------

instance (GHC.DataId name,AnnotateP name,GHC.OutputableBndr name)
   => AnnotateP (GHC.TyFamInstEqn name) where
  annotateP _ (GHC.TyFamEqn ln (GHC.HsWB pats _ _ _) typ) = do
    annotatePC ln
    mapM_ annotatePC pats
    addDeltaAnnotation GHC.AnnEqual
    annotatePC typ


-- ---------------------------------------------------------------------

instance (GHC.DataId name,AnnotateP name,GHC.OutputableBndr name)
  => AnnotateP (GHC.TyFamDefltEqn name) where
  annotateP _ (GHC.TyFamEqn ln (GHC.HsQTvs _ns bndrs) typ) = do
    annotatePC ln
    mapM_ annotatePC bndrs
    addDeltaAnnotation GHC.AnnEqual
    annotatePC typ

-- ---------------------------------------------------------------------

-- TODO: modify lexer etc, in the meantime to not set haddock flag
instance AnnotateP GHC.DocDecl where
  annotateP l _ = addDeltaAnnotationExt l GHC.AnnVal

-- ---------------------------------------------------------------------

annotateDataDefn :: (GHC.DataId name,GHC.OutputableBndr name,AnnotateP name)
  => GHC.SrcSpan -> GHC.HsDataDefn name -> AP ()
annotateDataDefn _ (GHC.HsDataDefn _ ctx typ mk cons mderivs) = do
  annotatePC ctx
  annotateMaybe typ
  annotateMaybe mk
  mapM_ annotatePC cons
  case mderivs of
    Nothing -> return ()
    Just d -> annotatePC d

-- ---------------------------------------------------------------------

-- Note: GHC.HsContext name aliases to here too
instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateP name)
     => AnnotateP [GHC.LHsType name] where
  annotateP l ts = do
    return () `debug` ("annotateP.HsContext:l=" ++ showGhc l)
    addDeltaAnnotation GHC.AnnDeriving
    addDeltaAnnotation GHC.AnnOpenP
    mapM_ annotatePC ts
    -- addDeltaAnnotation GHC.AnnUnit -- for empty context
    addDeltaAnnotation GHC.AnnCloseP
    addDeltaAnnotation GHC.AnnDarrow

-- ---------------------------------------------------------------------

instance (GHC.DataId name,AnnotateP name,GHC.OutputableBndr name)
      => AnnotateP (GHC.ConDecl name) where
  annotateP _ (GHC.ConDecl lns _expr (GHC.HsQTvs _ns bndrs) ctx
                         dets res _ _) = do
    case res of
      GHC.ResTyH98 -> do
        addDeltaAnnotation GHC.AnnForall
        mapM_ annotatePC bndrs
        addDeltaAnnotation GHC.AnnDot

        annotatePC ctx
        addDeltaAnnotation GHC.AnnDarrow

        case dets of
          GHC.InfixCon _ _ -> return ()
          _ -> mapM_ annotatePC lns

        annotateHsConDeclDetails lns dets

      GHC.ResTyGADT ls ty -> do
        -- only print names if not infix
        case dets of
          GHC.InfixCon _ _ -> return ()
          _ -> mapM_ annotatePC lns

        annotateHsConDeclDetails lns dets

        addDeltaAnnotation GHC.AnnDcolon

        annotatePC (GHC.L ls (ResTyGADTHook bndrs))

        annotatePC ctx
        addDeltaAnnotation GHC.AnnDarrow

        annotatePC ty


    addDeltaAnnotation GHC.AnnVbar

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateP name) =>
              AnnotateP (ResTyGADTHook name) where
  annotateP _ (ResTyGADTHook bndrs) = do
    addDeltaAnnotation GHC.AnnForall
    mapM_ annotatePC bndrs
    addDeltaAnnotation GHC.AnnDot

-- ---------------------------------------------------------------------

instance (AnnotateP name,AnnotateP a)
  => AnnotateP (GHC.HsRecField name (GHC.Located a)) where
  annotateP _ (GHC.HsRecField n e _) = do
    annotatePC n
    addDeltaAnnotation GHC.AnnEqual
    annotatePC e

-- ---------------------------------------------------------------------

instance (GHC.DataId name,AnnotateP name)
    => AnnotateP (GHC.FunDep (GHC.Located name)) where

  annotateP _ (ls,rs) = do
    mapM_ annotatePC ls
    addDeltaAnnotation GHC.AnnRarrow
    mapM_ annotatePC rs

-- ---------------------------------------------------------------------

instance AnnotateP (GHC.CType) where
  annotateP _ _ = do
    addDeltaAnnotation GHC.AnnOpen
    addDeltaAnnotation GHC.AnnHeader
    addDeltaAnnotation GHC.AnnVal
    addDeltaAnnotation GHC.AnnClose

-- ---------------------------------------------------------------------

-- | Given an enclosing Span @(p,e)@, and a list of sub SrcSpans @ds@,
-- identify all comments that are in @(p,e)@ but not in @ds@, and convert
-- them to be DComments relative to @p@
localComments :: Int -> Span -> [Comment] -> [Span] -> ([DComment],[Comment])
localComments co pin cs ds = r
  -- `debug` ("localComments:(p,ds,r):" ++ show ((p,e),ds,r))
  where
    r = (map (\c -> deltaComment co p c) matches,misses ++ missesRest)
    (p,e) = if pin == ((1,1),(1,1))
               then  ((1,1),(99999999,1))
               else pin

    (matches,misses) = partition notSub cs'
    (cs',missesRest) = partition (\(Comment _ com _) -> isSubPos com (p,e)) cs

    notSub :: Comment -> Bool
    notSub (Comment _ com _) = not $ any (\sub -> isSubPos com sub) ds

    isSubPos (subs,sube) (parents,parente)
      = parents <= subs && parente >= sube

-- ---------------------------------------------------------------------

-- | Apply the delta to the current position, taking into account the
-- current column offset
undeltaComment :: Pos -> Int -> DComment -> Comment
undeltaComment l con (DComment _coo b (dps,dpe) s) = r
    -- `debug` ("undeltaComment:(l,con,dcomment,r)=" ++ show (l,con,dco,r))
  where
    r = Comment b ((adj dps $ undelta l dps co),(adj dps $ undelta l dpe co)) s
    co = con
    dc = - con -- + (coo - con)

    -- adj makes provision for the possible movement of the
    -- surrounding context, and so applies the difference between the
    -- original and current offsets
    adj (DP (   0,_dco)) (row,c) = (row,c)
    adj (DP (_dro,_dco)) (row,c) = (row,c + dc)

deltaComment :: Int -> Pos -> Comment -> DComment
deltaComment co l (Comment b (s,e) str) = r
  -- `debug` ("deltaComment:(co,l,cin,r)=" ++ show (co,l,cin,r))
  where
    r = DComment co b ((ss2deltaP l s),(ss2deltaP l e)) str

-- | Create a delta covering the gap between the end of the first
-- @SrcSpan@ and the start of the second.
deltaFromSrcSpans :: GHC.SrcSpan -> GHC.SrcSpan -> DeltaPos
deltaFromSrcSpans ss1 ss2 = ss2delta (ss2posEnd ss1) ss2

ss2delta :: Pos -> GHC.SrcSpan -> DeltaPos
ss2delta ref ss = ss2deltaP ref (ss2pos ss)

-- | Convert the start of the second @Pos@ to be an offset from the
-- first. The assumption is the reference starts before the second @Pos@
ss2deltaP :: Pos -> Pos -> DeltaPos
ss2deltaP (refl,refc) (l,c) = DP (lo,co)
  where
    lo = l - refl
    co = if lo == 0 then c - refc
                    else c

-- | Apply the delta to the current position, taking into account the
-- current column offset
undelta :: Pos -> DeltaPos -> Int -> Pos
undelta (l,c) (DP (dl,dc)) co = (fl,fc)
  where
    fl = l + dl
    fc = if dl == 0 then c + dc else co + dc

-- prop_delta :: TODO

ss2pos :: GHC.SrcSpan -> Pos
ss2pos ss = (srcSpanStartLine ss,srcSpanStartColumn ss)

ss2posEnd :: GHC.SrcSpan -> Pos
ss2posEnd ss = (srcSpanEndLine ss,srcSpanEndColumn ss)

ss2span :: GHC.SrcSpan -> Span
ss2span ss = (ss2pos ss,ss2posEnd ss)

srcSpanStart :: GHC.SrcSpan -> Pos
srcSpanStart ss = (srcSpanStartLine ss,srcSpanStartColumn ss)

srcSpanEnd :: GHC.SrcSpan -> Pos
srcSpanEnd ss = (srcSpanEndLine ss,srcSpanEndColumn ss)


srcSpanEndColumn :: GHC.SrcSpan -> Int
srcSpanEndColumn (GHC.RealSrcSpan s) = GHC.srcSpanEndCol s
srcSpanEndColumn _ = 0

srcSpanStartColumn :: GHC.SrcSpan -> Int
srcSpanStartColumn (GHC.RealSrcSpan s) = GHC.srcSpanStartCol s
srcSpanStartColumn _ = 0

srcSpanEndLine :: GHC.SrcSpan -> Int
srcSpanEndLine (GHC.RealSrcSpan s) = GHC.srcSpanEndLine s
srcSpanEndLine _ = 0

srcSpanStartLine :: GHC.SrcSpan -> Int
srcSpanStartLine (GHC.RealSrcSpan s) = GHC.srcSpanStartLine s
srcSpanStartLine _ = 0

-- ---------------------------------------------------------------------

isPointSrcSpan :: GHC.SrcSpan -> Bool
isPointSrcSpan ss = s == e where (s,e) = ss2span ss

-- ---------------------------------------------------------------------

isListComp :: GHC.HsStmtContext name -> Bool
isListComp cts = case cts of
          GHC.ListComp  -> True
          GHC.MonadComp -> True
          GHC.PArrComp  -> True

          GHC.DoExpr       -> False
          GHC.MDoExpr      -> False
          GHC.ArrowExpr    -> False
          GHC.GhciStmtCtxt -> False

          GHC.PatGuard {}      -> False
          GHC.ParStmtCtxt {}   -> False
          GHC.TransStmtCtxt {} -> False

-- ---------------------------------------------------------------------

{-
deriving instance Eq GHC.Token

ghcIsComment :: PosToken -> Bool
ghcIsComment ((GHC.L _ (GHC.ITdocCommentNext _)),_s)  = True
ghcIsComment ((GHC.L _ (GHC.ITdocCommentPrev _)),_s)  = True
ghcIsComment ((GHC.L _ (GHC.ITdocCommentNamed _)),_s) = True
ghcIsComment ((GHC.L _ (GHC.ITdocSection _ _)),_s)    = True
ghcIsComment ((GHC.L _ (GHC.ITdocOptions _)),_s)      = True
ghcIsComment ((GHC.L _ (GHC.ITdocOptionsOld _)),_s)   = True
ghcIsComment ((GHC.L _ (GHC.ITlineComment _)),_s)     = True
ghcIsComment ((GHC.L _ (GHC.ITblockComment _)),_s)    = True
ghcIsComment ((GHC.L _ _),_s)                         = False
-}

ghcIsMultiLine :: GHC.Located GHC.AnnotationComment -> Bool
ghcIsMultiLine (GHC.L _ (GHC.AnnDocCommentNext _))  = False
ghcIsMultiLine (GHC.L _ (GHC.AnnDocCommentPrev _))  = False
ghcIsMultiLine (GHC.L _ (GHC.AnnDocCommentNamed _)) = False
ghcIsMultiLine (GHC.L _ (GHC.AnnDocSection _ _))    = False
ghcIsMultiLine (GHC.L _ (GHC.AnnDocOptions _))      = False
ghcIsMultiLine (GHC.L _ (GHC.AnnDocOptionsOld _))   = False
ghcIsMultiLine (GHC.L _ (GHC.AnnLineComment _))     = False
ghcIsMultiLine (GHC.L _ (GHC.AnnBlockComment _))    = True

ghcCommentText :: GHC.Located GHC.AnnotationComment -> String
ghcCommentText (GHC.L _ (GHC.AnnDocCommentNext s))  = s
ghcCommentText (GHC.L _ (GHC.AnnDocCommentPrev s))  = s
ghcCommentText (GHC.L _ (GHC.AnnDocCommentNamed s)) = s
ghcCommentText (GHC.L _ (GHC.AnnDocSection _ s))    = s
ghcCommentText (GHC.L _ (GHC.AnnDocOptions s))      = s
ghcCommentText (GHC.L _ (GHC.AnnDocOptionsOld s))   = s
ghcCommentText (GHC.L _ (GHC.AnnLineComment s))     = s
ghcCommentText (GHC.L _ (GHC.AnnBlockComment s))    = "{-" ++ s ++ "-}"

-- ---------------------------------------------------------------------

isSymbolRdrName :: GHC.RdrName -> Bool
isSymbolRdrName n = GHC.isSymOcc $ GHC.rdrNameOcc n

rdrName2String :: GHC.RdrName -> String
rdrName2String r =
  case GHC.isExact_maybe r of
    Just n  -> name2String n
    Nothing ->
      case r of
        GHC.Unqual _occ -> GHC.occNameString $ GHC.rdrNameOcc r
        GHC.Qual modname _occ -> GHC.moduleNameString modname ++ "."
                            ++ (GHC.occNameString $ GHC.rdrNameOcc r)

name2String :: GHC.Name -> String
name2String name = showGhc name

-- |Show a GHC API structure
showGhc :: (GHC.Outputable a) => a -> String
showGhc x = GHC.showPpr GHC.unsafeGlobalDynFlags x


-- |Show a GHC API structure
showGhcDebug :: (GHC.Outputable a) => a -> String
showGhcDebug x = GHC.showSDocDebug GHC.unsafeGlobalDynFlags (GHC.ppr x)

-- ---------------------------------------------------------------------

instance Show (GHC.GenLocated GHC.SrcSpan GHC.Token) where
  show (GHC.L l tok) = show ((srcSpanStart l, srcSpanEnd l),tok)

-- ---------------------------------------------------------------------

pp :: GHC.Outputable a => a -> String
pp a = GHC.showPpr GHC.unsafeGlobalDynFlags a

-- ---------------------------------------------------------------------

{-
-- |For debugging
type OrganisedAnns = Map.Map GHC.SrcSpan ([(AnnConName,Annotation)]
                                         ,[(KeywordId, [DeltaPos])] )

-- | Re-arrange the annotations to make it clearer for users how they
-- hang together.
organiseAnns :: Anns -> OrganisedAnns
organiseAnns (anne,annf) = r
  where
    insertAnnE :: OrganisedAnns
               -> ((GHC.SrcSpan,AnnConName), Annotation)
               -> OrganisedAnns
    insertAnnE m ((ss,conName),ann) =
      case Map.lookup ss m of
        Just (cas,kds) -> Map.insert ss ((conName,ann):cas,kds) m
        Nothing        -> Map.insert ss ([(conName,ann)],  [])  m
    insertAnnF m ((ss,kw),dps) =
      case Map.lookup ss m of
        Just (cas,kds) -> Map.insert ss (cas,(kw,dps):kds) m
        Nothing        -> Map.insert ss ([], [(kw,dps)])   m
    re = foldl insertAnnE Map.empty (Map.toList anne)
    r  = foldl insertAnnF re        (Map.toList annf)
-}

-- ---------------------------------------------------------------------

-- Based on ghc-syb-utils version, but adding the annotation
-- information to each SrcLoc.
showAnnData :: Data a => Anns -> Int -> a -> String
showAnnData anns n =
  generic `ext1Q` list `extQ` string `extQ` fastString `extQ` srcSpan
          `extQ` name `extQ` occName `extQ` moduleName `extQ` var `extQ` dataCon
          `extQ` overLit
          `extQ` bagName `extQ` bagRdrName `extQ` bagVar `extQ` nameSet
          `extQ` fixity
  where generic :: Data a => a -> String
        generic t = indent n ++ "(" ++ showConstr (toConstr t)
                 ++ space (concat (intersperse " " (gmapQ (showAnnData anns (n+1)) t))) ++ ")"
        space "" = ""
        space s  = ' ':s
        indent i = "\n" ++ replicate i ' '
        string     = show :: String -> String
        fastString = ("{FastString: "++) . (++"}") . show :: GHC.FastString -> String
        list l     = indent n ++ "["
                              ++ concat (intersperse "," (map (showAnnData anns (n+1)) l)) ++ "]"

        name       = ("{Name: "++) . (++"}") . showSDoc_ . GHC.ppr :: GHC.Name -> String
        occName    = ("{OccName: "++) . (++"}") .  OccName.occNameString
        moduleName = ("{ModuleName: "++) . (++"}") . showSDoc_ . GHC.ppr :: GHC.ModuleName -> String

        -- srcSpan    = ("{"++) . (++"}") . showSDoc_ . GHC.ppr :: GHC.SrcSpan -> String
        srcSpan :: GHC.SrcSpan -> String
        srcSpan ss = "{ "++ (showSDoc_ (GHC.hang (GHC.ppr ss) (n+2)
                                                 -- (GHC.ppr (Map.lookup ss anns)
                                                 (GHC.text "")
                                                 ))
                      ++"}"

        var        = ("{Var: "++) . (++"}") . showSDoc_ . GHC.ppr :: GHC.Var -> String
        dataCon    = ("{DataCon: "++) . (++"}") . showSDoc_ . GHC.ppr :: GHC.DataCon -> String

        overLit :: (GHC.HsOverLit GHC.RdrName) -> String
        overLit    = ("{HsOverLit:"++) . (++"}") . showSDoc_ . GHC.ppr

        bagRdrName:: GHC.Bag (GHC.Located (GHC.HsBind GHC.RdrName)) -> String
        bagRdrName = ("{Bag(Located (HsBind RdrName)): "++) . (++"}") . list . GHC.bagToList
        bagName   :: GHC.Bag (GHC.Located (GHC.HsBind GHC.Name)) -> String
        bagName    = ("{Bag(Located (HsBind Name)): "++) . (++"}") . list . GHC.bagToList
        bagVar    :: GHC.Bag (GHC.Located (GHC.HsBind GHC.Var)) -> String
        bagVar     = ("{Bag(Located (HsBind Var)): "++) . (++"}") . list . GHC.bagToList

        nameSet = ("{NameSet: "++) . (++"}") . list . GHC.nameSetElems

        fixity = ("{Fixity: "++) . (++"}") . showSDoc_ . GHC.ppr :: GHC.Fixity -> String


showSDoc_ :: GHC.SDoc -> String
showSDoc_ = GHC.showSDoc GHC.unsafeGlobalDynFlags

-- ---------------------------------------------------------------------
-- Putting these here for the time being, to avoid import loops

ghead :: String -> [a] -> a
ghead  info []    = error $ "ghead "++info++" []"
ghead _info (h:_) = h

glast :: String -> [a] -> a
glast  info []    = error $ "glast " ++ info ++ " []"
glast _info h     = last h

gtail :: String -> [a] -> [a]
gtail  info []   = error $ "gtail " ++ info ++ " []"
gtail _info h    = tail h

gfromJust :: [Char] -> Maybe a -> a
gfromJust _info (Just h) = h
gfromJust  info Nothing = error $ "gfromJust " ++ info ++ " Nothing"

-- -------------------------------------------------------------------..
-- Copied from MissingH, does not compile with HEAD


{- | Merge two sorted lists into a single, sorted whole.

Example:

> merge [1,3,5] [1,2,4,6] -> [1,1,2,3,4,5,6]

QuickCheck test property:

prop_merge xs ys =
    merge (sort xs) (sort ys) == sort (xs ++ ys)
          where types = xs :: [Int]
-}
merge ::  (Ord a) => [a] -> [a] -> [a]
merge = mergeBy (compare)

{- | Merge two sorted lists using into a single, sorted whole,
allowing the programmer to specify the comparison function.

QuickCheck test property:

prop_mergeBy xs ys =
    mergeBy cmp (sortBy cmp xs) (sortBy cmp ys) == sortBy cmp (xs ++ ys)
          where types = xs :: [ (Int, Int) ]
                cmp (x1,_) (x2,_) = compare x1 x2
-}
mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy _cmp [] ys = ys
mergeBy _cmp xs [] = xs
mergeBy cmp (allx@(x:xs)) (ally@(y:ys))
        -- Ordering derives Eq, Ord, so the comparison below is valid.
        -- Explanation left as an exercise for the reader.
        -- Someone please put this code out of its misery.
    | (x `cmp` y) <= EQ = x : mergeBy cmp xs ally
    | otherwise = y : mergeBy cmp allx ys

