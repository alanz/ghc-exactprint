{-# LANGUAGE RecordWildCards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.GHC.ExactPrint.Transform
--
-----------------------------------------------------------------------------
module Language.Haskell.GHC.ExactPrint.Transform
        (
        -- * The Transform Monad
          Transform
        , runTransform
        , logTr
        , getAnnsT, putAnnsT

        -- * Operations
        , uniqueSrcSpan
        , isUniqueSrcSpan

        , adjustAnnOffset
        , mergeAnns
        , setLocatedAnns
  , fixBuggySrcSpan
  , fixBugsInAst
        ) where

import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Utils
import Language.Haskell.GHC.ExactPrint.Annotate

import Control.Monad.RWS
import Control.Monad.State
import Data.List

import Data.Data

import qualified GHC
import qualified FastString    as GHC
import qualified SrcLoc    as GHC

import qualified Data.Generics as SYB

import qualified Data.Map as Map

import Control.Monad.Trans.Free

import Debug.Trace

------------------------------------------------------------------------------
-- Transformoation of source elements

-- | Monad type for updating the AST and managing the annotations at the same
-- time. The W state is used to generate logging information if required.
type Transform a = RWS () [String] (Anns,Int) a

runTransform :: Anns ->Transform a -> (a,(Anns,Int),[String])
runTransform ans f = runRWS f () (ans,0)

logTr :: String -> Transform ()
logTr str = tell [str]

getAnnsT :: Transform Anns
getAnnsT = gets fst

putAnnsT :: Anns -> Transform ()
putAnnsT ans = do
  (_,col) <- get
  put (ans,col)

-- ---------------------------------------------------------------------

-- TODO: do we have to match the filename for GHC compare functions?
uniqueSrcSpan :: Transform GHC.SrcSpan
uniqueSrcSpan = do
  (an,col) <- get
  put (an,col + 1 )
  let pos = GHC.mkSrcLoc (GHC.mkFastString "ghc-exactprint") (-1) col
  return $ GHC.mkSrcSpan pos pos

isUniqueSrcSpan :: GHC.SrcSpan -> Bool
isUniqueSrcSpan ss = srcSpanStartLine ss == -1

-- ---------------------------------------------------------------------

adjustAnnOffset :: ColDelta -> Annotation -> Annotation
adjustAnnOffset (ColDelta cd) (Ann (DP (ro,co)) (ColDelta ad) _ cs kds) = Ann edp cd' edp cs kds'
  where
    edp = case ro of
      0 -> DP (ro,co)
      _ -> DP (ro,co - cd)
    cd' = ColDelta (ad - cd)
    kds' = fmap adjustEntrySpan kds
    adjustEntrySpan (AnnSpanEntry,dp) =
      case dp of
        DP (0,c) -> (AnnSpanEntry,DP (0,c))
        DP (r,c) -> (AnnSpanEntry,DP (r, c - cd))
    adjustEntrySpan x = x

-- ---------------------------------------------------------------------

mergeAnns :: Anns -> Anns -> Anns
mergeAnns = Map.unionWith (<>)

-- ---------------------------------------------------------------------

-- |Update the DeltaPos for the given annotation keys
setLocatedAnns :: (SYB.Data a) => Anns -> [(GHC.Located a,Annotation)] -> Anns
setLocatedAnns anne kvs = foldl' setLocatedAnn anne kvs

setLocatedAnn :: (SYB.Data a) => Anns -> (GHC.Located a, Annotation) ->  Anns
setLocatedAnn aane (loc, annVal) = setAnn aane (mkAnnKey loc,annVal)

-- |Update the DeltaPos for the given annotation key/val
setAnn :: Anns -> (AnnKey, Annotation) -> Anns
setAnn anne (k, Ann dp col edp cs _) = case
  Map.lookup k anne of
    Nothing               -> Map.insert k (Ann dp col edp cs []) anne
    Just (Ann _ _ _ _ ks) -> Map.insert k (Ann dp col edp cs ks) anne


-- | In GHC 7.10.1 the HsPar statement has an incorrect SrcSpan.
-- See https://ghc.haskell.org/trac/ghc/ticket/10207
-- This provides a workaround for it
fixBuggySrcSpan :: (SYB.Data a) => Maybe GHC.SrcSpan -> GHC.Located a -> GHC.Located a
fixBuggySrcSpan mbegin orig@(GHC.L _l a) = r -- `debug` ("fixBuggySrcSpan for " ++ show (mkAnnKey orig,SYB.typeOf a))
  where
    -- Need fully expanded type for the match
    hasStmt :: (Data t) => t -> Maybe (GHC.StmtLR GHC.RdrName GHC.RdrName (GHC.GenLocated GHC.SrcSpan (GHC.HsExpr GHC.RdrName)))
    hasStmt = SYB.gfindtype

    parStmtBlocSpan :: GHC.ParStmtBlock GHC.RdrName GHC.RdrName -> GHC.SrcSpan
    parStmtBlocSpan (GHC.ParStmtBlock []    _ _) = GHC.noSrcSpan -- Should never happen
    parStmtBlocSpan (GHC.ParStmtBlock stmts _ _) = GHC.combineLocs (head stmts) (last stmts)

    r1@(GHC.L l1 a1) = case hasStmt orig of
      Nothing -> orig `debug` ("fixBuggySrcSpan: got Nothing")
      Just (GHC.ParStmt [] _ _) -> orig `debug` ("fixBuggySrcSpan:found empty ParStmt")
      Just (GHC.ParStmt pbs _ _) -> GHC.L ss' a `debug` ("fixBuggySrcSpan:found ParStmt:returning:" ++ showGhc ss')
           where ss' = GHC.combineSrcSpans (parStmtBlocSpan $ head pbs) (parStmtBlocSpan $ last pbs)
      _ -> orig

    r = case mbegin of
      Nothing -> r1
      Just begin -> GHC.L (GHC.combineSrcSpans begin l1) a1

-- ---------------------------------------------------------------------

interpretChange :: GHC.SrcSpan -> GHC.SrcSpan -> Annotated a ->
                   FB a
interpretChange old new = iterTM go
  where
    go :: AnnotationF (FB a) -> FB a
    go (MarkEOF next) =
      change GHC.AnnEofPos >> next
    go (MarkPrim kwid _ next) =
      change kwid  >> next
      -- let annString = fromMaybe (keywordToString kwid) mstr in
      --   printStringAtMaybeAnn (G kwid) annString >> next
    go (MarkOutside _ (G kwid) next) =
      change kwid  >> next
    go (MarkOutside _ _ next) =
      next
    go (MarkInside akwid next) =
      change akwid >> next
    go (MarkMany akwid next) =
      change akwid >> next
    go (MarkOffsetPrim kwid _ _ next) =
      change kwid >> next
    go (MarkAfter akwid next) =
      change akwid >> next
    go (WithAST _ _ action next) =
      (interpretChange old new action) >> next
    go (CountAnns _ next) = do
      next 0
    go (SetLayoutFlag action next) =
      (interpretChange old new action) >> next
    go (MarkExternal _ akwid _ next) =
      change akwid  >> next
    go (StoreOriginalSrcSpan ss next) = next ss
    go (GetSrcSpanForKw _ next) = next GHC.noSrcSpan
    go (StoreString _ _ next) = next

    change :: GHC.AnnKeywordId -> FB ()
    change kwid = do
      (as, cs) <- get
      let r = Map.lookup (old, kwid) as
      case r of
        Nothing -> return ()
        Just v  -> do
          traceShowM (old, kwid)
          let as' = Map.insert (new,kwid) v (Map.delete (old, kwid) as)
          put (as', cs)


-- |There are a number of bugs in GHC 7.10.1 which result in incorrect
-- SrcSpans being allocated to AST elements, such that they do not
-- include all the annotated items in the SrcSpan.
-- See https://ghc.haskell.org/trac/ghc/ticket/10207
-- and https://ghc.haskell.org/trac/ghc/ticket/10209
-- and https://ghc.haskell.org/trac/ghc/ticket/10214
type FB a = State GHC.ApiAnns a
 -- runState  :: State s a -> s -> (a, s)
fixBugsInAst :: (SYB.Data t) => GHC.ApiAnns -> t -> (GHC.ApiAnns,t)
fixBugsInAst anns t = (anns',t')
  where
    (t',anns') = runState f anns

    -- Note: bottom up
    f = SYB.everywhereM (SYB.mkM parStmtBlock `SYB.extM` parStmt
                                              `SYB.extM` hsKind) t

    -- ---------------------------------

    changeAnnSpan :: Annotate ast => GHC.Located ast -> GHC.SrcSpan -> GHC.SrcSpan -> FB ()
    changeAnnSpan ast old new = do
      interpretChange old new (annotate ast)


    addAnnotation :: GHC.SrcSpan -> GHC.SrcSpan -> GHC.AnnKeywordId -> FB ()
    addAnnotation parent loc kw = do
      (anKW,anComments) <- get
      let anKW' = Map.insertWith (++) (parent,kw) [loc] anKW
      put (anKW',anComments)

    -- ---------------------------------

    parStmtBlockSpan :: GHC.ParStmtBlock GHC.RdrName GHC.RdrName -> GHC.SrcSpan
    parStmtBlockSpan (GHC.ParStmtBlock []    _ _) = GHC.noSrcSpan -- Should never happen
    parStmtBlockSpan (GHC.ParStmtBlock stmts _ _) = GHC.combineLocs (head stmts) (last stmts)

    parStmt :: GHC.Located (GHC.StmtLR GHC.RdrName GHC.RdrName (GHC.GenLocated GHC.SrcSpan (GHC.HsExpr GHC.RdrName)))
        -> FB (GHC.Located (GHC.StmtLR GHC.RdrName GHC.RdrName (GHC.GenLocated GHC.SrcSpan (GHC.HsExpr GHC.RdrName))))
    parStmt ps@(GHC.L _(GHC.ParStmt [] _ _)) = return ps
    parStmt (GHC.L _ ps@(GHC.ParStmt pbs _ _)) = do
      let ss = GHC.combineSrcSpans (parStmtBlockSpan $ head pbs) (parStmtBlockSpan $ last pbs)
      return (GHC.L ss ps)
    parStmt a@(GHC.L ss ast@GHC.TransStmt{..}) =
      let ss' = GHC.combineLocs (head trS_stmts) trS_using in
        changeAnnSpan a ss ss' >> return (GHC.L ss' ast)
    parStmt x = return x

    -- ---------------------------------

    parStmtBlock :: GHC.GenLocated GHC.SrcSpan (GHC.ParStmtBlock GHC.RdrName GHC.RdrName)
                 -> FB (GHC.GenLocated GHC.SrcSpan (GHC.ParStmtBlock GHC.RdrName GHC.RdrName))
    parStmtBlock psb@(GHC.L _  (GHC.ParStmtBlock []    _  _ )) = return psb
    parStmtBlock    a@(GHC.L ss (GHC.ParStmtBlock stmts ns se)) = do
      changeAnnSpan a ss ss'
      return (GHC.L ss' (GHC.ParStmtBlock stmts ns se))
      where
        ss' = GHC.combineLocs (head stmts) (last stmts)

    -- ---------------------------------

    hsKind :: (GHC.GenLocated GHC.SrcSpan (GHC.HsType GHC.RdrName))
           -> FB (GHC.GenLocated GHC.SrcSpan (GHC.HsType GHC.RdrName))
    hsKind a@(GHC.L ss k) = do
      changeAnnSpan a ss ss'
      addAnnotation ss' ss GHC.AnnEofPos
      return (GHC.L ss' k)
      where
        ss' = case GHC.getAnnotation anns ss GHC.AnnDcolon of
          []     -> ss
          (ld:_) -> GHC.combineSrcSpans ld ss

-- ---------------------------------------------------------------------
