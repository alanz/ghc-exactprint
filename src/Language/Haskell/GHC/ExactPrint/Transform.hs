{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}
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
        , getAnnsT, putAnnsT, modifyAnnsT

        -- * Operations
        , uniqueSrcSpan
        , isUniqueSrcSpan

        -- * Managing lists
        , captureOrder

        -- * Other
        , adjustAnnOffset
        , mergeAnns
        , mergeAnnList
        , setLocatedAnns
        , setPrecedingLines
        , addSortKeyBefore

        ) where

import Language.Haskell.GHC.ExactPrint.Annotate
-- import Language.Haskell.GHC.ExactPrint.Delta
import Language.Haskell.GHC.ExactPrint.Preprocess
import Language.Haskell.GHC.ExactPrint.Internal.Types
import Language.Haskell.GHC.ExactPrint.Utils

import Control.Monad.RWS
import Control.Monad.State
import Data.List

-- import Data.Data

import GHC.Paths (libdir)

import qualified ApiAnnotation as GHC
-- import qualified DynFlags      as GHC
import qualified FastString    as GHC
import qualified GHC           as GHC hiding (parseModule)
-- import qualified HeaderInfo    as GHC
-- import qualified Lexer         as GHC
-- import qualified MonadUtils    as GHC
-- import qualified OrdList       as GHC
-- import qualified Outputable    as GHC
-- import qualified Parser        as GHC
-- import qualified RdrHsSyn      as GHC ( checkPattern )
import qualified SrcLoc        as GHC
-- import qualified StringBuffer  as GHC

import qualified Data.Generics as SYB

import Control.Monad.Trans.Free
import Data.Ratio
-- import Distribution.Helper

import qualified Data.Map as Map

import Debug.Trace

------------------------------------------------------------------------------
-- Transformation of source elements

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

modifyAnnsT :: (Anns -> Anns) -> Transform ()
modifyAnnsT f = do
  ans <- getAnnsT
  putAnnsT (f ans)

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

-- |If a list has been re-ordered or had items added, capture the new order in
-- the appropriate SortKeys.
captureOrder :: [GHC.Located a] -> Anns -> Anns
captureOrder ls ans = modifySortKeys reList ans
  where
    newList = map (\(ss,r) -> (ss,SortKey (r,1,1%2))) $ zip (map GHC.getLoc ls) [1..]
    reList sks = foldr (uncurry Map.insert) sks newList

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

-- | Left bias pair union
mergeAnns :: Anns -> Anns -> Anns
mergeAnns (Anns a1 k1) (Anns a2 k2)
  = Anns (Map.union a1 a2) (Map.union k1 k2)

mergeAnnList :: [Anns] -> Anns
mergeAnnList [] = error "mergeAnnList must have at lease one entry"
mergeAnnList (x:xs) = foldr mergeAnns x xs

-- ---------------------------------------------------------------------

-- |Update the DeltaPos for the given annotation keys
setLocatedAnns :: (SYB.Data a) => Anns -> [(GHC.Located a,Annotation)] -> Anns
setLocatedAnns anne kvs = foldl' setLocatedAnn anne kvs

setLocatedAnn :: (SYB.Data a) => Anns -> (GHC.Located a, Annotation) ->  Anns
setLocatedAnn aane (loc, annVal) = setAnn aane (mkAnnKey loc,annVal)

-- |Update the DeltaPos for the given annotation key/val
setAnn :: Anns -> (AnnKey, Annotation) -> Anns
setAnn as (k, Ann dp col edp cs _) =
  let newKds = maybe [] (annsDP) (Map.lookup k (getKeywordDeltas as)) in
    modifyKeywordDeltas (Map.insert k (Ann dp col edp cs newKds)) as

-- | Adjust the entry annotations to provide an `n` line preceding gap
setPrecedingLines :: (SYB.Data a) => Anns -> GHC.Located a -> Int -> Int -> Anns
setPrecedingLines anne ast n c =
  modifyKeywordDeltas (Map.alter go (mkAnnKey ast)) anne
  where
    go Nothing  = Just (Ann (DP (n,c)) (ColDelta c) (DP (n,c)) []  [])
    go (Just (Ann ed cd _ted cs dps)) = Just (Ann (DP (n,c)) cd (DP (n,c)) cs dps)

-- ---------------------------------------------------------------------

-- |Add a sort key for the first item to come before the second
addSortKeyBefore :: Anns -> GHC.Located a -> GHC.Located b -> Anns
addSortKeyBefore anns (GHC.L l1 _) (GHC.L l2 _) = anns { annsSortKeys = sk' }
  where
    sk = annsSortKeys anns
    (other,sk2) = case Map.lookup l2 sk of
      Just k -> (k,sk)
      Nothing -> (ss2SortKey l2,Map.insert l2 (ss2SortKey l2) sk)
    sk' = Map.insert l1 (sortKeyBefore other) sk2
          `debug` ("addSortKeyBefore:(l1,l2,otherk,new sk)=" ++ show (l1,l2,other,sortKeyBefore other))

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
    go (WithAST _ _ _ action next) =
      (interpretChange old new action) >> next
    go (CountAnns _ next) = do
      next 0
    go (SetLayoutFlag action next) =
      (interpretChange old new action) >> next
    go (MarkExternal _ akwid _ next) =
      change akwid  >> next
    go (StoreOriginalSrcSpan ss d next) = next (ss,d)
    go (GetSrcSpanForKw _ next) = next GHC.noSrcSpan
    go (StoreString _ _ next) = next
    go (GetNextDisambiguator next) = return NotNeeded >>= next
    go (AnnotationsToComments _ next) = next

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

