{-# LANGUAGE CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}
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
        , modifyKeywordDeltasT
        , uniqueSrcSpanT

        -- * Operations
        , isUniqueSrcSpan

        -- * Managing lists
        , HasDecls (..)
        , captureOrder
        , captureOrderAnnKey
        , insertAtStart
        , insertAtEnd
        , insertAfter
        , insertBefore

        -- * Managing decls
        , wrapDecl
        , wrapSig
        , decl2Sig
        , decl2Bind

        -- * Other
        , adjustAnnOffset
        , mergeAnns
        , mergeAnnList
        , setLocatedAnns
        , setPrecedingLinesDecl
        , setPrecedingLines
--        , addSortKeyBefore

        ) where

import Language.Haskell.GHC.ExactPrint.Internal.Types
import Language.Haskell.GHC.ExactPrint.Utils

import Control.Monad.RWS
import Data.List
import Data.Maybe

import qualified FastString    as GHC
import qualified GHC           as GHC hiding (parseModule)

import qualified Data.Generics as SYB

import Data.Data

import qualified Data.Map as Map

-- import Debug.Trace

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

modifyKeywordDeltasT :: (Map.Map AnnKey Annotation -> Map.Map AnnKey Annotation)
                     -> Transform ()
modifyKeywordDeltasT f = do
  ans <- getAnnsT
  putAnnsT (modifyKeywordDeltas f ans)

-- ---------------------------------------------------------------------

-- TODO: do we have to match the filename for GHC compare functions?
uniqueSrcSpanT :: Transform GHC.SrcSpan
uniqueSrcSpanT = do
  (an,col) <- get
  put (an,col + 1 )
  let pos = GHC.mkSrcLoc (GHC.mkFastString "ghc-exactprint") (-1) col
  return $ GHC.mkSrcSpan pos pos

isUniqueSrcSpan :: GHC.SrcSpan -> Bool
isUniqueSrcSpan ss = srcSpanStartLine ss == -1

-- ---------------------------------------------------------------------

-- |If a list has been re-ordered or had items added, capture the new order in
-- the appropriate SortKeys.
captureOrder :: (Data a,Data b) => GHC.Located a -> [GHC.Located b] -> Anns -> Anns
captureOrder parent ls ans = captureOrderAnnKey (mkAnnKey parent) ls ans

-- |If a list has been re-ordered or had items added, capture the new order in
-- the appropriate SortKeys.
captureOrderAnnKey :: (Data b) => AnnKey -> [GHC.Located b] -> Anns -> Anns
captureOrderAnnKey parentKey ls ans = ans'
  where
    newList = map GHC.getLoc ls
    Ann{ annCapturedSpan } = fromMaybe annNone $ Map.lookup parentKey (getKeywordDeltas ans)
    insertion = case annCapturedSpan of
                  Nothing -> parentKey
                  -- TODO: Remove this hardcoding by storing the
                  -- constructor name as well?
                  Just (ss, d) -> AnnKey ss (CN "HsValBinds") d
    reList = Map.adjust (\an -> an {annSortKey = Just newList }) insertion
    ans' = modifyKeywordDeltas reList ans

-- ---------------------------------------------------------------------

wrapDecl :: GHC.LHsBind name -> GHC.LHsDecl name
wrapDecl (GHC.L l d) = GHC.L l (GHC.ValD d)

wrapSig :: GHC.LSig name -> GHC.LHsDecl name
wrapSig (GHC.L l d) = GHC.L l (GHC.SigD d)

decl2Sig :: GHC.LHsDecl name -> [GHC.LSig name]
decl2Sig (GHC.L l (GHC.SigD s)) = [GHC.L l s]
decl2Sig _                      = []

decl2Bind :: GHC.LHsDecl name -> [GHC.LHsBind name]
decl2Bind (GHC.L l (GHC.ValD s)) = [GHC.L l s]
decl2Bind _                      = []

-- ---------------------------------------------------------------------

adjustAnnOffset :: ColDelta -> Annotation -> Annotation
adjustAnnOffset (ColDelta cd) (Ann (DP (ro,co)) (ColDelta ad) _ cs kds sks cp) = Ann edp cd' edp cs kds' sks cp
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
mergeAnns (Anns a1) (Anns a2)
  = Anns (Map.union a1 a2)

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
setAnn as (k, a) =
  let newKds = maybe [] (annsDP) (Map.lookup k (getKeywordDeltas as)) in
    modifyKeywordDeltas (Map.insert k (a { annsDP = newKds })) as

-- ---------------------------------------------------------------------

-- |Unwrap a HsDecl and call setPrecedingLines on it
setPrecedingLinesDecl :: Anns -> GHC.LHsDecl GHC.RdrName -> Int -> Anns
setPrecedingLinesDecl ans ld@(GHC.L l d) n =
  case d of
      GHC.TyClD d       -> setPrecedingLines ans (GHC.L l d) n 0
      GHC.InstD d       -> setPrecedingLines ans (GHC.L l d) n 0
      GHC.DerivD d      -> setPrecedingLines ans (GHC.L l d) n 0
      GHC.ValD d        -> setPrecedingLines ans (GHC.L l d) n 0
      GHC.SigD d        -> setPrecedingLines ans (GHC.L l d) n 0
      GHC.DefD d        -> setPrecedingLines ans (GHC.L l d) n 0
      GHC.ForD d        -> setPrecedingLines ans (GHC.L l d) n 0
      GHC.WarningD d    -> setPrecedingLines ans (GHC.L l d) n 0
      GHC.AnnD d        -> setPrecedingLines ans (GHC.L l d) n 0
      GHC.RuleD d       -> setPrecedingLines ans (GHC.L l d) n 0
      GHC.VectD d       -> setPrecedingLines ans (GHC.L l d) n 0
      GHC.SpliceD d     -> setPrecedingLines ans (GHC.L l d) n 0
      GHC.DocD d        -> setPrecedingLines ans (GHC.L l d) n 0
      GHC.RoleAnnotD d  -> setPrecedingLines ans (GHC.L l d) n 0
#if __GLASGOW_HASKELL__ < 711
      GHC.QuasiQuoteD d -> setPrecedingLines ans (GHC.L l d) n 0
#endif

-- ---------------------------------------------------------------------

-- | Adjust the entry annotations to provide an `n` line preceding gap
setPrecedingLines :: (SYB.Data a) => Anns -> GHC.Located a -> Int -> Int -> Anns
setPrecedingLines anne ast n c =
  modifyKeywordDeltas (Map.alter go (mkAnnKey ast)) anne
  where
    go Nothing  = Just (Ann (DP (n,c)) (ColDelta c) (DP (n,c)) []  [] Nothing Nothing )
    go (Just a) = Just (a { annEntryDelta     = DP (n, c)
                          , annTrueEntryDelta = DP (n, c) })

-- ---------------------------------------------------------------------

-- -----
-- Classes
-- MP: I think these should move into a separate module.

class HasDecls t where

    -- | Return the HsDecls that are directly enclosed in the
    -- given syntax phrase. They are always returned in the wrapped HsDecl form,
    -- even if orginating in local decls.
    hsDecls :: t -> [GHC.LHsDecl GHC.RdrName]

    -- | Replace the directly enclosed decl list by the given
    --  decl list. Runs in the ghc-exactprint Transform Monad to be able to
    --  update list order annotations.
    replaceDecls :: t -> [GHC.LHsDecl GHC.RdrName] -> t

instance HasDecls ast => HasDecls (GHC.GenLocated l ast) where
  hsDecls (GHC.L _ ast) = hsDecls ast
  replaceDecls (GHC.L l m) ds = GHC.L l (replaceDecls m ds)

instance HasDecls (GHC.HsModule GHC.RdrName) where
  hsDecls m = GHC.hsmodDecls m
  replaceDecls m ds = m { GHC.hsmodDecls = ds }

insertAt :: (Data ast, HasDecls ast)
              => (GHC.SrcSpan -> [GHC.SrcSpan] -> [GHC.SrcSpan])
              -> GHC.Located ast
              -> GHC.LHsDecl GHC.RdrName
              -> Transform (GHC.Located ast)
insertAt f m decl = do
  let newKey = GHC.getLoc decl
      modKey = mkAnnKey m
      newValue a@Ann{..} = a { annSortKey = f newKey <$> annSortKey }
      oldDecls = hsDecls m
  modifyAnnsT (modifyKeywordDeltas (Map.adjust newValue modKey))

  return $ replaceDecls m (decl : oldDecls )

insertAtStart, insertAtEnd :: (Data ast, HasDecls ast)
              => GHC.Located ast
              -> GHC.LHsDecl GHC.RdrName
              -> Transform (GHC.Located ast)

insertAtStart = insertAt (:)
insertAtEnd   = insertAt (\x xs -> xs ++ [x])

insertAfter, insertBefore :: (Data old, Data ast, HasDecls ast)
                          => GHC.Located old
                          -> GHC.Located ast
                          -> GHC.LHsDecl GHC.RdrName
                          -> Transform (GHC.Located ast)
-- insertAfter (mkAnnKey -> k) = insertAt findAfter
insertAfter (GHC.getLoc -> k) = insertAt findAfter
  where
    findAfter x xs =
      let (fs, b:bs) = span (/= k) xs
      in fs ++ (b : x : bs)
insertBefore (GHC.getLoc -> k) = insertAt findBefore
  where
    findBefore x xs =
      let (fs, bs) = span (/= k) xs
      in fs ++ (x : bs)

