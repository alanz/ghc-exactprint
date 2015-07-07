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

        -- * Operations
        , uniqueSrcSpan
        , isUniqueSrcSpan

        -- * Managing lists
        , HasDecls (..)
        , captureOrder
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
        , setPrecedingLines
--        , addSortKeyBefore

        ) where

import Language.Haskell.GHC.ExactPrint.Annotate
import Language.Haskell.GHC.ExactPrint.Preprocess
import Language.Haskell.GHC.ExactPrint.Internal.Types
import Language.Haskell.GHC.ExactPrint.Utils

import Control.Monad.RWS
import Data.List
import Data.Maybe

-- import GHC.Paths (libdir)

-- import qualified ApiAnnotation as GHC
import qualified FastString    as GHC
import qualified GHC           as GHC hiding (parseModule)
-- import qualified HsSyn         as GHC
-- import qualified RdrName       as GHC
-- import qualified SrcLoc        as GHC

import qualified Data.Generics as SYB

import Control.Monad.Trans.Free
import Data.Data

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
captureOrder :: (Data a,Data b) => GHC.Located a -> [GHC.Located b] -> Anns -> Anns
captureOrder parent ls ans = ans'
  where
    newList = map GHC.getLoc ls
    Ann{ annCapturedSpan } = fromMaybe annNone $ Map.lookup (mkAnnKey parent) (getKeywordDeltas ans)
    insertion = case annCapturedSpan of
                  Nothing -> mkAnnKey parent
                  -- TODO: Remove this hardcoding by storing the
                  -- constructor name as well?
                  Just (ss, d) -> AnnKey ss (CN "HsValBinds") d
    reList = Map.adjust (\an -> an {annSortKey = Just newList }) insertion
    ans' = modifyKeywordDeltas reList ans


{-
-- |If a list has been re-ordered or had items added, capture the new order in
-- the appropriate SortKeys.
captureOrder :: [GHC.Located a] -> Anns -> Anns
captureOrder ls ans = modifySortKeys reList ans
  where
    newList = map (\(ss,r) -> (ss,SortKey (r,1,1%2))) $ zip (map GHC.getLoc ls) [1..]
    reList sks = foldr (uncurry Map.insert) sks newList
-}
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

-- | Adjust the entry annotations to provide an `n` line preceding gap
setPrecedingLines :: (SYB.Data a) => Anns -> GHC.Located a -> Int -> Int -> Anns
setPrecedingLines anne ast n c =
  modifyKeywordDeltas (Map.alter go (mkAnnKey ast)) anne
  where
    go Nothing  = Just (Ann (DP (n,c)) (ColDelta c) (DP (n,c)) []  [] Nothing Nothing )
    go (Just a) = Just (a { annEntryDelta = DP (n, c)
                          , annTrueEntryDelta = DP (n, c) })

-- ---------------------------------------------------------------------
{-
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
-}


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

