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

        , wrapSigT,wrapDeclT
        , pushDeclAnnT
        , decl2BindT,decl2SigT

        , getEntryDPT


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
        , balanceComments

        -- * Managing decls
        , wrapDecl
        , wrapSig
        , decl2Sig
        , decl2Bind
        , mkAnnKeyDecl

        -- * Other
--        , adjustAnnOffset
        , mergeAnns
        , mergeAnnList
        , setLocatedAnns
        , setPrecedingLinesDecl
        , setPrecedingLines
        , getEntryDP

        ) where

import Language.Haskell.GHC.ExactPrint.Internal.Types
import Language.Haskell.GHC.ExactPrint.Utils

import Control.Monad.RWS
import Data.List

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

-- |Make a copy of an AST element, replacing the existing SrcSpans with new
-- ones, and duplicating the matching annotations.
cloneT :: GHC.Located a -> Transform (GHC.Located a)
cloneT _ast = do
  error "Transform.cloneT undefined"

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
    reList = Map.adjust (\an -> an {annSortKey = Just newList }) parentKey
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

-- Crazy. This needs to be set up so that the original annotation is restored
-- after a pushDeclAnnT call.
wrapSigT :: GHC.LSig GHC.RdrName -> Transform (GHC.LHsDecl GHC.RdrName)
wrapSigT d@(GHC.L _ s) = do
  newSpan <- uniqueSrcSpanT
  let
    f (Anns ans) = case Map.lookup (mkAnnKey d) ans of
      Nothing -> Anns ans
      Just ann -> Anns (
                  Map.insert (mkAnnKey (GHC.L newSpan s)) ann
                $ Map.insert (mkAnnKey (GHC.L newSpan (GHC.SigD s))) ann ans)
  modifyAnnsT f
  return (GHC.L newSpan (GHC.SigD s))

-- ---------------------------------------------------------------------

-- Crazy. This needs to be set up so that the original annotation is restored
-- after a pushDeclAnnT call.
wrapDeclT :: GHC.LHsBind GHC.RdrName -> Transform (GHC.LHsDecl GHC.RdrName)
wrapDeclT d@(GHC.L _ s) = do
  newSpan <- uniqueSrcSpanT
  let
    f (Anns ans) = case Map.lookup (mkAnnKey d) ans of
      Nothing -> Anns ans
      Just ann -> Anns (
                  Map.insert (mkAnnKey (GHC.L newSpan s)) ann
                $ Map.insert (mkAnnKey (GHC.L newSpan (GHC.ValD s))) ann ans)
  modifyAnnsT f
  return (GHC.L newSpan (GHC.ValD s))

-- ---------------------------------------------------------------------

-- |Copy the top level annotation to a new SrcSpan and the unwrapped decl.
pushDeclAnnT :: GHC.LHsDecl GHC.RdrName -> Transform (GHC.LHsDecl GHC.RdrName)
pushDeclAnnT ld@(GHC.L l decl) = do
  newSpan <- uniqueSrcSpanT
  let
    blend ann Nothing = ann
    blend ann (Just annd)
      = annd { annEntryDelta        = annEntryDelta ann
             , annTrueEntryDelta    = annTrueEntryDelta ann
             , annPriorComments     = annPriorComments     ann  ++ annPriorComments     annd
             , annFollowingComments = annFollowingComments annd ++ annFollowingComments ann
             }
    duplicateAnn d (Anns ans) =
      case Map.lookup (mkAnnKey ld) ans of
        Nothing -> error $ "pushDeclAnnT:no key found for:" ++ show (mkAnnKey ld)
        -- Nothing -> Anns ans
        Just ann -> Anns $ Map.insert (mkAnnKey (GHC.L newSpan d))
                                      (blend ann (Map.lookup (mkAnnKey (GHC.L l d)) ans))
                                      ans
  case decl of
    GHC.TyClD d       -> modifyAnnsT (duplicateAnn d)
    GHC.InstD d       -> modifyAnnsT (duplicateAnn d)
    GHC.DerivD d      -> modifyAnnsT (duplicateAnn d)
    GHC.ValD d        -> modifyAnnsT (duplicateAnn d)
    GHC.SigD d        -> modifyAnnsT (duplicateAnn d)
    GHC.DefD d        -> modifyAnnsT (duplicateAnn d)
    GHC.ForD d        -> modifyAnnsT (duplicateAnn d)
    GHC.WarningD d    -> modifyAnnsT (duplicateAnn d)
    GHC.AnnD d        -> modifyAnnsT (duplicateAnn d)
    GHC.RuleD d       -> modifyAnnsT (duplicateAnn d)
    GHC.VectD d       -> modifyAnnsT (duplicateAnn d)
    GHC.SpliceD d     -> modifyAnnsT (duplicateAnn d)
    GHC.DocD d        -> modifyAnnsT (duplicateAnn d)
    GHC.RoleAnnotD d  -> modifyAnnsT (duplicateAnn d)
#if __GLASGOW_HASKELL__ < 711
    GHC.QuasiQuoteD d -> modifyAnnsT (duplicateAnn d)
#endif
  return (GHC.L newSpan decl)

-- ---------------------------------------------------------------------

-- |Unwrap a LHsDecl to its underlying LHsBind, transferring the top level annotation to a
-- new SrcSpan in the process.
decl2BindT :: GHC.LHsDecl GHC.RdrName -> Transform [GHC.LHsBind GHC.RdrName]
decl2BindT vd@(GHC.L _ (GHC.ValD d)) = do
  newSpan <- uniqueSrcSpanT
  logTr $ "decl2BindT:newSpan=" ++ showGhc newSpan
  let
    duplicateAnn (Anns ans) =
      case Map.lookup (mkAnnKey vd) ans of
        Nothing -> Anns ans
        Just ann -> Anns $ Map.insert (mkAnnKey (GHC.L newSpan d)) ann ans
  modifyAnnsT duplicateAnn
  return [GHC.L newSpan d]
decl2BindT _ = return []

-- ---------------------------------------------------------------------

-- |Unwrap a LHsDecl to its underlying LSig, transferring the top level annotation to a
-- new SrcSpan in the process.
decl2SigT :: GHC.LHsDecl GHC.RdrName -> Transform [GHC.LSig GHC.RdrName]
decl2SigT vs@(GHC.L _ (GHC.SigD s)) = do
  newSpan <- uniqueSrcSpanT
  logTr $ "decl2SigT:newSpan=" ++ showGhc newSpan
  let
    duplicateAnn (Anns ans) =
      case Map.lookup (mkAnnKey vs) ans of
        Nothing -> Anns ans
        Just ann -> Anns $ Map.insert (mkAnnKey (GHC.L newSpan s)) ann ans
  modifyAnnsT duplicateAnn
  return [GHC.L newSpan s]
decl2SigT _ = return []

-- ---------------------------------------------------------------------

getEntryDPT :: (Data a) => GHC.Located a -> Transform DeltaPos
getEntryDPT ast = do
  anns <- getAnnsT
  return (getEntryDP anns ast)

-- ---------------------------------------------------------------------

mkAnnKeyDecl :: GHC.LHsDecl GHC.RdrName -> AnnKey
mkAnnKeyDecl = declFun mkAnnKey

-- ---------------------------------------------------------------------

{-
adjustAnnOffset :: Annotation -> Annotation
adjustAnnOffset (ColDelta cd) a@(Ann{ annEntryDelta=(DP (ro,co)), annDelta=(ColDelta ad), annsDP = kds})
  = a { annDelta = cd', annsDP = kds' }
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
-}
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
setPrecedingLinesDecl :: Anns -> GHC.LHsDecl GHC.RdrName -> Int -> Int -> Anns
setPrecedingLinesDecl ans ld n c =
  declFun (\a -> setPrecedingLines ans a n c) ld

declFun :: (forall a . Data a => GHC.Located a -> b) -> GHC.LHsDecl GHC.RdrName -> b
declFun f (GHC.L l de) =
  case de of
      GHC.TyClD d       -> f (GHC.L l d)
      GHC.InstD d       -> f (GHC.L l d)
      GHC.DerivD d      -> f (GHC.L l d)
      GHC.ValD d        -> f (GHC.L l d)
      GHC.SigD d        -> f (GHC.L l d)
      GHC.DefD d        -> f (GHC.L l d)
      GHC.ForD d        -> f (GHC.L l d)
      GHC.WarningD d    -> f (GHC.L l d)
      GHC.AnnD d        -> f (GHC.L l d)
      GHC.RuleD d       -> f (GHC.L l d)
      GHC.VectD d       -> f (GHC.L l d)
      GHC.SpliceD d     -> f (GHC.L l d)
      GHC.DocD d        -> f (GHC.L l d)
      GHC.RoleAnnotD d  -> f (GHC.L l d)
#if __GLASGOW_HASKELL__ < 711
      GHC.QuasiQuoteD d -> f (GHC.L l d)
#endif

-- ---------------------------------------------------------------------

-- | Adjust the entry annotations to provide an `n` line preceding gap
setPrecedingLines :: (SYB.Data a) => Anns -> GHC.Located a -> Int -> Int -> Anns
setPrecedingLines anne ast n c =
  modifyKeywordDeltas (Map.alter go (mkAnnKey ast)) anne
  where
    go Nothing  = Just (annNone { annEntryDelta = (DP (n,c))
                                , annTrueEntryDelta = (DP (n,c))  })
    go (Just a) = Just (a { annEntryDelta     = DP (n, c)
                             , annTrueEntryDelta = DP (1,0) })

-- ---------------------------------------------------------------------

getEntryDP :: (Data a) => Anns -> GHC.Located a -> DeltaPos
getEntryDP (Anns anns) ast =
  case Map.lookup (mkAnnKey ast) anns of
    Nothing  -> DP (0,0)
    Just ann -> annTrueEntryDelta ann

-- ---------------------------------------------------------------------

-- |Prior to moving an AST element, make sure any trailing comments belonging to
-- it are attached to it, and not the following element. Of necessity this is a
-- heuristic process, to be tuned later. Possibly a variant should be provided
-- with a passed-in decision function.
balanceComments :: (Data a,Data b) => GHC.Located a -> GHC.Located b -> Transform ()
balanceComments first second = do
  let
    k1 = mkAnnKey first
    k2 = mkAnnKey second
    moveComments p ans = ans'
      where
        an1 = gfromJust "balanceComments k1" $ Map.lookup k1 ans
        an2 = gfromJust "balanceComments k2" $ Map.lookup k2 ans
        -- cs1b = annPriorComments     an1
        cs1f = annFollowingComments an1
        cs2b = annPriorComments an2
        (move,stay) = break p cs2b
        an1' = an1 { annFollowingComments = cs1f ++ move}
        an2' = an2 { annPriorComments = stay}
        ans' = Map.insert k1 an1' $ Map.insert k2 an2' ans

    simpleBreak (_,DP (r,_c)) = r > 0

  modifyAnnsT (modifyKeywordDeltas (moveComments simpleBreak))

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

-- ---------------------------------------------------------------------
