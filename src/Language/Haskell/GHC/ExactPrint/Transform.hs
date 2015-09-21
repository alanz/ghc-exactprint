{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.GHC.ExactPrint.Transform
--
-- This module is currently under heavy development, and no promises are made
-- about API stability. Use with care.
--
-- We weclome any feedback / contributions on this, as it is the main point of
-- the library.
--
-----------------------------------------------------------------------------
module Language.Haskell.GHC.ExactPrint.Transform
        (
        -- * The Transform Monad
          Transform
        , runTransform
        , runTransformFrom

        -- * Transform monad operations
        , logTr
        , logDataWithAnnsTr
        , getAnnsT, putAnnsT, modifyAnnsT
        , uniqueSrcSpanT

        , cloneT

        , getEntryDPT
        , setEntryDPT
        , transferEntryDPT
        , setPrecedingLinesDeclT
        , setPrecedingLinesT
        , addSimpleAnnT
        , addTrailingCommaT
        , removeTrailingCommaT

        -- ** Managing declarations, in Transform monad
        , HasTransform (..)
        , HasDecls (..)
        , hsDeclsPatBind, hsDeclsPatBindD
        , hsDeclsValBinds, replaceDeclsValbinds
        , modifyDeclsT
        , modifyLocalDecl

        -- ** Managing lists, Transform monad
        , insertAtStart
        , insertAtEnd
        , insertAfter
        , insertBefore

        -- *** Low level operations used in 'HasDecls'
        , balanceComments
        , balanceTrailingComments
        , moveTrailingComments

        -- ** Managing lists, pure functions
        , captureOrder
        , captureOrderAnnKey

        -- * Operations
        , isUniqueSrcSpan

        -- * Pure functions
        , mergeAnns
        , mergeAnnList
        , setPrecedingLinesDecl
        , setPrecedingLines
        , getEntryDP
        , setEntryDP
        , transferEntryDP
        , addTrailingComma
        , wrapSig, wrapDecl
        , decl2Sig, decl2Bind

        ) where

import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Utils

import Control.Monad.RWS


import qualified Bag           as GHC
import qualified FastString    as GHC
import qualified GHC           as GHC hiding (parseModule)

import qualified Data.Generics as SYB

import Data.Data
import Data.List
import Data.Maybe

import qualified Data.Map as Map
import Control.Monad.Writer
import Control.Monad.State

import Debug.Trace

------------------------------------------------------------------------------
-- Transformation of source elements

-- | Monad type for updating the AST and managing the annotations at the same
-- time. The W state is used to generate logging information if required.
newtype Transform a = Transform { getTransform :: RWS () [String] (Anns,Int) a }
                        deriving (Monad, Applicative, Functor, MonadState (Anns, Int), MonadReader (), MonadWriter [String])

-- | Run a transformation in the 'Transform' monad, returning the updated
-- annotations and any logging generated via 'logTr'
runTransform :: Anns -> Transform a -> (a,(Anns,Int),[String])
runTransform ans f = runTransformFrom 0 ans f

-- | Run a transformation in the 'Transform' monad, returning the updated
-- annotations and any logging generated via 'logTr', allocating any new
-- SrcSpans from the provided initial value.
runTransformFrom :: Int -> Anns -> Transform a -> (a,(Anns,Int),[String])
runTransformFrom seed ans f = runRWS (getTransform f) () (ans,seed)

-- |Log a string to the output of the Monad
logTr :: String -> Transform ()
logTr str = tell [str]

-- |Log a representation of the given AST with annotations to the output of the
-- Monad
logDataWithAnnsTr :: (SYB.Data a) => String -> a -> Transform ()
logDataWithAnnsTr str ast = do
  anns <- getAnnsT
  logTr $ str ++ showAnnData anns 0 ast

-- |Access the 'Anns' being modified in this transformation
getAnnsT :: Transform Anns
getAnnsT = gets fst

-- |Replace the 'Anns' after any changes
putAnnsT :: Anns -> Transform ()
putAnnsT ans = do
  (_,col) <- get
  put (ans,col)

-- |Change the stored 'Anns'
modifyAnnsT :: (Anns -> Anns) -> Transform ()
modifyAnnsT f = do
  ans <- getAnnsT
  putAnnsT (f ans)

-- ---------------------------------------------------------------------

-- |Once we have 'Anns', a 'GHC.SrcSpan' is used purely as part of an 'AnnKey'
-- to index into the 'Anns'. If we need to add new elements to the AST, they
-- need their own 'GHC.SrcSpan' for this.
uniqueSrcSpanT :: Transform GHC.SrcSpan
uniqueSrcSpanT = do
  (an,col) <- get
  put (an,col + 1 )
  let pos = GHC.mkSrcLoc (GHC.mkFastString "ghc-exactprint") (-1) col
  return $ GHC.mkSrcSpan pos pos

-- |Test whether a given 'GHC.SrcSpan' was generated by 'uniqueSrcSpanT'
isUniqueSrcSpan :: GHC.SrcSpan -> Bool
isUniqueSrcSpan ss = srcSpanStartLine ss == -1

-- ---------------------------------------------------------------------

-- |Make a copy of an AST element, replacing the existing SrcSpans with new
-- ones, and duplicating the matching annotations.
cloneT :: (Data a,Typeable a) => a -> Transform (a, [(GHC.SrcSpan, GHC.SrcSpan)])
cloneT ast = do
  runWriterT $ SYB.everywhereM (return `SYB.ext2M` replaceLocated) ast
  where
    replaceLocated :: forall loc a. (Typeable loc,Typeable a, Data a)
                    => (GHC.GenLocated loc a) -> WriterT [(GHC.SrcSpan, GHC.SrcSpan)] Transform (GHC.GenLocated loc a)
    replaceLocated (GHC.L l t) = do
      case cast l :: Maybe GHC.SrcSpan of
        Just ss -> do
          newSpan <- lift uniqueSrcSpanT
          lift $ modifyAnnsT (\anns -> case Map.lookup (mkAnnKey (GHC.L ss t)) anns of
                                  Nothing -> anns
                                  Just an -> Map.insert (mkAnnKey (GHC.L newSpan t)) an anns)
          tell [(ss, newSpan)]
          return $ fromJust . cast  $ GHC.L newSpan t
        Nothing -> return (GHC.L l t)

-- ---------------------------------------------------------------------

-- |If a list has been re-ordered or had items added, capture the new order in
-- the appropriate 'annSortKey' attached to the 'Annotation' for the first
-- parameter.
captureOrder :: (Data a) => GHC.Located a -> [GHC.Located b] -> Anns -> Anns
captureOrder parent ls ans = captureOrderAnnKey (mkAnnKey parent) ls ans

-- |If a list has been re-ordered or had items added, capture the new order in
-- the appropriate 'annSortKey' item of the supplied 'AnnKey'
captureOrderAnnKey :: AnnKey -> [GHC.Located b] -> Anns -> Anns
captureOrderAnnKey parentKey ls ans = ans'
  where
    newList = map GHC.getLoc ls
    reList = Map.adjust (\an -> an {annSortKey = Just newList }) parentKey
    ans' = reList ans

-- ---------------------------------------------------------------------

-- |Pure function to convert a 'GHC.LHsDecl' to a 'GHC.LHsBind'. This does
-- nothing to any annotations that may be attached to either of the elements.
-- It is used as a utility function in 'replaceDecls'
decl2Bind :: GHC.LHsDecl name -> [GHC.LHsBind name]
decl2Bind (GHC.L l (GHC.ValD s)) = [GHC.L l s]
decl2Bind _                      = []

-- |Pure function to convert a 'GHC.LSig' to a 'GHC.LHsBind'. This does
-- nothing to any annotations that may be attached to either of the elements.
-- It is used as a utility function in 'replaceDecls'
decl2Sig :: GHC.LHsDecl name -> [GHC.LSig name]
decl2Sig (GHC.L l (GHC.SigD s)) = [GHC.L l s]
decl2Sig _                      = []

-- ---------------------------------------------------------------------

-- |Convert a 'GHC.LSig' into a 'GHC.LHsDecl'
wrapSig :: GHC.LSig GHC.RdrName -> GHC.LHsDecl GHC.RdrName
wrapSig (GHC.L l s) = GHC.L l (GHC.SigD s)

-- ---------------------------------------------------------------------

-- |Convert a 'GHC.LHsBind' into a 'GHC.LHsDecl'
wrapDecl :: GHC.LHsBind GHC.RdrName -> GHC.LHsDecl GHC.RdrName
wrapDecl (GHC.L l s) = GHC.L l (GHC.ValD s)

-- ---------------------------------------------------------------------

-- |Create a simple 'Annotation' without comments, and attach it to the first
-- parameter.
addSimpleAnnT :: (Data a) => GHC.Located a -> DeltaPos -> [(KeywordId, DeltaPos)] -> Transform ()
addSimpleAnnT ast dp kds = do
  let ann = annNone { annEntryDelta = dp
                    , annsDP = kds
                    }
  modifyAnnsT (Map.insert (mkAnnKey ast) ann)

-- ---------------------------------------------------------------------

-- |Add a trailing comma annotation, unless there is already one
addTrailingCommaT :: (Data a) => GHC.Located a -> Transform ()
addTrailingCommaT ast = do
  modifyAnnsT (addTrailingComma ast (DP (0,0)))

-- ---------------------------------------------------------------------

-- |Remove a trailing comma annotation, if there is one one
removeTrailingCommaT :: (Data a) => GHC.Located a -> Transform ()
removeTrailingCommaT ast = do
  modifyAnnsT (removeTrailingComma ast)

-- ---------------------------------------------------------------------

-- |'Transform' monad version of 'getEntryDP'
getEntryDPT :: (Data a) => GHC.Located a -> Transform DeltaPos
getEntryDPT ast = do
  anns <- getAnnsT
  return (getEntryDP anns ast)

-- ---------------------------------------------------------------------

-- |'Transform' monad version of 'getEntryDP'
setEntryDPT :: (Data a) => GHC.Located a -> DeltaPos -> Transform ()
setEntryDPT ast dp = do
  modifyAnnsT (setEntryDP ast dp)

-- ---------------------------------------------------------------------

-- |'Transform' monad version of 'transferEntryDP'
transferEntryDPT :: (Data a,Data b) => GHC.Located a -> GHC.Located b -> Transform ()
transferEntryDPT a b =
  modifyAnnsT (transferEntryDP a b)

-- ---------------------------------------------------------------------

-- |'Transform' monad version of 'setPrecedingLinesDecl'
setPrecedingLinesDeclT ::  GHC.LHsDecl GHC.RdrName -> Int -> Int -> Transform ()
setPrecedingLinesDeclT ld n c =
  modifyAnnsT (setPrecedingLinesDecl ld n c)

-- ---------------------------------------------------------------------

-- |'Transform' monad version of 'setPrecedingLines'
setPrecedingLinesT ::  (SYB.Data a) => GHC.Located a -> Int -> Int -> Transform ()
setPrecedingLinesT ld n c =
  modifyAnnsT (setPrecedingLines ld n c)

-- ---------------------------------------------------------------------

-- | Left bias pair union
mergeAnns :: Anns -> Anns -> Anns
mergeAnns
  = Map.union

-- |Combine a list of annotations
mergeAnnList :: [Anns] -> Anns
mergeAnnList [] = error "mergeAnnList must have at lease one entry"
mergeAnnList (x:xs) = foldr mergeAnns x xs

-- ---------------------------------------------------------------------

-- |Unwrap a HsDecl and call setPrecedingLines on it
-- ++AZ++ TODO: get rid of this, it is a synonym only
setPrecedingLinesDecl :: GHC.LHsDecl GHC.RdrName -> Int -> Int -> Anns -> Anns
setPrecedingLinesDecl ld n c ans = setPrecedingLines ld n c ans

-- ---------------------------------------------------------------------

-- | Adjust the entry annotations to provide an `n` line preceding gap
setPrecedingLines :: (SYB.Data a) => GHC.Located a -> Int -> Int -> Anns -> Anns
setPrecedingLines ast n c anne = setEntryDP ast (DP (n,c)) anne

-- ---------------------------------------------------------------------

-- |Return the true entry 'DeltaPos' from the annotation for a given AST
-- element. This is the 'DeltaPos' ignoring any comments.
getEntryDP :: (Data a) => Anns -> GHC.Located a -> DeltaPos
getEntryDP anns ast =
  case Map.lookup (mkAnnKey ast) anns of
    Nothing  -> DP (0,0)
    Just ann -> annTrueEntryDelta ann

-- ---------------------------------------------------------------------

-- |Set the true entry 'DeltaPos' from the annotation for a given AST
-- element. This is the 'DeltaPos' ignoring any comments.
setEntryDP :: (Data a) => GHC.Located a -> DeltaPos -> Anns -> Anns
setEntryDP ast dp anns =
  case Map.lookup (mkAnnKey ast) anns of
    Nothing  -> Map.insert (mkAnnKey ast) (annNone { annEntryDelta = dp}) anns
    Just ann -> Map.insert (mkAnnKey ast) (ann'    { annEntryDelta = annCommentEntryDelta ann' dp}) anns
      where
        ann' = setCommentEntryDP ann dp

-- ---------------------------------------------------------------------

-- |When setting an entryDP, the leading comment needs to be adjusted too
setCommentEntryDP :: Annotation -> DeltaPos -> Annotation
-- setCommentEntryDP ann dp = error $ "setCommentEntryDP:ann'=" ++ show ann'
setCommentEntryDP ann dp = ann'
  where
    ann' = case (annPriorComments ann) of
      [] -> ann
      [(pc,_)]     -> ann { annPriorComments = [(pc,dp)] }
      ((pc,_):pcs) -> ann { annPriorComments = ((pc,dp):pcs) }

-- ---------------------------------------------------------------------

-- |Take the annEntryDelta associated with the first item and associate it with the second.
-- Also transfer any comments occuring before it.
transferEntryDP :: (SYB.Data a, SYB.Data b) => GHC.Located a -> GHC.Located b -> Anns -> Anns
transferEntryDP a b anns = (const anns2) anns
  where
    maybeAnns = do -- Maybe monad
      anA <- Map.lookup (mkAnnKey a) anns
      anB <- Map.lookup (mkAnnKey b) anns
      let anB'  = Ann
            { annEntryDelta        = DP (0,0) -- Need to adjust for comments after
            , annPriorComments     = annPriorComments     anB
            , annFollowingComments = annFollowingComments anB
            , annsDP               = annsDP          anB
            , annSortKey           = annSortKey      anB
            , annCapturedSpan      = annCapturedSpan anB
            }
      return ((Map.insert (mkAnnKey b) anB' anns),annLeadingCommentEntryDelta anA)
    (anns',dp) = fromMaybe
                  (error $ "transferEntryDP: lookup failed (a,b)=" ++ show (mkAnnKey a,mkAnnKey b))
                  maybeAnns
    anns2 = setEntryDP b dp anns'

-- ---------------------------------------------------------------------

addTrailingComma :: (SYB.Data a) => GHC.Located a -> DeltaPos -> Anns -> Anns
addTrailingComma a dp anns =
  case Map.lookup (mkAnnKey a) anns of
    Nothing -> anns
    Just an ->
      case find isAnnComma (annsDP an) of
        Nothing -> Map.insert (mkAnnKey a) (an { annsDP = annsDP an ++ [(G GHC.AnnComma,dp)]}) anns
        Just _  -> anns
      where
        isAnnComma (G GHC.AnnComma,_) = True
        isAnnComma _                  = False

-- ---------------------------------------------------------------------

removeTrailingComma :: (SYB.Data a) => GHC.Located a -> Anns -> Anns
removeTrailingComma a anns =
  case Map.lookup (mkAnnKey a) anns of
    Nothing -> anns
    Just an ->
      case find isAnnComma (annsDP an) of
        Nothing -> anns
        Just _  -> Map.insert (mkAnnKey a) (an { annsDP = filter (not.isAnnComma) (annsDP an) }) anns
      where
        isAnnComma (G GHC.AnnComma,_) = True
        isAnnComma _                  = False

-- ---------------------------------------------------------------------

balanceComments :: (Data a,Data b) => GHC.Located a -> GHC.Located b -> Transform ()
balanceComments first second = do
  -- ++AZ++ : replace the nested casts with appropriate SYB.gmapM
  -- logTr $ "balanceComments entered"
  -- logDataWithAnnsTr "first" first
  case cast first :: Maybe (GHC.LHsDecl GHC.RdrName) of
    Just (GHC.L l (GHC.ValD fb@(GHC.FunBind{}))) -> do
      balanceCommentsFB (GHC.L l fb) second
    _ -> case cast first :: Maybe (GHC.LHsBind GHC.RdrName) of
      Just fb'@(GHC.L _ (GHC.FunBind{})) -> do
        balanceCommentsFB fb' second
      _ -> balanceComments' first second

balanceCommentsFB :: (Data b) => GHC.LHsBind GHC.RdrName -> GHC.Located b -> Transform ()
balanceCommentsFB (GHC.L _ (GHC.FunBind _ _ (GHC.MG matches _ _ _) _ _ _)) second = do
  -- logTr $ "balanceCommentsFB entered"
  balanceComments' (last matches) second
balanceCommentsFB f s = balanceComments' f s

-- |Prior to moving an AST element, make sure any trailing comments belonging to
-- it are attached to it, and not the following element. Of necessity this is a
-- heuristic process, to be tuned later. Possibly a variant should be provided
-- with a passed-in decision function.
balanceComments' :: (Data a,Data b) => GHC.Located a -> GHC.Located b -> Transform ()
balanceComments' first second = do
  let
    k1 = mkAnnKey first
    k2 = mkAnnKey second
    moveComments p ans = ans'
      where
        an1 = gfromJust "balanceComments' k1" $ Map.lookup k1 ans
        an2 = gfromJust "balanceComments' k2" $ Map.lookup k2 ans
        cs1f = annFollowingComments an1
        cs2b = annPriorComments an2
        (move,stay) = break p cs2b
        an1' = an1 { annFollowingComments = cs1f ++ move}
        an2' = an2 { annPriorComments = stay}
        ans' = Map.insert k1 an1' $ Map.insert k2 an2' ans

    simpleBreak (_,DP (r,_c)) = r > 0

  modifyAnnsT (moveComments simpleBreak)

-- ---------------------------------------------------------------------


-- |After moving an AST element, make sure any comments that may belong
-- with the following element in fact do. Of necessity this is a heuristic
-- process, to be tuned later. Possibly a variant should be provided with a
-- passed-in decision function.
balanceTrailingComments :: (Data a,Data b) => GHC.Located a -> GHC.Located b -> Transform [(Comment, DeltaPos)]
balanceTrailingComments first second = do
  let
    k1 = mkAnnKey first
    k2 = mkAnnKey second
    moveComments p ans = (ans',move)
      where
        an1 = gfromJust "balanceTrailingComments k1" $ Map.lookup k1 ans
        an2 = gfromJust "balanceTrailingComments k2" $ Map.lookup k2 ans
        cs1f = annFollowingComments an1
        (move,stay) = break p cs1f
        an1' = an1 { annFollowingComments = stay }
        ans' = Map.insert k1 an1' $ Map.insert k2 an2 ans

    simpleBreak (_,DP (r,_c)) = r > 0

  ans <- getAnnsT
  let (ans',mov) = moveComments simpleBreak ans
  putAnnsT ans'
  return mov

-- ---------------------------------------------------------------------

-- ++AZ++ TODO: This needs to be renamed/reworked, based on what it actually gets used for
-- |Move any 'annFollowingComments' values from the 'Annotation' associated to
-- the first parameter to that of the second.
moveTrailingComments :: (Data a,Data b)
                     => GHC.Located a -> GHC.Located b -> Transform ()
moveTrailingComments first second = do
  let
    k1 = mkAnnKey first
    k2 = mkAnnKey second
    moveComments ans = ans'
      where
        an1 = gfromJust "moveTrailingComments k1" $ Map.lookup k1 ans
        an2 = gfromJust "moveTrailingComments k2" $ Map.lookup k2 ans
        cs1f = annFollowingComments an1
        cs2f = annFollowingComments an2
        an1' = an1 { annFollowingComments = [] }
        an2' = an2 { annFollowingComments = cs1f ++ cs2f }
        ans' = Map.insert k1 an1' $ Map.insert k2 an2' ans

  modifyAnnsT moveComments

-- ---------------------------------------------------------------------

-- |Insert a declaration into an AST element having sub-declarations
-- (@HasDecls@) according to the given location function.
insertAt :: (Data ast, HasDecls (GHC.Located ast))
              => (GHC.SrcSpan -> [GHC.SrcSpan] -> [GHC.SrcSpan])
              -> GHC.Located ast
              -> GHC.LHsDecl GHC.RdrName
              -> Transform (GHC.Located ast)
insertAt f t decl = do
  let newKey = GHC.getLoc decl
      modKey = mkAnnKey t
      newValue a@Ann{..} = a { annSortKey = f newKey <$> annSortKey }
  oldDecls <- hsDecls t
  modifyAnnsT (Map.adjust newValue modKey)

  replaceDecls t (decl : oldDecls )

-- |Insert a declaration at the beginning or end of the subdecls of the given
-- AST item
insertAtStart, insertAtEnd :: (Data ast, HasDecls (GHC.Located ast))
              => GHC.Located ast
              -> GHC.LHsDecl GHC.RdrName
              -> Transform (GHC.Located ast)

insertAtStart = insertAt (:)
insertAtEnd   = insertAt (\x xs -> xs ++ [x])

insertAfter, insertBefore :: (Data ast, HasDecls (GHC.Located ast))
                          => GHC.Located old
                          -> GHC.Located ast
                          -> GHC.LHsDecl GHC.RdrName
                          -> Transform (GHC.Located ast)
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

-- =====================================================================
-- start of HasDecls instances
-- =====================================================================

-- |Provide a means to get and process the immediate child declartions of a
-- given AST element.
class (Data t) => HasDecls t where
-- ++AZ++: TODO: add tests to confirm that hsDecls followed by replaceDecls is idempotent

    -- | Return the 'GHC.HsDecl's that are directly enclosed in the
    -- given syntax phrase. They are always returned in the wrapped 'GHC.HsDecl'
    -- form, even if orginating in local decls. This is safe, as annotations
    -- never attach to the wrapper, only to the wrapped item.
    hsDecls :: t -> Transform [GHC.LHsDecl GHC.RdrName]

    -- | Replace the directly enclosed decl list by the given
    --  decl list. Runs in the 'Transform' monad to be able to update list order
    --  annotations, and rebalance comments and other layout changes as needed.
    --
    -- For example, a call on replaceDecls for a wrapped 'GHC.FunBind' having no
    -- where clause will convert
    --
    -- @
    -- -- |This is a function
    -- foo = x -- comment1
    -- @
    -- in to
    --
    -- @
    -- -- |This is a function
    -- foo = x -- comment1
    --   where
    --     nn = 2
    -- @
    replaceDecls :: t -> [GHC.LHsDecl GHC.RdrName] -> Transform t

-- ---------------------------------------------------------------------

instance HasDecls GHC.ParsedSource where
  hsDecls (GHC.L _ (GHC.HsModule _mn _exps _imps decls _ _)) = return decls
  replaceDecls m@(GHC.L l (GHC.HsModule mn exps imps _decls deps haddocks)) decls
    = do
        logTr "replaceDecls LHsModule"
        modifyAnnsT (captureOrder m decls)
        return (GHC.L l (GHC.HsModule mn exps imps decls deps haddocks))

-- ---------------------------------------------------------------------

instance HasDecls (GHC.LMatch GHC.RdrName (GHC.LHsExpr GHC.RdrName)) where
  hsDecls d@(GHC.L _ (GHC.Match _ _ _ (GHC.GRHSs _ lb))) = do
    decls <- hsDeclsValBinds lb
    orderedDecls' d decls

  replaceDecls m@(GHC.L l (GHC.Match mf p t (GHC.GRHSs rhs binds))) []
    = do
        logTr "replaceDecls LMatch"
        let
          noWhere (G GHC.AnnWhere,_) = False
          noWhere _                  = True

          removeWhere mkds =
            case Map.lookup (mkAnnKey m) mkds of
              Nothing -> error "wtf"
              Just ann -> Map.insert (mkAnnKey m) ann1 mkds
                where
                  ann1 = ann { annsDP = filter noWhere (annsDP ann)
                                 }
        modifyAnnsT removeWhere

        binds' <- replaceDeclsValbinds binds []
        return (GHC.L l (GHC.Match mf p t (GHC.GRHSs rhs binds')))

  replaceDecls m@(GHC.L l (GHC.Match mf p t (GHC.GRHSs rhs binds))) newBinds
    = do
        logTr "replaceDecls LMatch"
        -- Need to throw in a fresh where clause if the binds were empty,
        -- in the annotations.
        case binds of
          GHC.EmptyLocalBinds -> do
            let
              addWhere mkds =
                case Map.lookup (mkAnnKey m) mkds of
                  Nothing -> error "wtf"
                  Just ann -> Map.insert (mkAnnKey m) ann1 mkds
                    where
                      ann1 = ann { annsDP = annsDP ann ++ [(G GHC.AnnWhere,DP (1,2))]
                                 }
            modifyAnnsT addWhere
            modifyAnnsT (setPrecedingLines (ghead "LMatch.replaceDecls" newBinds) 1 4)

            -- only move the comment if the original where clause was empty.
            toMove <- balanceTrailingComments m m
            insertCommentBefore (mkAnnKey m) toMove (matchApiAnn GHC.AnnWhere)
          _ -> return ()

        modifyAnnsT (captureOrderAnnKey (mkAnnKey m) newBinds)
        binds' <- replaceDeclsValbinds binds newBinds
        -- logDataWithAnnsTr "Match.replaceDecls:binds'" binds'
        return (GHC.L l (GHC.Match mf p t (GHC.GRHSs rhs binds')))

-- ---------------------------------------------------------------------

instance HasDecls (GHC.LHsExpr GHC.RdrName) where
  hsDecls ls@(GHC.L _ (GHC.HsLet decls _ex)) = do
    ds <- hsDeclsValBinds decls
    orderedDecls' ls ds
  hsDecls _                               = return []

  replaceDecls e@(GHC.L l (GHC.HsLet decls ex)) newDecls
    = do
        logTr "replaceDecls HsLet"
        modifyAnnsT (captureOrder e newDecls)
        decls' <- replaceDeclsValbinds decls newDecls
        return (GHC.L l (GHC.HsLet decls' ex))
  replaceDecls (GHC.L l (GHC.HsPar e)) newDecls
    = do
        logTr "replaceDecls HsPar"
        e' <- replaceDecls e newDecls
        return (GHC.L l (GHC.HsPar e'))
  replaceDecls old _new = error $ "replaceDecls (GHC.LHsExpr GHC.RdrName) undefined for:" ++ showGhc old

-- ---------------------------------------------------------------------

hsDeclsPatBindD :: GHC.LHsDecl GHC.RdrName -> Transform [GHC.LHsDecl GHC.RdrName]
hsDeclsPatBindD (GHC.L l (GHC.ValD d)) = hsDeclsPatBind (GHC.L l d)
hsDeclsPatBindD x = error $ "hsDeclsPatBindD called for:" ++ showGhc x

hsDeclsPatBind :: GHC.LHsBind GHC.RdrName -> Transform [GHC.LHsDecl GHC.RdrName]
hsDeclsPatBind d@(GHC.L _ (GHC.PatBind _ (GHC.GRHSs _grhs lb) _ _ _)) = do
  decls <- hsDeclsValBinds lb
  orderedDecls' d decls
hsDeclsPatBind x = error $ "hsDeclsPatBind called for:" ++ showGhc x

-- -------------------------------------

replaceDeclsPatBindD :: GHC.LHsDecl GHC.RdrName -> [GHC.LHsDecl GHC.RdrName] -> Transform (GHC.LHsDecl GHC.RdrName)
replaceDeclsPatBindD (GHC.L l (GHC.ValD d)) newDecls = do
  (GHC.L _ d') <- replaceDeclsPatBind (GHC.L l d) newDecls
  return (GHC.L l (GHC.ValD d'))
replaceDeclsPatBindD x _ = error $ "replaceDeclsPatBindD called for:" ++ showGhc x

replaceDeclsPatBind :: GHC.LHsBind GHC.RdrName -> [GHC.LHsDecl GHC.RdrName] -> Transform (GHC.LHsBind GHC.RdrName)
replaceDeclsPatBind p@(GHC.L l (GHC.PatBind a (GHC.GRHSs rhss binds) b c d)) newDecls
    = do
        logTr "replaceDecls PatBind"
        -- Need to throw in a fresh where clause if the binds were empty,
        -- in the annotations.
        case binds of
          GHC.EmptyLocalBinds -> do
            let
              addWhere mkds =
                case Map.lookup (mkAnnKey p) mkds of
                  Nothing -> error "wtf"
                  Just ann -> Map.insert (mkAnnKey p) ann1 mkds
                    where
                      ann1 = ann { annsDP = annsDP ann ++ [(G GHC.AnnWhere,DP (1,2))]
                                 }
            modifyAnnsT addWhere
            modifyAnnsT (setPrecedingLines (ghead "LMatch.replaceDecls" newDecls) 1 4)

          _ -> return ()

        modifyAnnsT (captureOrderAnnKey (mkAnnKey p) newDecls)
        binds' <- replaceDeclsValbinds binds newDecls
        return (GHC.L l (GHC.PatBind a (GHC.GRHSs rhss binds') b c d))
replaceDeclsPatBind x _ = error $ "replaceDeclsPatBind called for:" ++ showGhc x

-- ---------------------------------------------------------------------

instance HasDecls (GHC.LStmt GHC.RdrName (GHC.LHsExpr GHC.RdrName)) where
  hsDecls ls@(GHC.L _ (GHC.LetStmt lb))       = do
    decls <- hsDeclsValBinds lb
    orderedDecls' ls decls
  hsDecls (GHC.L _ (GHC.LastStmt e _))        = hsDecls e
  hsDecls (GHC.L _ (GHC.BindStmt _pat e _ _)) = hsDecls e
  hsDecls (GHC.L _ (GHC.BodyStmt e _ _ _))    = hsDecls e
  hsDecls _                                   = return []

  replaceDecls s@(GHC.L l (GHC.LetStmt lb)) newDecls
    = do
        modifyAnnsT (captureOrder s newDecls)
        lb' <- replaceDeclsValbinds lb newDecls
        return (GHC.L l (GHC.LetStmt lb'))
  replaceDecls (GHC.L l (GHC.LastStmt e se)) newDecls
    = do
        e' <- replaceDecls e newDecls
        return (GHC.L l (GHC.LastStmt e' se))
  replaceDecls (GHC.L l (GHC.BindStmt pat e a b)) newDecls
    = do
      e' <- replaceDecls e newDecls
      return (GHC.L l (GHC.BindStmt pat e' a b))
  replaceDecls (GHC.L l (GHC.BodyStmt e a b c)) newDecls
    = do
      e' <- replaceDecls e newDecls
      return (GHC.L l (GHC.BodyStmt e' a b c))
  replaceDecls x _newDecls = return x

-- =====================================================================
-- end of HasDecls instances
-- =====================================================================

{-
-- |A 'GHC.FunBind' wraps up one or more 'GHC.Match' items. 'hsDecls' cannot
-- return anything for these as there is not meaningful 'replaceDecls' for it.
-- This function provides a version of 'hsDecls' that returns the 'GHC.FunBind'
-- decls too, where they are needed for analysis only.
hsDeclsGeneric :: (HasDecls t) => t -> Transform [GHC.LHsDecl GHC.RdrName]
hsDeclsGeneric =
  case cast
-}
-- ---------------------------------------------------------------------

-- |Look up the annotated order and sort the decls accordingly
orderedDecls :: (Data a,HasDecls b) => GHC.Located a -> b -> Transform [GHC.LHsDecl GHC.RdrName]
orderedDecls parent sub = do
  decls <- hsDecls sub
  orderedDecls' parent decls

-- |Look up the annotated order and sort the decls accordingly
orderedDecls' :: (Data a) => GHC.Located a -> [GHC.LHsDecl GHC.RdrName] -> Transform [GHC.LHsDecl GHC.RdrName]
orderedDecls' parent decls = do
  ans <- getAnnsT
  case getAnnotationEP parent ans of
    Nothing -> error $ "orderedDecls:no annotation for:" ++ showAnnData emptyAnns 0 parent
    Just ann -> case annSortKey ann of
      Nothing -> do
        return decls
      Just keys -> do
        let ds = map (\s -> (GHC.getLoc s,s)) decls
            ordered = orderByKey ds keys
        return ordered

-- ---------------------------------------------------------------------

hsDeclsValBinds :: GHC.HsLocalBinds GHC.RdrName -> Transform [GHC.LHsDecl GHC.RdrName]
hsDeclsValBinds lb = case lb of
    GHC.HsValBinds (GHC.ValBindsIn bs sigs) -> do
      let
        bds = map wrapDecl (GHC.bagToList bs)
        sds = map wrapSig sigs
      return (bds ++ sds)
    GHC.HsValBinds (GHC.ValBindsOut _ _) -> error $ "hsDecls.ValbindsOut not valid"
    GHC.HsIPBinds _     -> return []
    GHC.EmptyLocalBinds -> return []

replaceDeclsValbinds :: GHC.HsLocalBinds GHC.RdrName -> [GHC.LHsDecl GHC.RdrName] -> Transform (GHC.HsLocalBinds GHC.RdrName)
replaceDeclsValbinds (GHC.HsValBinds _b) new
    = do
        logTr "replaceDecls HsLocalBinds"
        let decs = GHC.listToBag $ concatMap decl2Bind new
        let sigs = concatMap decl2Sig new
        return (GHC.HsValBinds (GHC.ValBindsIn decs sigs))
replaceDeclsValbinds (GHC.HsIPBinds _b) _new    = error "undefined replaceDecls HsIPBinds"
replaceDeclsValbinds (GHC.EmptyLocalBinds) new
    = do
        logTr "replaceDecls HsLocalBinds"
        let newBinds = map decl2Bind new
            newSigs  = map decl2Sig  new
        let decs = GHC.listToBag $ concat newBinds
        let sigs = concat newSigs
        return (GHC.HsValBinds (GHC.ValBindsIn decs sigs))

-- ---------------------------------------------------------------------

type Decl  = GHC.LHsDecl GHC.RdrName
type Match = GHC.LMatch GHC.RdrName (GHC.LHsExpr GHC.RdrName)

modifyLocalDecl ::
                   GHC.SrcSpan
                -> Decl
                -> (Match -> [Decl] -> Transform ([Decl], Maybe t))
                -> Transform (Decl,Maybe t)
modifyLocalDecl p m@(GHC.L ss (GHC.ValD (GHC.PatBind {} ))) f =
         if ss == p
            then do
              ds <- hsDeclsPatBindD m
              (ds',r) <- f undefined ds
              m' <- replaceDeclsPatBindD m ds'
              return (m',r)
            else return (m,Nothing)
modifyLocalDecl p ast f = do
  (ast',r) <- runStateT (SYB.everywhereM (SYB.mkM doModLocal) ast) Nothing
  return (ast',r)
  where
    -- doModLocal :: Match
    --             -> Transform Match
    doModLocal  (m@(GHC.L ss _) :: Match) = do
         lift $ logTr $ "doModLocal entered:ss=" ++ showGhc ss
         if ss == p
           then do
             lift $ logTr "doModLocal hit"
             ds <- lift $ hsDecls m
             (ds',r) <- lift $ f m ds
             lift $ logTr $ "doModLocal:ds'=" ++ showGhc ds'
             put r
             lift $ replaceDecls m ds'
           else return m

-- ---------------------------------------------------------------------

-- |Used to integrate a @Transform@ into other Monad stacks
class (Monad m) => (HasTransform m) where
  liftT :: Transform a -> m a

-- ---------------------------------------------------------------------

-- | Apply a transformation to the decls contained in @t@
modifyDeclsT :: (HasDecls t,HasTransform m)
             => ([GHC.LHsDecl GHC.RdrName] -> m [GHC.LHsDecl GHC.RdrName])
             -> t -> m t
modifyDeclsT action t = do
  decls <- liftT $ hsDecls t
  decls' <- action decls
  liftT $ replaceDecls t decls'

-- ---------------------------------------------------------------------

matchApiAnn :: GHC.AnnKeywordId -> (KeywordId,DeltaPos) -> Bool
matchApiAnn mkw (kw,_)
  = case kw of
     (G akw) -> mkw == akw
     _       -> False


-- We comments extracted from annPriorComments or annFollowingComments, which
-- need to move to just before the item identified by the predicate, if it
-- fires, else at the end of the annotations.
insertCommentBefore :: AnnKey -> [(Comment, DeltaPos)]
                    -> ((KeywordId, DeltaPos) -> Bool) -> Transform ()
insertCommentBefore key toMove p = do
  let
    doInsert ans =
      case Map.lookup key ans of
        Nothing -> error $ "insertCommentBefore:no AnnKey for:" ++ showGhc key
        Just ann -> Map.insert key ann' ans
          where
            (before,after) = break p (annsDP ann)
            -- ann' = error $ "insertCommentBefore:" ++ showGhc (before,after)
            ann' = ann { annsDP = before ++ (map comment2dp toMove) ++ after}

  modifyAnnsT doInsert
