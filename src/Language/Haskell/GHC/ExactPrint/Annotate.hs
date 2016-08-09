{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-} -- Needed for the DataId constraint on ResTyGADTHook
-- | 'annotate' is a function which given a GHC AST fragment, constructs
-- a syntax tree which indicates which annotations belong to each specific
-- part of the fragment.
--
-- "Delta" and "Print" provide two interpreters for this structure. You
-- should probably use those unless you know what you're doing!
--
-- The functor 'AnnotationF' has a number of constructors which correspond
-- to different sitations which annotations can arise. It is hoped that in
-- future versions of GHC these can be simplified by making suitable
-- modifications to the AST.

module Language.Haskell.GHC.ExactPrint.Annotate
       (
         annotate
       , AnnotationF(..)
       , Annotated
       , Annotate(..)
       , withSortKeyContextsHelper
       ) where

-- import Data.Maybe ( fromMaybe )
#if __GLASGOW_HASKELL__ <= 710
import Data.Ord ( comparing )
import Data.List ( sortBy )
#endif

import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Utils

import qualified Bag            as GHC
import qualified BasicTypes     as GHC
import qualified BooleanFormula as GHC
import qualified Class          as GHC
import qualified CoAxiom        as GHC
import qualified FastString     as GHC
import qualified ForeignCall    as GHC
import qualified GHC            as GHC
#if __GLASGOW_HASKELL__ > 710
import qualified Lexeme         as GHC
#endif
import qualified Name           as GHC
import qualified OccName        as GHC
import qualified RdrName        as GHC
import qualified Outputable     as GHC

import Control.Monad.Trans.Free
import Control.Monad.Free.TH (makeFreeCon)
import Control.Monad.Identity
import Data.Data
import Data.Maybe

import qualified Data.Set as Set

import Debug.Trace


{-# ANN module "HLint: ignore Eta reduce" #-}
-- ---------------------------------------------------------------------

-- | ['MarkPrim'] The main constructor. Marks that a specific AnnKeywordId could
-- appear with an optional String which is used when printing.
-- ['MarkPPOptional'] Used to flag elements, such as optional braces, that are
--   not used in the pretty printer. This functions identically to 'MarkPrim'
--   for the other interpreters.
-- ['MarkEOF']
--    Special constructor which marks the end of file marker.
-- ['MarkExternal'] TODO
-- ['MarkOutside']  A @AnnKeywordId@ which is precisely located but not inside the
--    current context. This is usually used to reassociated located
--    @RdrName@ which are more naturally associated with their parent than
--    in their own annotation.
-- ['MarkInside']
--    The dual of MarkOutside. If we wish to mark a non-separating comma
--    or semi-colon then we must use this constructor.
-- ['MarkMany'] Some syntax elements allow an arbritary number of puncuation marks
-- without reflection in the AST. This construction greedily takes all of
-- the specified @AnnKeywordId@.
-- ['MarkOffsetPrim'] Some syntax elements have repeated @AnnKeywordId@ which are
--  seperated by different @AnnKeywordId@. Thus using MarkMany is
--  unsuitable and instead we provide an index to specify which specific
--  instance to choose each time.
-- ['WithAST'] TODO
-- ['CountAnns'] Sometimes the AST does not reflect the concrete source code and the
--  only way to tell what the concrete source was is to count a certain
--  kind of @AnnKeywordId@.
-- ['WithSortKey'] There are many places where the syntactic ordering of elements is
-- thrown away by the AST. This constructor captures the original
-- ordering and reflects any changes in ordered as specified by the
-- @annSortKey@ field in @Annotation@.
-- ['SetLayoutFlag'] It is important to know precisely where layout rules apply. This
--  constructor wraps a computation to indicate that LayoutRules apply to
--  the corresponding construct.
-- ['StoreOriginalSrcSpan'] TODO
-- ['GetSrcSpanFromKw'] TODO
-- ['StoreString'] TODO
-- ['AnnotationsToComments'] Used when the AST is sufficiently vague that there is no other
-- option but to convert a fragment of source code into a comment. This
-- means it is impossible to edit such a fragment but means that
-- processing files with such fragments is still possible.
data AnnotationF next where
  MarkPrim         :: GHC.AnnKeywordId -> Maybe String                     -> next -> AnnotationF next
  MarkPPOptional   :: GHC.AnnKeywordId -> Maybe String                     -> next -> AnnotationF next
  MarkEOF          ::                                                         next -> AnnotationF next
  MarkExternal     :: GHC.SrcSpan -> GHC.AnnKeywordId -> String            -> next -> AnnotationF next
  MarkOutside      :: GHC.AnnKeywordId -> KeywordId                        -> next -> AnnotationF next
  MarkInside       :: GHC.AnnKeywordId                                     -> next -> AnnotationF next
  MarkMany         :: GHC.AnnKeywordId                                     -> next -> AnnotationF next
  MarkManyOptional :: GHC.AnnKeywordId                                     -> next -> AnnotationF next
  MarkOffsetPrim   :: GHC.AnnKeywordId -> Int -> Maybe String              -> next -> AnnotationF next
  MarkOffsetPrimOptional :: GHC.AnnKeywordId -> Int -> Maybe String        -> next -> AnnotationF next
  WithAST          :: Data a => GHC.Located a
                             -> Annotated b                                -> next -> AnnotationF next
  CountAnns        :: GHC.AnnKeywordId                        -> (Int     -> next) -> AnnotationF next
  WithSortKey      :: [(GHC.SrcSpan, Annotated ())]                        -> next -> AnnotationF next

  SetLayoutFlag    ::  Rigidity -> Annotated ()                            -> next -> AnnotationF next

  -- Required to work around deficiencies in the GHC AST
  StoreOriginalSrcSpan :: GHC.SrcSpan -> AnnKey         -> (AnnKey -> next) -> AnnotationF next
  GetSrcSpanForKw :: GHC.SrcSpan -> GHC.AnnKeywordId -> (GHC.SrcSpan -> next) -> AnnotationF next
#if __GLASGOW_HASKELL__ <= 710
  StoreString :: String -> GHC.SrcSpan                  -> next -> AnnotationF next
#endif
  AnnotationsToComments :: [GHC.AnnKeywordId]           -> next -> AnnotationF next
#if __GLASGOW_HASKELL__ <= 710
  AnnotationsToCommentsBF :: (GHC.Outputable a) => GHC.BooleanFormula (GHC.Located a) -> [GHC.AnnKeywordId] -> next -> AnnotationF next
  FinalizeBF :: GHC.SrcSpan -> next -> AnnotationF next
#endif

  -- AZ experimenting with pretty printing
  -- Set the context for child element
  SetContextLevel :: Set.Set AstContext -> Int -> Annotated () -> next -> AnnotationF next
  UnsetContext    ::         AstContext        -> Annotated () -> next -> AnnotationF next
  -- Query the context while in a child element
  IfInContext  :: Set.Set AstContext -> Annotated () -> Annotated ()   -> next -> AnnotationF next
  NotInContext :: Set.Set AstContext -> Annotated ()                   -> next -> AnnotationF next
  BumpContext  :: Annotated ()                                         -> next -> AnnotationF next
  WithSortKeyContexts :: ListContexts -> [(GHC.SrcSpan, Annotated ())] -> next -> AnnotationF next

deriving instance Functor (AnnotationF)

type Annotated = FreeT AnnotationF Identity


-- ---------------------------------------------------------------------

makeFreeCon  'MarkEOF
makeFreeCon  'MarkPrim
makeFreeCon  'MarkPPOptional
makeFreeCon  'MarkOutside
makeFreeCon  'MarkInside
makeFreeCon  'MarkExternal
makeFreeCon  'MarkMany
makeFreeCon  'MarkManyOptional
makeFreeCon  'MarkOffsetPrim
makeFreeCon  'MarkOffsetPrimOptional
makeFreeCon  'CountAnns
makeFreeCon  'StoreOriginalSrcSpan
makeFreeCon  'GetSrcSpanForKw
#if __GLASGOW_HASKELL__ <= 710
makeFreeCon  'StoreString
#endif
makeFreeCon  'AnnotationsToComments
#if __GLASGOW_HASKELL__ <= 710
makeFreeCon  'AnnotationsToCommentsBF
makeFreeCon  'FinalizeBF
#endif
makeFreeCon  'WithSortKey
makeFreeCon  'SetContextLevel
makeFreeCon  'UnsetContext
makeFreeCon  'IfInContext
makeFreeCon  'NotInContext
makeFreeCon  'BumpContext
makeFreeCon  'WithSortKeyContexts

-- ---------------------------------------------------------------------

setContext :: Set.Set AstContext -> Annotated () -> Annotated ()
setContext ctxt action = liftF (SetContextLevel ctxt 3 action ())

setLayoutFlag :: Annotated () -> Annotated ()
setLayoutFlag action = liftF (SetLayoutFlag NormalLayout action ())

setRigidFlag :: Annotated () -> Annotated ()
setRigidFlag action = liftF (SetLayoutFlag RigidLayout action ())

-- | Construct a syntax tree which represent which KeywordIds must appear
-- where.
annotate :: (Annotate ast) => GHC.Located ast -> Annotated ()
annotate = markLocated

inContext :: Set.Set AstContext -> Annotated () -> Annotated ()
inContext ctxt action = liftF (IfInContext ctxt action (return ()) ())

-- ---------------------------------------------------------------------

#if __GLASGOW_HASKELL__ <= 710
workOutString :: GHC.SrcSpan -> GHC.AnnKeywordId -> (GHC.SrcSpan -> String) -> Annotated ()
workOutString l kw f = do
  ss <- getSrcSpanForKw l kw
  storeString (f ss) ss
#endif

-- ---------------------------------------------------------------------

-- |Main driver point for annotations.
withAST :: Data a => GHC.Located a -> Annotated () -> Annotated ()
withAST lss action =
  liftF (WithAST lss prog ())
  where
    prog = do
      action

-- ---------------------------------------------------------------------
-- Additional smart constructors

mark :: GHC.AnnKeywordId -> Annotated ()
mark kwid = markPrim kwid Nothing

markOptional :: GHC.AnnKeywordId -> Annotated ()
markOptional kwid = markPPOptional kwid Nothing

markWithString :: GHC.AnnKeywordId -> String -> Annotated ()
markWithString kwid s = markPrim kwid (Just s)

markWithStringOptional :: GHC.AnnKeywordId -> String -> Annotated ()
markWithStringOptional kwid s = markPPOptional kwid (Just s)

markOffsetWithString :: GHC.AnnKeywordId -> Int -> String -> Annotated ()
markOffsetWithString kwid n s = markOffsetPrim kwid n (Just s)

markOffset :: GHC.AnnKeywordId -> Int -> Annotated ()
markOffset kwid n = markOffsetPrim kwid n Nothing

markOffsetOptional :: GHC.AnnKeywordId -> Int -> Annotated ()
markOffsetOptional kwid n = markOffsetPrimOptional kwid n Nothing

markTrailingSemi :: Annotated ()
markTrailingSemi = markOutside GHC.AnnSemi AnnSemiSep

-- ---------------------------------------------------------------------

-- | Constructs a syntax tree which contains information about which
-- annotations are required by each element.
markLocated :: (Annotate ast) => GHC.Located ast -> Annotated ()
markLocated ast =
  case cast ast :: Maybe (GHC.LHsDecl GHC.RdrName) of
    Just d  -> markLHsDecl d
    Nothing -> withLocated ast markAST

withLocated :: Data a
            => GHC.Located a
            -> (GHC.SrcSpan -> a -> Annotated ())
            -> Annotated ()
withLocated a@(GHC.L l ast) action =
  withAST a (action l ast)

-- ---------------------------------------------------------------------

{-
markLocatedPushContext :: (Annotate ast) => Set.Set AstContext -> GHC.Located ast -> Annotated ()
markLocatedPushContext ctxt ast = do
  -- TODO:AZ: make this an if statement
  inContext ctxt $ do setContext ctxt (markLocated ast)

  notInContext ctxt $ do markLocated ast
-}

-- ---------------------------------------------------------------------

-- |When adding missing annotations, do not put a preceding space in front of a list
markListNoPrecedingSpace :: Annotate ast => Bool -> [GHC.Located ast] -> Annotated ()
markListNoPrecedingSpace intercal ls =
    case ls of
      [] -> return ()
      (l:ls') -> do
        if intercal
        then do
          if null ls'
            then setContext (Set.fromList [NoPrecedingSpace            ]) $ markLocated l
            else setContext (Set.fromList [NoPrecedingSpace,Intercalate]) $ markLocated l
          markListIntercalate ls'
        else do
          setContext (Set.singleton NoPrecedingSpace) $ markLocated l
          mapM_ markLocated ls'

-- ---------------------------------------------------------------------


-- |Mark a list, with the given keyword as a list item separator
markListIntercalate :: Annotate ast => [GHC.Located ast] -> Annotated ()
markListIntercalate ls = markListIntercalateWithFun markLocated ls

markListIntercalateWithFun :: (t -> Annotated ()) -> [t] -> Annotated ()
markListIntercalateWithFun f ls = markListIntercalateWithFunLevel f 3 ls
-- markListIntercalateWithFun f ls = markListIntercalateWithFunLevel f 2 ls

markListIntercalateWithFunLevel :: (t -> Annotated ()) -> Int -> [t] -> Annotated ()
markListIntercalateWithFunLevel f level ls = markListIntercalateWithFunLevelCtx f level Intercalate ls

markListIntercalateWithFunLevelCtx :: (t -> Annotated ()) -> Int -> AstContext -> [t] -> Annotated ()
markListIntercalateWithFunLevelCtx f level ctx ls = go ls
  where
    go []  = return ()
    go [x] = f x
    go (x:xs) = do
      setContextLevel (Set.singleton ctx) level $ f x
      go xs

-- ---------------------------------------------------------------------

markListInitialContext :: Annotate ast => Set.Set AstContext -> [GHC.Located ast] -> Annotated ()
markListInitialContext ctx ls =
  markListWithContexts ctx Set.empty ls

markListWithContexts :: Annotate ast => Set.Set AstContext -> Set.Set AstContext -> [GHC.Located ast] -> Annotated ()
markListWithContexts ctxInitial ctxRest ls =
  case ls of
    [] -> return ()
    [x] -> setContextLevel ctxInitial 2 $ markLocated x
    (x:xs) -> do
      setContextLevel ctxInitial 2 $ markLocated x
      setContextLevel ctxRest    2 $ mapM_ markLocated xs

-- ---------------------------------------------------------------------

-- Context for only if just one, else first item, middle ones, and last one
markListWithContexts' :: Annotate ast
                      => ListContexts
                      -> [GHC.Located ast] -> Annotated ()
markListWithContexts' (LC ctxOnly ctxInitial ctxMiddle ctxLast) ls =
  case ls of
    [] -> return ()
    [x] -> setContextLevel ctxOnly level $ markLocated x
    (x:xs) -> do
      setContextLevel ctxInitial level $ markLocated x
      go xs
  where
    level = 2
    go []  = return ()
    go [x] = setContextLevel ctxLast level $ markLocated x
    go (x:xs) = do
      setContextLevel ctxMiddle level $ markLocated x
      go xs


markListWithContextsFunction ::
                         ListContexts
                      -> (t -> Annotated ())
                      -> [t] -> Annotated ()
markListWithContextsFunction (LC ctxOnly ctxInitial ctxMiddle ctxLast) f ls =
  case ls of
    [] -> return ()
    [x] -> setContextLevel ctxOnly level $ f x
    (x:xs) -> do
      setContextLevel ctxInitial level $ f x
      go xs
  where
    level = 2
    go []  = return ()
    go [x] = setContextLevel ctxLast level $ f x
    go (x:xs) = do
      setContextLevel ctxMiddle level $ f x
      go xs

-- ---------------------------------------------------------------------


-- Expects the kws to be ordered already
withSortKeyContextsHelper :: (Monad m) => (Annotated () -> m ()) -> ListContexts -> [(GHC.SrcSpan, Annotated ())] -> m ()
withSortKeyContextsHelper interpret (LC ctxOnly ctxInitial ctxMiddle ctxLast) kws = do
  -- tellSortKey (map fst order)
  -- mapM_ (deltaInterpret . snd) order
  case kws of
    [] -> return ()
    [x] -> interpret (setContextLevel ctxOnly level $ snd x)
    (x:xs) -> do
      interpret (setContextLevel ctxInitial level $ snd x)
      go xs
  where
    -- order = sortBy (comparing fst) kws
    level = 2
    go []  = return ()
    go [x] = interpret (setContextLevel ctxLast level $ snd x)
    go (x:xs) = do
      interpret (setContextLevel ctxMiddle level $ snd x)
      go xs

-- ---------------------------------------------------------------------

markListWithLayout :: Annotate ast => [GHC.Located ast] -> Annotated ()
markListWithLayout ls =
  -- setLayoutFlag (mapM_ markLocated ls)
  setLayoutFlag $ markList ls

-- ---------------------------------------------------------------------

markList :: Annotate ast => [GHC.Located ast] -> Annotated ()
markList ls =
  setContext (Set.singleton NoPrecedingSpace)
   $ markListWithContexts (Set.singleton ListStart) (Set.singleton ListItem) ls

markLocalBindsWithLayout :: (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
  => GHC.HsLocalBinds name -> Annotated ()
markLocalBindsWithLayout binds =
  markHsLocalBinds binds

-- ---------------------------------------------------------------------

-- |This function is used to get around shortcomings in the GHC AST for 7.10.1
markLocatedFromKw :: (Annotate ast) => GHC.AnnKeywordId -> GHC.Located ast -> Annotated ()
markLocatedFromKw kw (GHC.L l a) = do
  -- Note: l is needed so that the pretty printer can make something up
  ss <- getSrcSpanForKw l kw
  AnnKey ss' _ <- storeOriginalSrcSpan l (mkAnnKey (GHC.L ss a))
  markLocated (GHC.L ss' a)

-- ---------------------------------------------------------------------

markMaybe :: (Annotate ast) => Maybe (GHC.Located ast) -> Annotated ()
markMaybe Nothing    = return ()
markMaybe (Just ast) = markLocated ast

-- ---------------------------------------------------------------------
-- Managing lists which have been separated, e.g. Sigs and Binds

prepareListAnnotation :: Annotate a => [GHC.Located a] -> [(GHC.SrcSpan,Annotated ())]
prepareListAnnotation ls = map (\b -> (GHC.getLoc b,markLocated b)) ls

prepareListAnnotationWithContext :: Annotate a => Set.Set AstContext
                                 -> [GHC.Located a] -> [(GHC.SrcSpan,Annotated ())]
prepareListAnnotationWithContext ctx ls = map (\b -> (GHC.getLoc b,setContext ctx (markLocated b))) ls

applyListAnnotations :: [(GHC.SrcSpan, Annotated ())] -> Annotated ()
applyListAnnotations ls = withSortKey ls

applyListAnnotationsContexts :: ListContexts -> [(GHC.SrcSpan, Annotated ())] -> Annotated ()
applyListAnnotationsContexts ctxt ls = withSortKeyContexts ctxt ls

#if __GLASGOW_HASKELL__ <= 710
lexicalSortLocated :: [GHC.Located a] -> [GHC.Located a]
lexicalSortLocated = sortBy (comparing GHC.getLoc)
#endif

applyListAnnotationsLayout :: [(GHC.SrcSpan, Annotated ())] -> Annotated ()
applyListAnnotationsLayout ls = setLayoutFlag $ setContext (Set.singleton NoPrecedingSpace)
                                              $ withSortKeyContexts listContexts ls

listContexts :: ListContexts
listContexts = (LC (Set.fromList [CtxOnly,ListStart])
                   (Set.fromList [CtxFirst,ListStart,Intercalate])
                   (Set.fromList [CtxMiddle,ListItem,Intercalate])
                   (Set.fromList [CtxLast,ListItem]))

-- ---------------------------------------------------------------------

class Data ast => Annotate ast where
  markAST :: GHC.SrcSpan -> ast -> Annotated ()

-- ---------------------------------------------------------------------

instance Annotate (GHC.HsModule GHC.RdrName) where
  markAST _ (GHC.HsModule mmn mexp imps decs mdepr _haddock) = do

    case mmn of
      Nothing -> return ()
      Just (GHC.L ln mn) -> do
        mark GHC.AnnModule
        markExternal ln GHC.AnnVal (GHC.moduleNameString mn)

        case mdepr of
          Nothing -> return ()
          Just depr -> markLocated depr

        case mexp of
          Nothing   -> return ()
          Just exps -> markLocated exps

        mark GHC.AnnWhere

    markOptional GHC.AnnOpenC -- Possible '{'
    markManyOptional GHC.AnnSemi -- possible leading semis
    setContextLevel (Set.singleton TopLevel) 2 $ markListWithLayout imps

    -- mapM_ markLocated decs
    setContextLevel (Set.singleton TopLevel) 2 $ markListWithLayout decs

    markOptional GHC.AnnCloseC -- Possible '}'

    markEOF

-- ---------------------------------------------------------------------

instance Annotate GHC.WarningTxt where
  markAST _ (GHC.WarningTxt (GHC.L ls txt) lss) = do
    markExternal ls GHC.AnnOpen txt
    mark GHC.AnnOpenS
    -- mapM_ markLocated lss
    markListIntercalate lss
    mark GHC.AnnCloseS
    markWithString GHC.AnnClose "#-}"

  markAST _ (GHC.DeprecatedTxt (GHC.L ls txt) lss) = do
    markExternal ls GHC.AnnOpen txt
    mark GHC.AnnOpenS
    markListIntercalate lss
    mark GHC.AnnCloseS
    markWithString GHC.AnnClose "#-}"

-- ---------------------------------------------------------------------
#if __GLASGOW_HASKELL__ > 710
instance Annotate GHC.StringLiteral where
  markAST l (GHC.StringLiteral src _) = do
    markExternal l GHC.AnnVal src
    inContext (Set.fromList [Intercalate]) $ mark GHC.AnnComma
#endif

-- ---------------------------------------------------------------------

instance Annotate (GHC.SourceText,GHC.FastString) where
  -- markAST l (_,fs) = markAST l fs
  markAST l (src,_fs) = do
    markExternal l GHC.AnnVal src

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.HasOccName name,Annotate name)
  => Annotate [GHC.LIE name] where
   markAST _ ls = do
     inContext (Set.singleton HasHiding) $ mark GHC.AnnHiding -- in an import decl
     mark GHC.AnnOpenP -- '('
     -- Can't use markListIntercalate, there can be trailing commas, but only in imports.
     -- setContextLevel (Set.singleton Intercalate) 2 $ mapM_ markLocated ls
     markListIntercalateWithFunLevel markLocated 2 ls

     mark GHC.AnnCloseP -- ')'

instance (GHC.DataId name,GHC.HasOccName name, Annotate name)
  => Annotate (GHC.IE name) where
  markAST _ ie = do

    case ie of
        (GHC.IEVar ln) -> do
          -- TODO: I am pretty sure this criterion is inadequate
          if (GHC.isDataOcc $ GHC.occName $ GHC.unLoc ln)
            then mark GHC.AnnPattern
            else markOptional GHC.AnnPattern
          setContext (Set.fromList [PrefixOp,InIE]) $ markLocated ln

        (GHC.IEThingAbs ln@(GHC.L _ n)) -> do
          {-
          At the moment (7.10.2) GHC does not cleanly represent an export of the form
           "type Foo"
          and it only captures the name "Foo".

          The Api Annotations workaround is to have the IEThingAbs SrcSpan
          extend across both the "type" and "Foo", and then to capture the
          individual item locations in an AnnType and AnnVal annotation.

          This need to be fixed for 7.12.

          -}
          -- cnt <- countAnns GHC.AnnType
          -- if cnt == 1
          -- if (GHC.isTcClsNameSpace $ GHC.occNameSpace $ GHC.occName n)

          -- The test on the following line is taken from GHC 7.10 src for
          -- lookupTopBndrRn_maybe, based on the -XTypeOperators extension

#if __GLASGOW_HASKELL__ <= 710
          if (GHC.isTcOcc $ GHC.occName n) && (GHC.isSymOcc $ GHC.occName n)
#else
          if ((GHC.isTcOcc $ GHC.occName n) && (GHC.isSymOcc $ GHC.occName n))
                 && (not $ GHC.isLexConSym $ GHC.occNameFS $ GHC.occName n) -- rule out (:-$) etc
#endif
            then do
              mark GHC.AnnType
              setContext (Set.singleton PrefixOp) $ markLocatedFromKw GHC.AnnVal ln
            else setContext (Set.singleton PrefixOp) $ markLocated ln
{-
ghc8:T10507:
ghc8:T10806:
    import GHC.TypeLits (Nat, type (<=))
                              ^^^^^^^^^ AnnVal
ghc710: ExplicitNamespaces:
    import GHC.TypeLits                   (KnownNat, type (+), type (-))
                                                     ^^^^^^^^  ^^^^^^^^ AnnVal

ghc710: Imports.hs
    module Imports( f, type (+), pattern Single ) where
                       ^^^^^^^^ AnnVal

ghc8: Eq.hs (using TypeOperators)
  (:==$), (:==$$), (:==$$$), (:/=$), (:/=$$), (:/=$$$)
  ^^^^^^  ^^^^^^^  ^^^^^^^^  ^^^^^^  ^^^^^^^  ^^^^^^^^

-}

#if __GLASGOW_HASKELL__ <= 710
        (GHC.IEThingWith ln ns) -> do
#else
        (GHC.IEThingWith ln wc ns _lfs) -> do
{-
  | IEThingWith (Located name)
                IEWildcard
                [Located name]
                [Located (FieldLbl name)]
                 -- ^ Class/Type plus some methods/constructors
                 -- and record fields; see Note [IEThingWith]

-}
#endif
          -- TODO: Deal with GHC 8.0 additions
          setContext (Set.singleton PrefixOp) $ markLocated ln
          mark GHC.AnnOpenP
#if __GLASGOW_HASKELL__ <= 710
          -- setContext (Set.fromList [PrefixOp,Intercalate]) $ mapM_ markLocated ns
          setContext (Set.singleton PrefixOp) $ markListIntercalate ns
#else
          case wc of
            -- GHC.NoIEWildcard -> setContext (Set.fromList [PrefixOp,Intercalate]) $ mapM_ markLocated ns
            GHC.NoIEWildcard -> unsetContext Intercalate $ setContext (Set.fromList [PrefixOp]) $ markListIntercalate ns
            -- GHC.NoIEWildcard -> setContext (Set.fromList [PrefixOp]) $ mapM_ markLocated ns
            GHC.IEWildcard n -> do
              setContext (Set.fromList [PrefixOp,Intercalate]) $ mapM_ markLocated (take n ns)
              -- setContext (Set.fromList [PrefixOp]) $ mapM_ markLocated (take n ns)
              mark GHC.AnnDotdot
              case drop n ns of
                [] -> return ()
                ns' -> do
                  -- markOffset GHC.AnnComma 0
                  mark GHC.AnnComma
                  setContext (Set.singleton PrefixOp) $ mapM_ markLocated ns'
#endif
          mark GHC.AnnCloseP

        (GHC.IEThingAll ln) -> do
          setContext (Set.fromList [PrefixOp]) $ markLocated ln
          mark GHC.AnnOpenP
          mark GHC.AnnDotdot
          mark GHC.AnnCloseP

        (GHC.IEModuleContents (GHC.L lm mn)) -> do
          mark GHC.AnnModule
          markExternal lm GHC.AnnVal (GHC.moduleNameString mn)

        -- Only used in Haddock mode so we can ignore them.
        (GHC.IEGroup _ _) -> return ()

        (GHC.IEDoc _)     -> return ()

        (GHC.IEDocNamed _)    -> return ()
    -- inContext (Set.fromList [Intercalate]) $ mark GHC.AnnComma
    ifInContext (Set.fromList [Intercalate])
      (mark         GHC.AnnComma)
      (markOptional GHC.AnnComma)

-- ---------------------------------------------------------------------
{-
-- For details on above see note [Api annotations] in ApiAnnotation
data RdrName
  = Unqual OccName
        -- ^ Used for ordinary, unqualified occurrences, e.g. @x@, @y@ or @Foo@.
        -- Create such a 'RdrName' with 'mkRdrUnqual'

  | Qual ModuleName OccName
        -- ^ A qualified name written by the user in
        -- /source/ code.  The module isn't necessarily
        -- the module where the thing is defined;
        -- just the one from which it is imported.
        -- Examples are @Bar.x@, @Bar.y@ or @Bar.Foo@.
        -- Create such a 'RdrName' with 'mkRdrQual'

  | Orig Module OccName
        -- ^ An original name; the module is the /defining/ module.
        -- This is used when GHC generates code that will be fed
        -- into the renamer (e.g. from deriving clauses), but where
        -- we want to say \"Use Prelude.map dammit\". One of these
        -- can be created with 'mkOrig'

  | Exact Name
        -- ^ We know exactly the 'Name'. This is used:
        --
        --  (1) When the parser parses built-in syntax like @[]@
        --      and @(,)@, but wants a 'RdrName' from it
        --
        --  (2) By Template Haskell, when TH has generated a unique name
        --
        -- Such a 'RdrName' can be created by using 'getRdrName' on a 'Name'
  deriving (Data, Typeable)
-}

isSymRdr :: GHC.RdrName -> Bool
isSymRdr n = (GHC.isSymOcc $ GHC.rdrNameOcc n) || rdrName2String n == "."

instance Annotate GHC.RdrName where
  markAST l n = do
    let
      str = rdrName2String n
      isSym = isSymRdr n
      -- isClass = GHC.isClsOcc $ GHC.rdrNameOcc n
      doNormalRdrName = do
        let str' = case str of
              -- TODO: unicode support?
                        "forall" -> if spanLength l == 1 then "∀" else str
                        _ -> str
        when (GHC.isTcClsNameSpace $ GHC.rdrNameSpace n) $ inContext (Set.singleton InIE) $ mark GHC.AnnType
        markOptional GHC.AnnType

        when isSym $ ifInContext (Set.singleton PrefixOp)
                                   (mark         GHC.AnnOpenP) -- '('
                                   (markOptional GHC.AnnOpenP)
        when (not isSym) $ inContext (Set.fromList [InOp]) $ markOffset GHC.AnnBackquote 0
        cnt  <- countAnns GHC.AnnVal
        case cnt of
          0 -> markExternal l GHC.AnnVal str'
          1 -> markWithString GHC.AnnVal str'
          _ -> traceM $ "Printing RdrName, more than 1 AnnVal:" ++ showGhc (l,n)
        when (not isSym) $ inContext (Set.fromList [InOp]) $ markOffset GHC.AnnBackquote 1
        when isSym $ ifInContext (Set.singleton PrefixOp)
                                   (mark         GHC.AnnCloseP)
                                   (markOptional GHC.AnnCloseP)
                                   -- else mark GHC.AnnCloseP

    case n of
      -- GHC.Unqual o -> case str of
      --    "$"  -> do
      --      markOptional GHC.AnnOpenP
      --      markExternal l GHC.AnnVal str
      --      markOptional GHC.AnnCloseP
      --    _    -> doNormalRdrName
      GHC.Unqual _ -> doNormalRdrName
      GHC.Qual _ _ -> doNormalRdrName
#if __GLASGOW_HASKELL__ <= 710
      GHC.Orig _ _ -> markExternal l GHC.AnnVal str
#else
      GHC.Orig _ _ -> if str == "~"
                        then do
                          mark GHC.AnnOpenP -- '('
                          -- NOTE: GHC8 parser annotates oqtycon with AnnVal, oqtycon_no_varcon with AnnTilde
                          markWithString GHC.AnnVal "~"
                          mark GHC.AnnTilde
                          mark GHC.AnnCloseP -- ')'
                        else markExternal l GHC.AnnVal str
#endif
      GHC.Exact n'  -> do
       case str of
         -- Special handling for Exact RdrNames, which are built-in Names
         "[]" -> do
           mark GHC.AnnOpenS  -- '['
           mark GHC.AnnCloseS -- ']'
         "()" -> do
           mark GHC.AnnOpenP  -- '('
           mark GHC.AnnCloseP -- ')'
         ('(':'#':_) -> do
           markWithString GHC.AnnOpen  "(#" -- '(#'
           -- markMany GHC.AnnCommaTuple
           let cnt = length $ filter (==',') str
           replicateM_ cnt (mark GHC.AnnCommaTuple)
           markWithString GHC.AnnClose  "#)"-- '#)'
         "[::]" -> do
           markWithString GHC.AnnOpen  "[:" -- '[:'
           markWithString GHC.AnnClose ":]" -- ':]'
         "(->)" -> do
           mark GHC.AnnOpenP -- '('
           mark GHC.AnnRarrow
           mark GHC.AnnCloseP -- ')'
         "~#"  -> do
           mark GHC.AnnOpenP -- '('
           mark GHC.AnnTildehsh
           mark GHC.AnnCloseP
         "*"  -> do
           markExternal l GHC.AnnVal str
         ":"  -> do
           doNormalRdrName
           -- mark GHC.AnnOpenP -- '('
           -- markExternal l GHC.AnnVal str
           -- mark GHC.AnnCloseP -- ')'
         ('(':',':_) -> do
           mark GHC.AnnOpenP
           let cnt = length $ filter (==',') str
           -- markExternal l GHC.AnnVal str
           replicateM_ cnt (mark GHC.AnnCommaTuple)
           mark GHC.AnnCloseP -- ')'
#if __GLASGOW_HASKELL__ <= 710
         "~" -> do
           mark GHC.AnnOpenP
           mark GHC.AnnTilde
           mark GHC.AnnCloseP
#endif
         -- _ -> doNormalRdrName
         _ -> do
            let isSym' = isSymRdr  (GHC.nameRdrName n')
            when isSym' $ mark GHC.AnnOpenP -- '('
            markWithString GHC.AnnVal str
            -- markExternal l GHC.AnnVal str
            when isSym $ mark GHC.AnnCloseP -- ')'
    -- inContext (Set.fromList [Intercalate]) $ mark GHC.AnnComma
    inContext (Set.fromList [Intercalate]) $ mark GHC.AnnComma `debug` ("AnnComma in RdrName")
    -- inContext (Set.fromList [Intercalate]) $ trace ("AnnComma in RdrName") $ mark GHC.AnnComma

-- ---------------------------------------------------------------------

-- TODO: What is this used for? Not in ExactPrint
instance Annotate GHC.Name where
  markAST l n = do
    markExternal l GHC.AnnVal (showGhc n)

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.HasOccName name,Annotate name)
  => Annotate (GHC.ImportDecl name) where
 markAST _ imp@(GHC.ImportDecl msrc modname mpkg src safeflag qualFlag _impl _as hiding) = do

   -- 'import' maybe_src maybe_safe optqualified maybe_pkg modid maybeas maybeimpspec
   mark GHC.AnnImport

   -- "{-# SOURCE" and "#-}"
   when src (markWithString GHC.AnnOpen (fromMaybe "{-# SOURCE" msrc)
             >> markWithString GHC.AnnClose "#-}")
   when safeflag (mark GHC.AnnSafe)
   when qualFlag (unsetContext TopLevel $ mark GHC.AnnQualified)
   case mpkg of
    Nothing -> return ()
#if __GLASGOW_HASKELL__ <= 710
    Just pkg -> markWithString GHC.AnnPackageName (show (GHC.unpackFS pkg))
#else
    Just (GHC.StringLiteral srcPkg _) -> markWithString GHC.AnnPackageName srcPkg
#endif

   markLocated modname

   case GHC.ideclAs imp of
      Nothing -> return ()
      Just mn -> do
          mark GHC.AnnAs
          markWithString GHC.AnnVal (GHC.moduleNameString mn)

   case hiding of
     Nothing -> return ()
     Just (isHiding,lie) -> do
       -- when isHiding $ unsetContext TopLevel $ mark GHC.AnnHiding
       if isHiding
         then setContext (Set.singleton HasHiding) $
                markLocated lie
         else markLocated lie
   markTrailingSemi

-- ---------------------------------------------------------------------

instance Annotate GHC.ModuleName where
   markAST l mname =
    markExternal l GHC.AnnVal (GHC.moduleNameString mname)

-- ---------------------------------------------------------------------

-- instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
markLHsDecl :: (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
            => GHC.LHsDecl name -> Annotated ()
markLHsDecl (GHC.L l decl) =
    case decl of
      GHC.TyClD d       -> markLocated (GHC.L l d)
      GHC.InstD d       -> markLocated (GHC.L l d)
      GHC.DerivD d      -> markLocated (GHC.L l d)
      GHC.ValD d        -> markLocated (GHC.L l d)
      GHC.SigD d        -> markLocated (GHC.L l d)
      GHC.DefD d        -> markLocated (GHC.L l d)
      GHC.ForD d        -> markLocated (GHC.L l d)
      GHC.WarningD d    -> markLocated (GHC.L l d)
      GHC.AnnD d        -> markLocated (GHC.L l d)
      GHC.RuleD d       -> markLocated (GHC.L l d)
      GHC.VectD d       -> markLocated (GHC.L l d)
      GHC.SpliceD d     -> markLocated (GHC.L l d)
      GHC.DocD d        -> markLocated (GHC.L l d)
      GHC.RoleAnnotD d  -> markLocated (GHC.L l d)
#if __GLASGOW_HASKELL__ < 711
      GHC.QuasiQuoteD d -> markLocated (GHC.L l d)
#endif

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
            => Annotate (GHC.HsDecl name) where
  markAST l d = markLHsDecl (GHC.L l d)

-- ---------------------------------------------------------------------

instance (Annotate name)
   => Annotate (GHC.RoleAnnotDecl name) where
  markAST _ (GHC.RoleAnnotDecl ln mr) = do
    mark GHC.AnnType
    mark GHC.AnnRole
    markLocated ln
    mapM_ markLocated mr

instance Annotate (Maybe GHC.Role) where
  markAST l Nothing  = markExternal l GHC.AnnVal "_"
  markAST l (Just r) = markExternal l GHC.AnnVal (GHC.unpackFS $ GHC.fsFromRole r)

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
   => Annotate (GHC.SpliceDecl name) where
  markAST _ (GHC.SpliceDecl e _flag) = do
#if __GLASGOW_HASKELL__ > 710
    mark GHC.AnnOpenPE
#endif
    markLocated e
#if __GLASGOW_HASKELL__ > 710
    mark GHC.AnnCloseP
#endif
    markTrailingSemi

{-
- data SpliceExplicitFlag = ExplicitSplice | -- <=> $(f x y)
-                           ImplicitSplice   -- <=> f x y,  i.e. a naked
-                           top level expression
-
-}

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
   => Annotate (GHC.VectDecl name) where
  markAST _ (GHC.HsVect src ln e) = do
    markWithString GHC.AnnOpen src -- "{-# VECTORISE"
    markLocated ln
    mark GHC.AnnEqual
    markLocated e
    markWithString GHC.AnnClose "#-}" -- "#-}"

  markAST _ (GHC.HsNoVect src ln) = do
    markWithString GHC.AnnOpen src -- "{-# NOVECTORISE"
    markLocated ln
    markWithString GHC.AnnClose "#-}" -- "#-}"

  markAST _ (GHC.HsVectTypeIn src _b ln mln) = do
    markWithString GHC.AnnOpen src -- "{-# VECTORISE" or "{-# VECTORISE SCALAR"
    mark GHC.AnnType
    markLocated ln
    case mln of
      Nothing -> return ()
      Just ln -> do
        mark GHC.AnnEqual
        markLocated ln
    markWithString GHC.AnnClose "#-}" -- "#-}"

  markAST _ (GHC.HsVectTypeOut {}) =
    traceM "warning: HsVectTypeOut appears after renaming"

  markAST _ (GHC.HsVectClassIn src ln) = do
    markWithString GHC.AnnOpen src -- "{-# VECTORISE"
    mark GHC.AnnClass
    markLocated ln
    markWithString GHC.AnnClose "#-}" -- "#-}"

  markAST _ (GHC.HsVectClassOut {}) =
    traceM "warning: HsVecClassOut appears after renaming"
  markAST _ (GHC.HsVectInstIn {})   =
    traceM "warning: HsVecInstsIn appears after renaming"
  markAST _ (GHC.HsVectInstOut {})   =
    traceM "warning: HsVecInstOut appears after renaming"

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
   => Annotate (GHC.RuleDecls name) where
   markAST _ (GHC.HsRules src rules) = do
     markWithString GHC.AnnOpen src
     mapM_ markLocated rules
     markWithString GHC.AnnClose "#-}"
     markTrailingSemi

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
   => Annotate (GHC.RuleDecl name) where
  markAST _ (GHC.HsRule ln act bndrs lhs _ rhs _) = do
    markLocated ln
    setContext (Set.singleton ExplicitNeverActive) $ markActivation act

    when (not $ null bndrs) $ do
      mark GHC.AnnForall
      mapM_ markLocated bndrs
      mark GHC.AnnDot

    markLocated lhs
    mark GHC.AnnEqual
    markLocated rhs
    markTrailingSemi

-- ---------------------------------------------------------------------

markActivation :: GHC.Activation -> Annotated ()
markActivation act = do
#if __GLASGOW_HASKELL__ <= 710
  case act of
    GHC.ActiveBefore n -> do
      mark GHC.AnnOpenS --  '['
      mark GHC.AnnTilde -- ~
      markWithString GHC.AnnVal (show n)
      mark GHC.AnnCloseS -- ']'
    GHC.ActiveAfter n -> do
      mark GHC.AnnOpenS --  '['
      markWithString GHC.AnnVal (show n)
      mark GHC.AnnCloseS -- ']'
    GHC.NeverActive -> do
      inContext (Set.singleton ExplicitNeverActive) $ do
        mark GHC.AnnOpenS --  '['
        mark GHC.AnnTilde -- ~
        mark GHC.AnnCloseS -- ']'
    _ -> return ()
#else
  case act of
    GHC.ActiveBefore src _ -> do
      mark GHC.AnnOpenS --  '['
      mark GHC.AnnTilde -- ~
      markWithString GHC.AnnVal src
      mark GHC.AnnCloseS -- ']'
    GHC.ActiveAfter src _ -> do
      mark GHC.AnnOpenS --  '['
      markWithString GHC.AnnVal src
      mark GHC.AnnCloseS -- ']'
    GHC.NeverActive -> do
      inContext (Set.singleton ExplicitNeverActive) $ do
        mark GHC.AnnOpenS --  '['
        mark GHC.AnnTilde -- ~
        mark GHC.AnnCloseS -- ']'
    _ -> return ()
#endif

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
   => Annotate (GHC.RuleBndr name) where
  markAST _ (GHC.RuleBndr ln) = markLocated ln
#if __GLASGOW_HASKELL__ <= 710
  markAST _ (GHC.RuleBndrSig ln (GHC.HsWB thing _ _ _)) = do
    mark GHC.AnnOpenP -- "("
    markLocated ln
    mark GHC.AnnDcolon
    markLocated thing
    mark GHC.AnnCloseP -- ")"
#else
  markAST _ (GHC.RuleBndrSig ln st) = do
    mark GHC.AnnOpenP -- "("
    markLocated ln
    mark GHC.AnnDcolon
    markLHsSigWcType st
    mark GHC.AnnCloseP -- ")"
#endif
-- ---------------------------------------------------------------------
#if __GLASGOW_HASKELL__ > 710
markLHsSigWcType :: (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
                 => GHC.LHsSigWcType name -> Annotated ()
markLHsSigWcType (GHC.HsIB _ (GHC.HsWC _ mwc ty)) = do
    case mwc of
      Nothing -> markLocated ty
      Just lwc -> do
       -- let sorted = lexicalSortLocated (GHC.L lwc GHC.HsWildCardTy:[ty])
       -- markLocated (GHC.L lc sorted)
       applyListAnnotations ([(lwc,markExternal lwc GHC.AnnVal "_")]
                          ++ prepareListAnnotation [ty]
                            )
#endif
-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
   => Annotate (GHC.AnnDecl name) where
   markAST _ (GHC.HsAnnotation src prov e) = do
     markWithString GHC.AnnOpen src
     case prov of
       (GHC.ValueAnnProvenance n) -> markLocated n
       (GHC.TypeAnnProvenance n) -> do
         mark GHC.AnnType
         markLocated n
       GHC.ModuleAnnProvenance -> mark GHC.AnnModule

     markLocated e
     markWithString GHC.AnnClose "#-}"
     markTrailingSemi

-- ---------------------------------------------------------------------

instance Annotate name => Annotate (GHC.WarnDecls name) where
   markAST _ (GHC.Warnings src warns) = do
     markWithString GHC.AnnOpen src
     mapM_ markLocated warns
     markWithString GHC.AnnClose "#-}"

-- ---------------------------------------------------------------------

instance (Annotate name)
   => Annotate (GHC.WarnDecl name) where
   markAST _ (GHC.Warning lns txt) = do
     markListIntercalate lns
     mark GHC.AnnOpenS -- "["
     case txt of
       GHC.WarningTxt    _src ls -> markListIntercalate ls
       GHC.DeprecatedTxt _src ls -> markListIntercalate ls
     mark GHC.AnnCloseS -- "]"

instance Annotate GHC.FastString where
  -- TODO: https://ghc.haskell.org/trac/ghc/ticket/10313 applies.
  markAST l fs = do
    markExternal l GHC.AnnVal (show (GHC.unpackFS fs))
    -- markExternal l GHC.AnnVal ('"':(GHC.unpackFS fs++"\""))
    inContext (Set.fromList [Intercalate]) $ mark GHC.AnnComma

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
   => Annotate (GHC.ForeignDecl name) where
#if __GLASGOW_HASKELL__ <= 710
  markAST _ (GHC.ForeignImport ln typ _
               (GHC.CImport cconv safety@(GHC.L ll _) _mh _imp (GHC.L ls src))) = do
#else
  markAST _ (GHC.ForeignImport ln (GHC.HsIB _ typ) _
               (GHC.CImport cconv safety@(GHC.L ll _) _mh _imp (GHC.L ls src))) = do
{-
  = ForeignImport
      { fd_name   :: Located name          -- defines this name
      , fd_sig_ty :: LHsSigType name       -- sig_ty
      , fd_co     :: PostTc name Coercion  -- rep_ty ~ sig_ty
      , fd_fi     :: ForeignImport }

-}
#endif
    mark GHC.AnnForeign
    mark GHC.AnnImport
    markLocated cconv
    if ll == GHC.noSrcSpan
      then return ()
      else markLocated safety
    -- markMaybe mh
#if __GLASGOW_HASKELL__ <= 710
    markExternal ls GHC.AnnVal (show src)
#else
    if GHC.unLoc cconv == GHC.PrimCallConv
      then markExternal ls GHC.AnnVal src
      else markExternal ls GHC.AnnVal (show src)
#endif
    markLocated ln
    mark GHC.AnnDcolon
    markLocated typ
    markTrailingSemi


#if __GLASGOW_HASKELL__ <= 710
  markAST _l (GHC.ForeignExport ln typ _ (GHC.CExport spec (GHC.L ls src))) = do
#else
  markAST _l (GHC.ForeignExport ln (GHC.HsIB _ typ) _ (GHC.CExport spec (GHC.L ls src))) = do
#endif
    mark GHC.AnnForeign
    mark GHC.AnnExport
    markLocated spec
    markExternal ls GHC.AnnVal (show src)
    -- markLocated ln
    setContext (Set.singleton PrefixOp) $ markLocated ln
    mark GHC.AnnDcolon
    markLocated typ


-- ---------------------------------------------------------------------

instance (Annotate GHC.CExportSpec) where
#if __GLASGOW_HASKELL__ <= 710
  markAST l (GHC.CExportStatic _ cconv) = markAST l cconv
#else
  markAST l (GHC.CExportStatic _src _ cconv) = markAST l cconv
#endif

-- ---------------------------------------------------------------------

instance (Annotate GHC.CCallConv) where
  markAST l GHC.StdCallConv        =  markExternal l  GHC.AnnVal "stdcall"
  markAST l GHC.CCallConv          =  markExternal l GHC.AnnVal "ccall"
  markAST l GHC.CApiConv           =  markExternal l GHC.AnnVal "capi"
  markAST l GHC.PrimCallConv       =  markExternal l GHC.AnnVal "prim"
  markAST l GHC.JavaScriptCallConv =  markExternal l GHC.AnnVal "javascript"

-- ---------------------------------------------------------------------

instance (Annotate GHC.Safety) where
  markAST l GHC.PlayRisky         = markExternal l GHC.AnnVal "unsafe"
  markAST l GHC.PlaySafe          = markExternal l GHC.AnnVal "safe"
  markAST l GHC.PlayInterruptible = markExternal l GHC.AnnVal "interruptible"

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
   => Annotate (GHC.DerivDecl name) where

#if __GLASGOW_HASKELL__ <= 710
  markAST _ (GHC.DerivDecl typ mov) = do
#else
  markAST _ (GHC.DerivDecl (GHC.HsIB _ typ) mov) = do
#endif
    mark GHC.AnnDeriving
    mark GHC.AnnInstance
    markMaybe mov
    markLocated typ

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
   => Annotate (GHC.DefaultDecl name) where

  markAST _ (GHC.DefaultDecl typs) = do
    mark GHC.AnnDefault
    mark GHC.AnnOpenP -- '('
    markListIntercalate typs
    mark GHC.AnnCloseP -- ')'
    markTrailingSemi

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
   => Annotate (GHC.InstDecl name) where

  markAST l (GHC.ClsInstD      cid) = markAST l  cid
  markAST l (GHC.DataFamInstD dfid) = markAST l dfid
  markAST l (GHC.TyFamInstD   tfid) = markAST l tfid

-- ---------------------------------------------------------------------

instance Annotate GHC.OverlapMode where
  markAST _ (GHC.NoOverlap src) = do
    markWithString GHC.AnnOpen src
    markWithString GHC.AnnClose "#-}"

  markAST _ (GHC.Overlappable src) = do
    markWithString GHC.AnnOpen src
    markWithString GHC.AnnClose "#-}"

  markAST _ (GHC.Overlapping src) = do
    markWithString GHC.AnnOpen src
    markWithString GHC.AnnClose "#-}"

  markAST _ (GHC.Overlaps src) = do
    markWithString GHC.AnnOpen src
    markWithString GHC.AnnClose "#-}"

  markAST _ (GHC.Incoherent src) = do
    markWithString GHC.AnnOpen src
    markWithString GHC.AnnClose "#-}"

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
   => Annotate (GHC.ClsInstDecl name) where

#if __GLASGOW_HASKELL__ <= 710
  markAST _ (GHC.ClsInstDecl poly binds sigs tyfams datafams mov) = do
#else
  markAST _ (GHC.ClsInstDecl (GHC.HsIB _ poly) binds sigs tyfams datafams mov) = do
#endif
    mark GHC.AnnInstance
    markMaybe mov
    markLocated poly
    mark GHC.AnnWhere
    markOptional GHC.AnnOpenC -- '{'
    markInside GHC.AnnSemi

    applyListAnnotationsLayout (prepareListAnnotation (GHC.bagToList binds)
                             ++ prepareListAnnotation sigs
                             ++ prepareListAnnotation tyfams
                             ++ prepareListAnnotation datafams
                               )

    markOptional GHC.AnnCloseC -- '}'
    markTrailingSemi

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
   => Annotate (GHC.TyFamInstDecl name) where

  markAST _ (GHC.TyFamInstDecl eqn _) = do
    mark GHC.AnnType
#if __GLASGOW_HASKELL__ <= 710
    -- markOptional GHC.AnnInstance -- Note: this keyword is optional
    inContext (Set.singleton TopLevel) $ mark GHC.AnnInstance -- Note: this keyword is optional
#else
    mark GHC.AnnInstance -- Note: this keyword is optional
#endif
    markLocated eqn
    markTrailingSemi

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
   => Annotate (GHC.DataFamInstDecl name) where

#if __GLASGOW_HASKELL__ <= 710
  markAST l (GHC.DataFamInstDecl ln (GHC.HsWB pats _ _ _) defn _) = do
#else
  markAST l (GHC.DataFamInstDecl ln (GHC.HsIB _ pats) defn _) = do
#endif
    case GHC.dd_ND defn of
      GHC.NewType  -> mark GHC.AnnNewtype
      GHC.DataType -> mark GHC.AnnData
    -- when (isGadt $ GHC.dd_cons defn) $ mark GHC.AnnInstance
    inContext (Set.singleton TopLevel) $ mark GHC.AnnInstance

    markOptional GHC.AnnOpenP

    applyListAnnotations (prepareListAnnotation [ln]
                       ++ prepareListAnnotation pats
                         )

    markOptional GHC.AnnCloseP
#if __GLASGOW_HASKELL__ > 710
    case (GHC.dd_kindSig defn) of
      Just s -> do
        mark GHC.AnnDcolon
        markLocated s
      Nothing -> return ()
#endif
    -- mark GHC.AnnWhere
    if (isGadt $ GHC.dd_cons defn)
      then mark GHC.AnnWhere
      else mark GHC.AnnEqual
    markDataDefn l defn

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
  => Annotate (GHC.HsBind name) where
#if __GLASGOW_HASKELL__ <= 710
  markAST _ (GHC.FunBind _ _ (GHC.MG matches _ _ _) _ _ _) = do
#else
  markAST _ (GHC.FunBind _ (GHC.MG (GHC.L _ matches) _ _ _) _ _ _) = do
#endif
    -- Note: from a layout perspective a FunBind should not exist, so the
    -- current context is passed through unchanged to the matches.
    -- TODO: perhaps bring the edp from the first match up to the annotation for
    -- the FunBind.
    -- bumpContext $ mapM_ markLocated matches
    ifInContext (Set.fromList [CtxOnly,CtxFirst])
      (markListWithContexts' listContexts matches)
      (markListWithContexts (lcMiddle listContexts) (lcLast listContexts) matches)

#if __GLASGOW_HASKELL__ <= 710
  markAST _ (GHC.PatBind lhs (GHC.GRHSs grhs lb) _typ _fvs _ticks) = do
#else
  markAST _ (GHC.PatBind lhs (GHC.GRHSs grhs (GHC.L _ lb)) _typ _fvs _ticks) = do
#endif
    markLocated lhs
    case grhs of
      (GHC.L _ (GHC.GRHS [] _):_) -> mark GHC.AnnEqual -- empty guards
      _ -> return ()
    markListIntercalateWithFunLevel markLocated 2 grhs
    when (not (GHC.isEmptyLocalBinds lb)) $ mark GHC.AnnWhere

    markLocalBindsWithLayout lb
    markTrailingSemi

  markAST _ (GHC.VarBind _n rhse _) =
    -- Note: this bind is introduced by the typechecker
    markLocated rhse

  markAST l (GHC.PatSynBind (GHC.PSB ln _fvs args def dir)) = do
    mark GHC.AnnPattern
    case args of
      GHC.InfixPatSyn la lb -> do
        markLocated la
        setContext (Set.singleton InOp) $ markLocated ln
        markLocated lb
      GHC.PrefixPatSyn ns -> do
        markLocated ln
        mapM_ markLocated ns
#if __GLASGOW_HASKELL__ > 710
      GHC.RecordPatSyn fs -> do
        markLocated ln
        mark GHC.AnnOpenC  -- '{'
        markListIntercalateWithFun (markLocated . GHC.recordPatSynSelectorId) fs
        mark GHC.AnnCloseC -- '}'
#endif
    case dir of
      GHC.ImplicitBidirectional -> mark GHC.AnnEqual
      _                         -> mark GHC.AnnLarrow

    markLocated def
    case dir of
      GHC.Unidirectional           -> return ()
      GHC.ImplicitBidirectional    -> return ()
      GHC.ExplicitBidirectional mg -> do
        mark GHC.AnnWhere
        mark GHC.AnnOpenC  -- '{'
        markMatchGroup l mg
        mark GHC.AnnCloseC -- '}'

    -- mark GHC.AnnWhere
    -- mark GHC.AnnOpenC  -- '{'
    -- mark GHC.AnnCloseC -- '}'
    markTrailingSemi

  -- Introduced after renaming.
  markAST _ (GHC.AbsBinds _ _ _ _ _) =
    traceM "warning: AbsBinds introduced after renaming"

#if __GLASGOW_HASKELL__ > 710
  -- Introduced after renaming.
  markAST _ GHC.AbsBindsSig{} =
    traceM "warning: AbsBindsSig introduced after renaming"
#endif

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
    => Annotate (GHC.IPBind name) where
  markAST _ (GHC.IPBind en e) = do
    case en of
      Left n -> markLocated n
      Right _i -> return ()
    mark GHC.AnnEqual
    markLocated e
    markTrailingSemi

-- ---------------------------------------------------------------------

instance Annotate GHC.HsIPName where
  markAST l (GHC.HsIPName n) = markExternal l (GHC.AnnVal) ("?" ++ GHC.unpackFS n)

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name,
                                                  Annotate body)
  => Annotate (GHC.Match name (GHC.Located body)) where

#if __GLASGOW_HASKELL__ <= 710
  markAST _ (GHC.Match mln pats _typ (GHC.GRHSs grhs lb)) = do
#else
  markAST _ (GHC.Match mln pats _typ (GHC.GRHSs grhs (GHC.L _ lb))) = do
#endif
    let
#if __GLASGOW_HASKELL__ <= 710
      get_infix Nothing = False
      get_infix (Just (_,f)) = f
#else
      get_infix GHC.NonFunBindMatch    = False
      get_infix (GHC.FunBindMatch _ f) = f
#endif
#if __GLASGOW_HASKELL__ <= 710
      isFunBind = isJust
#else
      isFunBind GHC.NonFunBindMatch = False
      isFunBind GHC.FunBindMatch{}  = True
#endif
    case (get_infix mln,pats) of
      (True, a:b:xs) -> do
        markOptional GHC.AnnOpenP
        markLocated a
        case mln of
#if __GLASGOW_HASKELL__ <= 710
          Nothing -> return ()
          Just (n,_) -> setContext (Set.singleton InOp) $ markLocated n
#else
          GHC.NonFunBindMatch -> return ()
          GHC.FunBindMatch n _ -> setContext (Set.singleton InOp) $ markLocated n
#endif
        markLocated b
        markOptional GHC.AnnCloseP
        mapM_ markLocated xs
      _ -> do
        annotationsToComments [GHC.AnnOpenP,GHC.AnnCloseP]
        inContext (Set.fromList [LambdaExpr]) $ do mark GHC.AnnLam -- For HsLam
#if __GLASGOW_HASKELL__ <= 710
        case mln of
          -- Nothing -> mark GHC.AnnFunId
          Nothing -> markListNoPrecedingSpace False pats
          Just (n,_) -> do
            setContext (Set.fromList [NoPrecedingSpace,PrefixOp]) $ markLocated n
            mapM_ markLocated pats
        -- markListNoPrecedingSpace pats
#else
        case mln of
          -- GHC.NonFunBindMatch  -> mark GHC.AnnFunId
          GHC.NonFunBindMatch  -> markListNoPrecedingSpace False pats
          GHC.FunBindMatch n _ -> do
            -- setContext (Set.singleton NoPrecedingSpace) $ markLocated n
            setContext (Set.fromList [NoPrecedingSpace,PrefixOp]) $ markLocated n
            mapM_ markLocated pats
#endif

    -- TODO: The AnnEqual annotation actually belongs in the first GRHS value
    case grhs of
      (GHC.L _ (GHC.GRHS [] _):_) -> when (isFunBind mln) $ mark GHC.AnnEqual -- empty guards
      _ -> return ()
    inContext (Set.fromList [LambdaExpr]) $ mark GHC.AnnRarrow -- For HsLam
    -- markListIntercalate grhs
    mapM_ markLocated grhs

    case lb of
      GHC.EmptyLocalBinds -> return ()
      _ -> do
        mark GHC.AnnWhere
        markOptional GHC.AnnOpenC -- '{'
        markInside GHC.AnnSemi
        markLocalBindsWithLayout lb
        markOptional GHC.AnnCloseC -- '}'
    markTrailingSemi

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,
          Annotate name, Annotate body)
  => Annotate (GHC.GRHS name (GHC.Located body)) where
  markAST _ (GHC.GRHS guards expr) = do
    case guards of
      [] -> return ()
      (_:_) -> do
        mark GHC.AnnVbar
        -- markListIntercalate guards
        -- markListIntercalateWithFunLevel markLocated 2 guards
        unsetContext Intercalate $ markListIntercalate guards
        ifInContext (Set.fromList [CaseAlt])
          (return ())
          (mark GHC.AnnEqual)

    inContext (Set.fromList [CaseAlt]) $ mark GHC.AnnRarrow -- For HsLam
    -- PrefixOp needed for single top-level HsVar
    -- setContextLevel (Set.singleton PrefixOp) 2 $ markLocated expr
    setContextLevel (Set.fromList [LeftMost,PrefixOp]) 2 $ markLocated expr

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
  => Annotate (GHC.Sig name) where

#if __GLASGOW_HASKELL__ <= 710
  markAST _ (GHC.TypeSig lns typ _) = do
#else
  markAST _ (GHC.TypeSig lns st)  = do
#endif
    setContext (Set.singleton PrefixOp) $ markListNoPrecedingSpace True lns
    mark GHC.AnnDcolon
#if __GLASGOW_HASKELL__ <= 710
    markLocated typ
#else
    markLHsSigWcType st
#endif
    markTrailingSemi

#if __GLASGOW_HASKELL__ <= 710
  markAST _ (GHC.PatSynSig ln (_,GHC.HsQTvs _ns bndrs) ctx1 ctx2 typ) = do
    mark GHC.AnnPattern
    markLocated ln
    mark GHC.AnnDcolon

    -- Note: The 'forall' bndrs '.' may occur multiple times
    mark GHC.AnnForall
    mapM_ markLocated bndrs
    mark GHC.AnnDot

    setContext (Set.singleton Parens) $ markLocated ctx1
    markOffset GHC.AnnDarrow 0
    setContext (Set.singleton Parens) $ markLocated ctx2
    markOffset GHC.AnnDarrow 1
    markLocated typ
    markTrailingSemi
#else
  markAST _ (GHC.PatSynSig ln (GHC.HsIB _ typ)) = do
    mark GHC.AnnPattern
    markLocated ln
    mark GHC.AnnDcolon
    markLocated typ
    markTrailingSemi
#endif

#if __GLASGOW_HASKELL__ <= 710
  markAST _ (GHC.GenericSig ns typ) = do
    mark GHC.AnnDefault
#else
  markAST _ (GHC.ClassOpSig isDefault ns (GHC.HsIB _ typ)) = do
    when isDefault $ mark GHC.AnnDefault
#endif
    -- markListIntercalate ns
    setContext (Set.singleton PrefixOp) $ markListIntercalate ns
    mark GHC.AnnDcolon
    markLocated typ
    markTrailingSemi

  markAST _ (GHC.IdSig _) =
    traceM "warning: Introduced after renaming"

  -- FixSig (FixitySig name)
#if __GLASGOW_HASKELL__ <= 710
  markAST _ (GHC.FixSig (GHC.FixitySig lns (GHC.Fixity v fdir))) = do
#else
  markAST _ (GHC.FixSig (GHC.FixitySig lns (GHC.Fixity src _v fdir))) = do
#endif
    let fixstr = case fdir of
         GHC.InfixL -> "infixl"
         GHC.InfixR -> "infixr"
         GHC.InfixN -> "infix"
    markWithString GHC.AnnInfix fixstr
#if __GLASGOW_HASKELL__ <= 710
    markWithString GHC.AnnVal (show v)
#else
    markWithString GHC.AnnVal src
#endif
    setContext (Set.singleton InOp) $ markListIntercalate lns
    markTrailingSemi

  -- InlineSig (Located name) InlinePragma
  -- '{-# INLINE' activation qvar '#-}'
  markAST _ (GHC.InlineSig ln inl) = do
    markWithString GHC.AnnOpen (GHC.inl_src inl) -- '{-# INLINE'
    markActivation (GHC.inl_act inl)
    -- markLocated ln
    setContext (Set.singleton PrefixOp) $ markLocated ln
    markWithString GHC.AnnClose "#-}" -- '#-}'
    markTrailingSemi

#if __GLASGOW_HASKELL__ <= 710
  markAST _ (GHC.SpecSig ln typs inl) = do
#else
  markAST _ (GHC.SpecSig ln typs inl) = do
{-
  | SpecSig     (Located name)     -- Specialise a function or datatype  ...
                [LHsSigType name]  -- ... to these types
                InlinePragma       -- The pragma on SPECIALISE_INLINE form.
                                   -- If it's just defaultInlinePragma, then we said
                                   --    SPECIALISE, not SPECIALISE_INLINE

-}
#endif
    markWithString GHC.AnnOpen (GHC.inl_src inl)
    markActivation (GHC.inl_act inl)
    markLocated ln
    mark GHC.AnnDcolon -- '::'
#if __GLASGOW_HASKELL__ <= 710
    markListIntercalate typs
#else
    markListIntercalateWithFun markLHsSigType typs
#endif
    markWithString GHC.AnnClose "#-}" -- '#-}'
    markTrailingSemi


  -- '{-# SPECIALISE' 'instance' inst_type '#-}'
  markAST _ (GHC.SpecInstSig src typ) = do
    markWithString GHC.AnnOpen src
    mark GHC.AnnInstance
#if __GLASGOW_HASKELL__ <= 710
    markLocated typ
#else
    markLHsSigType typ
#endif
    markWithString GHC.AnnClose "#-}" -- '#-}'
    markTrailingSemi



  -- MinimalSig (BooleanFormula (Located name))
  markAST l (GHC.MinimalSig src formula) = do
    markWithString GHC.AnnOpen src
#if __GLASGOW_HASKELL__ <= 710
    annotationsToCommentsBF formula [GHC.AnnOpenP,GHC.AnnCloseP,GHC.AnnComma,GHC.AnnVbar]
    markAST l formula
    finalizeBF l
#else
    markLocated formula
#endif
    markWithString GHC.AnnClose "#-}"
    markTrailingSemi

-- --------------------------------------------------------------------

#if __GLASGOW_HASKELL__ > 710
markLHsSigType :: (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
               => GHC.LHsSigType name -> Annotated ()
markLHsSigType (GHC.HsIB _ typ) = markLocated typ

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
   => Annotate [GHC.LHsSigType name] where
  markAST l ls = do
    mark GHC.AnnDeriving
    markMany GHC.AnnOpenP
    -- mapM_ markLHsSigType ls
    markListIntercalateWithFun markLHsSigType ls
    markMany GHC.AnnCloseP
#endif

-- --------------------------------------------------------------------

#if __GLASGOW_HASKELL__ <= 710
-- In practice, due to the way the BooleanFormula is constructed in the parser,
-- we will get the following variants
-- a | b : Or [a,b]
-- a , b : And [a,b]
-- ( a ) : a
-- A bottom level Located RdrName is captured in a Var. This is the only part
-- with a location in it.
--
-- So the best strategy might be to convert all the annotations into comments,
-- and then just print the names. DONE
instance  (Annotate name) => Annotate (GHC.BooleanFormula (GHC.Located name)) where
  -- markAST _ (GHC.Var x)  = markLocated x
  markAST _ (GHC.Var x)  = setContext (Set.singleton PrefixOp) $ markLocated x
  markAST l (GHC.Or ls)  = mapM_ (markAST l) ls
  markAST l (GHC.And ls) = mapM_ (markAST l) ls
#else
instance  (Annotate name) => Annotate (GHC.BooleanFormula (GHC.Located name)) where
  markAST _ (GHC.Var x)  = do
    markLocated x
    mark GHC.AnnVbar -- '|'
    inContext (Set.fromList [Intercalate]) $ mark GHC.AnnComma
  markAST l (GHC.Or ls)  = mapM_ markLocated ls
  markAST l (GHC.And ls) = do
    markListIntercalate ls
    mark GHC.AnnVbar -- '|'
    inContext (Set.fromList [Intercalate]) $ mark GHC.AnnComma
  markAST _ (GHC.Parens x)  = do
    mark GHC.AnnOpenP -- '('
    markLocated x
    mark GHC.AnnCloseP -- ')'
    mark GHC.AnnVbar -- '|'
    inContext (Set.fromList [Intercalate]) $ mark GHC.AnnComma
#endif

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
  => Annotate (GHC.HsTyVarBndr name) where
  markAST l (GHC.UserTyVar n) = do
#if __GLASGOW_HASKELL__ <= 710
    markAST l n
#else
    markLocated n
#endif

  markAST _ (GHC.KindedTyVar n ty) = do
    mark GHC.AnnOpenP  -- '('
    markLocated n
    mark GHC.AnnDcolon -- '::'
    markLocated ty
    mark GHC.AnnCloseP -- '('

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
   => Annotate (GHC.HsType name) where
  markAST loc ty = do
    markType loc ty
    inContext (Set.fromList [Intercalate]) $ mark GHC.AnnComma
   where

    -- markType :: GHC.SrcSpan -> ast -> Annotated ()
#if __GLASGOW_HASKELL__ <= 710
    markType _ (GHC.HsForAllTy _f mwc (GHC.HsQTvs _kvs tvs) ctx@(GHC.L lc ctxs) typ) = do
      -- mark GHC.AnnOpenP -- "("
      when (not $ null tvs) $ do
        mark GHC.AnnForall
        mapM_ markLocated tvs
        mark GHC.AnnDot

      case mwc of
        Nothing -> if lc /= GHC.noSrcSpan
          -- then setContext (Set.singleton Parens) $ markLocated ctx
          then markLocated ctx
          else return ()
        Just lwc -> do
         let sorted = lexicalSortLocated (GHC.L lwc GHC.HsWildcardTy:ctxs)
         setContext (Set.singleton Parens) $ markLocated (GHC.L lc sorted)
         -- mark GHC.AnnDarrow

      markLocated typ
      -- mark GHC.AnnCloseP -- ")"
#else
    markType _ (GHC.HsForAllTy tvs typ) = do
      -- mark GHC.AnnOpenP -- "("
      mark GHC.AnnForall
      mapM_ markLocated tvs
      mark GHC.AnnDot
      markLocated typ
      -- mark GHC.AnnCloseP -- ")"

  {-
    = HsForAllTy   -- See Note [HsType binders]
        { hst_bndrs :: [LHsTyVarBndr name]   -- Explicit, user-supplied 'forall a b c'
        , hst_body  :: LHsType name          -- body type
        }

  -}
#endif

#if __GLASGOW_HASKELL__ > 710
    markType l (GHC.HsQualTy cxt ty) = do
      inContext (Set.fromList [TypeAsKind]) $ do mark GHC.AnnDcolon -- for HsKind, alias for HsType
      setContext (Set.singleton Parens) $ markLocated cxt
      markLocated ty
  {-
    | HsQualTy   -- See Note [HsType binders]
        { hst_ctxt :: LHsContext name       -- Context C => blah
        , hst_body :: LHsType name }
  -}
#endif

    markType l (GHC.HsTyVar name) = do
      inContext (Set.fromList [TypeAsKind]) $ do mark GHC.AnnDcolon -- for HsKind, alias for HsType
#if __GLASGOW_HASKELL__ <= 710
      if GHC.isDataOcc $ GHC.occName name
        then do
            mark GHC.AnnSimpleQuote
            markLocatedFromKw GHC.AnnName (GHC.L l name)
        else unsetContext Intercalate  $ markAST l name
#else
      -- TODO: Should the isExactName test move into the RdrName Annotate instanced?
      if (GHC.isDataOcc $ GHC.occName $ GHC.unLoc name) && (not $ isExactName $ GHC.unLoc name)
        then do
            mark GHC.AnnSimpleQuote
            markLocatedFromKw GHC.AnnName name
        else markLocated name
#endif

#if __GLASGOW_HASKELL__ > 710
    markType _ (GHC.HsAppsTy ts) = do
      mapM_ markLocated ts
#endif

    markType _ (GHC.HsAppTy t1 t2) = do
      inContext (Set.fromList [TypeAsKind]) $ do mark GHC.AnnDcolon -- for HsKind, alias for HsType
      markLocated t1
      markLocated t2

    markType _ (GHC.HsFunTy t1 t2) = do
      inContext (Set.fromList [TypeAsKind]) $ do mark GHC.AnnDcolon -- for HsKind, alias for HsType
      markLocated t1
      mark GHC.AnnRarrow
      markLocated t2

    markType _ (GHC.HsListTy t) = do
      inContext (Set.fromList [TypeAsKind]) $ do mark GHC.AnnDcolon -- for HsKind, alias for HsType
      mark GHC.AnnOpenS -- '['
      markLocated t
      mark GHC.AnnCloseS -- ']'

    markType _ (GHC.HsPArrTy t) = do
      markWithString GHC.AnnOpen "[:" -- '[:'
      markLocated t
      markWithString GHC.AnnClose ":]" -- ':]'

    markType _ (GHC.HsTupleTy tt ts) = do
      inContext (Set.fromList [TypeAsKind]) $ do mark GHC.AnnDcolon -- for HsKind, alias for HsType
      case tt  of
        GHC.HsBoxedOrConstraintTuple -> mark GHC.AnnOpenP  -- '('
        _                            -> markWithString GHC.AnnOpen "(#" -- '(#'
      -- markListIntercalate ts
      markListIntercalateWithFunLevel markLocated 2 ts
      case tt  of
        GHC.HsBoxedOrConstraintTuple -> mark GHC.AnnCloseP  -- ')'
        _                            -> markWithString GHC.AnnClose "#)" -- '#)'

#if __GLASGOW_HASKELL__ <= 710
    markType _ (GHC.HsOpTy t1 (_,lo) t2) = do
#else
    markType _ (GHC.HsOpTy t1 lo t2) = do
    -- HsOpTy              (LHsType name) (Located name) (LHsType name)
#endif
      markLocated t1
      mark GHC.AnnSimpleQuote
      setContext (Set.singleton InOp) $ markLocated lo
      markLocated t2

    markType _ (GHC.HsParTy t) = do
      -- mark GHC.AnnDcolon -- for HsKind, alias for HsType
      inContext (Set.fromList [TypeAsKind]) $ do mark GHC.AnnDcolon -- for HsKind, alias for HsType
      mark GHC.AnnOpenP  -- '('
      markLocated t
      mark GHC.AnnCloseP -- ')'
  --    mark GHC.AnnDarrow -- May appear after context in a ConDecl

    markType _ (GHC.HsIParamTy (GHC.HsIPName n) t) = do
      markWithString GHC.AnnVal ("?" ++ (GHC.unpackFS n))
      mark GHC.AnnDcolon
      markLocated t

    markType _ (GHC.HsEqTy t1 t2) = do
      markLocated t1
      mark GHC.AnnTilde
      markLocated t2

    markType _ (GHC.HsKindSig t k) = do
      mark GHC.AnnOpenP  -- '('
      markLocated t
      mark GHC.AnnDcolon -- '::'
      markLocated k
      mark GHC.AnnCloseP -- ')'

    markType l (GHC.HsSpliceTy s _) = do
      -- inContext (Set.singleton InTypeBr) $ mark GHC.AnnOpenPE
      mark GHC.AnnOpenPE
      markAST l s
      -- inContext (Set.singleton InTypeBr) $ mark GHC.AnnCloseP
      mark GHC.AnnCloseP

    markType _ (GHC.HsDocTy t ds) = do
      markLocated t
      markLocated ds

#if __GLASGOW_HASKELL__ <= 710
    markType _ (GHC.HsBangTy b t) = do
      case b of
        (GHC.HsSrcBang ms (Just True) _) -> do
          markWithString GHC.AnnOpen  (maybe "{-# UNPACK" id ms)
          markWithString GHC.AnnClose "#-}"
        (GHC.HsSrcBang ms (Just False) _) -> do
          markWithString GHC.AnnOpen  (maybe "{-# NOUNPACK" id ms)
          markWithString GHC.AnnClose "#-}"
        _ -> return ()
      mark GHC.AnnBang
      markLocated t
#else
    markType _ (GHC.HsBangTy (GHC.HsSrcBang mt _up str) t) = do
      case mt of
        Nothing -> return ()
        Just src -> do
          markWithString GHC.AnnOpen src
          markWithString GHC.AnnClose "#-}"
      case str of
        GHC.SrcLazy     -> mark GHC.AnnTilde
        GHC.SrcStrict   -> mark GHC.AnnBang
        GHC.NoSrcStrict -> return ()

      markLocated t
  {-
    | HsBangTy    HsSrcBang (LHsType name)   -- Bang-style type annotations
  data HsSrcBang =
    HsSrcBang (Maybe SourceText) -- Note [Pragma source text] in BasicTypes
              SrcUnpackedness
              SrcStrictness
  data SrcStrictness = SrcLazy -- ^ Lazy, ie '~'
                     | SrcStrict -- ^ Strict, ie '!'
                     | NoSrcStrict -- ^ no strictness annotation

  data SrcUnpackedness = SrcUnpack -- ^ {-# UNPACK #-} specified
                       | SrcNoUnpack -- ^ {-# NOUNPACK #-} specified
                       | NoSrcUnpack -- ^ no unpack pragma

  -}
#endif

    -- HsRecTy [LConDeclField name]
    markType _ (GHC.HsRecTy cons) = do
      mark GHC.AnnOpenC  -- '{'
      -- mapM_ markLocated cons
      markListIntercalate cons
      mark GHC.AnnCloseC -- '}'

    -- HsCoreTy Type
    markType _ (GHC.HsCoreTy _t) =
      traceM "warning: HsCoreTy Introduced after renaming"

    markType _ (GHC.HsExplicitListTy _ ts) = do
      mark GHC.AnnSimpleQuote
      mark GHC.AnnOpenS  -- "["
      markListIntercalate ts
      mark GHC.AnnCloseS -- ']'

    markType _ (GHC.HsExplicitTupleTy _ ts) = do
      mark GHC.AnnSimpleQuote
      mark GHC.AnnOpenP
      markListIntercalate ts
      mark GHC.AnnCloseP

    -- HsTyLit HsTyLit
    markType l (GHC.HsTyLit lit) = do
      case lit of
        (GHC.HsNumTy s _) ->
          markExternal l GHC.AnnVal s
        (GHC.HsStrTy s _) ->
          markExternal l GHC.AnnVal s

    -- HsWrapTy HsTyAnnotated (HsType name)
#if __GLASGOW_HASKELL__ <= 710
    markType _ (GHC.HsWrapTy _ _) =
      traceM "warning: HsWrapTyy Introduced after renaming"
#endif

#if __GLASGOW_HASKELL__ <= 710
    markType l (GHC.HsWildcardTy) = do
      markExternal l GHC.AnnVal "_"
    markType l (GHC.HsNamedWildcardTy n) = do
      markExternal l GHC.AnnVal  (showGhc n)
#else
    markType l (GHC.HsWildCardTy (GHC.AnonWildCard _)) = do
      markExternal l GHC.AnnVal "_"
    -- markType l (GHC.HsWildCardTy (GHC.NamedWildCard n)) = do
    --   markExternal l GHC.AnnVal  (showGhc n)
#endif

#if __GLASGOW_HASKELL__ <= 710
    markType l (GHC.HsQuasiQuoteTy n) = do
      markAST l n
#endif

-- ---------------------------------------------------------------------
#if __GLASGOW_HASKELL__ > 710
instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
  => Annotate (GHC.HsAppType name) where
  markAST _ (GHC.HsAppInfix t)  = do
    mark GHC.AnnSimpleQuote
    setContext (Set.singleton InOp) $ markLocated t
  markAST _ (GHC.HsAppPrefix t) = do
    markOptional GHC.AnnTilde
    -- setContext (Set.singleton InOp) $ markLocated t
    markLocated t
#endif
-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
  => Annotate (GHC.HsSplice name) where
#if __GLASGOW_HASKELL__ > 710
  markAST l c =
    case c of
      GHC.HsQuasiQuote _ n _pos fs -> do
        markExternal l GHC.AnnVal
              ("[" ++ (showGhc n) ++ "|" ++ (GHC.unpackFS fs) ++ "|]")

      GHC.HsTypedSplice _n b@(GHC.L _ (GHC.HsVar (GHC.L _ n)))  -> do
        markWithString GHC.AnnThIdTySplice ("$$" ++ (GHC.occNameString (GHC.occName n)))
        markLocated b
      GHC.HsTypedSplice _n b -> do
        mark GHC.AnnOpenPTE
        markLocated b
        mark GHC.AnnCloseP

      GHC.HsUntypedSplice _n b@(GHC.L _ (GHC.HsVar (GHC.L _ n)))  -> do
        -- TODO: We do not seem to have any way to distinguish between which of
        -- the next two lines will emit output. If AnnThIdSplice is there, the
        -- markLocated b ends up with a negative offset so emits nothing.
        mark GHC.AnnOpenPE
        markWithString GHC.AnnThIdSplice ("$" ++ (GHC.occNameString (GHC.occName n)))
        markLocated b
        mark GHC.AnnCloseP
      GHC.HsUntypedSplice _n b  -> do
        -- TODO: when is this not optional?
        markOptional GHC.AnnThIdSplice
        mark GHC.AnnOpenPE
        markLocated b
        mark GHC.AnnCloseP
#else
  markAST _ c =
    case c of
      GHC.HsSplice _n b@(GHC.L _ (GHC.HsVar n))  -> do
        -- TODO: We do not seem to have any way to distinguish between which of
        -- the next two lines will emit output. If AnnThIdSplice is there, the
        -- markLocated b ends up with a negative offset so emits nothing.
        markWithString GHC.AnnThIdSplice   ("$" ++ (GHC.occNameString (GHC.occName n)))
        markLocated b
        mark GHC.AnnCloseP
      GHC.HsSplice _n b@(GHC.L _ (GHC.HsBracket _)) -> do
        markLocated b
      GHC.HsSplice _n b -> do
        mark GHC.AnnOpenPE
        markLocated b
        mark GHC.AnnCloseP
#endif

#if __GLASGOW_HASKELL__ <= 710
instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
  => Annotate (GHC.HsQuasiQuote name) where
  markAST l (GHC.HsQuasiQuote n _pos fs) = do
        markExternal l GHC.AnnVal
              ("[" ++ (showGhc n) ++ "|" ++ (GHC.unpackFS fs) ++ "|]")
#endif

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name) =>
                             Annotate (GHC.ConDeclField name) where
  markAST _ (GHC.ConDeclField ns ty mdoc) = do
{-
data ConDeclField name  -- Record fields have Haddoc docs on them
  = ConDeclField { cd_fld_names :: [LFieldOcc name],
                                   -- ^ See Note [ConDeclField names]
                   cd_fld_type :: LBangType name,
                   cd_fld_doc  :: Maybe LHsDocString }

-}
    -- markListIntercalate ns
    unsetContext Intercalate $ do
      markListIntercalate ns
      mark GHC.AnnDcolon
      markLocated ty
      markMaybe mdoc
    inContext (Set.fromList [Intercalate]) $ mark GHC.AnnComma

-- ---------------------------------------------------------------------

#if __GLASGOW_HASKELL__ > 710
instance (GHC.DataId name)
      => Annotate (GHC.FieldOcc name) where
  markAST l (GHC.FieldOcc rn _) = markLocated rn
#endif

-- ---------------------------------------------------------------------

instance Annotate GHC.HsDocString where
  markAST l (GHC.HsDocString s) = do
    markExternal l GHC.AnnVal (GHC.unpackFS s)

-- ---------------------------------------------------------------------
instance (GHC.DataId name,Annotate name,GHC.OutputableBndr name,GHC.HasOccName name)
  => Annotate (GHC.Pat name) where
  markAST loc typ = do
    markPat loc typ
    -- inContext (Set.fromList [Intercalate]) $ mark GHC.AnnComma
    inContext (Set.fromList [Intercalate]) $ mark GHC.AnnComma `debug` ("AnnComma in Pat")
    where
      markPat l (GHC.WildPat _) = markExternal l GHC.AnnVal "_"
      markPat l (GHC.VarPat n)  = do
        -- The parser inserts a placeholder value for a record pun rhs. This must be
        -- filtered out until https://ghc.haskell.org/trac/ghc/ticket/12224 is
        -- resolved, particularly for pretty printing where annotations are added.
        let pun_RDR = "pun-right-hand-side"
        when (showGhc n /= pun_RDR) $
#if __GLASGOW_HASKELL__ <= 710
          -- markAST l n
          unsetContext Intercalate $ markAST l n
#else
          unsetContext Intercalate $ markAST l (GHC.unLoc n)
          -- markLocated n
#endif
      markPat _ (GHC.LazyPat p) = do
        mark GHC.AnnTilde
        markLocated p

      markPat _ (GHC.AsPat ln p) = do
        markLocated ln
        mark GHC.AnnAt
        markLocated p

      markPat _ (GHC.ParPat p) = do
        mark GHC.AnnOpenP
        markLocated p
        mark GHC.AnnCloseP

      markPat _ (GHC.BangPat p) = do
        mark GHC.AnnBang
        markLocated p

      markPat _ (GHC.ListPat ps _ _) = do
        mark GHC.AnnOpenS
        -- markListIntercalate ps
        markListIntercalateWithFunLevel markLocated 2 ps
        mark GHC.AnnCloseS

      markPat _ (GHC.TuplePat pats b _) = do
        if b == GHC.Boxed then mark GHC.AnnOpenP
                          else markWithString GHC.AnnOpen "(#"
        -- markListIntercalate pats
        markListIntercalateWithFunLevel markLocated 2 pats
        if b == GHC.Boxed then mark GHC.AnnCloseP
                          else markWithString GHC.AnnClose "#)"

      markPat _ (GHC.PArrPat ps _) = do
        markWithString GHC.AnnOpen "[:"
        mapM_ markLocated ps
        markWithString GHC.AnnClose ":]"

      markPat _ (GHC.ConPatIn n dets) = do
        -- markHsConPatDetails n dets
        markHsConPatDetails n dets

      markPat _ (GHC.ConPatOut {}) =
        traceM "warning: ConPatOut Introduced after renaming"

      -- ViewPat (LHsExpr id) (LPat id) (PostTc id Type)
      markPat _ (GHC.ViewPat e pat _) = do
        markLocated e
        mark GHC.AnnRarrow
        markLocated pat

      -- SplicePat (HsSplice id)
      markPat l (GHC.SplicePat s) = do
        mark GHC.AnnOpenPE
        markAST l s
        mark GHC.AnnCloseP

      -- LitPat HsLit
      markPat l (GHC.LitPat lp) = markExternal l GHC.AnnVal (hsLit2String lp)

      -- NPat (HsOverLit id) (Maybe (SyntaxExpr id)) (SyntaxExpr id)
#if __GLASGOW_HASKELL__ <= 710
      markPat _ (GHC.NPat ol _ _) = do
#else
      markPat _ (GHC.NPat ol _ _ _) = do
#endif
        markOptional GHC.AnnMinus
        markLocated ol

      -- NPlusKPat (Located id) (HsOverLit id) (SyntaxExpr id) (SyntaxExpr id)
#if __GLASGOW_HASKELL__ <= 710
      markPat _ (GHC.NPlusKPat ln ol _ _) = do
#else
      markPat _ (GHC.NPlusKPat ln ol _ _ _ _) = do
#endif
        markLocated ln
        markWithString GHC.AnnVal "+"  -- "+"
        markLocated ol


#if __GLASGOW_HASKELL__ <= 710
      markPat _ (GHC.SigPatIn pat (GHC.HsWB ty _ _ _)) = do
        markLocated pat
        mark GHC.AnnDcolon
        markLocated ty
#else
      markPat _ (GHC.SigPatIn pat ty) = do
        markLocated pat
        mark GHC.AnnDcolon
        markLHsSigWcType ty
#endif

      markPat _ (GHC.SigPatOut {}) =
        traceM "warning: SigPatOut introduced after renaming"

      -- CoPat HsAnnotated (Pat id) Type
      markPat _ (GHC.CoPat {}) =
        traceM "warning: CoPat introduced after renaming"

#if __GLASGOW_HASKELL__ <= 710
      markPat l (GHC.QuasiQuotePat p) = markAST l p
#endif

-- ---------------------------------------------------------------------
hsLit2String :: GHC.HsLit -> GHC.SourceText
hsLit2String lit =
  case lit of
    GHC.HsChar       src _   -> src
    -- It should be included here
    -- https://github.com/ghc/ghc/blob/master/compiler/parser/Lexer.x#L1471
    GHC.HsCharPrim   src _   -> src ++ "#"
    GHC.HsString     src _   -> src
    GHC.HsStringPrim src _   -> src
    GHC.HsInt        src _   -> src
    GHC.HsIntPrim    src _   -> src
    GHC.HsWordPrim   src _   -> src
    GHC.HsInt64Prim  src _   -> src
    GHC.HsWord64Prim src _   -> src
    GHC.HsInteger    src _ _ -> src
    GHC.HsRat        (GHC.FL src _) _ -> src
    GHC.HsFloatPrim  (GHC.FL src _)   -> src ++ "#"
    GHC.HsDoublePrim (GHC.FL src _)   -> src ++ "##"

markHsConPatDetails :: (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
                      => GHC.Located name -> GHC.HsConPatDetails name -> Annotated ()
markHsConPatDetails ln dets = do
  case dets of
    GHC.PrefixCon args -> do
      markLocated ln
      mapM_ markLocated args
    GHC.RecCon (GHC.HsRecFields fs dd) -> do
      markLocated ln
      mark GHC.AnnOpenC -- '{'
      -- markListIntercalate fs
      markListIntercalateWithFunLevel markLocated 2 fs
      when (isJust dd) $ mark GHC.AnnDotdot
      mark GHC.AnnCloseC -- '}'
    GHC.InfixCon a1 a2 -> do
      markLocated a1
      markLocated ln
      markLocated a2

markHsConDeclDetails :: (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
                    => Bool -> Bool -> [GHC.Located name] -> GHC.HsConDeclDetails name -> Annotated ()

markHsConDeclDetails isDeprecated inGadt lns dets = do
  case dets of
    GHC.PrefixCon args -> mapM_ markLocated args
    GHC.RecCon fs -> do
      mark GHC.AnnOpenC
      if inGadt
        then do
          if isDeprecated
            then setContext (Set.fromList [InGadt]) $ markLocated fs
            else setContext (Set.fromList [InGadt,InRecCon]) $ markLocated fs
          -- markOptional GHC.AnnCloseC
        else do
          if isDeprecated
            then markLocated fs
            else setContext (Set.fromList [InRecCon]) $ markLocated fs
    GHC.InfixCon a1 a2 -> do
      markLocated a1
      mapM_ markLocated lns
      markLocated a2

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
   => Annotate [GHC.LConDeclField name] where
  markAST _ fs = do
       markOptional GHC.AnnOpenC -- '{'
       markListIntercalate fs
       markOptional GHC.AnnDotdot
       inContext (Set.singleton InRecCon) $ mark GHC.AnnCloseC -- '}'
       inContext (Set.singleton InGadt) $ do
         mark GHC.AnnRarrow

-- ---------------------------------------------------------------------

instance (GHC.DataId name) => Annotate (GHC.HsOverLit name) where
  markAST l ol =
    let str = case GHC.ol_val ol of
                GHC.HsIntegral src _ -> src
                GHC.HsFractional l2   -> (GHC.fl_text l2)
                GHC.HsIsString src _ -> src
    in
    markExternal l GHC.AnnVal str

-- ---------------------------------------------------------------------

#if __GLASGOW_HASKELL__ <= 710
instance (GHC.DataId name,Annotate arg)
    => Annotate (GHC.HsWithBndrs name (GHC.Located arg)) where
  markAST _ (GHC.HsWB thing _ _ _) = do
    markLocated thing
#else
instance (GHC.DataId name,Annotate arg)
    => Annotate (GHC.HsImplicitBndrs name (GHC.Located arg)) where
  markAST _ (GHC.HsIB _ thing) = do
    markLocated thing
#endif

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,Annotate name
         ,GHC.HasOccName name,Annotate body)
  => Annotate (GHC.Stmt name (GHC.Located body)) where

#if __GLASGOW_HASKELL__ <= 710
  markAST _ (GHC.LastStmt body _) = markLocated body
#else
  markAST _ (GHC.LastStmt body _ _) = markLocated body
#endif

#if __GLASGOW_HASKELL__ <= 710
  markAST _ (GHC.BindStmt pat body _ _) = do
#else
  markAST _ (GHC.BindStmt pat body _ _ _) = do
#endif
    -- unsetContext Intercalate $ markLocated pat
    unsetContext Intercalate $ setContext (Set.singleton PrefixOp) $ markLocated pat
    mark GHC.AnnLarrow
    -- markLocated body
    unsetContext Intercalate $ markLocated body

    ifInContext (Set.singleton Intercalate)
      (mark GHC.AnnComma)
      (inContext (Set.singleton AddVbar)     $ mark GHC.AnnVbar)
    markTrailingSemi

#if __GLASGOW_HASKELL__ > 710
  markAST _ GHC.ApplicativeStmt{}
    = error "ApplicativeStmt should not appear in ParsedSource"
#endif

  markAST _ (GHC.BodyStmt body _ _ _) = do
    -- markLocated body
    unsetContext Intercalate $ markLocated body
    inContext (Set.singleton AddVbar)     $ mark GHC.AnnVbar
    inContext (Set.singleton Intercalate) $ mark GHC.AnnComma
    markTrailingSemi

#if __GLASGOW_HASKELL__ <= 710
  markAST _ (GHC.LetStmt lb) = do
#else
  markAST _ (GHC.LetStmt (GHC.L l lb)) = do
#endif
    -- return () `debug` ("markP.LetStmt entered")
    mark GHC.AnnLet
    markOptional GHC.AnnOpenC -- '{'
    markInside GHC.AnnSemi
    markLocalBindsWithLayout lb
    markOptional GHC.AnnCloseC -- '}'
    -- return () `debug` ("markP.LetStmt done")
    ifInContext (Set.singleton Intercalate)
      (mark GHC.AnnComma)
      (inContext (Set.singleton AddVbar)     $ mark GHC.AnnVbar)
    markTrailingSemi

#if __GLASGOW_HASKELL__ <= 710
  markAST l (GHC.ParStmt pbs _ _) = do
#else
  markAST l (GHC.ParStmt pbs _ _ _) = do
#endif
    -- Within a given parallel list comprehension,one of the sections to be done
    -- in parallel. It is a normal list comprehension, so has a list of
    -- ParStmtBlock, one for each part of the sub- list comprehension

    -- mapM_ (markAST l) pbs

    ifInContext (Set.singleton Intercalate)
      (

      unsetContext Intercalate $
        markListWithContextsFunction
          (LC (Set.singleton Intercalate)  -- only
              (Set.empty) -- first
              (Set.empty) -- middle
              (Set.singleton Intercalate) -- last
          ) (markAST l) pbs
         )
      (
      unsetContext Intercalate $
        markListWithContextsFunction
          (LC (Set.empty) -- only
              (Set.fromList [AddVbar]) -- first
              (Set.fromList [AddVbar]) -- middle
              (Set.empty)              -- last
          ) (markAST l) pbs
       )
    markTrailingSemi

#if __GLASGOW_HASKELL__ <= 710
  markAST _ (GHC.TransStmt form stmts _b using by _ _ _) = do
#else
  markAST _ (GHC.TransStmt form stmts _b using by _ _ _ _) = do
#endif
    -- mapM_ markLocated stmts
    setContext (Set.singleton Intercalate) $ mapM_ markLocated stmts
    -- markListIntercalate stmts
    case form of
      GHC.ThenForm -> do
        mark GHC.AnnThen
        unsetContext Intercalate $ markLocated using
        case by of
          Just b -> do
            mark GHC.AnnBy
            unsetContext Intercalate $ markLocated b
          Nothing -> return ()
      GHC.GroupForm -> do
        mark GHC.AnnThen
        mark GHC.AnnGroup
        case by of
          Just b -> mark GHC.AnnBy >> markLocated b
          Nothing -> return ()
        mark GHC.AnnUsing
        markLocated using
    -- inContext (Set.fromList [ListComp]) $ mark GHC.AnnVbar -- possible in list comprehension
    -- inContext (Set.fromList [Intercalate]) $ mark GHC.AnnComma
    inContext (Set.singleton AddVbar)     $ mark GHC.AnnVbar
    inContext (Set.singleton Intercalate) $ mark GHC.AnnComma
    -- inContext (Set.fromList [ListComp]) $ mark GHC.AnnComma
    -- mark GHC.AnnComma
    markTrailingSemi

#if __GLASGOW_HASKELL__ <= 710
  markAST _ (GHC.RecStmt stmts _ _ _ _ _ _ _ _) = do
#else
  markAST _ (GHC.RecStmt stmts _ _ _ _ _ _ _ _ _) = do
#endif
    mark GHC.AnnRec
    markOptional GHC.AnnOpenC
    markInside GHC.AnnSemi
    mapM_ markLocated stmts
    markOptional GHC.AnnCloseC
    -- inContext (Set.fromList [ListComp]) $ mark GHC.AnnVbar -- possible in list comprehension
    inContext (Set.singleton AddVbar)     $ mark GHC.AnnVbar
    inContext (Set.singleton Intercalate) $ mark GHC.AnnComma
    markTrailingSemi

-- ---------------------------------------------------------------------

-- Note: We never have a located ParStmtBlock, so have nothing to hang the
-- annotation on. This means there is no pushing of context from the parent ParStmt.
instance  (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
  =>  Annotate (GHC.ParStmtBlock name name) where
  markAST _ (GHC.ParStmtBlock stmts _ns _) = do
    markListIntercalate stmts

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
  => Annotate (GHC.HsLocalBinds name) where
  markAST _ lb = markHsLocalBinds lb

-- ---------------------------------------------------------------------

markHsLocalBinds :: (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
                     => (GHC.HsLocalBinds name) -> Annotated ()
markHsLocalBinds (GHC.HsValBinds (GHC.ValBindsIn binds sigs)) =
    applyListAnnotationsLayout
       (prepareListAnnotation (GHC.bagToList binds)
     ++ prepareListAnnotation sigs
       )
markHsLocalBinds (GHC.HsValBinds (GHC.ValBindsOut {}))
   = traceM "warning: ValBindsOut introduced after renaming"

-- markHsLocalBinds (GHC.HsIPBinds (GHC.IPBinds binds _)) = mapM_ markLocated (reverse binds)
markHsLocalBinds (GHC.HsIPBinds (GHC.IPBinds binds _)) = markListWithLayout (reverse binds)
markHsLocalBinds (GHC.EmptyLocalBinds)                 = return ()

-- ---------------------------------------------------------------------

markMatchGroup :: (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name,
                                               Annotate body)
                   => GHC.SrcSpan -> GHC.MatchGroup name (GHC.Located body)
                   -> Annotated ()
#if __GLASGOW_HASKELL__ <= 710
markMatchGroup _ (GHC.MG matches _ _ _)
#else
markMatchGroup _ (GHC.MG (GHC.L _ matches) _ _ _)
#endif
  = setContextLevel (Set.singleton AdvanceLine) 2 $ markListWithLayout matches

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name,
                                               Annotate body)
  => Annotate [GHC.Located (GHC.Match name (GHC.Located body))] where
  markAST _ ls = mapM_ markLocated ls

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
  => Annotate (GHC.HsExpr name) where
  markAST loc expr = do
    markExpr loc expr
    inContext (Set.singleton AddVbar) $ mark GHC.AnnVbar
    -- TODO: If the AnnComma is not needed, revert to markAST
    inContext (Set.fromList [Intercalate]) $ mark GHC.AnnComma
   where
#if __GLASGOW_HASKELL__ <= 710
      markExpr l (GHC.HsVar n)           = unsetContext Intercalate $ markAST l n
#else
      markExpr l (GHC.HsVar n)           = unsetContext Intercalate $ markAST l (GHC.unLoc n)
#endif

#if __GLASGOW_HASKELL__ <= 710
#else
      markExpr l (GHC.HsRecFld f) = markAST l f

      markExpr l (GHC.HsOverLabel fs)
        = markExternal l GHC.AnnVal ("#" ++ GHC.unpackFS fs)
#endif

      markExpr l (GHC.HsIPVar (GHC.HsIPName v))         =
        markExternal l GHC.AnnVal ("?" ++ GHC.unpackFS v)
      markExpr l (GHC.HsOverLit ov)     = markAST l ov
      markExpr l (GHC.HsLit lit)        = markAST l lit

#if __GLASGOW_HASKELL__ <= 710
      markExpr _ (GHC.HsLam (GHC.MG [match] _ _ _)) = do
#else
      markExpr _ (GHC.HsLam (GHC.MG (GHC.L _ [match]) _ _ _)) = do
#endif
        setContext (Set.singleton LambdaExpr) $ do
        -- TODO: Change this, HsLam binds do not need obey layout rules.
        --       And will only ever have a single match
          markLocated match
      markExpr _ (GHC.HsLam _) = error $ "HsLam with other than one match"

      markExpr l (GHC.HsLamCase _ match) = do
        mark GHC.AnnLam
        mark GHC.AnnCase
        markOptional GHC.AnnOpenC
        setContext (Set.singleton CaseAlt) $ do
          markMatchGroup l match
        markOptional GHC.AnnCloseC

      markExpr _ (GHC.HsApp e1 e2) = do
        -- markLocated e1
        setContext (Set.singleton PrefixOp) $ markLocated e1
        -- markLocated e2
        setContext (Set.singleton PrefixOp) $ markLocated e2

      markExpr _ (GHC.OpApp e1 e2 _ e3) = do
        let
          isInfix = case e2 of
            -- TODO: generalise this. Is it a fixity thing?
#if __GLASGOW_HASKELL__ <= 710
            GHC.L _ (GHC.HsVar n) -> (GHC.occNameString $ GHC.occName n) == "."
#else
            GHC.L _ (GHC.HsVar (GHC.L _ n)) -> (GHC.occNameString $ GHC.occName n) == "."
#endif
            _                     -> False

          normal =
            -- When it is the leftmost item in a GRHS, e1 needs to have PrefixOp context
            ifInContext (Set.singleton LeftMost)
              (setContextLevel (Set.fromList [LeftMost,PrefixOp]) 2 $ markLocated e1)
              (markLocated e1)

        if isInfix
            then setContextLevel (Set.singleton PrefixOp) 2 $ markLocated e1
            else normal
{-
        case e2 of
          GHC.L _ (GHC.HsVar n) ->
            -- TODO: generalise this. Is it a fixity thing?
            if (GHC.occNameString $ GHC.occName n) == "."
            -- if False
              then setContextLevel (Set.singleton PrefixOp) 2 $ markLocated e1
              else normal
          _ -> normal
-}

        setContext (Set.singleton InOp) $ markLocated e2

        if isInfix
          then setContextLevel (Set.singleton PrefixOp) 2 $ markLocated e3
          else markLocated e3
{-
        case e2 of
          GHC.L _ (GHC.HsVar n) ->
            if (GHC.occNameString $ GHC.occName n) == "."
            -- if False
              then setContextLevel (Set.singleton PrefixOp) 2 $ markLocated e3
              else markLocated e3
          _ -> markLocated e3
-}

      markExpr _ (GHC.NegApp e _) = do
        mark GHC.AnnMinus
        markLocated e

      markExpr _ (GHC.HsPar e) = do
        mark GHC.AnnOpenP -- '('
        markLocated e
        mark GHC.AnnCloseP -- ')'

      markExpr _ (GHC.SectionL e1 e2) = do
        markLocated e1
        setContext (Set.singleton InOp) $ markLocated e2

      markExpr _ (GHC.SectionR e1 e2) = do
        setContext (Set.singleton InOp) $ markLocated e1
        markLocated e2

      markExpr _ (GHC.ExplicitTuple args b) = do
        if b == GHC.Boxed then mark GHC.AnnOpenP
                          else markWithString GHC.AnnOpen "(#"

        -- markListIntercalate args
        markListIntercalateWithFunLevel markLocated 2 args

        if b == GHC.Boxed then mark GHC.AnnCloseP
                          else markWithString GHC.AnnClose "#)"


      markExpr l (GHC.HsCase e1 matches) = setRigidFlag $ do
        mark GHC.AnnCase
        markLocated e1
        mark GHC.AnnOf
        markOptional GHC.AnnOpenC
        markInside GHC.AnnSemi
        setContext (Set.singleton CaseAlt) $ markMatchGroup l matches
        markOptional GHC.AnnCloseC

      -- We set the layout for HsIf even though it need not obey layout rules as
      -- when moving these expressions it's useful that they maintain "internal
      -- integrity", that is to say the subparts remain indented relative to each
      -- other.
      markExpr _ (GHC.HsIf _ e1 e2 e3) = setRigidFlag $ do
        mark GHC.AnnIf
        markLocated e1
        markOffsetOptional GHC.AnnSemi 0
        mark GHC.AnnThen
        markLocated e2
        markOffsetOptional GHC.AnnSemi 1
        mark GHC.AnnElse
        markLocated e3

      markExpr _ (GHC.HsMultiIf _ rhs) = do
        mark GHC.AnnIf
        markOptional GHC.AnnOpenC
        setContext (Set.singleton CaseAlt) $ do
          mapM_ markLocated rhs
        markOptional GHC.AnnCloseC

#if __GLASGOW_HASKELL__ <= 710
      markExpr _ (GHC.HsLet binds e) = do
#else
      markExpr _ (GHC.HsLet (GHC.L _ binds) e) = do
#endif
        setLayoutFlag (do -- Make sure the 'in' gets indented too
          mark GHC.AnnLet
          markOptional GHC.AnnOpenC
          markInside GHC.AnnSemi
          markLocalBindsWithLayout binds
          markOptional GHC.AnnCloseC
          mark GHC.AnnIn
          markLocated e)

      -- -------------------------------

#if __GLASGOW_HASKELL__ <= 710
      markExpr _ (GHC.HsDo cts es _) = do
#else
      markExpr _ (GHC.HsDo cts (GHC.L _ es) _) = do
#endif
        case cts of
          GHC.DoExpr  -> mark GHC.AnnDo
          GHC.MDoExpr -> mark GHC.AnnMdo
          _           -> return ()
        let (ostr,cstr) =
              if isListComp cts
                then case cts of
                       GHC.PArrComp -> ("[:",":]")
                       _            -> ("[",  "]")
                else ("{","}")

        -- markWithString GHC.AnnOpen ostr
        -- markPPOptional GHC.AnnOpen (Just ostr)
        when (isListComp cts) $ markWithString GHC.AnnOpen ostr
        markOptional GHC.AnnOpenS
        markOptional GHC.AnnOpenC
        markInside GHC.AnnSemi
        if isListComp cts
          then do
            markLocated (last es)
            mark GHC.AnnVbar
            setLayoutFlag (markListIntercalate (init es))
          else do
           markListWithLayout es
        markOptional GHC.AnnCloseS
        markOptional GHC.AnnCloseC
        -- markWithString GHC.AnnClose cstr
        -- markPPOptional GHC.AnnClose (Just cstr)
        when (isListComp cts) $ markWithString GHC.AnnClose cstr

      -- -------------------------------

      markExpr _ (GHC.ExplicitList _ _ es) = do
        mark GHC.AnnOpenS
        -- markListIntercalate es
        markListIntercalateWithFunLevel markLocated 2 es
        mark GHC.AnnCloseS

      markExpr _ (GHC.ExplicitPArr _ es)   = do
        markWithString GHC.AnnOpen "[:"
        mapM_ markLocated es
        markWithString GHC.AnnClose ":]"

#if __GLASGOW_HASKELL__ <= 710
      markExpr _ (GHC.RecordCon n _ (GHC.HsRecFields fs dd)) = do
#else
      markExpr _ (GHC.RecordCon n _ _ (GHC.HsRecFields fs dd)) = do
#endif
        markLocated n
        mark GHC.AnnOpenC
        case dd of
          Nothing -> markListIntercalate fs
          Just _ -> do
            setContext (Set.singleton Intercalate) $ mapM_ markLocated fs
            mark GHC.AnnDotdot
        mark GHC.AnnCloseC

#if __GLASGOW_HASKELL__ <= 710
      markExpr _ (GHC.RecordUpd e (GHC.HsRecFields fs _) _cons _ _) = do
#else
      markExpr _ (GHC.RecordUpd e fs _cons _ _ _) = do
#endif
        markLocated e
        mark GHC.AnnOpenC
        markListIntercalate fs
        markOptional GHC.AnnDotdot
        mark GHC.AnnCloseC

#if __GLASGOW_HASKELL__ <= 710
      markExpr _ (GHC.ExprWithTySig e typ _) = do
#else
      markExpr _ (GHC.ExprWithTySig e typ) = do
#endif
        markLocated e
        mark GHC.AnnDcolon
#if __GLASGOW_HASKELL__ <= 710
        markLocated typ
#else
        markLHsSigWcType typ
#endif

      markExpr _ (GHC.ExprWithTySigOut e typ) = do
        markLocated e
        mark GHC.AnnDcolon
#if __GLASGOW_HASKELL__ <= 710
        markLocated typ
#else
        markLHsSigWcType typ
#endif

      markExpr _ (GHC.ArithSeq _ _ seqInfo) = do
        mark GHC.AnnOpenS -- '['
        case seqInfo of
            GHC.From e -> do
              markLocated e
              mark GHC.AnnDotdot
            GHC.FromTo e1 e2 -> do
              markLocated e1
              mark GHC.AnnDotdot
              markLocated e2
            GHC.FromThen e1 e2 -> do
              markLocated e1
              mark GHC.AnnComma
              markLocated e2
              mark GHC.AnnDotdot
            GHC.FromThenTo e1 e2 e3 -> do
              markLocated e1
              mark GHC.AnnComma
              markLocated e2
              mark GHC.AnnDotdot
              markLocated e3
        mark GHC.AnnCloseS -- ']'

      markExpr _ (GHC.PArrSeq _ seqInfo) = do
        markWithString GHC.AnnOpen "[:" -- '[:'
        case seqInfo of
            GHC.From e -> do
              markLocated e
              mark GHC.AnnDotdot
            GHC.FromTo e1 e2 -> do
              markLocated e1
              mark GHC.AnnDotdot
              markLocated e2
            GHC.FromThen e1 e2 -> do
              markLocated e1
              mark GHC.AnnComma
              markLocated e2
              mark GHC.AnnDotdot
            GHC.FromThenTo e1 e2 e3 -> do
              markLocated e1
              mark GHC.AnnComma
              markLocated e2
              mark GHC.AnnDotdot
              markLocated e3
        markWithString GHC.AnnClose ":]" -- ':]'

      markExpr _ (GHC.HsSCC src csFStr e) = do
        markWithString GHC.AnnOpen src -- "{-# SCC"
#if __GLASGOW_HASKELL__ <= 710
        markWithStringOptional GHC.AnnVal (GHC.unpackFS csFStr)
        markWithString GHC.AnnValStr ("\"" ++ GHC.unpackFS csFStr ++ "\"")
#else
        markWithStringOptional GHC.AnnVal (GHC.sl_st csFStr)
        markWithString GHC.AnnValStr (GHC.sl_st csFStr)
#endif
        markWithString GHC.AnnClose "#-}"
        markLocated e

      markExpr _ (GHC.HsCoreAnn src csFStr e) = do
        markWithString GHC.AnnOpen src -- "{-# CORE"
#if __GLASGOW_HASKELL__ <= 710
        markWithString GHC.AnnVal ("\"" ++ GHC.unpackFS csFStr ++ "\"")
#else
        markWithString GHC.AnnVal (GHC.sl_st csFStr)
#endif
        markWithString GHC.AnnClose "#-}"
        markLocated e
      -- TODO: make monomorphic
      markExpr l (GHC.HsBracket (GHC.VarBr True v)) = do
        mark GHC.AnnSimpleQuote
        markLocatedFromKw GHC.AnnName (GHC.L l v)
      markExpr l (GHC.HsBracket (GHC.VarBr False v)) = do
        mark GHC.AnnThTyQuote
        markLocatedFromKw GHC.AnnName (GHC.L l v)
      markExpr _ (GHC.HsBracket (GHC.DecBrL ds)) = do
        markWithString GHC.AnnOpen "[d|"
        markOptional GHC.AnnOpenC
        mapM_ markLocated ds
        markOptional GHC.AnnCloseC
        markWithString GHC.AnnClose "|]"
      -- Introduced after the renamer
      markExpr _ (GHC.HsBracket (GHC.DecBrG _)) =
        traceM "warning: DecBrG introduced after renamer"
      markExpr l (GHC.HsBracket (GHC.ExpBr e)) = do
#if __GLASGOW_HASKELL__ <= 710
        -- This exists like this as the lexer collapses [e| and [| into the
        -- same construtor
        workOutString l GHC.AnnOpen
          (\ss -> if spanLength ss == 2
                    then "[|"
                    else "[e|")
#else
        markWithString GHC.AnnOpen "[|"
        markOptional GHC.AnnOpenE  -- "[e|"
#endif
        markLocated e
        markWithString GHC.AnnClose "|]"
      markExpr l (GHC.HsBracket (GHC.TExpBr e)) = do
#if __GLASGOW_HASKELL__ <= 710
        -- This exists like this as the lexer collapses [e|| and [|| into the
        -- same construtor
        workOutString l GHC.AnnOpen
          (\ss -> if spanLength ss == 3
                    then "[||"
                    else "[e||")
#else
        markWithString GHC.AnnOpen  "[||"
        markWithString GHC.AnnOpenE "[e||"
#endif
        markLocated e
        markWithString GHC.AnnClose "||]"
      markExpr _ (GHC.HsBracket (GHC.TypBr e)) = do
        markWithString GHC.AnnOpen "[t|"
        markLocated e
        markWithString GHC.AnnClose "|]"
      markExpr _ (GHC.HsBracket (GHC.PatBr e)) = do
        markWithString GHC.AnnOpen  "[p|"
        markLocated e
        markWithString GHC.AnnClose "|]"

      markExpr _ (GHC.HsRnBracketOut _ _) =
        traceM "warning: HsRnBracketOut introduced after renamer"
      markExpr _ (GHC.HsTcBracketOut _ _) =
        traceM "warning: HsTcBracketOut introduced after renamer"

#if __GLASGOW_HASKELL__ > 710
      markExpr l (GHC.HsSpliceE e) = do
        -- mark GHC.AnnOpenPE
        markAST l e
        -- mark GHC.AnnCloseP
#else
      markExpr l (GHC.HsSpliceE isTyped e) = do
        case e of
          GHC.HsSplice _n b@(GHC.L _ (GHC.HsVar n))  -> do
            mark GHC.AnnOpenPE
            if isTyped
              then markWithString GHC.AnnThIdTySplice ("$$" ++ (GHC.occNameString (GHC.occName n)))
              else markWithString GHC.AnnThIdSplice   ("$" ++ (GHC.occNameString (GHC.occName n)))
            markLocated b
            mark GHC.AnnCloseP
          GHC.HsSplice _n b -> do
            if isTyped
              then do
                mark GHC.AnnThIdSplice
                mark GHC.AnnOpenPTE
              else mark GHC.AnnOpenPE
            markLocated b
            mark GHC.AnnCloseP

      markExpr l (GHC.HsQuasiQuoteE e) = do
        markAST l e
#endif

      markExpr _ (GHC.HsProc p c) = do
        mark GHC.AnnProc
        markLocated p
        mark GHC.AnnRarrow
        markLocated c

      markExpr _ (GHC.HsStatic e) = do
        mark GHC.AnnStatic
        markLocated e

      markExpr _ (GHC.HsArrApp e1 e2 _ o isRightToLeft) = do
            -- isRightToLeft True  => right-to-left (f -< arg)
            --               False => left-to-right (arg >- f)
        if isRightToLeft
          then do
            markLocated e1
            case o of
              GHC.HsFirstOrderApp  -> mark GHC.Annlarrowtail
              GHC.HsHigherOrderApp -> mark GHC.AnnLarrowtail
            -- mark GHC.Annlarrowtail
            -- mark GHC.AnnLarrowtail
          else do
            markLocated e2
            case o of
              GHC.HsFirstOrderApp  -> mark GHC.Annrarrowtail
              GHC.HsHigherOrderApp -> mark GHC.AnnRarrowtail
            -- mark GHC.Annrarrowtail
            -- mark GHC.AnnRarrowtail

        if isRightToLeft
          then markLocated e2
          else markLocated e1

      markExpr _ (GHC.HsArrForm e _ cs) = do
        markWithString GHC.AnnOpen "(|"
        markLocated e
        mapM_ markLocated cs
        markWithString GHC.AnnClose "|)"

      markExpr _ (GHC.HsTick _ _) = return ()
      markExpr _ (GHC.HsBinTick _ _ _) = return ()

#if __GLASGOW_HASKELL__ <= 710
      markExpr _ (GHC.HsTickPragma src (str,(v1,v2),(v3,v4)) e) = do
        -- '{-# GENERATED' STRING INTEGER ':' INTEGER '-' INTEGER ':' INTEGER '#-}'
        markWithString       GHC.AnnOpen  src
        markOffsetWithString GHC.AnnVal 0 (show (GHC.unpackFS str)) -- STRING
        markOffsetWithString GHC.AnnVal 1 (show v1) -- INTEGER
        markOffset GHC.AnnColon 0 -- ':'
        markOffsetWithString GHC.AnnVal 2 (show v2) -- INTEGER
        mark   GHC.AnnMinus   -- '-'
        markOffsetWithString GHC.AnnVal 3 (show v3) -- INTEGER
        markOffset GHC.AnnColon 1 -- ':'
        markOffsetWithString GHC.AnnVal 4 (show v4) -- INTEGER
        markWithString   GHC.AnnClose  "#-}"
        markLocated e
#else
      markExpr _ (GHC.HsTickPragma src (str,_,_) ((v1,v2),(v3,v4)) e) = do
        -- '{-# GENERATED' STRING INTEGER ':' INTEGER '-' INTEGER ':' INTEGER '#-}'
        markWithString       GHC.AnnOpen  src
        markOffsetWithString GHC.AnnVal 0 (GHC.sl_st str) -- STRING
        markOffsetWithString GHC.AnnVal 1 v1 -- INTEGER
        markOffset GHC.AnnColon 0 -- ':'
        markOffsetWithString GHC.AnnVal 2 v2 -- INTEGER
        mark   GHC.AnnMinus   -- '-'
        markOffsetWithString GHC.AnnVal 3 v3 -- INTEGER
        markOffset GHC.AnnColon 1 -- ':'
        markOffsetWithString GHC.AnnVal 4 v4 -- INTEGER
        markWithString   GHC.AnnClose  "#-}"
        markLocated e
#endif

      markExpr l (GHC.EWildPat) = do
        markExternal l GHC.AnnVal "_"

      markExpr _ (GHC.EAsPat ln e) = do
        markLocated ln
        mark GHC.AnnAt
        markLocated e

      markExpr _ (GHC.EViewPat e1 e2) = do
        markLocated e1
        mark GHC.AnnRarrow
        markLocated e2

      markExpr _ (GHC.ELazyPat e) = do
        mark GHC.AnnTilde
        markLocated e

#if __GLASGOW_HASKELL__ <= 710
      markExpr _ (GHC.HsType ty) = markLocated ty
#else
      markExpr _ (GHC.HsAppType e ty) = do
        markLocated e
        mark GHC.AnnAt
        markLHsWcType ty
      markExpr _ (GHC.HsAppTypeOut _ _) =
        traceM "warning: HsAppTypeOut introduced after renaming"
#endif

      markExpr _ (GHC.HsWrap _ _) =
        traceM "warning: HsWrap introduced after renaming"
      markExpr _ (GHC.HsUnboundVar _) =
        traceM "warning: HsUnboundVar introduced after renaming"


-- ---------------------------------------------------------------------
#if __GLASGOW_HASKELL__ > 710
markLHsWcType :: (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
              => GHC.LHsWcType name -> Annotated ()
markLHsWcType (GHC.HsWC _ mwc ty) = do
  case mwc of
    Nothing -> markLocated ty
    Just lwc -> do
     -- let sorted = lexicalSortLocated (GHC.L lwc GHC.HsWildCardTy:[ty])
     -- markLocated (GHC.L lc sorted)
     applyListAnnotations ([(lwc,markExternal lwc GHC.AnnVal "_")]
                        ++ prepareListAnnotation [ty]
                          )
#endif
-- ---------------------------------------------------------------------

instance Annotate GHC.HsLit where
  markAST l lit = markExternal l GHC.AnnVal (hsLit2String lit)

-- ---------------------------------------------------------------------
#if __GLASGOW_HASKELL__ > 710
instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
  => Annotate (GHC.HsRecUpdField name) where
  markAST _ (GHC.HsRecField lbl expr punFlag) = do
    unsetContext Intercalate $ markLocated lbl
    -- when (punFlag == False) $ mark GHC.AnnEqual
    -- markLocated expr
    when (punFlag == False) $ do
      mark GHC.AnnEqual
      unsetContext Intercalate $ markLocated expr
    inContext (Set.fromList [Intercalate]) $ mark GHC.AnnComma
{-
type HsRecUpdField id     = HsRecField' (AmbiguousFieldOcc id) (LHsExpr id)

-- |  - 'ApiAnnotation.AnnKeywordId' : 'ApiAnnotation.AnnEqual',
--
-- For details on above see note [Api annotations] in ApiAnnotation
data HsRecField' id arg = HsRecField {
        hsRecFieldLbl :: Located id,
        hsRecFieldArg :: arg,           -- ^ Filled in by renamer when punning
        hsRecPun      :: Bool           -- ^ Note [Punning]
  } deriving (Data, Typeable)

-}

instance (GHC.DataId name)
  => Annotate (GHC.AmbiguousFieldOcc name) where
  markAST l (GHC.Unambiguous n _) = markLocated n
  markAST l (GHC.Ambiguous   n _) = markLocated n
#endif
-- ---------------------------------------------------------------------

-- |Used for declarations that need to be aligned together, e.g. in a
-- do or let .. in statement/expr
instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
  => Annotate ([GHC.ExprLStmt name]) where
  markAST _ ls = mapM_ markLocated ls

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
  => Annotate (GHC.HsTupArg name) where
  markAST _ (GHC.Present (GHC.L l e)) = do
    markAST l e

  markAST _ (GHC.Missing _) = do
    inContext (Set.fromList [Intercalate]) $ mark GHC.AnnComma

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
  => Annotate (GHC.HsCmdTop name) where
  markAST _ (GHC.HsCmdTop cmd _ _ _) = markLocated cmd

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
   => Annotate (GHC.HsCmd name) where
  markAST _ (GHC.HsCmdArrApp e1 e2 _ o isRightToLeft) = do
        -- isRightToLeft True  => right-to-left (f -< arg)
        --               False => left-to-right (arg >- f)
    if isRightToLeft
      then do
        markLocated e1
        case o of
          GHC.HsFirstOrderApp  -> mark GHC.Annlarrowtail
          GHC.HsHigherOrderApp -> mark GHC.AnnLarrowtail
      else do
        markLocated e2
        case o of
          GHC.HsFirstOrderApp  -> mark GHC.Annrarrowtail
          GHC.HsHigherOrderApp -> mark GHC.AnnRarrowtail
        -- mark GHC.Annrarrowtail
        -- mark GHC.AnnRarrowtail

    if isRightToLeft
      then markLocated e2
      else markLocated e1

  markAST _ (GHC.HsCmdArrForm e _mf cs) = do
    -- TODO: When RdrHsSyn.checkCmd hits an OpApp, it converts it to a HsCmdArrForm.
    --       In this case it will not have AnnOpen/AnnClose
    markWithString GHC.AnnOpen "(|"
    -- This may be an infix operation
    applyListAnnotationsContexts (LC (Set.singleton PrefixOp) (Set.singleton PrefixOp)
                                     (Set.singleton InOp) (Set.singleton InOp))
                       (prepareListAnnotation [e]
                         ++ prepareListAnnotation cs)
    markWithString GHC.AnnClose "|)"

  markAST _ (GHC.HsCmdApp e1 e2) = do
    markLocated e1
    markLocated e2

  markAST l (GHC.HsCmdLam match) = do
    mark GHC.AnnLam
    -- markMatchGroup l match
    setContext (Set.singleton LambdaExpr) $ do markMatchGroup l match

  markAST _ (GHC.HsCmdPar e) = do
    mark GHC.AnnOpenP
    markLocated e
    mark GHC.AnnCloseP -- ')'

  markAST l (GHC.HsCmdCase e1 matches) = do
    mark GHC.AnnCase
    markLocated e1
    mark GHC.AnnOf
    markOptional GHC.AnnOpenC
    setContext (Set.singleton CaseAlt) $ do
      markMatchGroup l matches
    markOptional GHC.AnnCloseC

  markAST _ (GHC.HsCmdIf _ e1 e2 e3) = do
    mark GHC.AnnIf
    markLocated e1
    markOffset GHC.AnnSemi 0
    mark GHC.AnnThen
    markLocated e2
    markOffset GHC.AnnSemi 1
    mark GHC.AnnElse
    markLocated e3

#if __GLASGOW_HASKELL__ <= 710
  markAST _ (GHC.HsCmdLet binds e) = do
#else
  markAST _ (GHC.HsCmdLet (GHC.L _ binds) e) = do
#endif
    mark GHC.AnnLet
    markOptional GHC.AnnOpenC
    markLocalBindsWithLayout binds
    markOptional GHC.AnnCloseC
    mark GHC.AnnIn
    markLocated e

#if __GLASGOW_HASKELL__ <= 710
  markAST _ (GHC.HsCmdDo es _) = do
#else
  markAST _ (GHC.HsCmdDo (GHC.L _ es) _) = do
#endif
    mark GHC.AnnDo
    markOptional GHC.AnnOpenC
    markListWithLayout es
    markOptional GHC.AnnCloseC

#if __GLASGOW_HASKELL__ <= 710
  markAST _ (GHC.HsCmdCast {}) =
    traceM "warning: HsCmdCast introduced after renaming"
#endif

#if __GLASGOW_HASKELL__ > 710
  markAST _ (GHC.HsCmdWrap {}) =
    traceM "warning: HsCmdWrap introduced after renaming"
{-
  | HsCmdWrap   HsWrapper
                (HsCmd id)     -- If   cmd :: arg1 --> res
                               --      wrap :: arg1 "->" arg2
                               -- Then (HsCmdWrap wrap cmd) :: arg2 --> res
-}
#endif

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
  => Annotate [GHC.Located (GHC.StmtLR name name (GHC.LHsCmd name))] where
  markAST _ ls = mapM_ markLocated ls

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
     => Annotate (GHC.TyClDecl name) where

  markAST l (GHC.FamDecl famdecl) = markAST l famdecl

#if __GLASGOW_HASKELL__ <= 710
  markAST _ (GHC.SynDecl ln (GHC.HsQTvs _ tyvars) typ _) = do
#else
  markAST _ (GHC.SynDecl ln (GHC.HsQTvs _ tyvars _) typ _) = do
#endif
    -- There may be arbitrary parens around parts of the constructor that are
    -- infix.
    -- Turn these into comments so that they feed into the right place automatically
    annotationsToComments [GHC.AnnOpenP,GHC.AnnCloseP]
    mark GHC.AnnType
    -- ln may be used infix, in which case rearrange the order. It may be
    -- simplest to just sort ln:tyvars
    if (null tyvars || GHC.getLoc ln < GHC.getLoc (head tyvars) ) -- prefix
      then
        applyListAnnotations (prepareListAnnotation [ln]
                             ++ prepareListAnnotation tyvars)
      else
        applyListAnnotations (prepareListAnnotationWithContext (Set.singleton InOp) [ln]
                             ++ prepareListAnnotation tyvars)
    -- markMany GHC.AnnCloseP
    mark GHC.AnnEqual
    markLocated typ
    markTrailingSemi

#if __GLASGOW_HASKELL__ <= 710
  markAST _ (GHC.DataDecl ln (GHC.HsQTvs _ns tyVars)
                (GHC.HsDataDefn nd ctx mctyp mk cons mderivs) _) = do
#else
  markAST _ (GHC.DataDecl ln (GHC.HsQTvs _ns tyVars _)
                (GHC.HsDataDefn nd ctx mctyp mk cons mderivs) _ _) = do
#endif
    if nd == GHC.DataType
      then mark GHC.AnnData
      else mark GHC.AnnNewtype
    markMaybe mctyp
    if null (GHC.unLoc ctx)
      then markOptional GHC.AnnDarrow
      else do
        setContext (Set.singleton Parens) $ markLocated ctx
    markTyClass ln tyVars
    case mk of
      Nothing -> return ()
      Just k -> do
        mark GHC.AnnDcolon
        markLocated k
    if (isGadt cons)
      then mark GHC.AnnWhere
      else unless (null cons) $ mark GHC.AnnEqual
    markOptional GHC.AnnOpenC
    -- mapM_ markLocated cons
    -- markListWithLayout cons
    setLayoutFlag $ setContext (Set.singleton NoPrecedingSpace)
                  $ markListWithContexts' listContexts cons
    markOptional GHC.AnnCloseC
    setContext (Set.fromList [Deriving,NoDarrow]) $ markMaybe mderivs
    markTrailingSemi

  -- -----------------------------------

#if __GLASGOW_HASKELL__ <= 710
  markAST _ (GHC.ClassDecl ctx ln (GHC.HsQTvs _ns tyVars) fds
                          sigs meths ats atdefs docs _) = do
#else
  markAST _ (GHC.ClassDecl ctx ln (GHC.HsQTvs _ns tyVars _) fds
                          sigs meths ats atdefs docs _) = do
#endif
    mark GHC.AnnClass
    unless (null $ GHC.unLoc ctx) $ markLocated ctx

    markTyClass ln tyVars

    when (not $ null fds) $ do
      mark GHC.AnnVbar
      markListIntercalate fds
    mark GHC.AnnWhere
    markOptional GHC.AnnOpenC -- '{'
    markInside GHC.AnnSemi
    -- AZ:TODO: we end up with both the tyVars and the following body of the
    -- class defn in annSortKey for the class. This could cause problems when
    -- changing things.
    -- setLayoutFlag $
    setContext (Set.singleton InClassDecl) $
      applyListAnnotationsLayout
                           (prepareListAnnotation sigs
                         ++ prepareListAnnotation (GHC.bagToList meths)
                         ++ prepareListAnnotation ats
                         ++ prepareListAnnotation atdefs
                         ++ prepareListAnnotation docs
                           )
    markOptional GHC.AnnCloseC -- '}'
    markTrailingSemi

-- ---------------------------------------------------------------------

markTyClass :: (Annotate a, Annotate ast)
                => GHC.Located a -> [GHC.Located ast] -> Annotated ()
markTyClass ln tyVars = do
    markManyOptional GHC.AnnOpenP
    applyListAnnotations (prepareListAnnotation [ln]
                      ++ prepareListAnnotation (take 2 tyVars))
    markManyOptional GHC.AnnCloseP
    mapM_ markLocated (drop 2 tyVars)

-- ---------------------------------------------------------------------

instance (GHC.DataId name,Annotate name, GHC.OutputableBndr name,GHC.HasOccName name)
   => Annotate (GHC.FamilyDecl name) where
#if __GLASGOW_HASKELL__ <= 710
  markAST _ (GHC.FamilyDecl info ln (GHC.HsQTvs _ tyvars) mkind) = do
#else
  markAST _ (GHC.FamilyDecl info ln (GHC.HsQTvs _ tyvars _) rsig minj) = do
{-
data FamilyDecl name = FamilyDecl
  { fdInfo           :: FamilyInfo name              -- type/data, closed/open
  , fdLName          :: Located name                 -- type constructor
  , fdTyVars         :: LHsQTyVars name              -- type variables
  , fdResultSig      :: LFamilyResultSig name        -- result signature
  , fdInjectivityAnn :: Maybe (LInjectivityAnn name) -- optional injectivity ann
  }
-}
#endif
    case info of
      GHC.DataFamily -> mark GHC.AnnData
      _              -> mark GHC.AnnType

#if __GLASGOW_HASKELL__ <= 710
    mark GHC.AnnFamily
#else
    ifInContext (Set.singleton InClassDecl) (return ()) (mark GHC.AnnFamily)
#endif

    markOptional GHC.AnnOpenP
    applyListAnnotationsContexts (LC (Set.singleton PrefixOp) (Set.singleton PrefixOp) Set.empty Set.empty)
                (prepareListAnnotation [ln]
              ++ prepareListAnnotation tyvars)
    markOptional GHC.AnnCloseP
#if __GLASGOW_HASKELL__ <= 710
    case mkind of
      Nothing -> return ()
      Just k -> do
        mark GHC.AnnDcolon
        markLocated k
#else
    case GHC.unLoc rsig of
      GHC.NoSig -> return ()
      GHC.KindSig _ -> do
        mark GHC.AnnDcolon
        markLocated rsig
      GHC.TyVarSig _ -> do
        mark GHC.AnnEqual
        markLocated rsig
    case minj of
      Nothing -> return ()
      Just inj -> do
        mark GHC.AnnVbar
        mark GHC.AnnEqual
        markLocated inj
#endif
    case info of
#if __GLASGOW_HASKELL__ > 710
      GHC.ClosedTypeFamily (Just eqns) -> do
        mark GHC.AnnWhere
        mark GHC.AnnOpenC -- {
        -- mapM_ markLocated eqns
        markListWithLayout eqns
        mark GHC.AnnCloseC -- }
      GHC.ClosedTypeFamily Nothing -> do
        mark GHC.AnnWhere
        mark GHC.AnnOpenC -- {
        mark GHC.AnnDotdot
        mark GHC.AnnCloseC -- }
#else
      GHC.ClosedTypeFamily eqns -> do
        mark GHC.AnnWhere
        markOptional GHC.AnnOpenC -- {
        -- mapM_ markLocated eqns
        markListWithLayout eqns
        markOptional GHC.AnnCloseC -- }
#endif
      _ -> return ()
    markTrailingSemi

-- ---------------------------------------------------------------------

#if __GLASGOW_HASKELL__ <= 710
#else
instance (GHC.DataId name,Annotate name,GHC.OutputableBndr name,GHC.HasOccName name)
  => Annotate (GHC.FamilyResultSig name) where
  markAST _ (GHC.NoSig)        = return ()
  markAST _ (GHC.KindSig k)    = markLocated k
  markAST _ (GHC.TyVarSig ltv) = markLocated ltv
#endif

-- ---------------------------------------------------------------------

#if __GLASGOW_HASKELL__ <= 710
#else
instance (GHC.DataId name,Annotate name)
  => Annotate (GHC.InjectivityAnn name) where
  markAST l (GHC.InjectivityAnn ln lns) = do
    mark GHC.AnnVbar
    markLocated ln
    mark GHC.AnnRarrow
    mapM_ markLocated lns
#endif

-- ---------------------------------------------------------------------

instance (GHC.DataId name,Annotate name,GHC.OutputableBndr name,GHC.HasOccName name)
  => Annotate (GHC.TyFamInstEqn name) where
#if __GLASGOW_HASKELL__ <= 710
  markAST _ (GHC.TyFamEqn ln (GHC.HsWB pats _ _ _) typ) = do
#else
  markAST _ (GHC.TyFamEqn ln (GHC.HsIB _ pats) typ) = do
#endif
    markOptional GHC.AnnOpenP
    applyListAnnotations (prepareListAnnotation [ln]
                         ++ prepareListAnnotation pats)
    markOptional GHC.AnnCloseP
    mark GHC.AnnEqual
    markLocated typ

-- ---------------------------------------------------------------------

instance (GHC.DataId name,Annotate name,GHC.OutputableBndr name,GHC.HasOccName name)
  => Annotate (GHC.TyFamDefltEqn name) where
#if __GLASGOW_HASKELL__ <= 710
  markAST _ (GHC.TyFamEqn ln (GHC.HsQTvs _ns bndrs) typ) = do
#else
  markAST _ (GHC.TyFamEqn ln (GHC.HsQTvs _ns bndrs _) typ) = do
#endif
    mark GHC.AnnType
    mark GHC.AnnInstance
    applyListAnnotations (prepareListAnnotation [ln]
                       ++ prepareListAnnotation bndrs
                         )
    mark GHC.AnnEqual
    markLocated typ

-- ---------------------------------------------------------------------

-- TODO: modify lexer etc, in the meantime to not set haddock flag
instance Annotate GHC.DocDecl where
  markAST l v =
    let str =
          case v of
            (GHC.DocCommentNext (GHC.HsDocString fs)) -> (GHC.unpackFS fs)
            (GHC.DocCommentPrev (GHC.HsDocString fs)) -> (GHC.unpackFS fs)
            (GHC.DocCommentNamed _s (GHC.HsDocString fs)) -> (GHC.unpackFS fs)
            (GHC.DocGroup _i (GHC.HsDocString fs)) -> (GHC.unpackFS fs)
    in
      markExternal l (GHC.AnnVal) str

-- ---------------------------------------------------------------------

markDataDefn :: (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
  => GHC.SrcSpan -> GHC.HsDataDefn name -> Annotated ()
markDataDefn _ (GHC.HsDataDefn _ ctx typ mk cons mderivs) = do
  markLocated ctx
  markMaybe typ
#if __GLASGOW_HASKELL__ <= 710
  markMaybe mk
#endif
  if isGadt cons
    then markListWithLayout cons
    else markListIntercalateWithFunLevel markLocated 2 cons
  case mderivs of
    Nothing -> return ()
    Just d -> setContext (Set.singleton Deriving) $ markLocated d

-- ---------------------------------------------------------------------

-- Note: GHC.HsContext name aliases to here too
instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
     => Annotate [GHC.LHsType name] where
  markAST _ ts = do
#if __GLASGOW_HASKELL__ <= 710
    inContext (Set.singleton Deriving) $ mark GHC.AnnDeriving
#endif
    -- Mote: At least for GHC 7.10, a single item in parens is parsed as a
    -- HsSigType, which is always a HsForAllTy. Without parens it is always a
    -- HsVar. So for round trip pretty printing we need to take this into
    -- account.
    case ts of
      []  -> markManyOptional GHC.AnnOpenP
      [GHC.L _ GHC.HsForAllTy{}] -> markMany GHC.AnnOpenP
      [x] -> markManyOptional GHC.AnnOpenP
      _   -> markMany         GHC.AnnOpenP

    unsetContext Intercalate $ markListIntercalateWithFunLevel markLocated 2 ts

    case ts of
      []  -> markManyOptional GHC.AnnCloseP
      [GHC.L _ GHC.HsForAllTy{}] -> markMany GHC.AnnCloseP
      [x] -> markManyOptional GHC.AnnCloseP
      _   -> markMany         GHC.AnnCloseP

    ifInContext (Set.singleton NoDarrow)
      (return ())
      (if null ts
        then markOptional GHC.AnnDarrow
        else mark         GHC.AnnDarrow)

-- ---------------------------------------------------------------------

instance (GHC.DataId name,Annotate name,GHC.OutputableBndr name,GHC.HasOccName name)
      => Annotate (GHC.ConDecl name) where
#if __GLASGOW_HASKELL__ <= 710
  markAST _ (GHC.ConDecl lns _expr (GHC.HsQTvs _ns bndrs) ctx
                         dets res _ depc_syntax) = do
    case res of
      GHC.ResTyH98 -> do

        when (not $ null bndrs) $ do
          mark GHC.AnnForall
          mapM_ markLocated bndrs
          mark GHC.AnnDot

        unless (null $ GHC.unLoc ctx) $ do
          setContext (Set.fromList [NoDarrow]) $ markLocated ctx
          mark GHC.AnnDarrow
        case dets of
          GHC.InfixCon _ _ -> return ()
          -- _ -> markListIntercalate lns
          _ -> setContext (Set.singleton PrefixOp) $ markListIntercalate lns

        markHsConDeclDetails False False lns dets

      GHC.ResTyGADT ls ty -> do
        -- only print names if not infix
        case dets of
          GHC.InfixCon _ _ -> return ()
          _ -> markListIntercalate lns

        if depc_syntax
          then do
            markHsConDeclDetails True False lns dets
            mark GHC.AnnCloseC
            mark GHC.AnnDcolon
            markManyOptional GHC.AnnOpenP

          else do
            mark GHC.AnnDcolon
            markLocated (GHC.L ls (ResTyGADTHook bndrs))
            markManyOptional GHC.AnnOpenP
            unless (null $ GHC.unLoc ctx) $ do
              markLocated ctx
            markHsConDeclDetails False True lns dets

        markLocated ty

        markManyOptional GHC.AnnCloseP


    case res of
      GHC.ResTyH98 -> inContext (Set.fromList [Intercalate]) $ mark GHC.AnnVbar
      _  -> return ()
    markTrailingSemi
#else
  markAST _ (GHC.ConDeclH98 ln mqtvs mctx
                         dets _ ) = do
{-
  | ConDeclH98
      { con_name    :: Located name

      , con_qvars     :: Maybe (LHsQTyVars name)
        -- User-written forall (if any), and its implicit
        -- kind variables
        -- Non-Nothing needs -XExistentialQuantification
        --               e.g. data T a = forall b. MkT b (b->a)
        --               con_qvars = {b}

      , con_cxt       :: Maybe (LHsContext name)
        -- ^ User-written context (if any)

      , con_details   :: HsConDeclDetails name
          -- ^ Arguments

      , con_doc       :: Maybe LHsDocString
          -- ^ A possible Haddock comment.

-}
    case mqtvs of
      Nothing -> return ()
      Just (GHC.HsQTvs _ns bndrs _) -> do
        mark GHC.AnnForall
        mapM_ markLocated bndrs
        mark GHC.AnnDot

    case mctx of
      Just ctx -> do
        -- setContext (Set.singleton Parens) $ markLocated ctx
        setContext (Set.fromList [Parens,NoDarrow]) $ markLocated ctx
        unless (null $ GHC.unLoc ctx) $ mark GHC.AnnDarrow
      Nothing -> return ()

    case dets of
      GHC.InfixCon _ _ -> return ()
      _ -> markLocated ln

    markHsConDeclDetails False False [ln] dets

    -- mark GHC.AnnVbar
    inContext (Set.fromList [Intercalate]) $ mark GHC.AnnVbar
    markTrailingSemi
  markAST _ (GHC.ConDeclGADT lns (GHC.HsIB _ typ) _) = do
    markListIntercalate lns
    mark GHC.AnnDcolon
    markLocated typ
    markTrailingSemi
#endif

-- ResTyGADT has a SrcSpan for the original sigtype, we need to create
-- a type for exactPC and annotatePC
data ResTyGADTHook name = ResTyGADTHook [GHC.LHsTyVarBndr name]
                   deriving (Typeable)
deriving instance (GHC.DataId name) => Data (ResTyGADTHook name)
deriving instance (Show (GHC.LHsTyVarBndr name)) => Show (ResTyGADTHook name)

instance (GHC.OutputableBndr name) => GHC.Outputable (ResTyGADTHook name) where
  ppr (ResTyGADTHook bs) = GHC.text "ResTyGADTHook" GHC.<+> GHC.ppr bs


#if __GLASGOW_HASKELL__ > 710
-- WildCardAnon exists because the GHC anonymous wildcard type is defined as
--      = AnonWildCard (PostRn name Name)
-- We need to reconstruct this from the typed hole SrcSpan in an HsForAllTy, but
-- the instance doing this is parameterised on name, so we cannot put a value in
-- for the (PostRn name Name) field. This is used instead.
data WildCardAnon = WildCardAnon deriving (Show,Data,Typeable)

instance Annotate WildCardAnon where
  markAST l WildCardAnon = do
    markExternal l GHC.AnnVal "_"
#endif

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
  => Annotate (ResTyGADTHook name) where
  markAST _ (ResTyGADTHook bndrs) = do
    when (not $ null bndrs) $ do
      mark GHC.AnnForall
      mapM_ markLocated bndrs
      mark GHC.AnnDot

-- ---------------------------------------------------------------------

instance (Annotate name, GHC.DataId name, GHC.OutputableBndr name,GHC.HasOccName name)
  => Annotate (GHC.HsRecField name (GHC.LPat name)) where
  markAST _ (GHC.HsRecField n e punFlag) = do
    -- markLocated n
    unsetContext Intercalate $ markLocated n
    -- mark GHC.AnnEqual
    when (punFlag == False) $ mark GHC.AnnEqual
    -- markLocated e
    unsetContext Intercalate $ markLocated e
    -- mark GHC.AnnEqual
    inContext (Set.fromList [Intercalate]) $ mark GHC.AnnComma


instance (Annotate name, GHC.DataId name, GHC.OutputableBndr name,GHC.HasOccName name)
  => Annotate (GHC.HsRecField name (GHC.LHsExpr name)) where
  markAST _ (GHC.HsRecField n e _) = do
    -- markLocated n
    unsetContext Intercalate $ markLocated n
    mark GHC.AnnEqual
    unsetContext Intercalate $ markLocated e
    inContext (Set.fromList [Intercalate]) $ mark GHC.AnnComma

-- ---------------------------------------------------------------------

-- instance (GHC.DataId name,Annotate name,GHC.OutputableBndr name,GHC.HasOccName name)
--   => Annotate [GHC.Located (GHC.FunDep (GHC.Located name))] where
--   -- markAST _ ls = markListIntercalate ls
--   markAST _ ls = error "foo"

-- -- -- [Located (FunDep (Located name))]

-- ---------------------------------------------------------------------

instance (GHC.DataId name,Annotate name)
    => Annotate (GHC.FunDep (GHC.Located name)) where

  markAST _ (ls,rs) = do
    mapM_ markLocated ls
    mark GHC.AnnRarrow
    mapM_ markLocated rs
    inContext (Set.fromList [Intercalate]) $ mark GHC.AnnComma

-- ---------------------------------------------------------------------

instance Annotate (GHC.CType) where
  markAST _ (GHC.CType src mh f) = do
    markWithString GHC.AnnOpen src
    case mh of
      Nothing -> return ()
#if __GLASGOW_HASKELL__ <= 710
      Just (GHC.Header h) ->
         markWithString GHC.AnnHeader ("\"" ++ GHC.unpackFS h ++ "\"")
    markWithString GHC.AnnVal ("\"" ++ GHC.unpackFS f ++ "\"")
#else
      Just (GHC.Header srcH _h) ->
         markWithString GHC.AnnHeader srcH
    markWithString GHC.AnnVal (fst f)
#endif
    markWithString GHC.AnnClose "#-}"

-- ---------------------------------------------------------------------
