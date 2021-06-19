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
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-} -- Needed for the DataId constraint on ResTyGADTHook
{-# LANGUAGE ViewPatterns      #-}

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

module Language.Haskell.GHC.ExactPrint.AnnotateTypes where

#if __GLASGOW_HASKELL__ <= 710
import Data.Ord ( comparing )
import Data.List ( sortBy )
#endif

import Language.Haskell.GHC.ExactPrint.Types

#if __GLASGOW_HASKELL__ >= 808
import qualified GHC.Types.Basic as GHC
#elif __GLASGOW_HASKELL__ > 800
import qualified BasicTypes     as GHC
#endif
import qualified GHC
#if __GLASGOW_HASKELL__ <= 710
import qualified BooleanFormula as GHC
import qualified Outputable     as GHC
#endif

import Control.Monad.Trans.Free
import Control.Monad.Free.TH (makeFreeCon)
import Control.Monad.Identity
import Data.Data

import qualified Data.Set as Set

{-# ANN module "HLint: ignore Eta reduce" #-}
{-# ANN module "HLint: ignore Redundant do" #-}
{-# ANN module "HLint: ignore Reduce duplication" #-}
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
#if __GLASGOW_HASKELL__ >= 800
  MarkInstead      :: GHC.AnnKeywordId -> KeywordId                        -> next -> AnnotationF next
#endif
  MarkOutside      :: GHC.AnnKeywordId -> KeywordId                        -> next -> AnnotationF next
  MarkInside       :: GHC.AnnKeywordId                                     -> next -> AnnotationF next
  MarkMany         :: GHC.AnnKeywordId                                     -> next -> AnnotationF next
  MarkManyOptional :: GHC.AnnKeywordId                                     -> next -> AnnotationF next
  MarkOffsetPrim   :: GHC.AnnKeywordId -> Int -> Maybe String              -> next -> AnnotationF next
  MarkOffsetPrimOptional :: GHC.AnnKeywordId -> Int -> Maybe String        -> next -> AnnotationF next
  WithAST          :: Data a => GHC.Located a
                             -> Annotated b                                -> next -> AnnotationF next
  CountAnns        :: GHC.AnnKeywordId                        -> (Int     -> next) -> AnnotationF next
  WithSortKey      :: [(AnnSpan, Annotated ())]                            -> next -> AnnotationF next

  SetLayoutFlag    ::  Rigidity -> Annotated ()                            -> next -> AnnotationF next
  MarkAnnBeforeAnn :: GHC.AnnKeywordId -> GHC.AnnKeywordId                 -> next -> AnnotationF next

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
  WithSortKeyContexts :: ListContexts -> [(AnnSpan, Annotated ())]     -> next -> AnnotationF next
  --
  TellContext :: Set.Set AstContext -> next -> AnnotationF next

deriving instance Functor AnnotationF

type Annotated = FreeT AnnotationF Identity


-- ---------------------------------------------------------------------

makeFreeCon  'MarkEOF
makeFreeCon  'MarkPrim
makeFreeCon  'MarkPPOptional
#if __GLASGOW_HASKELL__ >= 800
makeFreeCon  'MarkInstead
#endif
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
makeFreeCon  'WithSortKeyContexts
makeFreeCon  'TellContext
makeFreeCon  'MarkAnnBeforeAnn

-- ---------------------------------------------------------------------

setContext :: Set.Set AstContext -> Annotated () -> Annotated ()
setContext ctxt action = liftF (SetContextLevel ctxt 3 action ())

setLayoutFlag :: Annotated () -> Annotated ()
setLayoutFlag action = liftF (SetLayoutFlag NormalLayout action ())

setRigidFlag :: Annotated () -> Annotated ()
setRigidFlag action = liftF (SetLayoutFlag RigidLayout action ())

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
withAST lss action = liftF (WithAST lss action ())

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

withLocated :: Data a
            => GHC.Located a
            -> (GHC.SrcSpan -> a -> Annotated ())
            -> Annotated ()
withLocated a@(GHC.L l t) action =
  withAST a (action l t)

-- ---------------------------------------------------------------------


markListIntercalateWithFun :: (t -> Annotated ()) -> [t] -> Annotated ()
markListIntercalateWithFun f ls = markListIntercalateWithFunLevel f 2 ls

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
withSortKeyContextsHelper :: (Monad m) => (Annotated () -> m ()) -> ListContexts -> [(AnnSpan, Annotated ())] -> m ()
withSortKeyContextsHelper interpret (LC ctxOnly ctxInitial ctxMiddle ctxLast) kws = do
  case kws of
    [] -> return ()
    [x] -> interpret (setContextLevel (Set.insert (CtxPos 0) ctxOnly) level $ snd x)
    (x:xs) -> do
      interpret (setContextLevel (Set.insert (CtxPos 0) ctxInitial) level $ snd x)
      go 1 xs
  where
    level = 2
    go _ []  = return ()
    go n [x] = interpret (setContextLevel (Set.insert (CtxPos n) ctxLast) level $ snd x)
    go n (x:xs) = do
      interpret (setContextLevel (Set.insert (CtxPos n) ctxMiddle) level $ snd x)
      go (n+1) xs

-- ---------------------------------------------------------------------
-- Managing lists which have been separated, e.g. Sigs and Binds


applyListAnnotations :: [(AnnSpan, Annotated ())] -> Annotated ()
applyListAnnotations ls = withSortKey ls

applyListAnnotationsContexts :: ListContexts -> [(AnnSpan, Annotated ())] -> Annotated ()
applyListAnnotationsContexts ctxt ls = withSortKeyContexts ctxt ls

#if __GLASGOW_HASKELL__ <= 710
lexicalSortLocated :: [GHC.Located a] -> [GHC.Located a]
lexicalSortLocated = sortBy (comparing GHC.getLoc)
#endif

applyListAnnotationsLayout :: [(AnnSpan, Annotated ())] -> Annotated ()
applyListAnnotationsLayout ls = setLayoutFlag $ setContext (Set.singleton NoPrecedingSpace)
                                              $ withSortKeyContexts listContexts ls

listContexts :: ListContexts
listContexts = LC (Set.fromList [CtxOnly,ListStart])
                  (Set.fromList [CtxFirst,ListStart,Intercalate])
                  (Set.fromList [CtxMiddle,ListItem,Intercalate])
                  (Set.fromList [CtxLast,ListItem])

listContexts' :: ListContexts
listContexts' = LC (Set.fromList [CtxOnly,  ListStart])
                   (Set.fromList [CtxFirst, ListStart])
                   (Set.fromList [CtxMiddle,ListItem])
                   (Set.fromList [CtxLast,  ListItem])

-- ---------------------------------------------------------------------


#if __GLASGOW_HASKELL__ > 800
markAnnOpen :: GHC.SourceText -> String -> Annotated ()
markAnnOpen GHC.NoSourceText txt   =  markWithString GHC.AnnOpen txt
markAnnOpen (GHC.SourceText txt) _ =  markWithString GHC.AnnOpen txt

markSourceText :: GHC.SourceText -> String -> Annotated ()
markSourceText GHC.NoSourceText txt   =  markWithString GHC.AnnVal txt
markSourceText (GHC.SourceText txt) _ =  markWithString GHC.AnnVal txt

markExternalSourceText :: GHC.SrcSpan -> GHC.SourceText -> String -> Annotated ()
markExternalSourceText l GHC.NoSourceText txt   =  markExternal l GHC.AnnVal txt
markExternalSourceText l (GHC.SourceText txt) _ =  markExternal l GHC.AnnVal txt

sourceTextToString :: GHC.SourceText -> String -> String
sourceTextToString GHC.NoSourceText alt   = alt
sourceTextToString (GHC.SourceText txt) _ = txt
#endif

-- ---------------------------------------------------------------------
