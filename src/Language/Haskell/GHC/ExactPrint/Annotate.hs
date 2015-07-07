{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PatternSynonyms #-}
module Language.Haskell.GHC.ExactPrint.Annotate
       (
         annotate
       , AnnotationF(..)
       , Annotated
       , Annotate(..)) where

import Data.List ( sort, sortBy )
import Data.Maybe ( fromMaybe )
import Data.Ord ( comparing )

import Language.Haskell.GHC.ExactPrint.Internal.Types
import Language.Haskell.GHC.ExactPrint.Utils

import qualified Bag            as GHC
import qualified BasicTypes     as GHC
import qualified BooleanFormula as GHC
import qualified Class          as GHC
import qualified CoAxiom        as GHC
import qualified FastString     as GHC
import qualified ForeignCall    as GHC
import qualified GHC            as GHC
import qualified OccName        as GHC
import qualified Outputable     as GHC
import qualified SrcLoc         as GHC

import Control.Monad.Trans.Free
import Control.Monad.Free.TH (makeFreeCon)
import Control.Monad.Identity
import Control.Monad.Reader
import Data.Data

import Debug.Trace


-- ---------------------------------------------------------------------


data AnnotationF next where
  MarkEOF        ::                                                         next -> AnnotationF next
  MarkPrim       :: GHC.AnnKeywordId -> Maybe String                     -> next -> AnnotationF next
  MarkExternal   :: GHC.SrcSpan -> GHC.AnnKeywordId -> String            -> next -> AnnotationF next
  MarkOutside    :: GHC.AnnKeywordId -> KeywordId                        -> next -> AnnotationF next
  MarkInside     :: GHC.AnnKeywordId                                     -> next -> AnnotationF next
  MarkMany       :: GHC.AnnKeywordId                                     -> next -> AnnotationF next
  MarkOffsetPrim :: GHC.AnnKeywordId -> Int -> Maybe String              -> next -> AnnotationF next
  MarkAfter      :: GHC.AnnKeywordId                                     -> next -> AnnotationF next
  WithAST        :: Data a => GHC.Located a -> Disambiguator
                           -> LayoutFlag -> Annotated b                  -> next -> AnnotationF next
  CountAnns      :: GHC.AnnKeywordId                        -> (Int     -> next) -> AnnotationF next
  WithSortKey       :: [(GHC.SrcSpan, Annotated ())]                     -> next -> AnnotationF next

  -- | Abstraction breakers
  SetLayoutFlag  ::  Annotated ()                                        -> next -> AnnotationF next

  -- | Required to work around deficiencies in the GHC AST
  StoreOriginalSrcSpan :: GHC.SrcSpan -> Disambiguator  -> ((GHC.SrcSpan,Disambiguator) -> next) -> AnnotationF next
  GetSrcSpanForKw :: GHC.AnnKeywordId                   -> (GHC.SrcSpan -> next) -> AnnotationF next
  StoreString :: String -> GHC.SrcSpan                  -> next -> AnnotationF next
  GetNextDisambiguator ::                                  (Disambiguator -> next) -> AnnotationF next
  AnnotationsToComments :: [GHC.AnnKeywordId]           -> next -> AnnotationF next

deriving instance Functor (AnnotationF)

-- type Annotated = Free AnnotationF
type Annotated = FreeT AnnotationF Identity

type IAnnotated = ReaderT [Context] Annotated

data Context = None | Case | Where | Lam | MultiIf
             deriving (Eq, Ord, Show)

addContext :: Context -> IAnnotated () -> IAnnotated ()
addContext c = local (c:)

-- check :: Context -> IAnnotated () -> IAnnotated ()
-- check c = checkMany [c]

-- checkMany :: [Context] -> IAnnotated () -> IAnnotated ()
-- checkMany cs action = do
--   ctx <- ask
--   when (any (`elem` ctx) cs) action



-- ---------------------------------------------------------------------

makeFreeCon  'MarkEOF
makeFreeCon  'MarkPrim
makeFreeCon  'MarkOutside
makeFreeCon  'MarkInside
makeFreeCon  'MarkExternal
makeFreeCon  'MarkMany
makeFreeCon  'MarkOffsetPrim
makeFreeCon  'CountAnns
makeFreeCon  'StoreOriginalSrcSpan
makeFreeCon  'GetSrcSpanForKw
makeFreeCon  'StoreString
makeFreeCon  'GetNextDisambiguator
makeFreeCon  'AnnotationsToComments

-- ---------------------------------------------------------------------

-- |Main entry point
annotate :: (Annotate ast) => GHC.Located ast -> Annotated ()
annotate ast = runReaderT (markLocated ast) []

-- ---------------------------------------------------------------------

setLayoutFlag :: IAnnotated () -> IAnnotated ()
setLayoutFlag prog = do
  finalProg <- runReaderT prog <$> ask
  liftF (SetLayoutFlag finalProg ())

workOutString :: GHC.AnnKeywordId -> (GHC.SrcSpan -> String) -> IAnnotated ()
workOutString kw f = do
  ss <- getSrcSpanForKw kw
  storeString (f ss) ss

withSortKey :: [(GHC.SrcSpan,  IAnnotated ())] -> IAnnotated ()
withSortKey ls = do
  c <- ask
  let new = fmap (flip runReaderT c) <$> ls
  liftF (WithSortKey new ())


-- ---------------------------------------------------------------------

-- |Main driver point for annotations.
withAST :: Data a => GHC.Located a -> Disambiguator -> LayoutFlag -> IAnnotated () -> IAnnotated ()
withAST lss d layout action = do
  finalProg <- runReaderT prog <$> ask
  liftF (WithAST lss d layout finalProg ())
  where
    prog = do
      action
      -- Automatically add any trailing comma or semi
      markOutside GHC.AnnComma (G GHC.AnnComma)

-- ---------------------------------------------------------------------
-- Additional smart constructors

mark :: GHC.AnnKeywordId -> IAnnotated ()
mark kwid = markPrim kwid Nothing

markWithString :: GHC.AnnKeywordId -> String -> IAnnotated ()
markWithString kwid s = markPrim kwid (Just s)

markOffsetWithString :: GHC.AnnKeywordId -> Int -> String -> IAnnotated ()
markOffsetWithString kwid n s = markOffsetPrim kwid n (Just s)

markOffset :: GHC.AnnKeywordId -> Int -> IAnnotated ()
markOffset kwid n = markOffsetPrim kwid n Nothing

-- ---------------------------------------------------------------------

-- | Constructs a syntax tree which contains information about which
-- annotations are required by each element.
markLocated :: (Annotate ast) => GHC.Located ast -> IAnnotated ()
markLocated a = withLocated a NotNeeded NoLayoutRules markAST

-- | Constructs a syntax tree which contains information about which
-- annotations are required by each element, flagging the item to be IAnnotated
-- as requiring layout according to the haskell layout rules.
markWithLayout :: Annotate ast => GHC.Located ast -> Disambiguator -> IAnnotated ()
markWithLayout a d = withLocated a d LayoutRules markAST

withLocated :: Data a
            => GHC.Located a
            -> Disambiguator
            -> LayoutFlag
            -> (GHC.SrcSpan -> a -> IAnnotated ())
            -> IAnnotated ()
withLocated a@(GHC.L l ast) d layoutFlag action =
  withAST a d layoutFlag (action l ast)

-- ---------------------------------------------------------------------

markListWithLayout :: Annotate [GHC.Located ast] => [GHC.Located ast] -> IAnnotated ()
markListWithLayout ls = do
  let ss = getListSrcSpan ls
  d <- getNextDisambiguator
  (ss',d') <- storeOriginalSrcSpan ss d
  markWithLayout (GHC.L ss' ls) d'

markLocalBindsWithLayout :: (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
  => GHC.HsLocalBinds name -> IAnnotated ()
markLocalBindsWithLayout binds = do
  ss <- getLocalBindsSrcSpan binds
  let d  = NotNeeded
  (ss',d') <- storeOriginalSrcSpan ss d
  markWithLayout (GHC.L ss' binds) d'

-- ---------------------------------------------------------------------

-- | Local binds need to be indented as a group, and thus need to have a
-- SrcSpan around them so they can be processed via the normal
-- markLocated / exactPC machinery.
getLocalBindsSrcSpan :: GHC.HsLocalBinds name -> IAnnotated GHC.SrcSpan
getLocalBindsSrcSpan (GHC.HsValBinds (GHC.ValBindsIn binds sigs)) = do
  spans <- lexicalSortSrcSpans (map GHC.getLoc (GHC.bagToList binds) ++ map GHC.getLoc sigs)
  case spans of
    []  -> return GHC.noSrcSpan
    sss -> return $ GHC.combineSrcSpans (head sss) (last sss)
  where

getLocalBindsSrcSpan (GHC.HsValBinds (GHC.ValBindsOut {}))
   = return GHC.noSrcSpan

getLocalBindsSrcSpan (GHC.HsIPBinds (GHC.IPBinds binds _)) = do
  spans <- lexicalSortSrcSpans (map GHC.getLoc binds)
  case spans of
    [] -> return GHC.noSrcSpan
    sss -> return $ GHC.combineSrcSpans (head sss) (last sss)

getLocalBindsSrcSpan (GHC.EmptyLocalBinds) = return GHC.noSrcSpan

-- ---------------------------------------------------------------------

-- |This function is used to get around shortcomings in the GHC AST for 7.10.1
markLocatedFromKw :: (Annotate ast) => GHC.AnnKeywordId -> ast -> IAnnotated ()
markLocatedFromKw kw a = do
  ss <- getSrcSpanForKw kw
  (ss',_d') <- storeOriginalSrcSpan ss NotNeeded
  markLocated (GHC.L ss' a)

-- ---------------------------------------------------------------------

markMaybe :: (Annotate ast) => Maybe (GHC.Located ast) -> IAnnotated ()
markMaybe Nothing    = return ()
markMaybe (Just ast) = markLocated ast

-- Mark a list of items, ensuring that the SortKey is honoured.
markList :: (Annotate ast) => [GHC.Located ast] -> IAnnotated ()
markList xs = applyListAnnotations (prepareListAnnotation xs)

-- ---------------------------------------------------------------------
-- Managing lists which have been separated, e.g. Sigs and Binds

prepareListAnnotation :: Annotate a => [GHC.Located a] -> [(GHC.SrcSpan,IAnnotated ())]
prepareListAnnotation ls = map (\b -> (GHC.getLoc b,markLocated b)) ls

applyListAnnotations :: [(GHC.SrcSpan, IAnnotated ())] -> IAnnotated ()
applyListAnnotations ls = withSortKey ls
{-
  ls' <- mapM (\(ss,v) -> getSortKey ss >>= \sk -> return (sk ,v)) ls
  -- return () `debug` ("applyListAnnotations:sortkeys=" ++ show (map fst ls'))
  let lsSorted = sortBy (\(a,_) (b,_) -> compare a b) ls'
  return () `debug` ("applyListAnnotations:sortkeys=" ++ show (map fst lsSorted))
  mapM_ snd lsSorted
  -}

#if __GLASGOW_HASKELL__ <= 710
lexicalSortLocated :: [GHC.Located a] -> [GHC.Located a]
lexicalSortLocated = sortBy (comparing GHC.getLoc)
#endif
lexicalSortSrcSpans :: [GHC.SrcSpan] -> IAnnotated [GHC.SrcSpan]
lexicalSortSrcSpans ls = return $ sort ls

-- ---------------------------------------------------------------------

class Data ast => Annotate ast where
  markAST :: GHC.SrcSpan -> ast -> IAnnotated ()

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
      Just expr -> markLocated expr

    mark GHC.AnnWhere
    mark GHC.AnnOpenC -- Possible '{'
    markMany GHC.AnnSemi -- possible leading semis
    mapM_ markLocated imps

    markList decs

    mark GHC.AnnCloseC -- Possible '}'

    markEOF

-- ---------------------------------------------------------------------

instance Annotate GHC.WarningTxt where
  markAST _ (GHC.WarningTxt (GHC.L ls txt) lss) = do
    markExternal ls GHC.AnnOpen txt
    mark GHC.AnnOpenS
    mapM_ markLocated lss
    mark GHC.AnnCloseS
    markWithString GHC.AnnClose "#-}"

  markAST _ (GHC.DeprecatedTxt (GHC.L ls txt) lss) = do
    markExternal ls GHC.AnnOpen txt
    mark GHC.AnnOpenS
    mapM_ markLocated lss
    mark GHC.AnnCloseS
    markWithString GHC.AnnClose "#-}"

-- ---------------------------------------------------------------------
instance Annotate (GHC.SourceText,GHC.FastString) where
  markAST l (_,fs) = markAST l fs

-- ---------------------------------------------------------------------

instance (GHC.DataId name,Annotate name)
  => Annotate [GHC.LIE name] where
   markAST _ ls = do
     mark GHC.AnnHiding -- in an import decl
     mark GHC.AnnOpenP -- '('
     mapM_ markLocated ls
     mark GHC.AnnCloseP -- ')'

instance (GHC.DataId name,Annotate name)
  => Annotate (GHC.IE name) where
  markAST _ ie = do

    case ie of
        (GHC.IEVar ln) -> do
          mark GHC.AnnPattern
          mark GHC.AnnType
          markLocated ln

        (GHC.IEThingAbs ln@(GHC.L _ n)) -> do
          cnt <- countAnns GHC.AnnType
          if cnt == 1
            then do
              mark GHC.AnnType
              markLocatedFromKw GHC.AnnVal n
            else markLocated ln

        (GHC.IEThingWith ln ns) -> do
          markLocated ln
          mark GHC.AnnOpenP
          mapM_ markLocated ns
          mark GHC.AnnCloseP

        (GHC.IEThingAll ln) -> do
          markLocated ln
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

instance Annotate GHC.RdrName where
  markAST l n = do
    let
      str = rdrName2String n
      doNormalRdrName = do
        let str' = case str of
                        "forall" -> if spanLength l == 1 then "∀" else str
                        _ -> str
        mark GHC.AnnType
        mark GHC.AnnOpenP -- '('
        markOffset GHC.AnnBackquote 0
        cnt  <- countAnns GHC.AnnVal
        cntT <- countAnns GHC.AnnCommaTuple
        markMany GHC.AnnCommaTuple -- For '(,,,)'
        case cnt of
          0 -> if cntT > 0
                 then traceM $ "Printing RdrName, no AnnVal, multiple AnnCommTuple:" ++ showGhc (l,n)
                 else markExternal l GHC.AnnVal str'
          1 -> markWithString GHC.AnnVal str'
          _ -> traceM $ "Printing RdrName, more than 1 AnnVal:" ++ showGhc (l,n)
        markOffset GHC.AnnBackquote 1
        mark GHC.AnnCloseP

    case n of
      GHC.Unqual _ -> doNormalRdrName
      GHC.Qual _ _ -> doNormalRdrName
      _            -> do
       case str of
         -- Special handling for atypical RdrNames.
         "[]" -> do
           mark GHC.AnnOpenS  -- '['
           mark GHC.AnnCloseS -- ']'
         "()" -> do
           mark GHC.AnnOpenP  -- '('
           mark GHC.AnnCloseP -- ')'
         "(##)" -> do
           markWithString GHC.AnnOpen  "(#" -- '(#'
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
         "~" -> do
           mark GHC.AnnOpenP
           mark GHC.AnnTilde
           mark GHC.AnnCloseP
         _ -> doNormalRdrName

-- ---------------------------------------------------------------------

-- TODO: What is this used for? Not in ExactPrint
instance Annotate GHC.Name where
  markAST l n = do
    markExternal l GHC.AnnVal (showGhc n)

-- ---------------------------------------------------------------------

instance (GHC.DataId name,Annotate name)
  => Annotate (GHC.ImportDecl name) where
 markAST _ imp@(GHC.ImportDecl msrc (GHC.L ln _) mpkg src safeflag _qual _impl _as hiding) = do

   -- 'import' maybe_src maybe_safe optqualified maybe_pkg modid maybeas maybeimpspec
   mark GHC.AnnImport

   -- "{-# SOURCE" and "#-}"
   when src (markWithString GHC.AnnOpen (fromMaybe "{-# SOURCE" msrc)
             >> markWithString GHC.AnnClose "#-}")
   when safeflag (mark GHC.AnnSafe)
   mark GHC.AnnQualified
   case mpkg of
    Nothing -> return ()
#if __GLASGOW_HASKELL__ <= 710
    Just pkg -> markWithString GHC.AnnPackageName (show (GHC.unpackFS pkg))
#else
    Just (srcPkg,_pkg) -> markWithString GHC.AnnPackageName srcPkg
#endif

   markExternal ln GHC.AnnVal (GHC.moduleNameString $ GHC.unLoc $ GHC.ideclName imp)

   case GHC.ideclAs imp of
      Nothing -> return ()
      Just mn -> do
          mark GHC.AnnAs
          markWithString GHC.AnnVal (GHC.moduleNameString mn)

   case hiding of
     Nothing -> return ()
     Just (_isHiding,lie) -> do
       mark GHC.AnnHiding
       markLocated lie
   markOutside (GHC.AnnSemi) (G GHC.AnnSemi)

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
  => Annotate (GHC.HsDecl name) where
  markAST l decl = do
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
    mark GHC.AnnOpenPE
    markLocated e
    mark GHC.AnnCloseP
    markOutside GHC.AnnSemi AnnSemiSep

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
    mark GHC.AnnEqual
    markMaybe mln
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
     markOutside GHC.AnnSemi AnnSemiSep

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
   => Annotate (GHC.RuleDecl name) where
  markAST _ (GHC.HsRule ln act bndrs lhs _ rhs _) = do
    markLocated ln
    -- activation
    mark GHC.AnnOpenS -- "["
    mark GHC.AnnTilde
    case act of
      GHC.ActiveBefore n -> markWithString GHC.AnnVal (show n)
      GHC.ActiveAfter n  -> markWithString GHC.AnnVal (show n)
      _                  -> return ()
    mark GHC.AnnCloseS -- "]"

    mark GHC.AnnForall
    mapM_ markLocated bndrs
    mark GHC.AnnDot

    markLocated lhs
    mark GHC.AnnEqual
    markLocated rhs
    markOutside GHC.AnnSemi AnnSemiSep

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
   => Annotate (GHC.RuleBndr name) where
  markAST _ (GHC.RuleBndr ln) = markLocated ln
  markAST _ (GHC.RuleBndrSig ln (GHC.HsWB thing _ _ _)) = do
    mark GHC.AnnOpenP -- "("
    markLocated ln
    mark GHC.AnnDcolon
    markLocated thing
    mark GHC.AnnCloseP -- ")"

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
   => Annotate (GHC.AnnDecl name) where
   markAST _ (GHC.HsAnnotation src prov e) = do
     markWithString GHC.AnnOpen src
     mark GHC.AnnType
     mark GHC.AnnModule
     case prov of
       (GHC.ValueAnnProvenance n) -> markLocated n
       (GHC.TypeAnnProvenance n) -> markLocated n
       (GHC.ModuleAnnProvenance) -> return ()

     markLocated e
     markWithString GHC.AnnClose "#-}"

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
     mapM_ markLocated lns
     mark GHC.AnnOpenS -- "["
     case txt of
       GHC.WarningTxt    _src ls -> mapM_ markLocated ls
       GHC.DeprecatedTxt _src ls -> mapM_ markLocated ls
     mark GHC.AnnCloseS -- "]"

instance Annotate GHC.FastString where
  -- TODO: https://ghc.haskell.org/trac/ghc/ticket/10313 applies.
  markAST l fs = markExternal l GHC.AnnVal (show (GHC.unpackFS fs))
  -- markAST l fs = markExternal l GHC.AnnVal ('"':(GHC.unpackFS fs++"\""))

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
   => Annotate (GHC.ForeignDecl name) where

  markAST _ (GHC.ForeignImport ln typ _
               (GHC.CImport cconv safety@(GHC.L ll _) _mh _imp (GHC.L ls src))) = do
    mark GHC.AnnForeign
    mark GHC.AnnImport
    markLocated cconv
    if ll == GHC.noSrcSpan
      then return ()
      else markLocated safety
    -- markMaybe mh
    markExternal ls GHC.AnnVal (show src)
    markLocated ln
    mark GHC.AnnDcolon
    markLocated typ
    markOutside (GHC.AnnSemi) AnnSemiSep


  markAST _l (GHC.ForeignExport ln typ _ (GHC.CExport spec (GHC.L ls src))) = do
    mark GHC.AnnForeign
    mark GHC.AnnExport
    markLocated spec
    markExternal ls GHC.AnnVal (show src)
    markLocated ln
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

  markAST _ (GHC.DerivDecl typ mov) = do
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
    mapM_ markLocated typs
    mark GHC.AnnCloseP -- ')'
    markOutside GHC.AnnSemi (G GHC.AnnSemi)

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

  markAST _ (GHC.ClsInstDecl poly binds sigs tyfams datafams mov) = do
    mark GHC.AnnInstance
    markMaybe mov
    markLocated poly
    mark GHC.AnnWhere
    mark GHC.AnnOpenC -- '{'
    markInside GHC.AnnSemi

    applyListAnnotations (prepareListAnnotation (GHC.bagToList binds)
                       ++ prepareListAnnotation sigs
                       ++ prepareListAnnotation tyfams
                       ++ prepareListAnnotation datafams
                         )

    mark GHC.AnnCloseC -- '}'
    markOutside GHC.AnnSemi (AnnSemiSep)

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
   => Annotate (GHC.TyFamInstDecl name) where

  markAST _ (GHC.TyFamInstDecl eqn _) = do
    mark GHC.AnnType
    mark GHC.AnnInstance
    markLocated eqn
    markOutside GHC.AnnSemi AnnSemiSep

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
   => Annotate (GHC.DataFamInstDecl name) where

  markAST l (GHC.DataFamInstDecl ln (GHC.HsWB pats _ _ _) defn _) = do
    mark GHC.AnnData
    mark GHC.AnnNewtype
    mark GHC.AnnInstance
    mark GHC.AnnOpenP

    applyListAnnotations (prepareListAnnotation [ln]
                       ++ prepareListAnnotation pats
                         )

    mark GHC.AnnCloseP
    mark GHC.AnnWhere
    mark GHC.AnnEqual
    markDataDefn l defn

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
  => Annotate (GHC.HsBind name) where
  markAST _ (GHC.FunBind (GHC.L _ln _n) _ (GHC.MG matches _ _ _) _ _ _) = do
    mapM_ markLocated matches
    --markOutside GHC.AnnSemi AnnSemiSep
    -- markMatchGroup l mg

  markAST _ (GHC.PatBind lhs (GHC.GRHSs grhs lb) _typ _fvs _ticks) = do
    markLocated lhs
    mark GHC.AnnEqual
    mapM_ markLocated grhs
    mark GHC.AnnWhere

    markLocalBindsWithLayout lb
    markOutside GHC.AnnSemi (G GHC.AnnSemi)

  markAST _ (GHC.VarBind _n rhse _) =
    -- Note: this bind is introduced by the typechecker
    markLocated rhse

  markAST l (GHC.PatSynBind (GHC.PSB ln _fvs args def dir)) = do
    mark GHC.AnnPattern
    case args of
      GHC.InfixPatSyn la lb -> do
        markLocated la
        markLocated ln
        markLocated lb
      GHC.PrefixPatSyn ns -> do
        markLocated ln
        mapM_ markLocated ns
    mark GHC.AnnEqual
    mark GHC.AnnLarrow
    markLocated def
    case dir of
      GHC.Unidirectional           -> return ()
      GHC.ImplicitBidirectional    -> return ()
      GHC.ExplicitBidirectional mg -> markMatchGroup l mg

    mark GHC.AnnWhere
    mark GHC.AnnOpenC  -- '{'
    mark GHC.AnnCloseC -- '}'
    markOutside GHC.AnnSemi (G GHC.AnnSemi)

  -- Introduced after renaming.
  markAST _ (GHC.AbsBinds _ _ _ _ _) =
    traceM "warning: AbsBind introduced after renaming"


-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
    => Annotate (GHC.IPBind name) where
  markAST _ (GHC.IPBind en e) = do
    case en of
      Left n -> markLocated n
      Right _i -> return ()
    mark GHC.AnnEqual
    markLocated e
    markOutside GHC.AnnSemi AnnSemiSep

-- ---------------------------------------------------------------------

instance Annotate GHC.HsIPName where
  markAST l (GHC.HsIPName n) = markExternal l (GHC.AnnVal) ("?" ++ GHC.unpackFS n)

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name,
                                                  Annotate body)
  => Annotate (GHC.Match name (GHC.Located body)) where

  markAST _ (GHC.Match mln pats _typ (GHC.GRHSs grhs lb)) = do
    let
      get_infix Nothing = False
      get_infix (Just (_,f)) = f
    case (get_infix mln,pats) of
      (True, (a:b:xs)) -> do
        mark GHC.AnnOpenP
        markLocated a
        case mln of
          Nothing -> return ()
          Just (n,_) -> markLocated n
        markLocated b
        mark GHC.AnnCloseP
        mapM_ markLocated xs
      _ -> do
        case mln of
          Nothing -> mark GHC.AnnFunId
          Just (n,_) -> markLocated n
        mapM_ markLocated pats

    -- TODO: The AnnEqual annotation actually belongs in the first GRHS value
    mark GHC.AnnEqual
    mark GHC.AnnRarrow -- For HsLam

    mapM_ markLocated grhs

    mark GHC.AnnWhere
    mark GHC.AnnOpenC -- '{'
    markInside GHC.AnnSemi
    markLocalBindsWithLayout lb
    mark GHC.AnnCloseC -- '}'
    markOutside GHC.AnnSemi AnnSemiSep

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,
          Annotate name, Annotate body)
  => Annotate (GHC.GRHS name (GHC.Located body)) where
  markAST _ (GHC.GRHS guards expr) = do
    case guards of
      [] -> return ()
      (_:_) -> mark GHC.AnnVbar >> mapM_ markLocated guards
    mark GHC.AnnEqual
    cntL <- countAnns GHC.AnnLam
    when (cntL == 0) $ mark GHC.AnnRarrow -- For HsLam
    markLocated expr

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
  => Annotate (GHC.Sig name) where

  markAST _ (GHC.TypeSig lns typ _) = do
    mapM_ markLocated lns
    mark GHC.AnnDcolon
    markLocated typ
    markOutside (GHC.AnnSemi) (G GHC.AnnSemi)

  markAST _ (GHC.PatSynSig ln (_,GHC.HsQTvs _ns bndrs) ctx1 ctx2 typ) = do
    mark GHC.AnnPattern
    markLocated ln
    mark GHC.AnnDcolon

    -- Note: The 'forall' bndrs '.' may occur multiple times
    mark GHC.AnnForall
    mapM_ markLocated bndrs
    mark GHC.AnnDot

    markLocated ctx1
    markOffset GHC.AnnDarrow 0
    markLocated ctx2
    markOffset GHC.AnnDarrow 1
    markLocated typ


  markAST _ (GHC.GenericSig ns typ) = do
    mark GHC.AnnDefault
    mapM_ markLocated ns
    mark GHC.AnnDcolon
    markLocated typ

  markAST _ (GHC.IdSig _) =
    traceM "warning: Introduced after renaming"

  -- FixSig (FixitySig name)
  markAST _ (GHC.FixSig (GHC.FixitySig lns (GHC.Fixity v fdir))) = do
    let fixstr = case fdir of
         GHC.InfixL -> "infixl"
         GHC.InfixR -> "infixr"
         GHC.InfixN -> "infix"
    markWithString GHC.AnnInfix fixstr
    markWithString GHC.AnnVal (show v)
    mapM_ markLocated lns
    markOutside (GHC.AnnSemi) (G GHC.AnnSemi)

  -- InlineSig (Located name) InlinePragma
  -- '{-# INLINE' activation qvar '#-}'
  markAST _ (GHC.InlineSig ln inl) = do
    let actStr = case GHC.inl_act inl of
          GHC.NeverActive -> ""
          GHC.AlwaysActive -> ""
          GHC.ActiveBefore np -> show np
          GHC.ActiveAfter  np -> show np
    markWithString GHC.AnnOpen (GHC.inl_src inl) -- '{-# INLINE'
    mark GHC.AnnOpenS  -- '['
    mark  GHC.AnnTilde -- ~
    markWithString  GHC.AnnVal actStr -- e.g. 34
    mark GHC.AnnCloseS -- ']'
    markLocated ln
    markWithString GHC.AnnClose "#-}" -- '#-}'
    markOutside GHC.AnnSemi AnnSemiSep


  markAST _ (GHC.SpecSig ln typs inl) = do
    markWithString GHC.AnnOpen (GHC.inl_src inl)
    mark GHC.AnnOpenS --  '['
    mark GHC.AnnTilde -- ~

    mark GHC.AnnCloseS -- ']'
    markLocated ln
    mark GHC.AnnDcolon -- '::'
    mapM_ markLocated typs
    markWithString GHC.AnnClose "#-}" -- '#-}'
    markOutside GHC.AnnSemi AnnSemiSep


  -- '{-# SPECIALISE' 'instance' inst_type '#-}'
  markAST _ (GHC.SpecInstSig src typ) = do
    markWithString GHC.AnnOpen src
    mark GHC.AnnInstance
    markLocated typ
    markWithString GHC.AnnClose "#-}" -- '#-}'
    markOutside GHC.AnnSemi AnnSemiSep



  -- MinimalSig (BooleanFormula (Located name))
  markAST l (GHC.MinimalSig src  formula) = do
    markWithString GHC.AnnOpen src
    annotationsToComments [GHC.AnnOpenP,GHC.AnnCloseP,GHC.AnnComma,GHC.AnnVbar]
    markAST l formula
    markWithString GHC.AnnClose "#-}"
    markOutside GHC.AnnSemi AnnSemiSep


-- --------------------------------------------------------------------

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
  markAST _ (GHC.Var x)  = markLocated x
  markAST l (GHC.Or ls)  = mapM_ (markAST l) ls
  markAST l (GHC.And ls) = mapM_ (markAST l) ls

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
  => Annotate (GHC.HsTyVarBndr name) where
  markAST l (GHC.UserTyVar n) = do
    markAST l n

  markAST _ (GHC.KindedTyVar n ty) = do
    mark GHC.AnnOpenP  -- '('
    markLocated n
    mark GHC.AnnDcolon -- '::'
    markLocated ty
    mark GHC.AnnCloseP -- '('

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
   => Annotate (GHC.HsType name) where

  markAST _ (GHC.HsForAllTy _f mwc (GHC.HsQTvs _kvs tvs) ctx@(GHC.L lc ctxs) typ) = do
    mark GHC.AnnOpenP -- "("
    mark GHC.AnnForall
    mapM_ markLocated tvs
    mark GHC.AnnDot

    case mwc of
      Nothing -> if lc /= GHC.noSrcSpan then markLocated ctx else return ()
      Just lwc -> do
#if __GLASGOW_HASKELL__ <= 710
       let sorted = lexicalSortLocated (GHC.L lwc GHC.HsWildcardTy:ctxs)
       markLocated (GHC.L lc sorted)
#else
        applyListAnnotations (prepareListAnnotation [GHC.L lwc WildCardAnon]
                           ++ prepareListAnnotation ctxs)
#endif

    mark GHC.AnnDarrow
    markLocated typ
    mark GHC.AnnCloseP -- ")"

  markAST l (GHC.HsTyVar name) = do
    mark GHC.AnnDcolon -- for HsKind, alias for HsType
    n <- countAnns  GHC.AnnSimpleQuote
    case n of
      1 -> do
          mark GHC.AnnSimpleQuote
          markLocatedFromKw GHC.AnnName name
      _ -> markAST l name

  markAST _ (GHC.HsAppTy t1 t2) = do
    mark GHC.AnnDcolon -- for HsKind, alias for HsType
    markLocated t1
    markLocated t2

  markAST _ (GHC.HsFunTy t1 t2) = do
    mark GHC.AnnDcolon -- for HsKind, alias for HsType
    markLocated t1
    mark GHC.AnnRarrow
    markLocated t2

  markAST _ (GHC.HsListTy t) = do
    mark GHC.AnnDcolon -- for HsKind, alias for HsType
    mark GHC.AnnOpenS -- '['
    markLocated t
    mark GHC.AnnCloseS -- ']'

  markAST _ (GHC.HsPArrTy t) = do
    markWithString GHC.AnnOpen "[:" -- '[:'
    markLocated t
    markWithString GHC.AnnClose ":]" -- ':]'

  markAST _ (GHC.HsTupleTy _tt ts) = do
    mark GHC.AnnDcolon -- for HsKind, alias for HsType
    markWithString GHC.AnnOpen "(#" -- '(#'
    mark GHC.AnnOpenP  -- '('
    mapM_ markLocated ts
    mark GHC.AnnCloseP -- ')'
    markWithString GHC.AnnClose "#)" --  '#)'

  markAST _ (GHC.HsOpTy t1 (_,lo) t2) = do
    markLocated t1
    mark GHC.AnnSimpleQuote
    markLocated lo
    markLocated t2

  markAST _ (GHC.HsParTy t) = do
    mark GHC.AnnDcolon -- for HsKind, alias for HsType
    mark GHC.AnnOpenP  -- '('
    markLocated t
    mark GHC.AnnCloseP -- ')'
--    mark GHC.AnnDarrow -- May appear after context in a ConDecl

  markAST _ (GHC.HsIParamTy (GHC.HsIPName n) t) = do
    markWithString GHC.AnnVal ("?" ++ (GHC.unpackFS n))
    mark GHC.AnnDcolon
    markLocated t

  markAST _ (GHC.HsEqTy t1 t2) = do
    markLocated t1
    mark GHC.AnnTilde
    markLocated t2

  markAST _ (GHC.HsKindSig t k) = do
    mark GHC.AnnOpenP  -- '('
    markLocated t
    mark GHC.AnnDcolon -- '::'
    markLocated k
    mark GHC.AnnCloseP -- ')'

  markAST l (GHC.HsSpliceTy s _) = do
    mark GHC.AnnOpenPE
    markAST l s
    mark GHC.AnnCloseP

  markAST _ (GHC.HsDocTy t ds) = do
    markLocated t
    markLocated ds

  markAST _ (GHC.HsBangTy b t) = do
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

  -- HsRecTy [LConDeclField name]
  markAST _ (GHC.HsRecTy cons) = do
    mark GHC.AnnOpenC  -- '{'
    mapM_ markLocated cons
    mark GHC.AnnCloseC -- '}'

  -- HsCoreTy Type
  markAST _ (GHC.HsCoreTy _t) =
    traceM "warning: HsCoreTy Introduced after renaming"

  markAST _ (GHC.HsExplicitListTy _ ts) = do
    mark GHC.AnnSimpleQuote
    mark GHC.AnnOpenS  -- "["
    mapM_ markLocated ts
    mark GHC.AnnCloseS -- ']'

  markAST _ (GHC.HsExplicitTupleTy _ ts) = do
    mark GHC.AnnSimpleQuote
    mark GHC.AnnOpenP
    mapM_ markLocated ts
    mark GHC.AnnCloseP

  -- HsTyLit HsTyLit
  markAST l (GHC.HsTyLit lit) = do
    case lit of
      (GHC.HsNumTy s _) ->
        markExternal l GHC.AnnVal s
      (GHC.HsStrTy s _) ->
        markExternal l GHC.AnnVal s

  -- HsWrapTy HsTyAnnotated (HsType name)
  markAST _ (GHC.HsWrapTy _ _) =
    traceM "warning: HsWrapTyy Introduced after renaming"

#if __GLASGOW_HASKELL__ <= 710
  markAST l (GHC.HsWildcardTy) = do
    markExternal l GHC.AnnVal "_"
  markAST l (GHC.HsNamedWildcardTy n) = do
    markExternal l GHC.AnnVal  (showGhc n)
#else
  markAST l (GHC.HsWildCardTy (GHC.AnonWildCard _)) = do
    markExternal l GHC.AnnVal "_"
  markAST l (GHC.HsWildCardTy (GHC.NamedWildCard n)) = do
    markExternal l GHC.AnnVal  (showGhc n)
#endif

#if __GLASGOW_HASKELL__ <= 710
  markAST l (GHC.HsQuasiQuoteTy n) = do
    markAST l n
#endif

instance Annotate WildCardAnon where
  markAST l WildCardAnon = do
    markExternal l GHC.AnnVal "_"

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
  => Annotate (GHC.HsSplice name) where
#if __GLASGOW_HASKELL__ > 710
  markAST l c =
    case c of
      GHC.HsQuasiQuote _ n _pos fs -> do
        markExternal l GHC.AnnVal
              ("[" ++ (showGhc n) ++ "|" ++ (GHC.unpackFS fs) ++ "|]")

      GHC.HsTypedSplice _n b@(GHC.L _ (GHC.HsVar n))  -> do
        markWithString GHC.AnnThIdTySplice ("$$" ++ (GHC.occNameString (GHC.occName n)))
        markLocated b
      GHC.HsTypedSplice _n b -> do
        mark GHC.AnnOpenPTE
        markLocated b
        mark GHC.AnnCloseP

      GHC.HsUntypedSplice _n b@(GHC.L _ (GHC.HsVar n))  -> do
        markWithString GHC.AnnThIdSplice ("$" ++ (GHC.occNameString (GHC.occName n)))
        markLocated b
      GHC.HsUntypedSplice _n b  -> do
        mark GHC.AnnThIdSplice
        mark GHC.AnnOpenPE
        markLocated b
        mark GHC.AnnCloseP
#else
  markAST _ c =
    case c of
      GHC.HsSplice _n b@(GHC.L _ (GHC.HsVar n))  -> do
        markWithString GHC.AnnThIdSplice   ("$" ++ (GHC.occNameString (GHC.occName n)))
        markWithString GHC.AnnThIdTySplice ("$$" ++ (GHC.occNameString (GHC.occName n)))
        markLocated b
      GHC.HsSplice _n b -> do
        mark GHC.AnnThIdSplice
        mark GHC.AnnOpenPTE
        mark GHC.AnnOpenPE
        markLocated b
        mark GHC.AnnCloseP
#endif

#if __GLASGOW_HASKELL__ > 710
#else
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
    mapM_ markLocated ns
    mark GHC.AnnDcolon
    markLocated ty
    markMaybe mdoc

-- ---------------------------------------------------------------------

instance Annotate GHC.HsDocString where
  markAST l (GHC.HsDocString s) = do
    markExternal l GHC.AnnVal (GHC.unpackFS s)

-- ---------------------------------------------------------------------
instance (GHC.DataId name,Annotate name,GHC.OutputableBndr name,GHC.HasOccName name)
  => Annotate (GHC.Pat name) where
  markAST l (GHC.WildPat _) = markExternal l GHC.AnnVal "_"
  markAST l (GHC.VarPat n)  = do
    markAST l n
  markAST _ (GHC.LazyPat p) = do
    mark GHC.AnnTilde
    markLocated p

  markAST _ (GHC.AsPat ln p) = do
    markLocated ln
    mark GHC.AnnAt
    markLocated p

  markAST _ (GHC.ParPat p) = do
    mark GHC.AnnOpenP
    markLocated p
    mark GHC.AnnCloseP

  markAST _ (GHC.BangPat p) = do
    mark GHC.AnnBang
    markLocated p

  markAST _ (GHC.ListPat ps _ _) = do
    mark GHC.AnnOpenS
    mapM_ markLocated ps
    mark GHC.AnnCloseS

  markAST _ (GHC.TuplePat pats b _) = do
    if b == GHC.Boxed then mark GHC.AnnOpenP
                      else markWithString GHC.AnnOpen "(#"
    mapM_ markLocated pats
    if b == GHC.Boxed then mark GHC.AnnCloseP
                      else markWithString GHC.AnnClose "#)"

  markAST _ (GHC.PArrPat ps _) = do
    markWithString GHC.AnnOpen "[:"
    mapM_ markLocated ps
    markWithString GHC.AnnClose ":]"

  markAST _ (GHC.ConPatIn n dets) = do
    markHsConPatDetails n dets

  markAST _ (GHC.ConPatOut {}) =
    traceM "warning: ConPatOut Introduced after renaming"

  -- ViewPat (LHsExpr id) (LPat id) (PostTc id Type)
  markAST _ (GHC.ViewPat e pat _) = do
    markLocated e
    mark GHC.AnnRarrow
    markLocated pat

  -- SplicePat (HsSplice id)
  markAST l (GHC.SplicePat s) = do
    mark GHC.AnnOpenPE
    markAST l s
    mark GHC.AnnCloseP

  -- LitPat HsLit
  markAST l (GHC.LitPat lp) = markExternal l GHC.AnnVal (hsLit2String lp)

  -- NPat (HsOverLit id) (Maybe (SyntaxExpr id)) (SyntaxExpr id)
  markAST _ (GHC.NPat ol _ _) = do
    mark GHC.AnnMinus
    markLocated ol

  -- NPlusKPat (Located id) (HsOverLit id) (SyntaxExpr id) (SyntaxExpr id)
  markAST _ (GHC.NPlusKPat ln ol _ _) = do
    markLocated ln
    markWithString GHC.AnnVal "+"  -- "+"
    markLocated ol


  markAST _ (GHC.SigPatIn pat (GHC.HsWB ty _ _ _)) = do
    markLocated pat
    mark GHC.AnnDcolon
    markLocated ty

  markAST _ (GHC.SigPatOut {}) =
    traceM "warning: SigPatOut introduced after renaming"

  -- CoPat HsAnnotated (Pat id) Type
  markAST _ (GHC.CoPat {}) =
    traceM "warning: CoPat introduced after renaming"

#if __GLASGOW_HASKELL__ <= 710
  markAST l (GHC.QuasiQuotePat p) = markAST l p
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
    GHC.HsDoublePrim (GHC.FL src _)   -> src

markHsConPatDetails :: (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
                      => GHC.Located name -> GHC.HsConPatDetails name -> IAnnotated ()
markHsConPatDetails ln dets = do
  case dets of
    GHC.PrefixCon args -> do
      markLocated ln
      mapM_ markLocated args
    GHC.RecCon (GHC.HsRecFields fs _) -> do
      markLocated ln
      mark GHC.AnnOpenC -- '{'
      mapM_ markLocated fs
      mark GHC.AnnDotdot
      mark GHC.AnnCloseC -- '}'
    GHC.InfixCon a1 a2 -> do
      markLocated a1
      markLocated ln
      markLocated a2

markHsConDeclDetails :: (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
                    =>  [GHC.Located name] -> GHC.HsConDeclDetails name -> IAnnotated ()
markHsConDeclDetails lns dets = do
  case dets of
    GHC.PrefixCon args -> mapM_ markLocated args
    GHC.RecCon fs -> do
      mark GHC.AnnOpenC
      markLocated fs
      mark GHC.AnnCloseC
    GHC.InfixCon a1 a2 -> do
      markLocated a1
      mapM_ markLocated lns
      markLocated a2

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
   => Annotate [GHC.LConDeclField name] where
  markAST _ fs = do
       mark GHC.AnnOpenC -- '{'
       mapM_ markLocated fs
       mark GHC.AnnDotdot
       mark GHC.AnnCloseC -- '}'
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

instance (GHC.DataId name,Annotate arg)
    => Annotate (GHC.HsWithBndrs name (GHC.Located arg)) where
  markAST _ (GHC.HsWB thing _ _ _) = do
    markLocated thing

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,Annotate name
         ,GHC.HasOccName name,Annotate body)
  => Annotate (GHC.Stmt name (GHC.Located body)) where

  markAST _ (GHC.LastStmt body _) = markLocated body

  markAST _ (GHC.BindStmt pat body _ _) = do
    markLocated pat
    mark GHC.AnnLarrow
    markLocated body
    mark GHC.AnnVbar -- possible in list comprehension
    markOutside GHC.AnnSemi (G GHC.AnnSemi)

  markAST _ (GHC.BodyStmt body _ _ _) = do
    markLocated body
    mark GHC.AnnVbar -- possible in list comprehension
    markOutside GHC.AnnSemi (G GHC.AnnSemi)

  markAST _ (GHC.LetStmt lb) = do
    -- return () `debug` ("markP.LetStmt entered")
    mark GHC.AnnLet
    mark GHC.AnnOpenC -- '{'
    --markOffset GHC.AnnSemi 0
    markInside GHC.AnnSemi
    markLocalBindsWithLayout lb
    mark GHC.AnnCloseC -- '}'
    -- return () `debug` ("markP.LetStmt done")
    mark GHC.AnnVbar -- possible in list comprehension
    markOutside GHC.AnnSemi AnnSemiSep

  markAST l (GHC.ParStmt pbs _ _) = do
    mapM_ (markAST l) pbs
    mark GHC.AnnVbar -- possible in list comprehension
    markOutside GHC.AnnSemi (G GHC.AnnSemi)

  markAST _ (GHC.TransStmt form stmts _b using by _ _ _) = do
    mapM_ markLocated stmts
    case form of
      GHC.ThenForm -> do
        mark GHC.AnnThen
        markLocated using
        case by of
          Just b -> mark GHC.AnnBy >> markLocated b
          Nothing -> return ()
      GHC.GroupForm -> do
        mark GHC.AnnThen
        mark GHC.AnnGroup
        case by of
          Just b -> mark GHC.AnnBy >> markLocated b
          Nothing -> return ()
        mark GHC.AnnUsing
        markLocated using
    mark GHC.AnnVbar -- possible in list comprehension
    markOutside GHC.AnnSemi (G GHC.AnnSemi)

  markAST _ (GHC.RecStmt stmts _ _ _ _ _ _ _ _) = do
    mark GHC.AnnRec
    mark GHC.AnnOpenC
    markInside GHC.AnnSemi
    mapM_ markLocated stmts
    mark GHC.AnnCloseC
    mark GHC.AnnVbar -- possible in list comprehension
    markOutside GHC.AnnSemi AnnSemiSep

-- ---------------------------------------------------------------------

instance  (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
  =>  Annotate (GHC.ParStmtBlock name name) where
  markAST _ (GHC.ParStmtBlock stmts _ns _) =
    mapM_ markLocated stmts

-- ---------------------------------------------------------------------

-- | Generate a SrcSpan that enclosed the given list
getListSrcSpan :: [GHC.Located a] -> GHC.SrcSpan
getListSrcSpan ls
  = case ls of
      []  -> GHC.noSrcSpan
      sss -> GHC.combineLocs (head sss) (last sss)

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
  => Annotate (GHC.HsLocalBinds name) where
  markAST _ lb = markHsLocalBinds lb

-- ---------------------------------------------------------------------

markHsLocalBinds :: (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
                     => (GHC.HsLocalBinds name) -> IAnnotated ()
markHsLocalBinds (GHC.HsValBinds (GHC.ValBindsIn binds sigs)) =
    applyListAnnotations (prepareListAnnotation (GHC.bagToList binds)
                       ++ prepareListAnnotation sigs
                         )
markHsLocalBinds (GHC.HsValBinds (GHC.ValBindsOut {}))
   = traceM "warning: ValBindsOut introduced after renaming"

markHsLocalBinds (GHC.HsIPBinds (GHC.IPBinds binds _)) = mapM_ markLocated (reverse binds)
markHsLocalBinds (GHC.EmptyLocalBinds)                 = return ()

-- ---------------------------------------------------------------------

markMatchGroup :: (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name,
                                               Annotate body)
                   => GHC.SrcSpan -> GHC.MatchGroup name (GHC.Located body)
                   -> IAnnotated ()
markMatchGroup _ (GHC.MG matches _ _ _)
  = markListWithLayout matches

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name,
                                               Annotate body)
  => Annotate [GHC.Located (GHC.Match name (GHC.Located body))] where
  markAST _ ls = mapM_ markLocated ls

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
  => Annotate (GHC.HsExpr name) where
  markAST l (GHC.HsVar n)           = markAST l n
  markAST l (GHC.HsIPVar (GHC.HsIPName v))         =
    markExternal l GHC.AnnVal ("?" ++ GHC.unpackFS v)
  markAST l (GHC.HsOverLit ov)     = markAST l ov
  markAST l (GHC.HsLit lit)           = markAST l lit

  markAST l (GHC.HsLam match)       = do
    mark GHC.AnnLam
    markMatchGroup l match

  markAST l (GHC.HsLamCase _ match) = do
    mark GHC.AnnLam
    mark GHC.AnnCase
    mark GHC.AnnOpenC
    addContext Case (markMatchGroup l match)
    mark GHC.AnnCloseC

  markAST _ (GHC.HsApp e1 e2) = do
    markLocated e1
    markLocated e2

  markAST _ (GHC.OpApp e1 e2 _ e3) = do
    markLocated e1
    markLocated e2
    markLocated e3

  markAST _ (GHC.NegApp e _) = do
    mark GHC.AnnMinus
    markLocated e

  markAST _ (GHC.HsPar e) = do
    mark GHC.AnnOpenP -- '('
    markLocated e
    mark GHC.AnnCloseP -- ')'

  markAST _ (GHC.SectionL e1 e2) = do
    markLocated e1
    markLocated e2

  markAST _ (GHC.SectionR e1 e2) = do
    markLocated e1
    markLocated e2

  markAST _ (GHC.ExplicitTuple args b) = do
    if b == GHC.Boxed then mark GHC.AnnOpenP
                      else markWithString GHC.AnnOpen "(#"

    mapM_ markLocated args

    if b == GHC.Boxed then mark GHC.AnnCloseP
                      else markWithString GHC.AnnClose "#)"

  markAST l (GHC.HsCase e1 matches) = do
    mark GHC.AnnCase
    markLocated e1
    mark GHC.AnnOf
    mark GHC.AnnOpenC
    markInside GHC.AnnSemi
    addContext Case (markMatchGroup l matches)
    mark GHC.AnnCloseC

  markAST _ (GHC.HsIf _ e1 e2 e3) = do
    mark GHC.AnnIf
    markLocated e1
    markOffset GHC.AnnSemi 0
    mark GHC.AnnThen
    markLocated e2
    markOffset GHC.AnnSemi 1
    mark GHC.AnnElse
    markLocated e3

  markAST _ (GHC.HsMultiIf _ rhs) = do
    mark GHC.AnnIf
    addContext MultiIf (mapM_ markLocated rhs)

  markAST _ (GHC.HsLet binds e) = do
    setLayoutFlag (do -- Make sure the 'in' gets indented too
      mark GHC.AnnLet
      mark GHC.AnnOpenC
      markInside GHC.AnnSemi
      markLocalBindsWithLayout binds
      mark GHC.AnnCloseC
      mark GHC.AnnIn
      markLocated e)

  markAST _ (GHC.HsDo cts es _) = do
    mark GHC.AnnDo
    mark GHC.AnnMdo
    let (ostr,cstr,_isComp) =
          if isListComp cts
            then case cts of
                   GHC.PArrComp -> ("[:",":]",True)
                   _            -> ("[",  "]",True)
            else ("{","}",False)

    markWithString GHC.AnnOpen ostr
    mark GHC.AnnOpenS
    mark GHC.AnnOpenC
    markInside GHC.AnnSemi
    if isListComp cts
      then do
        markLocated (last es)
        mark GHC.AnnVbar
        mapM_ markLocated (init es)
      else do
        markListWithLayout es
    mark GHC.AnnCloseS
    mark GHC.AnnCloseC
    markWithString GHC.AnnClose cstr

  markAST _ (GHC.ExplicitList _ _ es) = do
    mark GHC.AnnOpenS
    mapM_ markLocated es
    mark GHC.AnnCloseS

  markAST _ (GHC.ExplicitPArr _ es)   = do
    markWithString GHC.AnnOpen "[:"
    mapM_ markLocated es
    markWithString GHC.AnnClose ":]"

  markAST _ (GHC.RecordCon n _ (GHC.HsRecFields fs _)) = do
    markLocated n
    mark GHC.AnnOpenC
    mapM_ markLocated fs
    mark GHC.AnnDotdot
    mark GHC.AnnCloseC

  markAST _ (GHC.RecordUpd e (GHC.HsRecFields fs _) _cons _ _) = do
    markLocated e
    mark GHC.AnnOpenC
    mapM_ markLocated fs
    mark GHC.AnnDotdot
    mark GHC.AnnCloseC

  markAST _ (GHC.ExprWithTySig e typ _) = do
    markLocated e
    mark GHC.AnnDcolon
    markLocated typ

  markAST _ (GHC.ExprWithTySigOut e typ) = do
    markLocated e
    mark GHC.AnnDcolon
    markLocated typ

  markAST _ (GHC.ArithSeq _ _ seqInfo) = do
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

  markAST _ (GHC.PArrSeq _ seqInfo) = do
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

  markAST _ (GHC.HsSCC src csFStr e) = do
    markWithString GHC.AnnOpen src -- "{-# SCC"
#if __GLASGOW_HASKELL__ <= 710
    markWithString GHC.AnnVal (GHC.unpackFS csFStr)
    markWithString GHC.AnnValStr ("\"" ++ GHC.unpackFS csFStr ++ "\"")
#else
    markWithString GHC.AnnVal (fst csFStr)
    markWithString GHC.AnnValStr (fst csFStr)
#endif
    markWithString GHC.AnnClose "#-}"
    markLocated e

  markAST _ (GHC.HsCoreAnn src csFStr e) = do
    markWithString GHC.AnnOpen src -- "{-# CORE"
#if __GLASGOW_HASKELL__ <= 710
    markWithString GHC.AnnVal ("\"" ++ GHC.unpackFS csFStr ++ "\"")
#else
    markWithString GHC.AnnVal (fst csFStr)
#endif
    markWithString GHC.AnnClose "#-}"
    markLocated e
  -- TODO: make monomorphic
  markAST _ (GHC.HsBracket (GHC.VarBr _single v)) = do
    mark GHC.AnnSimpleQuote
    mark GHC.AnnThTyQuote
    markLocatedFromKw GHC.AnnName v
  markAST _ (GHC.HsBracket (GHC.DecBrL ds)) = do
    markWithString GHC.AnnOpen "[d|"
    mark GHC.AnnOpenC
    mapM_ markLocated ds
    mark GHC.AnnCloseC
    markWithString GHC.AnnClose "|]"
  -- Introduced after the renamer
  markAST _ (GHC.HsBracket (GHC.DecBrG _)) =
    traceM "warning: DecBrG introduced after renamer"
  markAST _ (GHC.HsBracket (GHC.ExpBr e)) = do
--    markWithString GHC.AnnOpen "[|"
    workOutString GHC.AnnOpen
      (\ss -> if spanLength ss == 2
                then "[|"
                else "[e|")
    markLocated e
    markWithString GHC.AnnClose "|]"
  markAST _ (GHC.HsBracket (GHC.TExpBr e)) = do
    markWithString GHC.AnnOpen "[||"
    markLocated e
    markWithString GHC.AnnClose "||]"
  markAST _ (GHC.HsBracket (GHC.TypBr e)) = do
    markWithString GHC.AnnOpen "[t|"
    markLocated e
    markWithString GHC.AnnClose "|]"
  markAST _ (GHC.HsBracket (GHC.PatBr e)) = do
    markWithString GHC.AnnOpen  "[p|"
    markLocated e
    markWithString GHC.AnnClose "|]"

  markAST _ (GHC.HsRnBracketOut _ _) =
    traceM "warning: HsRnBracketOut introduced after renamer"
  markAST _ (GHC.HsTcBracketOut _ _) =
    traceM "warning: HsTcBracketOut introduced after renamer"

#if __GLASGOW_HASKELL__ > 710
  markAST l (GHC.HsSpliceE e) = do
    mark GHC.AnnOpenPE
    markAST l e
    mark GHC.AnnCloseP
#else
  markAST l (GHC.HsSpliceE _ e) = do
    mark GHC.AnnOpenPE
    markAST l e
    mark GHC.AnnCloseP

  markAST l (GHC.HsQuasiQuoteE e) = do
    markAST l e
#endif

  markAST _ (GHC.HsProc p c) = do
    mark GHC.AnnProc
    markLocated p
    mark GHC.AnnRarrow
    markLocated c

  markAST _ (GHC.HsStatic e) = do
    mark GHC.AnnStatic
    markLocated e

  markAST _ (GHC.HsArrApp e1 e2 _ _ _) = do
    markLocated e1
    -- only one of the next 4 will be resent
    mark GHC.Annlarrowtail
    mark GHC.Annrarrowtail
    mark GHC.AnnLarrowtail
    mark GHC.AnnRarrowtail

    markLocated e2

  markAST _ (GHC.HsArrForm e _ cs) = do
    markWithString GHC.AnnOpen "(|"
    markLocated e
    mapM_ markLocated cs
    markWithString GHC.AnnClose "|)"

  markAST _ (GHC.HsTick _ _) = return ()
  markAST _ (GHC.HsBinTick _ _ _) = return ()

  markAST _ (GHC.HsTickPragma src (str,(v1,v2),(v3,v4)) e) = do
    -- '{-# GENERATED' STRING INTEGER ':' INTEGER '-' INTEGER ':' INTEGER '#-}'
    markWithString       GHC.AnnOpen  src
#if __GLASGOW_HASKELL__ <= 710
    markOffsetWithString GHC.AnnVal 0 (show (GHC.unpackFS str)) -- STRING
#else
    markOffsetWithString GHC.AnnVal 0 (fst str) -- STRING
#endif
    markOffsetWithString GHC.AnnVal 1 (show v1) -- INTEGER
    markOffset GHC.AnnColon 0 -- ':'
    markOffsetWithString GHC.AnnVal 2 (show v2) -- INTEGER
    mark   GHC.AnnMinus   -- '-'
    markOffsetWithString GHC.AnnVal 3 (show v3) -- INTEGER
    markOffset GHC.AnnColon 1 -- ':'
    markOffsetWithString GHC.AnnVal 4 (show v4) -- INTEGER
    markWithString   GHC.AnnClose  "#-}"
    markLocated e

  markAST l (GHC.EWildPat) = do
    markExternal l GHC.AnnVal "_"

  markAST _ (GHC.EAsPat ln e) = do
    markLocated ln
    mark GHC.AnnAt
    markLocated e

  markAST _ (GHC.EViewPat e1 e2) = do
    markLocated e1
    mark GHC.AnnRarrow
    markLocated e2

  markAST _ (GHC.ELazyPat e) = do
    mark GHC.AnnTilde
    markLocated e

  markAST _ (GHC.HsType ty) = markLocated ty

  markAST _ (GHC.HsWrap _ _) =
    traceM "warning: HsWrap introduced after renaming"
  markAST _ (GHC.HsUnboundVar _) =
    traceM "warning: HsUnboundVar introduced after renaming"

instance Annotate GHC.HsLit where
  markAST l lit = markExternal l GHC.AnnVal (hsLit2String lit)
-- ---------------------------------------------------------------------

-- |Used for declarations that need to be aligned together, e.g. in a
-- do or let .. in statement/expr
instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
  => Annotate ([GHC.ExprLStmt name]) where
  markAST _ ls = mapM_ markLocated ls

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
  => Annotate (GHC.HsTupArg name) where
  markAST _ (GHC.Present e) = do
    markLocated e

  markAST _ (GHC.Missing _) = do
    mark GHC.AnnComma

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
  => Annotate (GHC.HsCmdTop name) where
  markAST _ (GHC.HsCmdTop cmd _ _ _) = markLocated cmd

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
   => Annotate (GHC.HsCmd name) where
  markAST _ (GHC.HsCmdArrApp e1 e2 _ _ _) = do
    markLocated e1
    -- only one of the next 4 will be resent
    mark GHC.Annlarrowtail
    mark GHC.Annrarrowtail
    mark GHC.AnnLarrowtail
    mark GHC.AnnRarrowtail

    markLocated e2

  markAST _ (GHC.HsCmdArrForm e _mf cs) = do
    markWithString GHC.AnnOpen "(|"
    -- This may be an infix operation
    applyListAnnotations (prepareListAnnotation [e]
                         ++ prepareListAnnotation cs)
    -- markLocated e
    -- mapM_ markLocated cs
    markWithString GHC.AnnClose "|)"

  markAST _ (GHC.HsCmdApp e1 e2) = do
    markLocated e1
    markLocated e2

  markAST l (GHC.HsCmdLam match) = do
    mark GHC.AnnLam
    markMatchGroup l match

  markAST _ (GHC.HsCmdPar e) = do
    mark GHC.AnnOpenP
    markLocated e
    mark GHC.AnnCloseP -- ')'

  markAST l (GHC.HsCmdCase e1 matches) = do
    mark GHC.AnnCase
    markLocated e1
    mark GHC.AnnOf
    mark GHC.AnnOpenC
    addContext Case (markMatchGroup l matches)
    mark GHC.AnnCloseC

  markAST _ (GHC.HsCmdIf _ e1 e2 e3) = do
    mark GHC.AnnIf
    markLocated e1
    markOffset GHC.AnnSemi 0
    mark GHC.AnnThen
    markLocated e2
    markOffset GHC.AnnSemi 1
    mark GHC.AnnElse
    markLocated e3

  markAST _ (GHC.HsCmdLet binds e) = do
    mark GHC.AnnLet
    mark GHC.AnnOpenC
    markLocalBindsWithLayout binds
    mark GHC.AnnCloseC
    mark GHC.AnnIn
    markLocated e

  markAST _ (GHC.HsCmdDo es _) = do
    mark GHC.AnnDo
    mark GHC.AnnOpenC
    markListWithLayout es
    mark GHC.AnnCloseC

  markAST _ (GHC.HsCmdCast {}) =
    traceM "warning: HsCmdCast introduced after renaming"


-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
  => Annotate [GHC.Located (GHC.StmtLR name name (GHC.LHsCmd name))] where
  markAST _ ls = mapM_ markLocated ls

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
     => Annotate (GHC.TyClDecl name) where

  markAST l (GHC.FamDecl famdecl) = markAST l famdecl

  markAST _ (GHC.SynDecl ln (GHC.HsQTvs _ tyvars) typ _) = do
    -- There may be arbitrary parens around parts of the constructor that are
    -- infix.
    -- Turn these into comments so that they feed into the right place automatically
    annotationsToComments [GHC.AnnOpenP,GHC.AnnCloseP]
    mark GHC.AnnType
    -- ln may be used infix, in which case rearrange the order. It may be
    -- simplest to just sort ln:tyvars
    applyListAnnotations (prepareListAnnotation [ln]
                         ++ prepareListAnnotation tyvars)
    -- markMany GHC.AnnCloseP
    mark GHC.AnnEqual
    markLocated typ
    markOutside GHC.AnnSemi (G GHC.AnnSemi)

  markAST _ (GHC.DataDecl ln (GHC.HsQTvs _ns tyVars)
                (GHC.HsDataDefn _ ctx mctyp mk cons mderivs) _) = do
    mark GHC.AnnData
    mark GHC.AnnNewtype
    markMaybe mctyp
    markLocated ctx
    mark GHC.AnnDarrow
    markTyClass ln tyVars
    mark GHC.AnnDcolon
    markMaybe mk
    mark GHC.AnnEqual
    mark GHC.AnnWhere
    mark GHC.AnnOpenC
    mapM_ markLocated cons
    markMaybe mderivs
    mark GHC.AnnCloseC
    markOutside GHC.AnnSemi (G GHC.AnnSemi)

  -- -----------------------------------

  markAST _ (GHC.ClassDecl ctx ln (GHC.HsQTvs _ns tyVars) fds
                          sigs meths ats atdefs docs _) = do
    mark GHC.AnnClass
    markLocated ctx

    markTyClass ln tyVars

    mark GHC.AnnVbar
    mapM_ markLocated fds
    mark GHC.AnnWhere
    mark GHC.AnnOpenC -- '{'
    markInside GHC.AnnSemi
    applyListAnnotations (prepareListAnnotation sigs
                       ++ prepareListAnnotation (GHC.bagToList meths)
                       ++ prepareListAnnotation ats
                       ++ prepareListAnnotation atdefs
                       ++ prepareListAnnotation docs
                         )
    mark GHC.AnnCloseC -- '}'
    markOutside (GHC.AnnSemi) AnnSemiSep

-- ---------------------------------------------------------------------

markTyClass :: (Annotate a, Annotate ast)
                => GHC.Located a -> [GHC.Located ast] -> IAnnotated ()
markTyClass ln tyVars = do
    markMany GHC.AnnOpenP
    applyListAnnotations (prepareListAnnotation [ln]
                      ++ prepareListAnnotation (take 2 tyVars))
    markMany GHC.AnnCloseP
    mapM_ markLocated (drop 2 tyVars)

-- ---------------------------------------------------------------------

instance (GHC.DataId name,Annotate name, GHC.OutputableBndr name,GHC.HasOccName name)
   => Annotate (GHC.FamilyDecl name) where
  markAST _ (GHC.FamilyDecl info ln (GHC.HsQTvs _ tyvars) mkind) = do
    mark GHC.AnnType
    mark GHC.AnnData
    mark GHC.AnnFamily
    mark GHC.AnnOpenP
    applyListAnnotations (prepareListAnnotation [ln]
                         ++ prepareListAnnotation tyvars)
    mark GHC.AnnCloseP
    mark GHC.AnnDcolon
    markMaybe mkind
    mark GHC.AnnWhere
    mark GHC.AnnOpenC -- {
    case info of
#if __GLASGOW_HASKELL__ > 710
      GHC.ClosedTypeFamily (Just eqns) -> mapM_ markLocated eqns
#else
      GHC.ClosedTypeFamily eqns -> mapM_ markLocated eqns
#endif
      _ -> return ()
    mark GHC.AnnCloseC -- }
    markOutside GHC.AnnSemi AnnSemiSep

-- ---------------------------------------------------------------------

instance (GHC.DataId name,Annotate name,GHC.OutputableBndr name,GHC.HasOccName name)
  => Annotate (GHC.TyFamInstEqn name) where
  markAST _ (GHC.TyFamEqn ln (GHC.HsWB pats _ _ _) typ) = do
    mark GHC.AnnOpenP
    applyListAnnotations (prepareListAnnotation [ln]
                         ++ prepareListAnnotation pats)
    mark GHC.AnnCloseP
    mark GHC.AnnEqual
    markLocated typ

-- ---------------------------------------------------------------------

instance (GHC.DataId name,Annotate name,GHC.OutputableBndr name,GHC.HasOccName name)
  => Annotate (GHC.TyFamDefltEqn name) where
  markAST _ (GHC.TyFamEqn ln (GHC.HsQTvs _ns bndrs) typ) = do
    mark GHC.AnnType
    mark GHC.AnnInstance
    markLocated ln
    mapM_ markLocated bndrs
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
  => GHC.SrcSpan -> GHC.HsDataDefn name -> IAnnotated ()
markDataDefn _ (GHC.HsDataDefn _ ctx typ mk cons mderivs) = do
  markLocated ctx
  markMaybe typ
  markMaybe mk
  mapM_ markLocated cons
  case mderivs of
    Nothing -> return ()
    Just d -> markLocated d

-- ---------------------------------------------------------------------

-- Note: GHC.HsContext name aliases to here too
instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
     => Annotate [GHC.LHsType name] where
  markAST _ ts = do
    mark GHC.AnnDeriving
    markMany GHC.AnnOpenP -- may be nested parens around context
    mapM_ markLocated ts
    markMany GHC.AnnCloseP -- may be nested parens around context
    -- mark GHC.AnnDarrow
    markOutside GHC.AnnDarrow (G GHC.AnnDarrow)

-- ---------------------------------------------------------------------

instance (GHC.DataId name,Annotate name,GHC.OutputableBndr name,GHC.HasOccName name)
      => Annotate (GHC.ConDecl name) where
  markAST _ (GHC.ConDecl lns _expr (GHC.HsQTvs _ns bndrs) ctx
                         dets res _ depc_syntax) = do
    case res of
      GHC.ResTyH98 -> do

        mark GHC.AnnForall
        mapM_ markLocated bndrs
        mark GHC.AnnDot

        markLocated ctx
        mark GHC.AnnDarrow
        case dets of
          GHC.InfixCon _ _ -> return ()
          _ -> mapM_ markLocated lns

        markHsConDeclDetails lns dets

      GHC.ResTyGADT ls ty -> do
        -- only print names if not infix
        case dets of
          GHC.InfixCon _ _ -> return ()
          _ -> mapM_ markLocated lns

        when depc_syntax ( do
          markHsConDeclDetails lns dets
          mark GHC.AnnDcolon
          markMany GHC.AnnOpenP
          )

        when (not depc_syntax) ( do
          mark GHC.AnnDcolon
          markLocated (GHC.L ls (ResTyGADTHook bndrs))
          markMany GHC.AnnOpenP
          markLocated ctx
          mark GHC.AnnDarrow
          markHsConDeclDetails lns dets )

        markLocated ty

        markMany GHC.AnnCloseP


    mark GHC.AnnVbar
    markOutside GHC.AnnSemi (G GHC.AnnSemi)

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,GHC.HasOccName name,Annotate name)
  => Annotate (ResTyGADTHook name) where
  markAST _ (ResTyGADTHook bndrs) = do
    mark GHC.AnnForall
    mapM_ markLocated bndrs
    mark GHC.AnnDot

-- ---------------------------------------------------------------------

instance (Annotate name, GHC.DataId name, GHC.OutputableBndr name,GHC.HasOccName name)
  => Annotate (GHC.HsRecField name (GHC.LPat name)) where
  markAST _ (GHC.HsRecField n e _) = do
    markLocated n
    mark GHC.AnnEqual
    markLocated e


instance (Annotate name, GHC.DataId name, GHC.OutputableBndr name,GHC.HasOccName name)
  => Annotate (GHC.HsRecField name (GHC.LHsExpr name)) where
  markAST _ (GHC.HsRecField n e _) = do
    markLocated n
    mark GHC.AnnEqual
    markLocated e

-- ---------------------------------------------------------------------

instance (GHC.DataId name,Annotate name)
    => Annotate (GHC.FunDep (GHC.Located name)) where

  markAST _ (ls,rs) = do
    mapM_ markLocated ls
    mark GHC.AnnRarrow
    mapM_ markLocated rs

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
