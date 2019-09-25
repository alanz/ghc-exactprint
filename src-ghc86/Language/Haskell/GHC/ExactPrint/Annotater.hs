{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

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

module Language.Haskell.GHC.ExactPrint.Annotater
       (
         annotate
       , AnnotationF(..)
       , Annotated
       , Annotate(..)
       , withSortKeyContextsHelper
       ) where


import Language.Haskell.GHC.ExactPrint.AnnotateTypes
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
--  import qualified HsDoc          as GHC
import qualified Name           as GHC
import qualified RdrName        as GHC
import qualified Outputable     as GHC

import Control.Monad.Identity
import Data.Data
import Data.Maybe

import qualified Data.Set as Set

import Debug.Trace


{-# ANN module "HLint: ignore Eta reduce" #-}
{-# ANN module "HLint: ignore Redundant do" #-}
{-# ANN module "HLint: ignore Reduce duplication" #-}
-- ---------------------------------------------------------------------

class Data ast => Annotate ast where
  markAST :: GHC.SrcSpan -> ast -> Annotated ()

-- ---------------------------------------------------------------------

-- | Construct a syntax tree which represent which KeywordIds must appear
-- where.
annotate :: (Annotate ast) => GHC.Located ast -> Annotated ()
annotate = markLocated

-- ---------------------------------------------------------------------

-- | Constructs a syntax tree which contains information about which
-- annotations are required by each element.
markLocated :: (Annotate ast) => GHC.Located ast -> Annotated ()
markLocated ast =
  case cast ast :: Maybe (GHC.LHsDecl GHC.GhcPs) of
    Just d  -> markLHsDecl d
    Nothing -> withLocated ast markAST

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

-- ---------------------------------------------------------------------

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


-- ---------------------------------------------------------------------

markListWithLayout :: Annotate ast => [GHC.Located ast] -> Annotated ()
markListWithLayout ls =
  setLayoutFlag $ markList ls

-- ---------------------------------------------------------------------

markList :: Annotate ast => [GHC.Located ast] -> Annotated ()
markList ls =
  setContext (Set.singleton NoPrecedingSpace)
   $ markListWithContexts' listContexts' ls

markLocalBindsWithLayout :: GHC.HsLocalBinds GHC.GhcPs -> Annotated ()
markLocalBindsWithLayout binds = markHsLocalBinds binds

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

-- ---------------------------------------------------------------------

instance Annotate (GHC.HsModule GHC.GhcPs) where
  markAST _ (GHC.HsModule mmn mexp imps decs mdepr _haddock) = do

    case mmn of
      Nothing -> return ()
      Just (GHC.L ln mn) -> do
        mark GHC.AnnModule
        markExternal ln GHC.AnnVal (GHC.moduleNameString mn)

        forM_ mdepr markLocated
        forM_ mexp markLocated

        mark GHC.AnnWhere

    markOptional GHC.AnnOpenC -- Possible '{'
    markManyOptional GHC.AnnSemi -- possible leading semis
    setContextLevel (Set.singleton TopLevel) 2 $ markListWithLayout imps

    setContextLevel (Set.singleton TopLevel) 2 $ markListWithLayout decs

    markOptional GHC.AnnCloseC -- Possible '}'

    markEOF

-- ---------------------------------------------------------------------

instance Annotate GHC.WarningTxt where
  markAST _ (GHC.WarningTxt (GHC.L _ txt) lss) = do
    markAnnOpen txt "{-# WARNING"
    mark GHC.AnnOpenS
    markListIntercalate lss
    mark GHC.AnnCloseS
    markWithString GHC.AnnClose "#-}"

  markAST _ (GHC.DeprecatedTxt (GHC.L _ txt) lss) = do
    markAnnOpen txt "{-# DEPRECATED"
    mark GHC.AnnOpenS
    markListIntercalate lss
    mark GHC.AnnCloseS
    markWithString GHC.AnnClose "#-}"

-- ---------------------------------------------------------------------

instance Annotate GHC.StringLiteral where
  markAST l (GHC.StringLiteral src fs) = do
    markExternalSourceText l src (show (GHC.unpackFS fs))
    inContext (Set.fromList [Intercalate]) $ mark GHC.AnnComma

-- ---------------------------------------------------------------------

instance Annotate (GHC.SourceText,GHC.FastString) where
  markAST l (src,fs) = do
    markExternalSourceText l src (show (GHC.unpackFS fs))

-- ---------------------------------------------------------------------

instance Annotate [GHC.LIE GHC.GhcPs] where
   markAST _ ls = do
     inContext (Set.singleton HasHiding) $ mark GHC.AnnHiding -- in an import decl
     mark GHC.AnnOpenP -- '('
     -- Can't use markListIntercalate, there can be trailing commas, but only in imports.
     markListIntercalateWithFunLevel markLocated 2 ls

     mark GHC.AnnCloseP -- ')'

instance Annotate (GHC.IE GHC.GhcPs) where
  markAST _ ie = do

    case ie of
        GHC.IEVar _ ln -> markLocated ln

        GHC.IEThingAbs _ ln -> do
          setContext (Set.singleton PrefixOp) $ markLocated ln

        GHC.IEThingWith _ ln wc ns _lfs -> do
          setContext (Set.singleton PrefixOp) $ markLocated ln
          mark GHC.AnnOpenP
          case wc of
            GHC.NoIEWildcard ->
              unsetContext Intercalate $ setContext (Set.fromList [PrefixOp])
                $ markListIntercalate ns
            GHC.IEWildcard n -> do
              setContext (Set.fromList [PrefixOp,Intercalate])
                $ mapM_ markLocated (take n ns)
              mark GHC.AnnDotdot
              case drop n ns of
                [] -> return ()
                ns' -> do
                  mark GHC.AnnComma
                  unsetContext Intercalate $ setContext (Set.fromList [PrefixOp])
                    $ markListIntercalate ns'
          mark GHC.AnnCloseP

        (GHC.IEThingAll _ ln) -> do
          setContext (Set.fromList [PrefixOp]) $ markLocated ln
          mark GHC.AnnOpenP
          mark GHC.AnnDotdot
          mark GHC.AnnCloseP

        (GHC.IEModuleContents _ (GHC.L lm mn)) -> do
          mark GHC.AnnModule
          markExternal lm GHC.AnnVal (GHC.moduleNameString mn)

        -- Only used in Haddock mode so we can ignore them.
        (GHC.IEGroup {})    -> return ()

        (GHC.IEDoc {})      -> return ()

        (GHC.IEDocNamed {}) -> return ()
        GHC.XIE x -> error $ "got XIE for :" ++ showGhc x
    ifInContext (Set.fromList [Intercalate])
      (mark         GHC.AnnComma)
      (markOptional GHC.AnnComma)

-- ---------------------------------------------------------------------

instance Annotate (GHC.IEWrappedName GHC.RdrName) where
  markAST _ (GHC.IEName ln) = do
    unsetContext Intercalate $ setContext (Set.fromList [PrefixOp])
      $ markLocated ln
    inContext (Set.fromList [Intercalate]) $ mark GHC.AnnComma
  markAST _ (GHC.IEPattern ln) = do
    mark GHC.AnnPattern
    setContext (Set.singleton PrefixOp) $ markLocated ln
    inContext (Set.fromList [Intercalate]) $ mark GHC.AnnComma
  markAST _ (GHC.IEType ln) = do
    mark GHC.AnnType
    setContext (Set.singleton PrefixOp) $ markLocated ln
    inContext (Set.fromList [Intercalate]) $ mark GHC.AnnComma

-- ---------------------------------------------------------------------

isSymRdr :: GHC.RdrName -> Bool
isSymRdr n = GHC.isSymOcc (GHC.rdrNameOcc n) || rdrName2String n == "."

instance Annotate GHC.RdrName where
  markAST l n = do
    let
      str = rdrName2String n
      isSym = isSymRdr n
      canParen = isSym
      doNormalRdrName = do
        let str' = case str of
              -- TODO: unicode support?
                        "forall" -> if spanLength l == 1 then "∀" else str
                        _ -> str

        let
          markParen :: GHC.AnnKeywordId -> Annotated ()
          markParen pa = do
            if canParen
              then ifInContext (Set.singleton PrefixOp)
                                       (mark         pa) -- '('
                                       (markOptional pa)
              else if isSym
                then ifInContext (Set.singleton PrefixOpDollar)
                       (mark pa)
                       (markOptional pa)
                else markOptional pa

        markOptional GHC.AnnSimpleQuote
        markParen GHC.AnnOpenP
        unless isSym $ inContext (Set.fromList [InfixOp]) $ markOffset GHC.AnnBackquote 0
        cnt  <- countAnns GHC.AnnVal
        case cnt of
          0 -> markExternal l GHC.AnnVal str'
          1 -> markWithString GHC.AnnVal str'
          _ -> traceM $ "Printing RdrName, more than 1 AnnVal:" ++ showGhc (l,n)
        unless isSym $ inContext (Set.fromList [InfixOp]) $ markOffset GHC.AnnBackquote 1
        markParen GHC.AnnCloseP

    case n of
      GHC.Unqual _ -> doNormalRdrName
      GHC.Qual _ _ -> doNormalRdrName
      GHC.Orig _ _ -> if str == "~"
                        then doNormalRdrName
                        -- then error $ "GHC.orig:(isSym,canParen)=" ++ show (isSym,canParen)
                        else markExternal l GHC.AnnVal str
      -- GHC.Orig _ _ -> markExternal l GHC.AnnVal str
      -- GHC.Orig _ _ -> error $ "GHC.orig:str=[" ++ str ++ "]"
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
           let cnt = length $ filter (==',') str
           replicateM_ cnt (mark GHC.AnnCommaTuple)
           markWithString GHC.AnnClose  "#)"-- '#)'
         "[::]" -> do
           markWithString GHC.AnnOpen  "[:" -- '[:'
           markWithString GHC.AnnClose ":]" -- ':]'
         "->" -> do
           mark GHC.AnnOpenP -- '('
           mark GHC.AnnRarrow
           mark GHC.AnnCloseP -- ')'
         -- "~#"  -> do
         --   mark GHC.AnnOpenP -- '('
         --   mark GHC.AnnTildehsh
         --   mark GHC.AnnCloseP
         "*"  -> do
           markExternal l GHC.AnnVal str
         "★"  -> do -- Note: unicode star
           markExternal l GHC.AnnVal str
         ":"  -> do
           -- Note: The OccName for ":" has the following attributes (via occAttributes)
           -- (d, Data DataSym Sym Val )
           -- consDataConName   = mkWiredInDataConName BuiltInSyntax gHC_TYPES (fsLit ":") consDataConKey consDataCon
           doNormalRdrName
           -- trace ("RdrName.checking :" ++ (occAttributes $ GHC.occName n)) doNormalRdrName
         ('(':',':_) -> do
           mark GHC.AnnOpenP
           let cnt = length $ filter (==',') str
           replicateM_ cnt (mark GHC.AnnCommaTuple)
           mark GHC.AnnCloseP -- ')'
         _ -> do
            let isSym' = isSymRdr  (GHC.nameRdrName n')
            when isSym' $ mark GHC.AnnOpenP -- '('
            markWithString GHC.AnnVal str
            when isSym $ mark GHC.AnnCloseP -- ')'
    inContext (Set.fromList [Intercalate]) $ mark GHC.AnnComma `debug` ("AnnComma in RdrName")

-- ---------------------------------------------------------------------

instance Annotate (GHC.ImportDecl GHC.GhcPs) where
 markAST _ imp@(GHC.ImportDecl _ msrc modname mpkg _src safeflag qualFlag _impl _as hiding) = do

   -- 'import' maybe_src maybe_safe optqualified maybe_pkg modid maybeas maybeimpspec
   mark GHC.AnnImport

   -- "{-# SOURCE" and "#-}"
   case msrc of
     GHC.SourceText _txt -> do
       markAnnOpen msrc "{-# SOURCE"
       markWithString GHC.AnnClose "#-}"
     GHC.NoSourceText -> return ()
   when safeflag (mark GHC.AnnSafe)
   when qualFlag (unsetContext TopLevel $ mark GHC.AnnQualified)
   case mpkg of
    Just (GHC.StringLiteral (GHC.SourceText srcPkg) _) ->
      markWithString GHC.AnnPackageName srcPkg
    _ -> return ()

   markLocated modname

   case GHC.ideclAs imp of
      Nothing -> return ()
      Just mn -> do
          mark GHC.AnnAs
          markLocated mn

   case hiding of
     Nothing -> return ()
     Just (isHiding,lie) -> do
       if isHiding
         then setContext (Set.singleton HasHiding) $
                markLocated lie
         else markLocated lie
   markTrailingSemi

 markAST _ (GHC.XImportDecl x) = error $ "got XImportDecl for:" ++ showGhc x

-- ---------------------------------------------------------------------

instance Annotate GHC.ModuleName where
   markAST l mname =
    markExternal l GHC.AnnVal (GHC.moduleNameString mname)

-- ---------------------------------------------------------------------

markLHsDecl :: GHC.LHsDecl GHC.GhcPs -> Annotated ()
markLHsDecl (GHC.L l decl) =
    case decl of
      GHC.TyClD _ d       -> markLocated (GHC.L l d)
      GHC.InstD _ d       -> markLocated (GHC.L l d)
      GHC.DerivD _ d      -> markLocated (GHC.L l d)
      GHC.ValD _ d        -> markLocated (GHC.L l d)
      GHC.SigD _ d        -> markLocated (GHC.L l d)
      GHC.DefD _ d        -> markLocated (GHC.L l d)
      GHC.ForD _ d        -> markLocated (GHC.L l d)
      GHC.WarningD _ d    -> markLocated (GHC.L l d)
      GHC.AnnD _ d        -> markLocated (GHC.L l d)
      GHC.RuleD _ d       -> markLocated (GHC.L l d)
      GHC.SpliceD _ d     -> markLocated (GHC.L l d)
      GHC.DocD _ d        -> markLocated (GHC.L l d)
      GHC.RoleAnnotD _ d  -> markLocated (GHC.L l d)
      GHC.XHsDecl x  -> error $ "got XHsDecl for:" ++ showGhc x

instance Annotate (GHC.HsDecl GHC.GhcPs) where
  markAST l d = markLHsDecl (GHC.L l d)

-- ---------------------------------------------------------------------

instance Annotate (GHC.RoleAnnotDecl GHC.GhcPs) where
  markAST _ (GHC.RoleAnnotDecl _ ln mr) = do
    mark GHC.AnnType
    mark GHC.AnnRole
    markLocated ln
    mapM_ markLocated mr
  markAST _ (GHC.XRoleAnnotDecl x) = error $ "got XRoleAnnotDecl for:" ++ showGhc x

instance Annotate (Maybe GHC.Role) where
  markAST l Nothing  = markExternal l GHC.AnnVal "_"
  markAST l (Just r) = markExternal l GHC.AnnVal (GHC.unpackFS $ GHC.fsFromRole r)

-- ---------------------------------------------------------------------

instance Annotate (GHC.SpliceDecl GHC.GhcPs) where
  markAST _ (GHC.SpliceDecl _ e@(GHC.L _ (GHC.HsQuasiQuote{})) _flag) = do
    markLocated e
    markTrailingSemi
  markAST _ (GHC.SpliceDecl _ e _flag) = do
    markLocated e
    markTrailingSemi

  markAST _ (GHC.XSpliceDecl x) = error $ "got XSpliceDecl for:" ++ showGhc x

-- ---------------------------------------------------------------------

instance Annotate (GHC.RuleDecls GHC.GhcPs) where
  markAST _ (GHC.HsRules _ src rules) = do
    markAnnOpen src "{-# RULES"
    setLayoutFlag $ markListIntercalateWithFunLevel markLocated 2 rules
    markWithString GHC.AnnClose "#-}"
    markTrailingSemi
  markAST _ (GHC.XRuleDecls x) = error $ "got XRuleDecls for:" ++ showGhc x

-- ---------------------------------------------------------------------

instance Annotate (GHC.RuleDecl GHC.GhcPs) where
  markAST l (GHC.HsRule _ ln act bndrs lhs rhs) = do
    markLocated ln
    setContext (Set.singleton ExplicitNeverActive) $ markActivation l act

    unless (null bndrs) $ do
      mark GHC.AnnForall
      mapM_ markLocated bndrs
      mark GHC.AnnDot

    markLocated lhs
    mark GHC.AnnEqual
    markLocated rhs
    inContext (Set.singleton Intercalate) $ mark GHC.AnnSemi
    markTrailingSemi

  markAST _ (GHC.XRuleDecl x) = error $ "got XRuleDecl for:" ++ showGhc x

-- ---------------------------------------------------------------------

markActivation :: GHC.SrcSpan -> GHC.Activation -> Annotated ()
markActivation _ act = do
  case act of
    GHC.ActiveBefore src phase -> do
      mark GHC.AnnOpenS --  '['
      mark GHC.AnnTilde -- ~
      markSourceText src (show phase)
      mark GHC.AnnCloseS -- ']'
    GHC.ActiveAfter src phase -> do
      mark GHC.AnnOpenS --  '['
      markSourceText src (show phase)
      mark GHC.AnnCloseS -- ']'
    GHC.NeverActive -> do
      inContext (Set.singleton ExplicitNeverActive) $ do
        mark GHC.AnnOpenS --  '['
        mark GHC.AnnTilde -- ~
        mark GHC.AnnCloseS -- ']'
    _ -> return ()

-- ---------------------------------------------------------------------

instance Annotate (GHC.RuleBndr GHC.GhcPs) where
  markAST _ (GHC.RuleBndr _ ln) = markLocated ln
  markAST _ (GHC.RuleBndrSig _ ln st) = do
    mark GHC.AnnOpenP -- "("
    markLocated ln
    mark GHC.AnnDcolon
    markLHsSigWcType st
    mark GHC.AnnCloseP -- ")"
  markAST _ (GHC.XRuleBndr x) = error $ "got XRuleBndr for:" ++ showGhc x

-- ---------------------------------------------------------------------

markLHsSigWcType :: GHC.LHsSigWcType GHC.GhcPs -> Annotated ()
markLHsSigWcType (GHC.HsWC _ (GHC.HsIB _ ty)) = do
  markLocated ty
markLHsSigWcType (GHC.HsWC _ (GHC.XHsImplicitBndrs _)) = error "markLHsSigWcType extension hit"
markLHsSigWcType (GHC.XHsWildCardBndrs _)              = error "markLHsSigWcType extension hit"

-- ---------------------------------------------------------------------

instance Annotate (GHC.AnnDecl GHC.GhcPs) where
   markAST _ (GHC.HsAnnotation _ src prov e) = do
     markAnnOpen src "{-# ANN"
     case prov of
       (GHC.ValueAnnProvenance n) -> markLocated n
       (GHC.TypeAnnProvenance n) -> do
         mark GHC.AnnType
         markLocated n
       GHC.ModuleAnnProvenance -> mark GHC.AnnModule

     markLocated e
     markWithString GHC.AnnClose "#-}"
     markTrailingSemi

   markAST _ (GHC.XAnnDecl x) = error $ "got XAnnDecl for:" ++ showGhc x

-- ---------------------------------------------------------------------

instance Annotate (GHC.WarnDecls GHC.GhcPs) where
   markAST _ (GHC.Warnings _ src warns) = do
     markAnnOpen src "{-# WARNING" -- Note: might be {-# DEPRECATED
     mapM_ markLocated warns
     markWithString GHC.AnnClose "#-}"

   markAST _ (GHC.XWarnDecls x) = error $ "got XWarnDecls for:" ++ showGhc x

-- ---------------------------------------------------------------------

instance Annotate (GHC.WarnDecl GHC.GhcPs) where
   markAST _ (GHC.Warning _ lns txt) = do
     markListIntercalate lns
     mark GHC.AnnOpenS -- "["
     case txt of
       GHC.WarningTxt    _src ls -> markListIntercalate ls
       GHC.DeprecatedTxt _src ls -> markListIntercalate ls
     mark GHC.AnnCloseS -- "]"

   markAST _ (GHC.XWarnDecl x) = error $ "got XWarnDecl for:" ++ showGhc x

instance Annotate GHC.FastString where
  -- TODO: https://ghc.haskell.org/trac/ghc/ticket/10313 applies.
  markAST l fs = do
    markExternal l GHC.AnnVal (show (GHC.unpackFS fs))
    inContext (Set.fromList [Intercalate]) $ mark GHC.AnnComma

-- ---------------------------------------------------------------------

instance Annotate (GHC.ForeignDecl GHC.GhcPs) where
  markAST _ (GHC.ForeignImport _ ln (GHC.HsIB _ typ)
               (GHC.CImport cconv safety@(GHC.L ll _) _mh _imp (GHC.L ls src))) = do
    mark GHC.AnnForeign
    mark GHC.AnnImport
    markLocated cconv
    unless (ll == GHC.noSrcSpan) $ markLocated safety
    markExternalSourceText ls src ""
    markLocated ln
    mark GHC.AnnDcolon
    markLocated typ
    markTrailingSemi

  markAST _l (GHC.ForeignExport _ ln (GHC.HsIB _ typ) (GHC.CExport spec (GHC.L ls src))) = do
    mark GHC.AnnForeign
    mark GHC.AnnExport
    markLocated spec
    markExternal ls GHC.AnnVal (sourceTextToString src "")
    setContext (Set.singleton PrefixOp) $ markLocated ln
    mark GHC.AnnDcolon
    markLocated typ


  markAST _ (GHC.ForeignImport _ _ (GHC.XHsImplicitBndrs _) _) = error "markAST ForeignDecl hit extenstion"
  markAST _ (GHC.ForeignExport _ _ (GHC.XHsImplicitBndrs _) _) = error "markAST ForeignDecl hit extenstion"
  markAST _ (GHC.XForeignDecl _)                               = error "markAST ForeignDecl hit extenstion"

-- ---------------------------------------------------------------------

instance (Annotate GHC.CExportSpec) where
  markAST l (GHC.CExportStatic _src _ cconv) = markAST l cconv

-- ---------------------------------------------------------------------

instance (Annotate GHC.CCallConv) where
  markAST l GHC.StdCallConv        =  markExternal l GHC.AnnVal "stdcall"
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

instance Annotate (GHC.DerivDecl GHC.GhcPs) where

  markAST _ (GHC.DerivDecl _ (GHC.HsWC _ (GHC.HsIB _ typ)) ms mov) = do
    mark GHC.AnnDeriving
    markMaybe ms
    mark GHC.AnnInstance
    markMaybe mov
    markLocated typ
    markTrailingSemi

{-
data DerivDecl pass = DerivDecl
        { deriv_ext          :: XCDerivDecl pass
        , deriv_type         :: LHsSigWcType pass
          -- ^ The instance type to derive.
          --
          -- It uses an 'LHsSigWcType' because the context is allowed to be a
          -- single wildcard:
          --
          -- > deriving instance _ => Eq (Foo a)
          --
          -- Which signifies that the context should be inferred.

          -- See Note [Inferring the instance context] in TcDerivInfer.

        , deriv_strategy     :: Maybe (LDerivStrategy pass)
        , deriv_overlap_mode :: Maybe (Located OverlapMode)

type LHsSigWcType pass = HsWildCardBndrs pass (LHsSigType pass) -- Both

data HsWildCardBndrs pass thing
    -- See Note [HsType binders]
    -- See Note [The wildcard story for types]
  = HsWC { hswc_ext :: XHsWC pass thing
                -- after the renamer
                -- Wild cards, both named and anonymous

         , hswc_body :: thing
                -- Main payload (type or list of types)
                -- If there is an extra-constraints wildcard,
                -- it's still there in the hsc_body.
    }


-}


  markAST _ (GHC.DerivDecl _ (GHC.HsWC _ (GHC.XHsImplicitBndrs _)) _ _) = error "markAST DerivDecl hit extension"
  markAST _ (GHC.DerivDecl _ (GHC.XHsWildCardBndrs _) _ _)              = error "markAST DerivDecl hit extension"
  markAST _ (GHC.XDerivDecl _)                                          = error "markAST DerivDecl hit extension"

-- ---------------------------------------------------------------------

instance Annotate (GHC.DerivStrategy GHC.GhcPs) where

  markAST _ GHC.StockStrategy    = mark GHC.AnnStock
  markAST _ GHC.AnyclassStrategy = mark GHC.AnnAnyclass
  markAST _ GHC.NewtypeStrategy  = mark GHC.AnnNewtype
  markAST _ (GHC.ViaStrategy (GHC.HsIB _ ty)) = do
    mark GHC.AnnVia
    markLocated ty
  markAST _ (GHC.ViaStrategy (GHC.XHsImplicitBndrs _))
    = error $ "got XHsImplicitBndrs in AnnDerivStrategy"

-- ---------------------------------------------------------------------

instance Annotate (GHC.DefaultDecl GHC.GhcPs) where

  markAST _ (GHC.DefaultDecl _ typs) = do
    mark GHC.AnnDefault
    mark GHC.AnnOpenP -- '('
    markListIntercalate typs
    mark GHC.AnnCloseP -- ')'
    markTrailingSemi

  markAST _ (GHC.XDefaultDecl x) = error $ "got XDefaultDecl for:" ++ showGhc x

-- ---------------------------------------------------------------------

instance Annotate (GHC.InstDecl GHC.GhcPs) where

  markAST l (GHC.ClsInstD     _  cid) = markAST l  cid
  markAST l (GHC.DataFamInstD _ dfid) = markAST l dfid
  markAST l (GHC.TyFamInstD   _ tfid) = markAST l tfid
  markAST _ (GHC.XInstDecl x) = error $ "got XInstDecl for:" ++ showGhc x

-- ---------------------------------------------------------------------

instance Annotate GHC.OverlapMode where

  -- NOTE: NoOverlap is only used in the typechecker
  markAST _ (GHC.NoOverlap src) = do
    markAnnOpen src "{-# NO_OVERLAP"
    markWithString GHC.AnnClose "#-}"

  markAST _ (GHC.Overlappable src) = do
    markAnnOpen src "{-# OVERLAPPABLE"
    markWithString GHC.AnnClose "#-}"

  markAST _ (GHC.Overlapping src) = do
    markAnnOpen src "{-# OVERLAPPING"
    markWithString GHC.AnnClose "#-}"

  markAST _ (GHC.Overlaps src) = do
    markAnnOpen src "{-# OVERLAPS"
    markWithString GHC.AnnClose "#-}"

  markAST _ (GHC.Incoherent src) = do
    markAnnOpen src "{-# INCOHERENT"
    markWithString GHC.AnnClose "#-}"

-- ---------------------------------------------------------------------

instance Annotate (GHC.ClsInstDecl GHC.GhcPs) where

  markAST _ (GHC.ClsInstDecl _ (GHC.HsIB _ poly) binds sigs tyfams datafams mov) = do
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

  markAST _ (GHC.ClsInstDecl _ (GHC.XHsImplicitBndrs _) _ _ _ _ _) = error "extension hit for ClsInstDecl"
  markAST _ (GHC.XClsInstDecl _)                                   = error "extension hit for ClsInstDecl"

-- ---------------------------------------------------------------------

instance Annotate (GHC.TyFamInstDecl GHC.GhcPs) where

  markAST _ (GHC.TyFamInstDecl (GHC.HsIB _ eqn)) = do
    mark GHC.AnnType
    mark GHC.AnnInstance -- Note: this keyword is optional
    markFamEqn eqn
    markTrailingSemi

  markAST _ (GHC.TyFamInstDecl (GHC.XHsImplicitBndrs _)) = error "extension hit for TyFamInstDecl"

-- ---------------------------------------------------------------------

markFamEqn :: (Annotate (GHC.IdP pass), Annotate ast1, Annotate ast2)
           => GHC.FamEqn pass [GHC.Located ast1] (GHC.Located ast2)
                    -> Annotated ()
markFamEqn (GHC.FamEqn _ ln pats fixity rhs) = do
  markTyClass fixity ln pats
  mark GHC.AnnEqual
  markLocated rhs
markFamEqn (GHC.XFamEqn _) = error "got XFamEqn"

-- ---------------------------------------------------------------------

instance Annotate (GHC.DataFamInstDecl GHC.GhcPs) where

  markAST l (GHC.DataFamInstDecl (GHC.HsIB _ (GHC.FamEqn _ ln pats fixity
             defn@(GHC.HsDataDefn _ nd ctx typ _mk cons mderivs) ))) = do
    case GHC.dd_ND defn of
      GHC.NewType  -> mark GHC.AnnNewtype
      GHC.DataType -> mark GHC.AnnData
    mark GHC.AnnInstance

    markLocated ctx

    markTyClass fixity ln pats

    case (GHC.dd_kindSig defn) of
      Just s -> do
        mark GHC.AnnDcolon
        markLocated s
      Nothing -> return ()
    if isGadt $ GHC.dd_cons defn
      then mark GHC.AnnWhere
      else mark GHC.AnnEqual
    markDataDefn l (GHC.HsDataDefn GHC.noExt nd (GHC.noLoc []) typ _mk cons mderivs)
    markTrailingSemi

{-
newtype DataFamInstDecl pass
  = DataFamInstDecl { dfid_eqn :: FamInstEqn pass (HsDataDefn pass) }

type FamInstEqn pass rhs
  = HsImplicitBndrs pass (FamEqn pass (HsTyPats pass) rhs)

data FamEqn pass pats rhs
  = FamEqn
       { feqn_ext    :: XCFamEqn pass pats rhs
       , feqn_tycon  :: Located (IdP pass)
       , feqn_pats   :: pats
       , feqn_fixity :: LexicalFixity -- ^ Fixity used in the declaration
       , feqn_rhs    :: rhs
       }
-}

  markAST _
            (GHC.DataFamInstDecl
             (GHC.HsIB _ (GHC.FamEqn _ _ _ _ (GHC.XHsDataDefn _))))
    = error "extension hit for DataFamInstDecl"
  markAST _ (GHC.DataFamInstDecl (GHC.HsIB _ (GHC.XFamEqn _)))
    = error "extension hit for DataFamInstDecl"
  markAST _ (GHC.DataFamInstDecl (GHC.XHsImplicitBndrs _))
    = error "extension hit for DataFamInstDecl"

-- ---------------------------------------------------------------------

instance Annotate (GHC.HsBind GHC.GhcPs) where
  markAST _ (GHC.FunBind _ _ (GHC.MG _ (GHC.L _ matches) _) _ _) = do
    -- Note: from a layout perspective a FunBind should not exist, so the
    -- current context is passed through unchanged to the matches.
    -- TODO: perhaps bring the edp from the first match up to the annotation for
    -- the FunBind.
    let
      tlFun =
        ifInContext (Set.fromList [CtxOnly,CtxFirst])
          (markListWithContexts' listContexts matches)
          (markListWithContexts (lcMiddle listContexts) (lcLast listContexts) matches)
    ifInContext (Set.singleton TopLevel)
      (setContextLevel (Set.singleton TopLevel) 2 tlFun)
      tlFun

  -- -----------------------------------

  markAST _ (GHC.PatBind _ lhs (GHC.GRHSs _ grhs (GHC.L _ lb)) _ticks) = do
    markLocated lhs
    case grhs of
      (GHC.L _ (GHC.GRHS _ [] _):_) -> mark GHC.AnnEqual -- empty guards
      _ -> return ()
    markListIntercalateWithFunLevel markLocated 2 grhs

    -- TODO: extract this common code
    case lb of
      GHC.EmptyLocalBinds{} -> return ()
      _ -> do
        mark GHC.AnnWhere
        markOptional GHC.AnnOpenC -- '{'
        markInside GHC.AnnSemi
        markLocalBindsWithLayout lb
        markOptional GHC.AnnCloseC -- '}'
    markTrailingSemi

  -- -----------------------------------

  markAST _ (GHC.VarBind _ _n rhse _) =
    -- Note: this bind is introduced by the typechecker
    markLocated rhse

  -- -----------------------------------

  -- Introduced after renaming.
  markAST _ (GHC.AbsBinds {}) =
    traceM "warning: AbsBinds introduced after renaming"

  -- -----------------------------------

  markAST l (GHC.PatSynBind _ (GHC.PSB _ ln args def dir)) = do
    mark GHC.AnnPattern
    case args of
      GHC.InfixCon la lb -> do
        markLocated la
        setContext (Set.singleton InfixOp) $ markLocated ln
        markLocated lb
      GHC.PrefixCon ns -> do
        markLocated ln
        mapM_ markLocated ns
      GHC.RecCon fs -> do
        markLocated ln
        mark GHC.AnnOpenC  -- '{'
        markListIntercalateWithFun (markLocated . GHC.recordPatSynSelectorId) fs
        mark GHC.AnnCloseC -- '}'
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

    markTrailingSemi

  -- -----------------------------------

  markAST _ (GHC.FunBind _ _ (GHC.XMatchGroup _) _ _)
    = error "extension hit for HsBind"
  markAST _ (GHC.PatBind _ _ (GHC.XGRHSs _) _)
    = error "extension hit for HsBind"
  markAST _ (GHC.PatSynBind _ (GHC.XPatSynBind _))
    = error "extension hit for HsBind"
  markAST _ (GHC.XHsBindsLR _)
    = error "extension hit for HsBind"

-- ---------------------------------------------------------------------

instance Annotate (GHC.IPBind GHC.GhcPs) where
  markAST _ (GHC.IPBind _ en e) = do
    case en of
      Left n   -> markLocated n
      Right _i -> return ()
    mark GHC.AnnEqual
    markLocated e
    markTrailingSemi

  -- markAST _ (GHC.XCIPBind x) = error $ "got XIPBind for:" ++ showGhc x
  markAST _ (GHC.XIPBind x) = error $ "got XIPBind for:" ++ showGhc x

-- ---------------------------------------------------------------------

instance Annotate GHC.HsIPName where
  markAST l (GHC.HsIPName n) = markExternal l GHC.AnnVal ("?" ++ GHC.unpackFS n)

-- ---------------------------------------------------------------------

instance (Annotate body)
  => Annotate (GHC.Match GHC.GhcPs (GHC.Located body)) where

  markAST _ (GHC.Match _ mln pats (GHC.GRHSs _ grhs (GHC.L _ lb))) = do
    let
      get_infix (GHC.FunRhs _ f _) = f
      get_infix _                  = GHC.Prefix

      isFunBind GHC.FunRhs{} = True
      isFunBind _            = False
    case (get_infix mln,pats) of
      (GHC.Infix, a:b:xs) -> do
        if null xs
          then markOptional GHC.AnnOpenP
          else mark         GHC.AnnOpenP
        markLocated a
        case mln of
          GHC.FunRhs n _ _ -> setContext (Set.singleton InfixOp) $ markLocated n
          _              -> return ()
        markLocated b
        if null xs
         then markOptional GHC.AnnCloseP
         else mark         GHC.AnnCloseP
        mapM_ markLocated xs
      _ -> do
        annotationsToComments [GHC.AnnOpenP,GHC.AnnCloseP]
        inContext (Set.fromList [LambdaExpr]) $ do mark GHC.AnnLam -- For HsLam
        case mln of
          GHC.FunRhs n _ s -> do
            setContext (Set.fromList [NoPrecedingSpace,PrefixOp]) $ do
              when (s == GHC.SrcStrict) $ mark GHC.AnnBang
              markLocated n
            mapM_ markLocated pats
          _  -> markListNoPrecedingSpace False pats

    -- TODO: The AnnEqual annotation actually belongs in the first GRHS value
    case grhs of
      (GHC.L _ (GHC.GRHS _ [] _):_) -> when (isFunBind mln) $ mark GHC.AnnEqual -- empty guards
      _ -> return ()
    inContext (Set.fromList [LambdaExpr]) $ mark GHC.AnnRarrow -- For HsLam
    mapM_ markLocated grhs

    case lb of
      GHC.EmptyLocalBinds{} -> return ()
      _ -> do
        mark GHC.AnnWhere
        markOptional GHC.AnnOpenC -- '{'
        markInside GHC.AnnSemi
        markLocalBindsWithLayout lb
        markOptional GHC.AnnCloseC -- '}'
    markTrailingSemi

  -- -----------------------------------

  markAST _ (GHC.Match _ _ _ (GHC.XGRHSs _))
    = error "hit extension for Match"
  markAST _ (GHC.XMatch _)
    = error "hit extension for Match"

-- ---------------------------------------------------------------------

instance (Annotate body)
  => Annotate (GHC.GRHS GHC.GhcPs (GHC.Located body)) where
  markAST _ (GHC.GRHS _ guards expr) = do
    case guards of
      [] -> return ()
      (_:_) -> do
        mark GHC.AnnVbar
        unsetContext Intercalate $ setContext (Set.fromList [LeftMost,PrefixOp])
          $ markListIntercalate guards
        ifInContext (Set.fromList [CaseAlt])
          (return ())
          (mark GHC.AnnEqual)

    markOptional GHC.AnnEqual -- For apply-refact Structure8.hs test

    inContext (Set.fromList [CaseAlt]) $ mark GHC.AnnRarrow -- For HsLam
    setContextLevel (Set.fromList [LeftMost,PrefixOp]) 2 $ markLocated expr

  markAST _ (GHC.XGRHS x) = error $ "got XGRHS for:" ++ showGhc x

-- ---------------------------------------------------------------------

instance Annotate (GHC.Sig GHC.GhcPs) where

  markAST _ (GHC.TypeSig _ lns st)  = do
    setContext (Set.singleton PrefixOp) $ markListNoPrecedingSpace True lns
    mark GHC.AnnDcolon
    markLHsSigWcType st
    markTrailingSemi
    tellContext (Set.singleton FollowingLine)

  markAST _ (GHC.PatSynSig _ lns (GHC.HsIB _ typ)) = do
    mark GHC.AnnPattern
    markListIntercalate lns
    mark GHC.AnnDcolon
    markLocated typ
    markTrailingSemi

  markAST _ (GHC.ClassOpSig _ isDefault ns (GHC.HsIB _ typ)) = do
    when isDefault $ mark GHC.AnnDefault
    setContext (Set.singleton PrefixOp) $ markListIntercalate ns
    mark GHC.AnnDcolon
    markLocated typ
    markTrailingSemi

  markAST _ (GHC.IdSig {}) =
    traceM "warning: Introduced after renaming"

  markAST _ (GHC.FixSig _ (GHC.FixitySig _ lns (GHC.Fixity src v fdir))) = do
    let fixstr = case fdir of
         GHC.InfixL -> "infixl"
         GHC.InfixR -> "infixr"
         GHC.InfixN -> "infix"
    markWithString GHC.AnnInfix fixstr
    markSourceText src (show v)
    setContext (Set.singleton InfixOp) $ markListIntercalate lns
    markTrailingSemi

  markAST l (GHC.InlineSig _ ln inl) = do
    markAnnOpen (GHC.inl_src inl) "{-# INLINE"
    markActivation l (GHC.inl_act inl)
    setContext (Set.singleton PrefixOp) $ markLocated ln
    markWithString GHC.AnnClose "#-}" -- '#-}'
    markTrailingSemi

  markAST l (GHC.SpecSig _ ln typs inl) = do
    markAnnOpen (GHC.inl_src inl) "{-# SPECIALISE" -- Note: may be {-# SPECIALISE_INLINE
    markActivation l (GHC.inl_act inl)
    markLocated ln
    mark GHC.AnnDcolon -- '::'
    markListIntercalateWithFunLevel markLHsSigType 2 typs
    markWithString GHC.AnnClose "#-}" -- '#-}'
    markTrailingSemi


  markAST _ (GHC.SpecInstSig _ src typ) = do
    markAnnOpen src "{-# SPECIALISE"
    mark GHC.AnnInstance
    markLHsSigType typ
    markWithString GHC.AnnClose "#-}" -- '#-}'
    markTrailingSemi


  markAST _ (GHC.MinimalSig _ src formula) = do
    markAnnOpen src "{-# MINIMAL"
    markLocated formula
    markWithString GHC.AnnClose "#-}"
    markTrailingSemi

  markAST _ (GHC.SCCFunSig _ src ln ml) = do
    markAnnOpen src "{-# SCC"
    markLocated ln
    markMaybe ml
    markWithString GHC.AnnClose "#-}"
    markTrailingSemi

  markAST _ (GHC.CompleteMatchSig _ src (GHC.L _ ns) mlns) = do
    markAnnOpen src "{-# COMPLETE"
    markListIntercalate ns
    case mlns of
      Nothing -> return ()
      Just _ -> do
        mark GHC.AnnDcolon
        markMaybe mlns
    markWithString GHC.AnnClose "#-}" -- '#-}'
    markTrailingSemi

  -- -----------------------------------
  markAST _ (GHC.PatSynSig _ _ (GHC.XHsImplicitBndrs _))
    = error "hit extension for Sig"
  markAST _ (GHC.ClassOpSig _ _ _ (GHC.XHsImplicitBndrs _))
    = error "hit extension for Sig"
  markAST _ (GHC.FixSig _ (GHC.XFixitySig _))
    = error "hit extension for Sig"
  markAST _ (GHC.XSig _)
    = error "hit extension for Sig"

-- --------------------------------------------------------------------

markLHsSigType :: GHC.LHsSigType GHC.GhcPs -> Annotated ()
markLHsSigType (GHC.HsIB _ typ) = markLocated typ
markLHsSigType (GHC.XHsImplicitBndrs x) = error $ "got XHsImplicitBndrs for:" ++ showGhc x

instance Annotate [GHC.LHsSigType GHC.GhcPs] where
  markAST _ ls = do
    -- mark GHC.AnnDeriving
    -- Mote: a single item in parens is parsed as a HsAppsTy. Without parens it
    -- is a HsTyVar. So for round trip pretty printing we need to take this into
    -- account.
    let marker = case ls of
          []  -> markManyOptional
          [GHC.HsIB _ t] -> if GHC.hsTypeNeedsParens GHC.appPrec (GHC.unLoc t)
                           then markMany
                           else markManyOptional
          _   -> markMany -- Need parens if more than one entry
    marker GHC.AnnOpenP
    markListIntercalateWithFun markLHsSigType ls
    marker GHC.AnnCloseP

-- --------------------------------------------------------------------

instance  (Annotate name) => Annotate (GHC.BooleanFormula (GHC.Located name)) where
  markAST _ (GHC.Var x)  = do
    setContext (Set.singleton PrefixOp) $ markLocated x
    inContext (Set.fromList [AddVbar]) $ mark GHC.AnnVbar
    inContext (Set.fromList [Intercalate]) $ mark GHC.AnnComma
  markAST _ (GHC.Or ls)  = markListIntercalateWithFunLevelCtx markLocated 2 AddVbar ls
  markAST _ (GHC.And ls) = do
    markListIntercalateWithFunLevel markLocated 2 ls
    inContext (Set.fromList [AddVbar]) $ mark GHC.AnnVbar
    inContext (Set.fromList [Intercalate]) $ mark GHC.AnnComma
  markAST _ (GHC.Parens x)  = do
    mark GHC.AnnOpenP -- '('
    markLocated x
    mark GHC.AnnCloseP -- ')'
    inContext (Set.fromList [AddVbar]) $ mark GHC.AnnVbar
    inContext (Set.fromList [Intercalate]) $ mark GHC.AnnComma

-- ---------------------------------------------------------------------

instance Annotate (GHC.HsTyVarBndr GHC.GhcPs) where
  markAST _l (GHC.UserTyVar _ n) = do
    markLocated n

  markAST _ (GHC.KindedTyVar _ n ty) = do
    mark GHC.AnnOpenP  -- '('
    markLocated n
    mark GHC.AnnDcolon -- '::'
    markLocated ty
    mark GHC.AnnCloseP -- '('

  markAST _l (GHC.XTyVarBndr x) = error $ "got XTyVarBndr for:" ++ showGhc x

-- ---------------------------------------------------------------------

instance Annotate (GHC.HsType GHC.GhcPs) where
  markAST loc ty = do
    markType loc ty
    inContext (Set.fromList [Intercalate]) $ mark GHC.AnnComma
    (inContext (Set.singleton AddVbar) $ mark GHC.AnnVbar)
   where

    -- markType :: GHC.SrcSpan -> ast -> Annotated ()
    markType :: GHC.SrcSpan -> (GHC.HsType GHC.GhcPs) -> Annotated ()
    markType _ (GHC.HsForAllTy _ tvs typ) = do
      mark GHC.AnnForall
      mapM_ markLocated tvs
      mark GHC.AnnDot
      markLocated typ

    markType _ (GHC.HsQualTy _ cxt typ) = do
      markLocated cxt
      markLocated typ

    markType _ (GHC.HsTyVar _ promoted name) = do
      when (promoted == GHC.Promoted) $ mark GHC.AnnSimpleQuote
      unsetContext InfixOp $ setContext (Set.singleton PrefixOp) $ markLocated name

    markType _ (GHC.HsAppTy _ t1 t2) = do
      setContext (Set.singleton PrefixOp) $ markLocated t1
      markLocated t2

    markType _ (GHC.HsFunTy _ t1 t2) = do
      markLocated t1
      mark GHC.AnnRarrow
      markLocated t2
      -- markManyOptional GHC.AnnCloseP -- For trailing parens after res_ty in ConDeclGADT

    markType _ (GHC.HsListTy _ t) = do
      mark GHC.AnnOpenS -- '['
      markLocated t
      mark GHC.AnnCloseS -- ']'

    markType _ (GHC.HsTupleTy _ tt ts) = do
      case tt  of
        GHC.HsBoxedOrConstraintTuple -> mark GHC.AnnOpenP  -- '('
        _                            -> markWithString GHC.AnnOpen "(#" -- '(#'
      markListIntercalateWithFunLevel markLocated 2 ts
      case tt  of
        GHC.HsBoxedOrConstraintTuple -> mark GHC.AnnCloseP  -- ')'
        _                            -> markWithString GHC.AnnClose "#)" -- '#)'

    markType _ (GHC.HsSumTy _ tys) = do
      markWithString GHC.AnnOpen "(#"
      markListIntercalateWithFunLevelCtx markLocated 2 AddVbar tys
      markWithString GHC.AnnClose "#)"

    markType _ (GHC.HsOpTy _ t1 lo t2) = do
      markLocated t1
      if (GHC.isTcOcc $ GHC.occName $ GHC.unLoc lo)
        then do
          markOptional GHC.AnnSimpleQuote
        else do
          mark GHC.AnnSimpleQuote
      unsetContext PrefixOp $ setContext (Set.singleton InfixOp) $ markLocated lo
      markLocated t2

    markType _ (GHC.HsParTy _ t) = do
      mark GHC.AnnOpenP  -- '('
      markLocated t
      mark GHC.AnnCloseP -- ')'

    markType _ (GHC.HsIParamTy _ n t) = do
      markLocated n
      mark GHC.AnnDcolon
      markLocated t

    markType l (GHC.HsStarTy _ isUnicode) = do
      if isUnicode
        then markExternal l GHC.AnnVal "\x2605" -- Unicode star
        else markExternal l GHC.AnnVal "*"

    markType _ (GHC.HsKindSig _ t k) = do
      mark GHC.AnnOpenP  -- '('
      markLocated t
      mark GHC.AnnDcolon -- '::'
      markLocated k
      mark GHC.AnnCloseP -- ')'

    markType l (GHC.HsSpliceTy _ s) = do
      markAST l s

    markType _ (GHC.HsDocTy _ t ds) = do
      markLocated t
      markLocated ds

    markType _ (GHC.HsBangTy _ (GHC.HsSrcBang mt _up str) t) = do
      case mt of
        GHC.NoSourceText -> return ()
        GHC.SourceText src -> do
          markWithString GHC.AnnOpen src
          markWithString GHC.AnnClose "#-}"
      case str of
        GHC.SrcLazy     -> mark GHC.AnnTilde
        GHC.SrcStrict   -> mark GHC.AnnBang
        GHC.NoSrcStrict -> return ()

      markLocated t

    markType _ (GHC.HsRecTy _ cons) = do
      mark GHC.AnnOpenC  -- '{'
      markListIntercalate cons
      mark GHC.AnnCloseC -- '}'

    markType _ (GHC.HsExplicitListTy _ promoted ts) = do
      when (promoted == GHC.Promoted) $ mark GHC.AnnSimpleQuote
      mark GHC.AnnOpenS  -- "["
      markListIntercalate ts
      mark GHC.AnnCloseS -- ']'

    markType _ (GHC.HsExplicitTupleTy _ ts) = do
      mark GHC.AnnSimpleQuote
      mark GHC.AnnOpenP
      markListIntercalate ts
      mark GHC.AnnCloseP

    markType l (GHC.HsTyLit _ lit) = do
      case lit of
        (GHC.HsNumTy s v) ->
          markExternalSourceText l s (show v)
        (GHC.HsStrTy s v) ->
          markExternalSourceText l s (show v)

    markType l (GHC.HsWildCardTy _) = do
      markExternal l GHC.AnnVal "_"

    markType _ (GHC.XHsType x) = error $ "got XHsType for:" ++ showGhc x


-- ---------------------------------------------------------------------

-- instance Annotate (GHC.HsAppType GHC.GhcPs) where
--   markAST _ (GHC.HsAppInfix _ n)  = do
--     when (GHC.isDataOcc $ GHC.occName $ GHC.unLoc n) $ mark GHC.AnnSimpleQuote
--     setContext (Set.singleton InfixOp) $ markLocated n
--   markAST _ (GHC.HsAppPrefix _ t) = do
--     markOptional GHC.AnnTilde
--     setContext (Set.singleton PrefixOp) $ markLocated t

-- ---------------------------------------------------------------------

instance Annotate (GHC.HsSplice GHC.GhcPs) where
  markAST l c =
    case c of
      GHC.HsQuasiQuote _ _ n _pos fs -> do
        markExternal l GHC.AnnVal
              -- Note: Lexer.x does not provide unicode alternative. 2017-02-26
              ("[" ++ (showGhc n) ++ "|" ++ (GHC.unpackFS fs) ++ "|]")

      GHC.HsTypedSplice _ hasParens _n b@(GHC.L _ (GHC.HsVar _ (GHC.L _ n)))  -> do
        when (hasParens == GHC.HasParens) $ mark GHC.AnnOpenPTE
        if (hasParens == GHC.HasDollar)
          then markWithString GHC.AnnThIdTySplice ("$$" ++ (GHC.occNameString (GHC.occName n)))
          else markLocated b
        when (hasParens == GHC.HasParens) $ mark GHC.AnnCloseP

      GHC.HsTypedSplice _ hasParens _n b -> do
        when (hasParens == GHC.HasParens) $ mark GHC.AnnOpenPTE
        markLocated b
        when (hasParens == GHC.HasParens) $ mark GHC.AnnCloseP

      -- -------------------------------

      GHC.HsUntypedSplice _ hasParens _n b@(GHC.L _ (GHC.HsVar _ (GHC.L _ n)))  -> do
        when (hasParens == GHC.HasParens) $  mark GHC.AnnOpenPE
        if (hasParens == GHC.HasDollar)
          then markWithString GHC.AnnThIdSplice ("$" ++ (GHC.occNameString (GHC.occName n)))
          else markLocated b
        when (hasParens == GHC.HasParens) $ mark GHC.AnnCloseP

      GHC.HsUntypedSplice _ hasParens _n b  -> do
        case hasParens of
          GHC.HasParens -> mark GHC.AnnOpenPE
          GHC.HasDollar -> mark GHC.AnnThIdSplice
          GHC.NoParens  -> return ()
        markLocated b
        when (hasParens == GHC.HasParens) $ mark GHC.AnnCloseP

      GHC.HsSpliced{} -> error "HsSpliced only exists between renamer and typechecker in GHC"

      -- -------------------------------

      (GHC.XSplice x) -> error $ "got XSplice for:" ++ showGhc x

-- ---------------------------------------------------------------------

instance Annotate (GHC.ConDeclField GHC.GhcPs) where
  markAST _ (GHC.ConDeclField _ ns ty mdoc) = do
    unsetContext Intercalate $ do
      markListIntercalate ns
      mark GHC.AnnDcolon
      markLocated ty
      markMaybe mdoc
    inContext (Set.fromList [Intercalate]) $ mark GHC.AnnComma

  markAST _ (GHC.XConDeclField x) = error $ "got XConDeclField for:" ++ showGhc x

-- ---------------------------------------------------------------------

instance Annotate (GHC.FieldOcc GHC.GhcPs) where
  markAST _ (GHC.FieldOcc _ rn) = do
    markLocated rn
    inContext (Set.fromList [Intercalate]) $ mark GHC.AnnComma

  markAST _ (GHC.XFieldOcc x) = error $ "got XFieldOcc for:" ++ showGhc x

-- ---------------------------------------------------------------------

instance Annotate GHC.HsDocString where
  markAST l s = do
    markExternal l GHC.AnnVal (GHC.unpackHDS s)

-- ---------------------------------------------------------------------

instance Annotate (GHC.Pat GHC.GhcPs) where
  markAST loc typ = do
    markPat loc typ
    inContext (Set.fromList [Intercalate]) $ mark GHC.AnnComma `debug` ("AnnComma in Pat")
    where
      markPat l (GHC.WildPat _) = markExternal l GHC.AnnVal "_"
      markPat l (GHC.VarPat _ n) = do
        -- The parser inserts a placeholder value for a record pun rhs. This must be
        -- filtered out until https://ghc.haskell.org/trac/ghc/ticket/12224 is
        -- resolved, particularly for pretty printing where annotations are added.
        let pun_RDR = "pun-right-hand-side"
        when (showGhc n /= pun_RDR) $
          unsetContext Intercalate $ setContext (Set.singleton PrefixOp) $ markAST l (GHC.unLoc n)
      markPat _ (GHC.LazyPat _ p) = do
        mark GHC.AnnTilde
        markLocated p

      markPat _ (GHC.AsPat _ ln p) = do
        markLocated ln
        mark GHC.AnnAt
        markLocated p

      markPat _ (GHC.ParPat _ p) = do
        mark GHC.AnnOpenP
        markLocated p
        mark GHC.AnnCloseP

      markPat _ (GHC.BangPat _ p) = do
        mark GHC.AnnBang
        markLocated p

      markPat _ (GHC.ListPat _ ps) = do
        mark GHC.AnnOpenS
        markListIntercalateWithFunLevel markLocated 2 ps
        mark GHC.AnnCloseS

      markPat _ (GHC.TuplePat _ pats b) = do
        if b == GHC.Boxed then mark GHC.AnnOpenP
                          else markWithString GHC.AnnOpen "(#"
        markListIntercalateWithFunLevel markLocated 2 pats
        if b == GHC.Boxed then mark GHC.AnnCloseP
                          else markWithString GHC.AnnClose "#)"

      markPat _ (GHC.SumPat _ pat alt arity) = do
        markWithString GHC.AnnOpen "(#"
        replicateM_ (alt - 1) $ mark GHC.AnnVbar
        markLocated pat
        replicateM_ (arity - alt) $ mark GHC.AnnVbar
        markWithString GHC.AnnClose "#)"

      markPat _ (GHC.ConPatIn n dets) = do
        markHsConPatDetails n dets

      markPat _ GHC.ConPatOut {} =
        traceM "warning: ConPatOut Introduced after renaming"

      markPat _ (GHC.ViewPat _ e pat) = do
        markLocated e
        mark GHC.AnnRarrow
        markLocated pat

      markPat l (GHC.SplicePat _ s) = do
        markAST l s

      markPat l (GHC.LitPat _ lp) = markAST l lp

      markPat _ (GHC.NPat _ ol mn _) = do
        when (isJust mn) $ mark GHC.AnnMinus
        markLocated ol

      markPat _ (GHC.NPlusKPat _ ln ol _ _ _) = do
        markLocated ln
        markWithString GHC.AnnVal "+"  -- "+"
        markLocated ol


      markPat _ (GHC.SigPat ty pat) = do
        markLocated pat
        mark GHC.AnnDcolon
        markLHsSigWcType ty

      markPat _ GHC.CoPat {} =
        traceM "warning: CoPat introduced after renaming"

      markPat _ (GHC.XPat x) = error $ "got XPat for:" ++ showGhc x

-- ---------------------------------------------------------------------

hsLit2String :: GHC.HsLit GHC.GhcPs -> String
hsLit2String lit =
  case lit of
    GHC.HsChar       src v   -> toSourceTextWithSuffix src v ""
    -- It should be included here
    -- https://github.com/ghc/ghc/blob/master/compiler/parser/Lexer.x#L1471
    GHC.HsCharPrim   src p   -> toSourceTextWithSuffix src p "#"
    GHC.HsString     src v   -> toSourceTextWithSuffix src v ""
    GHC.HsStringPrim src v   -> toSourceTextWithSuffix src v ""
    GHC.HsInt        _ (GHC.IL src _ v)   -> toSourceTextWithSuffix src v ""
    GHC.HsIntPrim    src v   -> toSourceTextWithSuffix src v ""
    GHC.HsWordPrim   src v   -> toSourceTextWithSuffix src v ""
    GHC.HsInt64Prim  src v   -> toSourceTextWithSuffix src v ""
    GHC.HsWord64Prim src v   -> toSourceTextWithSuffix src v ""
    GHC.HsInteger    src v _ -> toSourceTextWithSuffix src v ""
    GHC.HsRat        _ (GHC.FL src _ v) _ -> toSourceTextWithSuffix src v ""
    GHC.HsFloatPrim  _ (GHC.FL src _ v)   -> toSourceTextWithSuffix src v "#"
    GHC.HsDoublePrim _ (GHC.FL src _ v)   -> toSourceTextWithSuffix src v "##"
    (GHC.XLit x) -> error $ "got XLit for:" ++ showGhc x

toSourceTextWithSuffix :: (Show a) => GHC.SourceText -> a -> String -> String
toSourceTextWithSuffix (GHC.NoSourceText)    alt suffix = show alt ++ suffix
toSourceTextWithSuffix (GHC.SourceText txt) _alt suffix = txt ++ suffix

-- --------------------------------------------------------------------

markHsConPatDetails :: GHC.Located GHC.RdrName -> GHC.HsConPatDetails GHC.GhcPs -> Annotated ()
markHsConPatDetails ln dets = do
  case dets of
    GHC.PrefixCon args -> do
      setContext (Set.singleton PrefixOp) $ markLocated ln
      mapM_ markLocated args
    GHC.RecCon (GHC.HsRecFields fs dd) -> do
      markLocated ln
      mark GHC.AnnOpenC -- '{'
      case dd of
        Nothing ->  markListIntercalateWithFunLevel markLocated 2 fs
        Just _ -> do
          setContext (Set.singleton Intercalate) $ mapM_ markLocated fs
          mark GHC.AnnDotdot
      mark GHC.AnnCloseC -- '}'
    GHC.InfixCon a1 a2 -> do
      markLocated a1
      unsetContext PrefixOp $ setContext (Set.singleton InfixOp) $ markLocated ln
      markLocated a2

markHsConDeclDetails ::
  Bool -> Bool -> [GHC.Located GHC.RdrName] -> GHC.HsConDeclDetails GHC.GhcPs -> Annotated ()

markHsConDeclDetails isDeprecated inGadt lns dets = do
  case dets of
    GHC.PrefixCon args ->
      setContext (Set.singleton PrefixOp) $ mapM_ markLocated args
    -- GHC.RecCon fs -> markLocated fs
    GHC.RecCon fs -> do
      mark GHC.AnnOpenC
      if inGadt
        then do
          if isDeprecated
            then setContext (Set.fromList [InGadt]) $ markLocated fs
            else setContext (Set.fromList [InGadt,InRecCon]) $ markLocated fs
        else do
          if isDeprecated
            then markLocated fs
            else setContext (Set.fromList [InRecCon]) $ markLocated fs
    GHC.InfixCon a1 a2 -> do
      markLocated a1
      setContext (Set.singleton InfixOp) $ mapM_ markLocated lns
      markLocated a2

-- ---------------------------------------------------------------------

instance Annotate [GHC.LConDeclField GHC.GhcPs] where
  markAST _ fs = do
       markOptional GHC.AnnOpenC -- '{'
       markListIntercalate fs
       markOptional GHC.AnnDotdot
       inContext (Set.singleton InRecCon) $ mark GHC.AnnCloseC -- '}'
       inContext (Set.singleton InGadt) $ do
         mark GHC.AnnRarrow

-- ---------------------------------------------------------------------

instance Annotate (GHC.HsOverLit GHC.GhcPs) where
  markAST l ol =
    let str = case GHC.ol_val ol of
                GHC.HsIntegral   (GHC.IL src _ _) -> src
                GHC.HsFractional (GHC.FL src _ _) -> src
                GHC.HsIsString src _ -> src
    in
    markExternalSourceText l str ""

-- ---------------------------------------------------------------------

instance (Annotate arg)
    => Annotate (GHC.HsImplicitBndrs GHC.GhcPs (GHC.Located arg)) where
  markAST _ (GHC.HsIB _ thing) = do
    markLocated thing
  markAST _ (GHC.XHsImplicitBndrs x) = error $ "got XHsImplicitBndrs for:" ++ showGhc x

-- ---------------------------------------------------------------------

instance (Annotate body) => Annotate (GHC.Stmt GHC.GhcPs (GHC.Located body)) where

  markAST _ (GHC.LastStmt _ body _ _)
    = setContextLevel (Set.fromList [LeftMost,PrefixOp]) 2 $ markLocated body

  markAST _ (GHC.BindStmt _ pat body _ _) = do
    unsetContext Intercalate $ setContext (Set.singleton PrefixOp) $ markLocated pat
    mark GHC.AnnLarrow
    unsetContext Intercalate $ setContextLevel (Set.fromList [LeftMost,PrefixOp]) 2 $ markLocated body

    ifInContext (Set.singleton Intercalate)
      (mark GHC.AnnComma)
      (inContext (Set.singleton AddVbar) $ mark GHC.AnnVbar)
    markTrailingSemi

  markAST _ GHC.ApplicativeStmt{}
    = error "ApplicativeStmt should not appear in ParsedSource"

  markAST _ (GHC.BodyStmt _ body _ _) = do
    unsetContext Intercalate $ markLocated body
    inContext (Set.singleton AddVbar)     $ mark GHC.AnnVbar
    inContext (Set.singleton Intercalate) $ mark GHC.AnnComma
    markTrailingSemi

  markAST _ (GHC.LetStmt _ (GHC.L _ lb)) = do
    mark GHC.AnnLet
    markOptional GHC.AnnOpenC -- '{'
    markInside GHC.AnnSemi
    markLocalBindsWithLayout lb
    markOptional GHC.AnnCloseC -- '}'
    ifInContext (Set.singleton Intercalate)
      (mark GHC.AnnComma)
      (inContext (Set.singleton AddVbar)     $ mark GHC.AnnVbar)
    markTrailingSemi

  markAST l (GHC.ParStmt _ pbs _ _) = do
    -- Within a given parallel list comprehension,one of the sections to be done
    -- in parallel. It is a normal list comprehension, so has a list of
    -- ParStmtBlock, one for each part of the sub- list comprehension


    ifInContext (Set.singleton Intercalate)
      (

      unsetContext Intercalate $
        markListWithContextsFunction
          (LC (Set.singleton Intercalate)  -- only
              Set.empty -- first
              Set.empty -- middle
              (Set.singleton Intercalate) -- last
          ) (markAST l) pbs
         )
      (
      unsetContext Intercalate $
        markListWithContextsFunction
          (LC Set.empty -- only
              (Set.fromList [AddVbar]) -- first
              (Set.fromList [AddVbar]) -- middle
              Set.empty                -- last
          ) (markAST l) pbs
       )
    markTrailingSemi

  markAST _ (GHC.TransStmt _ form stmts _b using by _ _ _) = do
    setContext (Set.singleton Intercalate) $ mapM_ markLocated stmts
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
    inContext (Set.singleton AddVbar)     $ mark GHC.AnnVbar
    inContext (Set.singleton Intercalate) $ mark GHC.AnnComma
    markTrailingSemi

  markAST _ (GHC.RecStmt _ stmts _ _ _ _ _) = do
    mark GHC.AnnRec
    markOptional GHC.AnnOpenC
    markInside GHC.AnnSemi
    mapM_ markLocated stmts
    markOptional GHC.AnnCloseC
    inContext (Set.singleton AddVbar)     $ mark GHC.AnnVbar
    inContext (Set.singleton Intercalate) $ mark GHC.AnnComma
    markTrailingSemi

  markAST _ (GHC.XStmtLR x) = error $ "got XStmtLR for:" ++ showGhc x

-- ---------------------------------------------------------------------

-- Note: We never have a located ParStmtBlock, so have nothing to hang the
-- annotation on. This means there is no pushing of context from the parent ParStmt.
instance Annotate (GHC.ParStmtBlock GHC.GhcPs GHC.GhcPs) where
  markAST _ (GHC.ParStmtBlock _ stmts _ns _) = do
    markListIntercalate stmts
  markAST _ (GHC.XParStmtBlock x) = error $ "got XParStmtBlock for:" ++ showGhc x

-- ---------------------------------------------------------------------

instance Annotate (GHC.HsLocalBinds GHC.GhcPs) where
  markAST _ lb = markHsLocalBinds lb

-- ---------------------------------------------------------------------

markHsLocalBinds :: GHC.HsLocalBinds GHC.GhcPs -> Annotated ()
markHsLocalBinds (GHC.HsValBinds _ (GHC.ValBinds _ binds sigs)) =
    applyListAnnotationsLayout
       (prepareListAnnotation (GHC.bagToList binds)
     ++ prepareListAnnotation sigs
       )
markHsLocalBinds (GHC.HsIPBinds _ (GHC.IPBinds _ binds)) = markListWithLayout binds
markHsLocalBinds GHC.EmptyLocalBinds{}                   = return ()

markHsLocalBinds (GHC.HsValBinds _ (GHC.XValBindsLR _)) = error "markHsLocalBinds:got extension"
markHsLocalBinds (GHC.HsIPBinds _ (GHC.XHsIPBinds _))   = error "markHsLocalBinds:got extension"
markHsLocalBinds (GHC.XHsLocalBindsLR _)                = error "markHsLocalBinds:got extension"

-- ---------------------------------------------------------------------

markMatchGroup :: (Annotate body)
                   => GHC.SrcSpan -> GHC.MatchGroup GHC.GhcPs (GHC.Located body)
                   -> Annotated ()
markMatchGroup _ (GHC.MG _ (GHC.L _ matches) _)
  = setContextLevel (Set.singleton AdvanceLine) 2 $ markListWithLayout matches
markMatchGroup _ (GHC.XMatchGroup x) = error $ "got XMatchGroup for:" ++ showGhc x

-- ---------------------------------------------------------------------

instance (Annotate body)
  => Annotate [GHC.Located (GHC.Match GHC.GhcPs (GHC.Located body))] where
  markAST _ ls = mapM_ markLocated ls

-- ---------------------------------------------------------------------

instance Annotate (GHC.HsExpr GHC.GhcPs) where
  markAST loc expr = do
    markExpr loc expr
    inContext (Set.singleton AddVbar) $ mark GHC.AnnVbar
    -- TODO: If the AnnComma is not needed, revert to markAST
    inContext (Set.fromList [Intercalate]) $ mark GHC.AnnComma
   where
      markExpr _ (GHC.HsVar _ n) = unsetContext Intercalate $ do
        ifInContext (Set.singleton PrefixOp)
          (setContext (Set.singleton PrefixOp) $ markLocated n)
          (ifInContext (Set.singleton InfixOp)
            (setContext (Set.singleton InfixOp) $ markLocated n)
            (markLocated n)
            )

      markExpr l (GHC.HsRecFld _ f) = markAST l f

      markExpr l (GHC.HsOverLabel _ _ fs)
        = markExternal l GHC.AnnVal ("#" ++ GHC.unpackFS fs)


      markExpr l (GHC.HsIPVar _ n@(GHC.HsIPName _v))         =
        markAST l n
      markExpr l (GHC.HsOverLit _ ov)     = markAST l ov
      markExpr l (GHC.HsLit _ lit)        = markAST l lit

      markExpr _ (GHC.HsLam _ (GHC.MG _ (GHC.L _ [match]) _)) = do
        setContext (Set.singleton LambdaExpr) $ do
        -- TODO: Change this, HsLam binds do not need obey layout rules.
        --       And will only ever have a single match
          markLocated match
      markExpr _ (GHC.HsLam _ _) = error $ "HsLam with other than one match"

      markExpr l (GHC.HsLamCase _ match) = do
        mark GHC.AnnLam
        mark GHC.AnnCase
        markOptional GHC.AnnOpenC
        setContext (Set.singleton CaseAlt) $ do
          markMatchGroup l match
        markOptional GHC.AnnCloseC

      markExpr _ (GHC.HsApp _ e1 e2) = do
        setContext (Set.singleton PrefixOp) $ markLocated e1
        setContext (Set.singleton PrefixOp) $ markLocated e2

      markExpr _ (GHC.OpApp _ e1 e2 e3) = do
        let
          isInfix = case e2 of
            -- TODO: generalise this. Is it a fixity thing?
            GHC.L _ (GHC.HsVar{}) -> True
            _                     -> False

          normal =
            -- When it is the leftmost item in a GRHS, e1 needs to have PrefixOp context
            ifInContext (Set.singleton LeftMost)
              (setContextLevel (Set.fromList [LeftMost,PrefixOp]) 2 $ markLocated e1)
              (markLocated e1)

        if isInfix
            then setContextLevel (Set.singleton PrefixOp) 2 $ markLocated e1
            else normal

        unsetContext PrefixOp $ setContext (Set.singleton InfixOp) $ markLocated e2

        if isInfix
          then setContextLevel (Set.singleton PrefixOp) 2 $ markLocated e3
          else markLocated e3

      markExpr _ (GHC.NegApp _ e _) = do
        mark GHC.AnnMinus
        markLocated e

      markExpr _ (GHC.HsPar _ e) = do
        mark GHC.AnnOpenP -- '('
        markLocated e
        mark GHC.AnnCloseP -- ')'

      markExpr _ (GHC.SectionL _ e1 e2) = do
        markLocated e1
        setContext (Set.singleton InfixOp) $ markLocated e2

      markExpr _ (GHC.SectionR _ e1 e2) = do
        setContext (Set.singleton InfixOp) $ markLocated e1
        markLocated e2

      markExpr _ (GHC.ExplicitTuple _ args b) = do
        if b == GHC.Boxed then mark GHC.AnnOpenP
                          else markWithString GHC.AnnOpen "(#"

        setContext (Set.singleton PrefixOp) $ markListIntercalateWithFunLevel markLocated 2 args

        if b == GHC.Boxed then mark GHC.AnnCloseP
                          else markWithString GHC.AnnClose "#)"

      markExpr _ (GHC.ExplicitSum _ alt arity e) = do
        markWithString GHC.AnnOpen "(#"
        replicateM_ (alt - 1) $ mark GHC.AnnVbar
        markLocated e
        replicateM_ (arity - alt) $ mark GHC.AnnVbar
        markWithString GHC.AnnClose "#)"

      markExpr l (GHC.HsCase _ e1 matches) = setRigidFlag $ do
        mark GHC.AnnCase
        setContextLevel (Set.singleton PrefixOp) 2 $ markLocated e1
        mark GHC.AnnOf
        markOptional GHC.AnnOpenC
        markInside GHC.AnnSemi
        setContext (Set.singleton CaseAlt) $ markMatchGroup l matches
        markOptional GHC.AnnCloseC

      -- We set the layout for HsIf even though it need not obey layout rules as
      -- when moving these expressions it's useful that they maintain "internal
      -- integrity", that is to say the subparts remain indented relative to each
      -- other.
      markExpr _ (GHC.HsIf _ _ e1 e2 e3) = setLayoutFlag $ do
      -- markExpr _ (GHC.HsIf _ e1 e2 e3) = setRigidFlag $ do
        mark GHC.AnnIf
        markLocated e1
        markAnnBeforeAnn GHC.AnnSemi GHC.AnnThen
        mark GHC.AnnThen
        setContextLevel (Set.singleton ListStart) 2 $ markLocated e2
        markAnnBeforeAnn GHC.AnnSemi GHC.AnnElse
        mark GHC.AnnElse
        setContextLevel (Set.singleton ListStart) 2 $ markLocated e3

      markExpr _ (GHC.HsMultiIf _ rhs) = do
        mark GHC.AnnIf
        markOptional GHC.AnnOpenC
        setContext (Set.singleton CaseAlt) $ do
          -- mapM_ markLocated rhs
          markListWithLayout rhs
        markOptional GHC.AnnCloseC

      markExpr _ (GHC.HsLet _ (GHC.L _ binds) e) = do
        setLayoutFlag (do -- Make sure the 'in' gets indented too
          mark GHC.AnnLet
          markOptional GHC.AnnOpenC
          markInside GHC.AnnSemi
          markLocalBindsWithLayout binds
          markOptional GHC.AnnCloseC
          mark GHC.AnnIn
          markLocated e)

      -- -------------------------------

      markExpr _ (GHC.HsDo _ cts (GHC.L _ es)) = do
        case cts of
          GHC.DoExpr  -> mark GHC.AnnDo
          GHC.MDoExpr -> mark GHC.AnnMdo
          _           -> return ()
        let (ostr,cstr) =
              if isListComp cts
                then ("[", "]")
                else ("{", "}")

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
        when (isListComp cts) $ markWithString GHC.AnnClose cstr

      -- -------------------------------

      markExpr _ (GHC.ExplicitList _ _ es) = do
        mark GHC.AnnOpenS
        setContext (Set.singleton PrefixOp) $ markListIntercalateWithFunLevel markLocated 2 es
        mark GHC.AnnCloseS

      markExpr _ (GHC.RecordCon _ n (GHC.HsRecFields fs dd)) = do
        markLocated n
        mark GHC.AnnOpenC
        case dd of
          Nothing -> markListIntercalate fs
          Just _ -> do
            setContext (Set.singleton Intercalate) $ mapM_ markLocated fs
            mark GHC.AnnDotdot
        mark GHC.AnnCloseC

      markExpr _ (GHC.RecordUpd _ e fs) = do
        markLocated e
        mark GHC.AnnOpenC
        markListIntercalate fs
        mark GHC.AnnCloseC

      markExpr _ (GHC.ExprWithTySig typ e) = do
        setContextLevel (Set.singleton PrefixOp) 2 $ markLocated e
        mark GHC.AnnDcolon
        markLHsSigWcType typ

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

      markExpr _ (GHC.HsSCC _ src csFStr e) = do
        markAnnOpen src "{-# SCC"
        let txt = sourceTextToString (GHC.sl_st csFStr) (GHC.unpackFS $ GHC.sl_fs csFStr)
        markWithStringOptional GHC.AnnVal    txt
        markWithString         GHC.AnnValStr txt
        markWithString GHC.AnnClose "#-}"
        markLocated e

      markExpr _ (GHC.HsCoreAnn _ src csFStr e) = do
        -- markWithString GHC.AnnOpen src -- "{-# CORE"
        markAnnOpen src "{-# CORE"
        -- markWithString GHC.AnnVal (GHC.sl_st csFStr)
        markSourceText (GHC.sl_st csFStr) (GHC.unpackFS $ GHC.sl_fs csFStr)
        markWithString GHC.AnnClose "#-}"
        markLocated e
      -- TODO: make monomorphic
      markExpr l (GHC.HsBracket _ (GHC.VarBr _ True v)) = do
        mark GHC.AnnSimpleQuote
        setContext (Set.singleton PrefixOpDollar) $ markLocatedFromKw GHC.AnnName (GHC.L l v)
      markExpr l (GHC.HsBracket _ (GHC.VarBr _ False v)) = do
        mark GHC.AnnThTyQuote
        markLocatedFromKw GHC.AnnName (GHC.L l v)
      markExpr _ (GHC.HsBracket _ (GHC.DecBrL _ ds)) = do
        markWithString GHC.AnnOpen "[d|"
        markOptional GHC.AnnOpenC
        setContext (Set.singleton NoAdvanceLine)
             $ setContextLevel (Set.singleton TopLevel) 2 $ markListWithLayout ds
        markOptional GHC.AnnCloseC
        mark GHC.AnnCloseQ -- "|]"
      -- Introduced after the renamer
      markExpr _ (GHC.HsBracket _ (GHC.DecBrG _ _)) =
        traceM "warning: DecBrG introduced after renamer"
      markExpr _l (GHC.HsBracket _ (GHC.ExpBr _ e)) = do
        mark GHC.AnnOpenEQ -- "[|"
        markOptional GHC.AnnOpenE  -- "[e|"
        markLocated e
        mark GHC.AnnCloseQ -- "|]"
      markExpr _l (GHC.HsBracket _ (GHC.TExpBr _ e)) = do
        markWithString GHC.AnnOpen  "[||"
        markWithStringOptional GHC.AnnOpenE "[e||"
        markLocated e
        markWithString GHC.AnnClose "||]"
      markExpr _ (GHC.HsBracket _ (GHC.TypBr _ e)) = do
        markWithString GHC.AnnOpen "[t|"
        markLocated e
        mark GHC.AnnCloseQ -- "|]"
      markExpr _ (GHC.HsBracket _ (GHC.PatBr _ e)) = do
        markWithString GHC.AnnOpen  "[p|"
        markLocated e
        mark GHC.AnnCloseQ -- "|]"

      markExpr _ (GHC.HsRnBracketOut {}) =
        traceM "warning: HsRnBracketOut introduced after renamer"
      markExpr _ (GHC.HsTcBracketOut {}) =
        traceM "warning: HsTcBracketOut introduced after renamer"

      markExpr l (GHC.HsSpliceE _ e) = markAST l e

      markExpr _ (GHC.HsProc _ p c) = do
        mark GHC.AnnProc
        markLocated p
        mark GHC.AnnRarrow
        markLocated c

      markExpr _ (GHC.HsStatic _ e) = do
        mark GHC.AnnStatic
        markLocated e

      markExpr _ (GHC.HsArrApp _ e1 e2  o isRightToLeft) = do
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

        if isRightToLeft
          then markLocated e2
          else markLocated e1

      markExpr _ (GHC.HsArrForm _ e _ cs) = do
        markWithString GHC.AnnOpenB "(|"
        markLocated e
        mapM_ markLocated cs
        markWithString GHC.AnnCloseB "|)"

      markExpr _ (GHC.HsTick {}) = return ()
      markExpr _ (GHC.HsBinTick {}) = return ()

      markExpr _ (GHC.HsTickPragma _ src (str,(v1,v2),(v3,v4)) ((s1,s2),(s3,s4)) e) = do
        -- '{-# GENERATED' STRING INTEGER ':' INTEGER '-' INTEGER ':' INTEGER '#-}'
        markAnnOpen src  "{-# GENERATED"
        markOffsetWithString GHC.AnnVal 0 (stringLiteralToString str) -- STRING

        let
          markOne n  v GHC.NoSourceText   = markOffsetWithString GHC.AnnVal n (show v)
          markOne n _v (GHC.SourceText s) = markOffsetWithString GHC.AnnVal n s

        markOne  1 v1 s1 -- INTEGER
        markOffset GHC.AnnColon 0 -- ':'
        markOne  2 v2 s2 -- INTEGER
        mark   GHC.AnnMinus   -- '-'
        markOne  3 v3 s3 -- INTEGER
        markOffset GHC.AnnColon 1 -- ':'
        markOne  4 v4 s4 -- INTEGER
        markWithString   GHC.AnnClose  "#-}"
        markLocated e

      markExpr l (GHC.EWildPat _) = do
        ifInContext (Set.fromList [InfixOp])
          (do  mark GHC.AnnBackquote
               markWithString GHC.AnnVal "_"
               mark GHC.AnnBackquote)
          (markExternal l GHC.AnnVal "_")

      markExpr _ (GHC.EAsPat _ ln e) = do
        markLocated ln
        mark GHC.AnnAt
        markLocated e

      markExpr _ (GHC.EViewPat _ e1 e2) = do
        markLocated e1
        mark GHC.AnnRarrow
        markLocated e2

      markExpr _ (GHC.ELazyPat _ e) = do
        mark GHC.AnnTilde
        markLocated e

      markExpr _ (GHC.HsAppType ty e) = do
        markLocated e
        markInstead GHC.AnnAt AnnTypeApp
        markLHsWcType ty

      markExpr _ (GHC.HsWrap {}) =
        traceM "warning: HsWrap introduced after renaming"
      markExpr _ (GHC.HsUnboundVar {}) =
        traceM "warning: HsUnboundVar introduced after renaming"

      markExpr _ (GHC.HsConLikeOut{}) =
        traceM "warning: HsConLikeOut introduced after type checking"

      markExpr _ (GHC.HsBracket _ (GHC.XBracket _)) = error "markExpr got extension"
      markExpr _ (GHC.XExpr _)                      = error "markExpr got extension"

-- ---------------------------------------------------------------------

markLHsWcType :: GHC.LHsWcType GHC.GhcPs -> Annotated ()
markLHsWcType (GHC.HsWC _ ty) = do
  markLocated ty
markLHsWcType (GHC.XHsWildCardBndrs x) = error $ "markLHsWcType got :" ++ showGhc x

-- ---------------------------------------------------------------------

instance Annotate (GHC.HsLit GHC.GhcPs) where
  markAST l lit = markExternal l GHC.AnnVal (hsLit2String lit)

-- ---------------------------------------------------------------------

instance Annotate (GHC.HsRecUpdField GHC.GhcPs) where
  markAST _ (GHC.HsRecField lbl expr punFlag) = do
    unsetContext Intercalate $ markLocated lbl
    when (punFlag == False) $ do
      mark GHC.AnnEqual
      unsetContext Intercalate $ markLocated expr
    inContext (Set.fromList [Intercalate]) $ mark GHC.AnnComma

instance Annotate (GHC.AmbiguousFieldOcc GHC.GhcPs) where
  markAST _ (GHC.Unambiguous _ n) = markLocated n
  markAST _ (GHC.Ambiguous   _ n) = markLocated n
  markAST _ (GHC.XAmbiguousFieldOcc x) = error $ "got XAmbiguousFieldOcc for:" ++ showGhc x

-- ---------------------------------------------------------------------

-- |Used for declarations that need to be aligned together, e.g. in a
-- do or let .. in statement/expr
instance Annotate [GHC.ExprLStmt GHC.GhcPs] where
  markAST _ ls = mapM_ markLocated ls

-- ---------------------------------------------------------------------

instance Annotate (GHC.HsTupArg GHC.GhcPs) where
  markAST _ (GHC.Present _ (GHC.L l e)) = do
    markLocated (GHC.L l e)
    inContext (Set.fromList [Intercalate]) $ markOutside GHC.AnnComma (G GHC.AnnComma)

  markAST _ (GHC.Missing _) = do
    inContext (Set.fromList [Intercalate]) $ mark GHC.AnnComma

  markAST _ (GHC.XTupArg x) = error $ "got XTupArg got:" ++ showGhc x

-- ---------------------------------------------------------------------

instance Annotate (GHC.HsCmdTop GHC.GhcPs) where
  markAST _ (GHC.HsCmdTop _ cmd) = markLocated cmd
  markAST _ (GHC.XCmdTop x) = error $ "got XCmdTop for:" ++ showGhc x

instance Annotate (GHC.HsCmd GHC.GhcPs) where
  markAST _ (GHC.HsCmdArrApp _ e1 e2 o isRightToLeft) = do
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

    if isRightToLeft
      then markLocated e2
      else markLocated e1

  markAST _ (GHC.HsCmdArrForm _ e fixity _mf cs) = do
    -- The AnnOpen should be marked for a prefix usage, not for a postfix one,
    -- due to the way checkCmd maps both HsArrForm and OpApp to HsCmdArrForm

    let isPrefixOp = case fixity of
          GHC.Infix  -> False
          GHC.Prefix -> True
    when isPrefixOp $ mark GHC.AnnOpenB -- "(|"

    -- This may be an infix operation
    applyListAnnotationsContexts (LC (Set.singleton PrefixOp) (Set.singleton PrefixOp)
                                     (Set.singleton InfixOp) (Set.singleton InfixOp))
                       (prepareListAnnotation [e]
                         ++ prepareListAnnotation cs)
    when isPrefixOp $ mark GHC.AnnCloseB -- "|)"

  markAST _ (GHC.HsCmdApp _ e1 e2) = do
    markLocated e1
    markLocated e2

  markAST l (GHC.HsCmdLam _ match) = do
    setContext (Set.singleton LambdaExpr) $ do markMatchGroup l match

  markAST _ (GHC.HsCmdPar _ e) = do
    mark GHC.AnnOpenP
    markLocated e
    mark GHC.AnnCloseP -- ')'

  markAST l (GHC.HsCmdCase _ e1 matches) = do
    mark GHC.AnnCase
    markLocated e1
    mark GHC.AnnOf
    markOptional GHC.AnnOpenC
    setContext (Set.singleton CaseAlt) $ do
      markMatchGroup l matches
    markOptional GHC.AnnCloseC

  markAST _ (GHC.HsCmdIf _ _ e1 e2 e3) = do
    mark GHC.AnnIf
    markLocated e1
    markOffset GHC.AnnSemi 0
    mark GHC.AnnThen
    markLocated e2
    markOffset GHC.AnnSemi 1
    mark GHC.AnnElse
    markLocated e3

  markAST _ (GHC.HsCmdLet _ (GHC.L _ binds) e) = do
    mark GHC.AnnLet
    markOptional GHC.AnnOpenC
    markLocalBindsWithLayout binds
    markOptional GHC.AnnCloseC
    mark GHC.AnnIn
    markLocated e

  markAST _ (GHC.HsCmdDo _ (GHC.L _ es)) = do
    mark GHC.AnnDo
    markOptional GHC.AnnOpenC
    markListWithLayout es
    markOptional GHC.AnnCloseC

  markAST _ (GHC.HsCmdWrap {}) =
    traceM "warning: HsCmdWrap introduced after renaming"

  markAST _ (GHC.XCmd x) = error $ "got XCmd for:" ++ showGhc x

-- ---------------------------------------------------------------------

instance Annotate [GHC.Located (GHC.StmtLR GHC.GhcPs GHC.GhcPs (GHC.LHsCmd GHC.GhcPs))] where
  markAST _ ls = mapM_ markLocated ls

-- ---------------------------------------------------------------------

instance Annotate (GHC.TyClDecl GHC.GhcPs) where

  markAST l (GHC.FamDecl _ famdecl) = markAST l famdecl >> markTrailingSemi

  markAST _ (GHC.SynDecl _ ln (GHC.HsQTvs _ tyvars) fixity typ) = do
    -- There may be arbitrary parens around parts of the constructor that are
    -- infix.
    -- Turn these into comments so that they feed into the right place automatically
    -- annotationsToComments [GHC.AnnOpenP,GHC.AnnCloseP]
    mark GHC.AnnType

    markTyClass fixity ln tyvars
    mark GHC.AnnEqual
    markLocated typ
    markTrailingSemi

  markAST _ (GHC.DataDecl _ ln (GHC.HsQTvs _ tyVars) fixity
                (GHC.HsDataDefn _ nd ctx mctyp mk cons derivs)) = do
    if nd == GHC.DataType
      then mark GHC.AnnData
      else mark GHC.AnnNewtype
    markMaybe mctyp
    markLocated ctx
    markTyClass fixity ln tyVars
    case mk of
      Nothing -> return ()
      Just k -> do
        mark GHC.AnnDcolon
        markLocated k
    if isGadt cons
      then mark GHC.AnnWhere
      else unless (null cons) $ mark GHC.AnnEqual
    markOptional GHC.AnnWhere
    markOptional GHC.AnnOpenC
    setLayoutFlag $ setContext (Set.singleton NoPrecedingSpace)
                  $ markListWithContexts' listContexts cons
    markOptional GHC.AnnCloseC
    setContext (Set.fromList [Deriving,NoDarrow]) $ markLocated derivs
    markTrailingSemi

  -- -----------------------------------

  markAST _ (GHC.ClassDecl _ ctx ln (GHC.HsQTvs _ tyVars) fixity fds
                          sigs meths ats atdefs docs) = do
    mark GHC.AnnClass
    markLocated ctx

    markTyClass fixity ln tyVars

    unless (null fds) $ do
      mark GHC.AnnVbar
      markListIntercalateWithFunLevel markLocated 2 fds
    mark GHC.AnnWhere
    markOptional GHC.AnnOpenC -- '{'
    markInside GHC.AnnSemi
    -- AZ:TODO: we end up with both the tyVars and the following body of the
    -- class defn in annSortKey for the class. This could cause problems when
    -- changing things.
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
{-
  | ClassDecl { tcdCExt    :: XClassDecl pass,         -- ^ Post renamer, FVs
                tcdCtxt    :: LHsContext pass,         -- ^ Context...
                tcdLName   :: Located (IdP pass),      -- ^ Name of the class
                tcdTyVars  :: LHsQTyVars pass,         -- ^ Class type variables
                tcdFixity  :: LexicalFixity, -- ^ Fixity used in the declaration
                tcdFDs     :: [Located (FunDep (Located (IdP pass)))],
                                                        -- ^ Functional deps
                tcdSigs    :: [LSig pass],              -- ^ Methods' signatures
                tcdMeths   :: LHsBinds pass,            -- ^ Default methods
                tcdATs     :: [LFamilyDecl pass],       -- ^ Associated types;
                tcdATDefs  :: [LTyFamDefltEqn pass],
                                                   -- ^ Associated type defaults
                tcdDocs    :: [LDocDecl]                -- ^ Haddock docs
    }

-}

  markAST _ (GHC.SynDecl _ _ (GHC.XLHsQTyVars _) _ _)
    = error "extension hit for TyClDecl"
  markAST _ (GHC.DataDecl _ _ (GHC.HsQTvs _ _) _ (GHC.XHsDataDefn _))
    = error "extension hit for TyClDecl"
  markAST _ (GHC.DataDecl _ _ (GHC.XLHsQTyVars _) _ _)
    = error "extension hit for TyClDecl"
  markAST _ (GHC.ClassDecl _ _ _ (GHC.XLHsQTyVars _) _ _ _ _ _ _ _)
    = error "extension hit for TyClDecl"
  markAST _ (GHC.XTyClDecl _)
    = error "extension hit for TyClDecl"

-- ---------------------------------------------------------------------

markTyClass :: (Annotate a, Annotate ast)
                => GHC.LexicalFixity -> GHC.Located a -> [GHC.Located ast] -> Annotated ()
markTyClass fixity ln tyVars = do
    -- There may be arbitrary parens around parts of the constructor
    -- Turn these into comments so that they feed into the right place automatically
    annotationsToComments [GHC.AnnOpenP,GHC.AnnCloseP]
    let markParens = if fixity == GHC.Infix && length tyVars > 2
          then markMany
          else markManyOptional
    if fixity == GHC.Prefix
      then do
        markManyOptional GHC.AnnOpenP
        setContext (Set.singleton PrefixOp) $ markLocated ln
        -- setContext (Set.singleton PrefixOp) $ mapM_ markLocated tyVars
        setContext (Set.singleton PrefixOp) $ mapM_ markLocated $ take 2 tyVars
        when (length tyVars >= 2) $ do
          markParens GHC.AnnCloseP
          setContext (Set.singleton PrefixOp) $ mapM_ markLocated $ drop 2 tyVars
        markManyOptional GHC.AnnCloseP
      else do
        case tyVars of
          (x:y:xs) -> do
            markParens GHC.AnnOpenP
            markLocated x
            setContext (Set.singleton InfixOp) $ markLocated ln
            markLocated y
            markParens GHC.AnnCloseP
            mapM_ markLocated xs
            markManyOptional GHC.AnnCloseP
          _ -> error $ "markTyClass: Infix op without operands"

-- ---------------------------------------------------------------------

instance Annotate [GHC.LHsDerivingClause GHC.GhcPs] where
  markAST _ ds = mapM_ markLocated ds

-- ---------------------------------------------------------------------

instance Annotate (GHC.HsDerivingClause GHC.GhcPs) where
  markAST _ (GHC.HsDerivingClause _ mstrategy typs) = do
    mark GHC.AnnDeriving
    case mstrategy of
      Nothing -> return ()
      Just (GHC.L _ (GHC.ViaStrategy{})) -> return ()
      Just s -> markLocated s
    markLocated typs
    case mstrategy of
      Just s@(GHC.L _ (GHC.ViaStrategy{})) -> markLocated s
      _ -> return ()

  markAST _ (GHC.XHsDerivingClause x) = error $ "got XHsDerivingClause for:" ++ showGhc x

{-
  = HsDerivingClause
    { deriv_clause_ext :: XCHsDerivingClause pass
    , deriv_clause_strategy :: Maybe (LDerivStrategy pass)
      -- ^ The user-specified strategy (if any) to use when deriving
      -- 'deriv_clause_tys'.
    , deriv_clause_tys :: Located [LHsSigType pass]
      -- ^ The types to derive.
      --
      -- It uses 'LHsSigType's because, with @-XGeneralizedNewtypeDeriving@,
      -- we can mention type variables that aren't bound by the datatype, e.g.
      --
      -- > data T b = ... deriving (C [a])
      --
      -- should produce a derived instance for @C [a] (T b)@.
    }

-}

-- ---------------------------------------------------------------------

instance Annotate (GHC.FamilyDecl GHC.GhcPs) where
  markAST _ (GHC.FamilyDecl _ info ln (GHC.HsQTvs _ tyvars) fixity rsig minj) = do
    case info of
      GHC.DataFamily -> mark GHC.AnnData
      _              -> mark GHC.AnnType

    mark GHC.AnnFamily

    markTyClass fixity ln tyvars
    case GHC.unLoc rsig of
      GHC.NoSig _ -> return ()
      GHC.KindSig _ _ -> do
        mark GHC.AnnDcolon
        markLocated rsig
      GHC.TyVarSig _ _ -> do
        mark GHC.AnnEqual
        markLocated rsig
      (GHC.XFamilyResultSig x) -> error $ "FamilyDecl:got XFamilyResultSig for:" ++ showGhc x
    case minj of
      Nothing -> return ()
      Just inj -> do
        mark GHC.AnnVbar
        markLocated inj
    case info of
      GHC.ClosedTypeFamily (Just eqns) -> do
        mark GHC.AnnWhere
        markOptional GHC.AnnOpenC -- {
        markListWithLayout eqns
        markOptional GHC.AnnCloseC -- }
      GHC.ClosedTypeFamily Nothing -> do
        mark GHC.AnnWhere
        mark GHC.AnnOpenC -- {
        mark GHC.AnnDotdot
        mark GHC.AnnCloseC -- }
      _ -> return ()
    markTrailingSemi

  markAST _ (GHC.FamilyDecl _ _ _ (GHC.XLHsQTyVars _) _ _ _)
    = error "got extension for FamilyDecl"
  markAST _ (GHC.XFamilyDecl _)
    = error "got extension for FamilyDecl"

-- ---------------------------------------------------------------------

instance Annotate (GHC.FamilyResultSig GHC.GhcPs) where
  markAST _ (GHC.NoSig _)        = return ()
  markAST _ (GHC.KindSig _ k)    = markLocated k
  markAST _ (GHC.TyVarSig _ ltv) = markLocated ltv
  markAST _ (GHC.XFamilyResultSig x) = error $ "got XFamilyResultSig for:" ++ showGhc x

-- ---------------------------------------------------------------------

instance Annotate (GHC.InjectivityAnn GHC.GhcPs) where
  markAST _ (GHC.InjectivityAnn ln lns) = do
    markLocated ln
    mark GHC.AnnRarrow
    mapM_ markLocated lns

-- ---------------------------------------------------------------------

instance Annotate (GHC.TyFamInstEqn GHC.GhcPs) where

  markAST _ (GHC.HsIB _ eqn) = do
    markFamEqn eqn
    markTrailingSemi
  markAST _ (GHC.XHsImplicitBndrs x) = error $ "got XHsImplicitBndrs for:" ++ showGhc x

-- ---------------------------------------------------------------------

instance Annotate (GHC.TyFamDefltEqn GHC.GhcPs) where

  markAST _ (GHC.FamEqn _ ln (GHC.HsQTvs _ bndrs) fixity typ) = do
    mark GHC.AnnType
    mark GHC.AnnInstance
    markTyClass fixity ln bndrs
    mark GHC.AnnEqual
    markLocated typ

  markAST _ (GHC.FamEqn _ _ (GHC.XLHsQTyVars _) _ _)
    = error "TyFamDefltEqn hit extension point"
  markAST _ (GHC.XFamEqn _)
    = error "TyFamDefltEqn hit extension point"

-- ---------------------------------------------------------------------

-- TODO: modify lexer etc, in the meantime to not set haddock flag
instance Annotate GHC.DocDecl where
  markAST l v =
    let str =
          case v of
            (GHC.DocCommentNext ds)     -> GHC.unpackHDS ds
            (GHC.DocCommentPrev ds)     -> GHC.unpackHDS ds
            (GHC.DocCommentNamed _s ds) -> GHC.unpackHDS ds
            (GHC.DocGroup _i ds)        -> GHC.unpackHDS ds
    in
      markExternal l GHC.AnnVal str >> markTrailingSemi
{-
data DocDecl
  = DocCommentNext HsDocString
  | DocCommentPrev HsDocString
  | DocCommentNamed String HsDocString
  | DocGroup Int HsDocString

-}

-- ---------------------------------------------------------------------

markDataDefn :: GHC.SrcSpan -> GHC.HsDataDefn GHC.GhcPs -> Annotated ()
markDataDefn _ (GHC.HsDataDefn _ _ ctx typ _mk cons derivs) = do
  markLocated ctx
  markMaybe typ
  if isGadt cons
    then markListWithLayout cons
    else markListIntercalateWithFunLevel markLocated 2 cons
  setContext (Set.singleton Deriving) $ markLocated derivs
markDataDefn _ (GHC.XHsDataDefn x) = error $ "got XHsDataDefn for:" ++ showGhc x

-- ---------------------------------------------------------------------

-- Note: GHC.HsContext name aliases to here too
instance Annotate [GHC.LHsType GHC.GhcPs] where
  markAST l ts = do
    -- Note: A single item in parens in a standalone deriving clause
    -- is parsed as a HsSigType, which is always a HsForAllTy or
    -- HsQualTy. Without parens it is always a HsVar. So for round
    -- trip pretty printing we need to take this into account.
    let
      parenIfNeeded' pa =
        case ts of
          []  -> if l == GHC.noSrcSpan
            then markManyOptional pa
            else markMany pa
          [GHC.L _ GHC.HsForAllTy{}] -> markMany pa
          [GHC.L _ GHC.HsQualTy{}] -> markMany pa
          [_] -> markManyOptional pa
          _   -> markMany         pa

      parenIfNeeded'' pa =
        ifInContext (Set.singleton Parens) -- AZ:TODO: this is never set?
          (markMany pa)
          (parenIfNeeded' pa)

      parenIfNeeded pa =
        case ts of
          [GHC.L _ GHC.HsParTy{}] -> markOptional pa
          _ -> parenIfNeeded'' pa

    -- -------------

    parenIfNeeded GHC.AnnOpenP

    unsetContext Intercalate $ markListIntercalateWithFunLevel markLocated 2 ts

    parenIfNeeded GHC.AnnCloseP

    ifInContext (Set.singleton NoDarrow)
      (return ())
      (if null ts && (l == GHC.noSrcSpan)
         then markOptional GHC.AnnDarrow
         else mark         GHC.AnnDarrow)

-- ---------------------------------------------------------------------

instance Annotate (GHC.ConDecl GHC.GhcPs) where
  markAST _ (GHC.ConDeclH98 _ ln _fa mqtvs mctx
                         dets _) = do
    case mqtvs of
      [] -> return ()
      bndrs -> do
        mark GHC.AnnForall
        mapM_ markLocated bndrs
        mark GHC.AnnDot

    case mctx of
      Just ctx -> do
        setContext (Set.fromList [NoDarrow]) $ markLocated ctx
        unless (null $ GHC.unLoc ctx) $ mark GHC.AnnDarrow
      Nothing -> return ()

    case dets of
      GHC.InfixCon _ _ -> return ()
      _ -> setContext (Set.singleton PrefixOp) $ markLocated ln

    markHsConDeclDetails False False [ln] dets

    inContext (Set.fromList [Intercalate]) $ mark GHC.AnnVbar
    markTrailingSemi
{-
  | ConDeclH98
      { con_ext     :: XConDeclH98 pass
      , con_name    :: Located (IdP pass)

      , con_forall  :: Bool   -- ^ True <=> explicit user-written forall
                              --     e.g. data T a = forall b. MkT b (b->a)
                              --     con_ex_tvs = {b}
                              -- False => con_ex_tvs is empty
      , con_ex_tvs :: [LHsTyVarBndr pass]      -- ^ Existentials only
      , con_mb_cxt :: Maybe (LHsContext pass)  -- ^ User-written context (if any)
      , con_args   :: HsConDeclDetails pass    -- ^ Arguments; can be InfixCon

      , con_doc       :: Maybe LHsDocString
          -- ^ A possible Haddock comment.
      }

-}
  markAST _ (GHC.ConDeclGADT _ lns (GHC.L l forall) (GHC.HsQTvs _ qvars) mbCxt args typ _) = do
    setContext (Set.singleton PrefixOp) $ markListIntercalate lns
    mark GHC.AnnDcolon
    annotationsToComments [GHC.AnnOpenP]
    markLocated (GHC.L l (ResTyGADTHook forall qvars))
    markMaybe mbCxt
    markHsConDeclDetails False True lns args
    markLocated typ
    markManyOptional GHC.AnnCloseP
    markTrailingSemi
{-
  = ConDeclGADT
      { con_g_ext   :: XConDeclGADT pass
      , con_names   :: [Located (IdP pass)]

      -- The next four fields describe the type after the '::'
      -- See Note [GADT abstract syntax]
      , con_forall  :: Located Bool      -- ^ True <=> explicit forall
                                         --   False => hsq_explicit is empty
      , con_qvars   :: LHsQTyVars pass
                       -- Whether or not there is an /explicit/ forall, we still
                       -- need to capture the implicitly-bound type/kind variables

      , con_mb_cxt  :: Maybe (LHsContext pass) -- ^ User-written context (if any)
      , con_args    :: HsConDeclDetails pass   -- ^ Arguments; never InfixCon
      , con_res_ty  :: LHsType pass            -- ^ Result type

      , con_doc     :: Maybe LHsDocString
          -- ^ A possible Haddock comment.
      }

-}

  markAST _ (GHC.ConDeclGADT _ _ (GHC.L _ _) (GHC.XLHsQTyVars _) _ _ _ _)
    = error "hit extension point in ConDecl"
  markAST _ (GHC.XConDecl _)
    = error "hit extension point in ConDecl"

-- ResTyGADT has a SrcSpan for the original sigtype, we need to create
-- a type for exactPC and annotatePC
data ResTyGADTHook = ResTyGADTHook Bool [GHC.LHsTyVarBndr GHC.GhcPs]
                   deriving (Typeable)
deriving instance Data (ResTyGADTHook)

instance GHC.Outputable ResTyGADTHook where
  ppr (ResTyGADTHook b bs) = GHC.text "ResTyGADTHook" GHC.<+> GHC.ppr b GHC.<+> GHC.ppr bs


-- WildCardAnon exists because the GHC anonymous wildcard type is defined as
--      = AnonWildCard (PostRn name Name)
-- We need to reconstruct this from the typed hole SrcSpan in an HsForAllTy, but
-- the instance doing this is parameterised on name, so we cannot put a value in
-- for the (PostRn name Name) field. This is used instead.
data WildCardAnon = WildCardAnon deriving (Show,Data,Typeable)

instance Annotate WildCardAnon where
  markAST l WildCardAnon = do
    markExternal l GHC.AnnVal "_"

-- ---------------------------------------------------------------------

instance Annotate ResTyGADTHook where
  markAST _ (ResTyGADTHook forall bndrs) = do
    unless (null bndrs) $ do
      when forall $ mark GHC.AnnForall
      mapM_ markLocated bndrs
      when forall $ mark GHC.AnnDot

-- ---------------------------------------------------------------------

instance Annotate (GHC.HsRecField GHC.GhcPs (GHC.LPat GHC.GhcPs)) where
  markAST _ (GHC.HsRecField n e punFlag) = do
    unsetContext Intercalate $ markLocated n
    unless punFlag $ do
      mark GHC.AnnEqual
      unsetContext Intercalate $ markLocated e
    inContext (Set.fromList [Intercalate]) $ mark GHC.AnnComma


instance Annotate (GHC.HsRecField GHC.GhcPs (GHC.LHsExpr GHC.GhcPs)) where
  markAST _ (GHC.HsRecField n e punFlag) = do
    unsetContext Intercalate $ markLocated n
    unless punFlag $ do
      mark GHC.AnnEqual
      unsetContext Intercalate $ markLocated e
    inContext (Set.fromList [Intercalate]) $ mark GHC.AnnComma

-- ---------------------------------------------------------------------

instance Annotate (GHC.FunDep (GHC.Located GHC.RdrName)) where

  markAST _ (ls,rs) = do
    mapM_ markLocated ls
    mark GHC.AnnRarrow
    mapM_ markLocated rs
    inContext (Set.fromList [Intercalate]) $ mark GHC.AnnComma

-- ---------------------------------------------------------------------

instance Annotate GHC.CType where
  markAST _ (GHC.CType src mh f) = do
    -- markWithString GHC.AnnOpen src
    markAnnOpen src ""
    case mh of
      Nothing -> return ()
      Just (GHC.Header srcH _h) ->
         -- markWithString GHC.AnnHeader srcH
         markWithString GHC.AnnHeader (toSourceTextWithSuffix srcH "" "")
    -- markWithString GHC.AnnVal (fst f)
    markSourceText  (fst f) (GHC.unpackFS $ snd f)
    markWithString GHC.AnnClose "#-}"

-- ---------------------------------------------------------------------

stringLiteralToString :: GHC.StringLiteral -> String
stringLiteralToString (GHC.StringLiteral st fs) =
  case st of
    GHC.NoSourceText   -> GHC.unpackFS fs
    GHC.SourceText src -> src
