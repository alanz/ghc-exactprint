{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Language.Haskell.GHC.ExactPrint.Annotate
       (
         markLocated
       , AnnotationF(..)
       , Annotated
       , Annotate(..)) where

import Control.Exception (assert)
import Data.Data (Data)
import Data.List (sort, sortBy)
import Data.Maybe (fromMaybe)
import Control.Monad (when)

import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Utils (rdrName2String, showGhc, isListComp, debug)

import qualified Bag            as GHC
import qualified BasicTypes     as GHC
import qualified BooleanFormula as GHC
import qualified Class          as GHC
import qualified CoAxiom        as GHC
import qualified FastString     as GHC
import qualified ForeignCall    as GHC
import qualified GHC            as GHC
import qualified Outputable     as GHC
import qualified SrcLoc         as GHC

import Debug.Trace


import Control.Monad.Trans.Free
import Control.Monad.Free.TH (makeFreeCon)

-- ---------------------------------------------------------------------


data AnnotationF next where
  MarkEOF  :: next -> AnnotationF next
  MarkPrim :: GHC.AnnKeywordId -> Maybe String -> next -> AnnotationF next
  MarkExternal :: GHC.SrcSpan -> GHC.AnnKeywordId -> String -> next -> AnnotationF next
  MarkOutside :: GHC.AnnKeywordId -> KeywordId -> next -> AnnotationF next
  MarkInside :: GHC.AnnKeywordId -> next -> AnnotationF next
  MarkMany :: GHC.AnnKeywordId -> next -> AnnotationF next
  MarkOffsetPrim :: GHC.AnnKeywordId -> Int -> Maybe String -> next -> AnnotationF next
  MarkAfter :: GHC.AnnKeywordId -> next -> AnnotationF next
  WithAST  :: Data a => GHC.Located a -> LayoutFlag -> Annotated b ->  next -> AnnotationF next
  CountAnns ::  GHC.AnnKeywordId -> (Int -> next) -> AnnotationF next
  -- Abstraction breakers
  SetLayoutFlag ::  GHC.AnnKeywordId -> Annotated () -> next -> AnnotationF next
  OutputKD :: (DeltaPos, (GHC.SrcSpan, KeywordId)) -> next -> AnnotationF next

deriving instance Functor (AnnotationF)


type Annotated = Free AnnotationF

-- ---------------------------------------------------------------------

makeFreeCon  'OutputKD
makeFreeCon  'MarkEOF
makeFreeCon  'MarkPrim
makeFreeCon  'MarkOutside
makeFreeCon  'MarkInside
makeFreeCon  'MarkExternal
makeFreeCon  'MarkMany
makeFreeCon  'MarkOffsetPrim
makeFreeCon  'MarkAfter
makeFreeCon  'CountAnns
makeFreeCon  'SetLayoutFlag
-- ---------------------------------------------------------------------
-- Additional smart constructors

mark :: GHC.AnnKeywordId -> Annotated ()
mark kwid = markPrim kwid Nothing

markWithString :: GHC.AnnKeywordId -> String -> Annotated ()
markWithString kwid s = markPrim kwid (Just s)

markOffsetWithString :: GHC.AnnKeywordId -> Int -> String -> Annotated ()
markOffsetWithString kwid n s = markOffsetPrim kwid n (Just s)

markOffset :: GHC.AnnKeywordId -> Int -> Annotated ()
markOffset kwid n = markOffsetPrim kwid n Nothing

withAST :: Data a => GHC.Located a -> LayoutFlag -> Annotated () -> Annotated ()
withAST lss layout action = liftF (WithAST lss layout prog ())
  where
    prog = do
      action
      -- Automatically add any trailing comma or semi
      markAfter GHC.AnnComma
      markOutside GHC.AnnSemi AnnSemiSep
-- ---------------------------------------------------------------------


-- | Constructs a syntax tree which contains information about which
-- annotations are required by each element.
markLocated :: (Annotate ast) => GHC.Located ast -> Annotated ()
markLocated a = withLocated a NoLayoutRules markAST

withLocated :: Data a
            => GHC.Located a
            -> LayoutFlag
            -> (GHC.SrcSpan -> a -> Annotated ())
            -> Annotated ()
withLocated a@(GHC.L l ast) layoutFlag action =
  withAST a layoutFlag (action l ast)

markMaybe :: (Annotate ast) => Maybe (GHC.Located ast) -> Annotated ()
markMaybe Nothing    = return ()
markMaybe (Just ast) = markLocated ast

markList :: (Annotate ast) => [GHC.Located ast] -> Annotated ()
markList xs = mapM_ markLocated xs

-- | Flag the item to be annotated as requiring layout.
markWithLayout :: Annotate ast => GHC.Located ast -> Annotated ()
markWithLayout a = withLocated a LayoutRules markAST

markListWithLayout :: Annotate [GHC.Located ast] => GHC.SrcSpan -> [GHC.Located ast] -> Annotated ()
markListWithLayout l ls = do
  let ss = getListSrcSpan ls
  outputKD $ ((DP (0,0)), (l,AnnList ss))
  markWithLayout (GHC.L ss ls)

-- ---------------------------------------------------------------------
-- Managing lists which have been separated, e.g. Sigs and Binds

prepareListAnnotation :: Annotate a => [GHC.Located a] -> [(GHC.SrcSpan,Annotated ())]
prepareListAnnotation ls = map (\b@(GHC.L l _) -> (l,markLocated b)) ls

applyListAnnotations :: [(GHC.SrcSpan, Annotated ())] -> Annotated ()
applyListAnnotations ls
  = mapM_ snd $ sortBy (\(a,_) (b,_) -> compare a b) ls

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

        (GHC.IEThingAbs ln) -> do
          mark GHC.AnnType
          markLocated ln

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


-- ---------------------------------------------------------------------

instance Annotate GHC.RdrName where
  markAST l n = do
    case rdrName2String n of
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
      str ->  do
        mark GHC.AnnType
        mark GHC.AnnOpenP -- '('
        markOffset GHC.AnnBackquote 0
        markMany GHC.AnnCommaTuple -- For '(,,,)'
        cnt <- countAnns GHC.AnnVal
        cntT <- countAnns GHC.AnnCommaTuple
        cntR <- countAnns GHC.AnnRarrow
        case cnt of
          0 -> if cntT >0 || cntR >0
                 then return ()
                 else markExternal l GHC.AnnVal str
          1 -> markWithString GHC.AnnVal str
          x -> error $ "markP.RdrName: too many AnnVal :" ++ showGhc (l,x)
        mark GHC.AnnTildehsh
        mark GHC.AnnTilde
        mark GHC.AnnRarrow
        markOffset GHC.AnnBackquote 1
        mark GHC.AnnCloseP -- ')'

-- ---------------------------------------------------------------------

-- TODO: What is this used for? Not in ExactPrint
instance Annotate GHC.Name where
  markAST l n = do
    markExternal l GHC.AnnVal (showGhc n)

-- ---------------------------------------------------------------------

instance (GHC.DataId name,Annotate name)
  => Annotate (GHC.ImportDecl name) where
 markAST _ imp@(GHC.ImportDecl msrc (GHC.L ln _) _pkg src safeflag _qual _impl _as hiding) = do

   -- 'import' maybe_src maybe_safe optqualified maybe_pkg modid maybeas maybeimpspec
   mark GHC.AnnImport

   -- "{-# SOURCE" and "#-}"
   when src (markWithString GHC.AnnOpen (fromMaybe "{-# SOURCE" msrc)
             >> markWithString GHC.AnnClose "#-}")
   when safeflag (mark GHC.AnnSafe)
   mark GHC.AnnQualified
   mark GHC.AnnPackageName

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

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,Annotate name)
  => Annotate (GHC.HsDecl name) where
  markAST l decl = do
    case decl of
      GHC.TyClD d       -> markAST l d
      GHC.InstD d       -> markAST l d
      GHC.DerivD d      -> markAST l d
      GHC.ValD d        -> markAST l d
      GHC.SigD d        -> markAST l d
      GHC.DefD d        -> markAST l d
      GHC.ForD d        -> markAST l d
      GHC.WarningD d    -> markAST l d
      GHC.AnnD d        -> markAST l d
      GHC.RuleD d       -> markAST l d
      GHC.VectD d       -> markAST l d
      GHC.SpliceD d     -> markAST l d
      GHC.DocD d        -> markAST l d
      GHC.QuasiQuoteD d -> markAST l d
      GHC.RoleAnnotD d  -> markAST l d

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

instance (Annotate name)
   => Annotate (GHC.HsQuasiQuote name) where
  markAST _ (GHC.HsQuasiQuote _n _ss _fs) = assert False undefined

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,Annotate name)
   => Annotate (GHC.SpliceDecl name) where
  markAST _ (GHC.SpliceDecl (GHC.L _ (GHC.HsSplice _n e)) flag) = do
    case flag of
      GHC.ExplicitSplice ->
        markWithString GHC.AnnOpen "$("
      GHC.ImplicitSplice ->
        markWithString GHC.AnnOpen "$$("
    markLocated e
    markWithString GHC.AnnClose ")"

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,Annotate name)
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

  markAST _ (GHC.HsVectTypeOut {}) = error $ "markP.HsVectTypeOut: only valid after type checker"

  markAST _ (GHC.HsVectClassIn src ln) = do
    markWithString GHC.AnnOpen src -- "{-# VECTORISE"
    mark GHC.AnnClass
    markLocated ln
    markWithString GHC.AnnClose "#-}" -- "#-}"

  markAST _ (GHC.HsVectClassOut {}) = error $ "markP.HsVectClassOut: only valid after type checker"
  markAST _ (GHC.HsVectInstIn {})   = error $ "markP.HsVectInstIn: not supported?"
  markAST _ (GHC.HsVectInstOut {})   = error $ "markP.HsVectInstOut: not supported?"

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,Annotate name)
   => Annotate (GHC.RuleDecls name) where
   markAST _ (GHC.HsRules src rules) = do
     markWithString GHC.AnnOpen src
     mapM_ markLocated rules
     markWithString GHC.AnnClose "#-}"

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,Annotate name)
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

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,Annotate name)
   => Annotate (GHC.RuleBndr name) where
  markAST _ (GHC.RuleBndr ln) = markLocated ln
  markAST _ (GHC.RuleBndrSig ln (GHC.HsWB thing _ _ _)) = do
    mark GHC.AnnOpenP -- "("
    markLocated ln
    mark GHC.AnnDcolon
    markLocated thing
    mark GHC.AnnCloseP -- ")"

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,Annotate name)
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
  markAST l fs = markExternal l GHC.AnnVal (show (GHC.unpackFS fs))

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,Annotate name)
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
    markExternal ls GHC.AnnVal ("\"" ++ src ++ "\"")
    markLocated ln
    mark GHC.AnnDcolon
    markLocated typ


  markAST _l (GHC.ForeignExport ln typ _ (GHC.CExport spec (GHC.L ls src))) = do
    mark GHC.AnnForeign
    mark GHC.AnnExport
    markLocated spec
    markExternal ls GHC.AnnVal ("\"" ++ src ++ "\"")
    markLocated ln
    mark GHC.AnnDcolon
    markLocated typ


-- ---------------------------------------------------------------------

instance (Annotate GHC.CExportSpec) where
  markAST l (GHC.CExportStatic _ cconv) = markAST l cconv

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

instance (GHC.DataId name,GHC.OutputableBndr name,Annotate name)
   => Annotate (GHC.DerivDecl name) where

  markAST _ (GHC.DerivDecl typ mov) = do
    mark GHC.AnnDeriving
    mark GHC.AnnInstance
    markMaybe mov
    markLocated typ

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,Annotate name)
   => Annotate (GHC.DefaultDecl name) where

  markAST _ (GHC.DefaultDecl typs) = do
    mark GHC.AnnDefault
    mark GHC.AnnOpenP -- '('
    mapM_ markLocated typs
    mark GHC.AnnCloseP -- ')'

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,Annotate name)
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

instance (GHC.DataId name,GHC.OutputableBndr name,Annotate name)
   => Annotate (GHC.ClsInstDecl name) where

  markAST _ (GHC.ClsInstDecl poly binds sigs tyfams datafams mov) = do
    mark GHC.AnnInstance
    markMaybe mov
    markLocated poly
    mark GHC.AnnWhere
    mark GHC.AnnOpenC -- '{'
    markInside GHC.AnnSemi

    -- AZ:Need to turn this into a located list annotation.
    -- must merge all the rest
    applyListAnnotations (prepareListAnnotation (GHC.bagToList binds)
                       ++ prepareListAnnotation sigs
                       ++ prepareListAnnotation tyfams
                       ++ prepareListAnnotation datafams
                         )

    mark GHC.AnnCloseC -- '}'

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,Annotate name)
   => Annotate (GHC.TyFamInstDecl name) where

  markAST _ (GHC.TyFamInstDecl eqn _) = do
    mark GHC.AnnType
    mark GHC.AnnInstance
    markLocated eqn

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,Annotate name)
   => Annotate (GHC.DataFamInstDecl name) where

  markAST l (GHC.DataFamInstDecl ln (GHC.HsWB pats _ _ _) defn _) = do
    mark GHC.AnnData
    mark GHC.AnnNewtype
    mark GHC.AnnInstance
    markLocated ln
    mapM_ markLocated pats
    mark GHC.AnnWhere
    mark GHC.AnnEqual
    markDataDefn l defn

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,Annotate name) =>
                                                  Annotate (GHC.HsBind name) where
  markAST _ (GHC.FunBind (GHC.L _ln _n) _ (GHC.MG matches _ _ _) _ _ _) = do
    mapM_ markLocated matches
    -- markMatchGroup l mg

  markAST _ (GHC.PatBind lhs (GHC.GRHSs grhs lb) _typ _fvs _ticks) = do
    markLocated lhs
    mark GHC.AnnEqual
    mapM_ markLocated grhs
    mark GHC.AnnWhere

    -- TODO: Store the following SrcSpan in an AnnList instance for exactPC
    markLocated (GHC.L (getLocalBindsSrcSpan lb) lb)

  markAST _ (GHC.VarBind _n rhse _) =
    -- Note: this bind is introduced by the typechecker
    markLocated rhse

  markAST l (GHC.PatSynBind (GHC.PSB ln _fvs args def dir)) = do
    mark GHC.AnnPattern
    markLocated ln
    case args of
      GHC.InfixPatSyn la lb -> do
        markLocated la
        markLocated lb
      GHC.PrefixPatSyn ns -> do
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

    return ()

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,Annotate name)
    => Annotate (GHC.IPBind name) where
  markAST _ (GHC.IPBind en e) = do
    case en of
      Left n -> markLocated n
      Right _i -> error $ "markP.IPBind:should not happen"
    mark GHC.AnnEqual
    markLocated e

-- ---------------------------------------------------------------------

instance Annotate GHC.HsIPName where
  markAST l (GHC.HsIPName n) = markExternal l (GHC.AnnVal) ("?" ++ GHC.unpackFS n)

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,Annotate name,
                                                  Annotate body)
  => Annotate (GHC.Match name (GHC.Located body)) where

  markAST _ (GHC.Match mln pats _typ (GHC.GRHSs grhs lb)) = do
    let
      get_infix Nothing = False
      get_infix (Just (_,f)) = f
    case (get_infix mln,pats) of
      (True,[a,b]) -> do
        markLocated a
        case mln of
          Nothing -> do
            markWithString GHC.AnnOpen "`" -- possible '`'
            mark GHC.AnnFunId
            markWithString GHC.AnnClose "`"-- possible '`'
          Just (n,_) -> markLocated n
        markLocated b
      _ -> do
        case mln of
          Nothing -> mark GHC.AnnFunId
          Just (n,_) -> markLocated n
        mapM_ markLocated pats

    mark GHC.AnnEqual
    mark GHC.AnnRarrow -- For HsLam

    mapM_ markLocated grhs

    mark GHC.AnnWhere
    mark GHC.AnnOpenC -- '{'
    markInside GHC.AnnSemi
    markWithLayout (GHC.L (getLocalBindsSrcSpan lb) lb)
    mark GHC.AnnCloseC -- '}'

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,Annotate name,
                                                  Annotate body)
  => Annotate (GHC.GRHS name (GHC.Located body)) where
  markAST _ (GHC.GRHS guards expr) = do

    mark GHC.AnnVbar
    mapM_ markLocated guards
    mark GHC.AnnEqual
    mark GHC.AnnRarrow -- in case alts
    markLocated expr

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,Annotate name)
  => Annotate (GHC.Sig name) where

  markAST _ (GHC.TypeSig lns typ _) = do
    mapM_ markLocated lns
    mark GHC.AnnDcolon
    markLocated typ

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

  markAST _ (GHC.IdSig _) = return ()

  -- FixSig (FixitySig name)
  markAST _ (GHC.FixSig (GHC.FixitySig lns (GHC.Fixity v fdir))) = do
    let fixstr = case fdir of
         GHC.InfixL -> "infixl"
         GHC.InfixR -> "infixr"
         GHC.InfixN -> "infix"
    markWithString GHC.AnnInfix fixstr
    markWithString GHC.AnnVal (show v)
    mapM_ markLocated lns

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


  markAST _ (GHC.SpecSig ln typs inl) = do
    markWithString GHC.AnnOpen (GHC.inl_src inl)
    mark GHC.AnnOpenS --  '['
    mark GHC.AnnTilde -- ~
    markWithString GHC.AnnVal  "TODO: What here"

    mark GHC.AnnCloseS -- ']'
    markLocated ln
    mark GHC.AnnDcolon -- '::'
    mapM_ markLocated typs
    markWithString GHC.AnnClose "#-}" -- '#-}'


  -- '{-# SPECIALISE' 'instance' inst_type '#-}'
  markAST _ (GHC.SpecInstSig src typ) = do
    markWithString GHC.AnnOpen src
    mark GHC.AnnInstance
    markLocated typ
    markWithString GHC.AnnClose "#-}" -- '#-}'


  -- MinimalSig (BooleanFormula (Located name))
  markAST _ (GHC.MinimalSig src  formula) = do
    markWithString GHC.AnnOpen src
    markBooleanFormula formula
    markWithString GHC.AnnClose "#-}"


-- ---------------------------------------------------------------------

markBooleanFormula :: GHC.BooleanFormula (GHC.Located name) -> Annotated ()
markBooleanFormula = assert False undefined

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,Annotate name) =>
                     Annotate (GHC.HsTyVarBndr name) where
  markAST l (GHC.UserTyVar n) = do
    markAST l n

  markAST _ (GHC.KindedTyVar n ty) = do
    mark GHC.AnnOpenP  -- '('
    markLocated n
    mark GHC.AnnDcolon -- '::'
    markLocated ty
    mark GHC.AnnCloseP -- '('

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,Annotate name)
   => Annotate (GHC.HsType name) where

  markAST _ (GHC.HsForAllTy _f mwc (GHC.HsQTvs _kvs tvs) ctx@(GHC.L lc ctxs) typ) = do
    mark GHC.AnnOpenP -- "("
    mark GHC.AnnForall
    mapM_ markLocated tvs
    mark GHC.AnnDot

    case mwc of
      Nothing -> if lc /= GHC.noSrcSpan then markLocated ctx else return ()
      Just lwc -> markLocated (GHC.L lc (GHC.sortLocated ((GHC.L lwc GHC.HsWildcardTy):ctxs)))

    mark GHC.AnnDarrow
    markLocated typ
    mark GHC.AnnCloseP -- ")"

  markAST l (GHC.HsTyVar n) = do
    mark GHC.AnnDcolon -- for HsKind, alias for HsType
    markAST l n

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
    markLocated lo
    markLocated t2

  markAST _ (GHC.HsParTy t) = do
    mark GHC.AnnDcolon -- for HsKind, alias for HsType
    mark GHC.AnnOpenP  -- '('
    markLocated t
    mark GHC.AnnCloseP -- ')'

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

  -- HsQuasiQuoteTy (HsQuasiQuote name)
  -- TODO: Probably wrong
  markAST l (GHC.HsQuasiQuoteTy (GHC.HsQuasiQuote n _ss q)) = do
    markExternal l GHC.AnnVal
      ("[" ++ (showGhc n) ++ "|" ++ (GHC.unpackFS q) ++ "|]")

  -- HsSpliceTy (HsSplice name) (PostTc name Kind)
  markAST _ (GHC.HsSpliceTy (GHC.HsSplice _is e) _) = do
    markWithString GHC.AnnOpen "$(" -- '$('
    markLocated e
    markWithString GHC.AnnClose ")" -- ')'

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
  markAST _ (GHC.HsCoreTy _t) = return ()

  markAST _ (GHC.HsExplicitListTy _ ts) = do
    -- TODO: what about SIMPLEQUOTE?
    markWithString GHC.AnnOpen "'[" -- "'["
    mapM_ markLocated ts
    mark GHC.AnnCloseS -- ']'

  markAST _ (GHC.HsExplicitTupleTy _ ts) = do
    markWithString GHC.AnnOpen "'(" -- "'("
    mapM_ markLocated ts
    markWithString GHC.AnnClose ")" -- ')'

  -- HsTyLit HsTyLit
  markAST l (GHC.HsTyLit lit) = do
    case lit of
      (GHC.HsNumTy s _) ->
        markExternal l GHC.AnnVal s
      (GHC.HsStrTy s _) ->
        markExternal l GHC.AnnVal s

  -- HsWrapTy HsTyAnnotated (HsType name)
  markAST _ (GHC.HsWrapTy _ _) = return ()

  markAST l (GHC.HsWildcardTy) = do
    markExternal l GHC.AnnVal "_"
    mark GHC.AnnDarrow -- if only part of a partial type signature context
-- TODO: Probably wrong
  markAST l (GHC.HsNamedWildcardTy n) = do
    markExternal l GHC.AnnVal  (showGhc n)

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,Annotate name) =>
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

instance (GHC.DataId name,Annotate name,GHC.OutputableBndr name)
  => Annotate (GHC.Pat name) where
  markAST l (GHC.WildPat _) = markExternal l GHC.AnnVal "_"
  -- TODO: probably wrong
  markAST l (GHC.VarPat n)  = markExternal l GHC.AnnVal (showGhc n)
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

  markAST _ (GHC.ConPatOut {}) = return ()

  -- ViewPat (LHsExpr id) (LPat id) (PostTc id Type)
  markAST _ (GHC.ViewPat e pat _) = do
    markLocated e
    mark GHC.AnnRarrow
    markLocated pat

  -- SplicePat (HsSplice id)
  markAST _ (GHC.SplicePat (GHC.HsSplice _ e)) = do
    markWithString GHC.AnnOpen "$(" -- '$('
    markLocated e
    markWithString GHC.AnnClose ")" -- ')'

  -- QuasiQuotePat (HsQuasiQuote id)
  -- TODO
  markAST l (GHC.QuasiQuotePat (GHC.HsQuasiQuote n _ q)) = do
    markExternal l GHC.AnnVal
      ("[" ++ (showGhc n) ++ "|" ++ (GHC.unpackFS q) ++ "|]")

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

  markAST l (GHC.SigPatIn pat ty) = do
    markLocated pat
    mark GHC.AnnDcolon
    markAST l ty

  markAST _ (GHC.SigPatOut {}) = return ()

  -- CoPat HsAnnotated (Pat id) Type
  markAST _ (GHC.CoPat {}) = return ()

-- ---------------------------------------------------------------------
hsLit2String :: GHC.HsLit -> GHC.SourceText
hsLit2String lit =
  case lit of
    GHC.HsChar       src _   -> src
    GHC.HsCharPrim   src _   -> src
    GHC.HsString     src _   -> src
    GHC.HsStringPrim src _   -> src
    GHC.HsInt        src _   -> src
    GHC.HsIntPrim    src _   -> src
    GHC.HsWordPrim   src _   -> src
    GHC.HsInt64Prim  src _   -> src
    GHC.HsWord64Prim src _   -> src
    GHC.HsInteger    src _ _ -> src
    GHC.HsRat        (GHC.FL src _) _ -> src
    GHC.HsFloatPrim  (GHC.FL src _)   -> src
    GHC.HsDoublePrim (GHC.FL src _)   -> src

markHsConPatDetails :: (GHC.DataId name,GHC.OutputableBndr name,Annotate name)
                      => GHC.Located name -> GHC.HsConPatDetails name -> Annotated ()
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

markHsConDeclDetails :: (GHC.DataId name,GHC.OutputableBndr name,Annotate name)
                    =>  [GHC.Located name] -> GHC.HsConDeclDetails name -> Annotated ()
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

instance (GHC.DataId name,GHC.OutputableBndr name,Annotate name)
   => Annotate [GHC.LConDeclField name] where
  markAST _ fs = do
       mark GHC.AnnOpenC -- '{'
       mapM_ markLocated fs
       mark GHC.AnnDotdot
       mark GHC.AnnCloseC -- '}'

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
  markAST _ (GHC.HsWB thing _ _ _) = markLocated thing

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,Annotate name,Annotate body) =>
                            Annotate (GHC.Stmt name (GHC.Located body)) where

  markAST _ (GHC.LastStmt body _) = markLocated body

  markAST _ (GHC.BindStmt pat body _ _) = do
    markLocated pat
    mark GHC.AnnLarrow
    markLocated body
    mark GHC.AnnVbar -- possible in list comprehension

  markAST _ (GHC.BodyStmt body _ _ _) = do
    markLocated body

  markAST _ (GHC.LetStmt lb) = do
    -- return () `debug` ("markP.LetStmt entered")
    mark GHC.AnnLet
    mark GHC.AnnOpenC -- '{'
    markWithLayout (GHC.L (getLocalBindsSrcSpan lb) lb)
    mark GHC.AnnCloseC -- '}'
    -- return () `debug` ("markP.LetStmt done")

  markAST _ (GHC.ParStmt pbs _ _) = do
    mapM_ markParStmtBlock pbs

  markAST _ (GHC.TransStmt form stmts _b using by _ _ _) = do
    mapM_ markLocated stmts
    case form of
      GHC.ThenForm -> do
        mark GHC.AnnThen
        markLocated using
        mark GHC.AnnBy
        case by of
          Just b -> markLocated b
          Nothing -> return ()
      GHC.GroupForm -> do
        mark GHC.AnnThen
        mark GHC.AnnGroup
        mark GHC.AnnBy
        case by of
          Just b -> markLocated b
          Nothing -> return ()
        mark GHC.AnnUsing
        markLocated using

  markAST _ (GHC.RecStmt stmts _ _ _ _ _ _ _ _) = do
    mark GHC.AnnRec
    mark GHC.AnnOpenC
    markInside GHC.AnnSemi
    mapM_ markLocated stmts
    mark GHC.AnnCloseC

-- ---------------------------------------------------------------------

markParStmtBlock :: (GHC.DataId name,GHC.OutputableBndr name, Annotate name)
  =>  GHC.ParStmtBlock name name -> Annotated ()
markParStmtBlock (GHC.ParStmtBlock stmts _ns _) =
  mapM_ markLocated stmts

-- ---------------------------------------------------------------------

-- | Local binds need to be indented as a group, and thus need to have a
-- SrcSpan around them so they can be processed via the normal
-- markLocated / exactPC machinery.
getLocalBindsSrcSpan :: GHC.HsLocalBinds name -> GHC.SrcSpan
getLocalBindsSrcSpan (GHC.HsValBinds (GHC.ValBindsIn binds sigs))
  = case spans of
      []  -> GHC.noSrcSpan
      sss -> GHC.combineSrcSpans (head sss) (last sss)
  where
    spans = sort (map GHC.getLoc (GHC.bagToList binds) ++ map GHC.getLoc sigs)

getLocalBindsSrcSpan (GHC.HsValBinds (GHC.ValBindsOut {}))
   = error "getLocalBindsSrcSpan: only valid after type checking"

getLocalBindsSrcSpan (GHC.HsIPBinds (GHC.IPBinds binds _))
  = case sort (map GHC.getLoc binds) of
      [] -> GHC.noSrcSpan
      sss -> GHC.combineSrcSpans (head sss) (last sss)

getLocalBindsSrcSpan (GHC.EmptyLocalBinds) = GHC.noSrcSpan

-- ---------------------------------------------------------------------

-- | Generate a SrcSpan that enclosed the given list
getListSrcSpan :: [GHC.Located a] -> GHC.SrcSpan
getListSrcSpan ls
  = case ls of
      []  -> GHC.noSrcSpan
      sss -> GHC.combineLocs (head sss) (last sss)

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,Annotate name)
  => Annotate (GHC.HsLocalBinds name) where
  markAST _ lb = markHsLocalBinds lb

-- ---------------------------------------------------------------------

markHsLocalBinds :: (GHC.DataId name,GHC.OutputableBndr name,Annotate name)
                     => (GHC.HsLocalBinds name) -> Annotated ()
markHsLocalBinds (GHC.HsValBinds (GHC.ValBindsIn binds sigs)) = do
    applyListAnnotations (prepareListAnnotation (GHC.bagToList binds)
                       ++ prepareListAnnotation sigs
                         )
markHsLocalBinds (GHC.HsValBinds (GHC.ValBindsOut {}))
   = error $ "markHsLocalBinds: only valid after type checking"

markHsLocalBinds (GHC.HsIPBinds (GHC.IPBinds binds _)) = mapM_ markLocated binds
markHsLocalBinds (GHC.EmptyLocalBinds)                 = return ()

-- ---------------------------------------------------------------------

markMatchGroup :: (GHC.DataId name,GHC.OutputableBndr name,Annotate name,
                                               Annotate body)
                   => GHC.SrcSpan -> (GHC.MatchGroup name (GHC.Located body))
                   -> Annotated ()
markMatchGroup l (GHC.MG matches _ _ _)
  = markListWithLayout l matches

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,Annotate name,
                                               Annotate body)
  => Annotate [GHC.Located (GHC.Match name (GHC.Located body))] where
  markAST _ ls = mapM_ markLocated ls

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,Annotate name)
  => Annotate (GHC.HsExpr name) where
  markAST l (GHC.HsVar n)           = markAST l n
  markAST l (GHC.HsIPVar (GHC.HsIPName v))         =
    markExternal l GHC.AnnVal ("?" ++ GHC.unpackFS v)
  markAST l (GHC.HsOverLit ov)     = markAST l ov
  markAST l (GHC.HsLit lit)           = markAST l lit

  markAST l (GHC.HsLam match)       = do
    mark GHC.AnnLam
    markMatchGroup l match

  markAST l (GHC.HsLamCase _ match) = markMatchGroup l match

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
    markMatchGroup l matches
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
    mapM_ markLocated rhs

  markAST _ (GHC.HsLet binds e) = do
    mark GHC.AnnLet
    setLayoutFlag GHC.AnnLet (do -- Make sure the 'in' gets indented too
      mark GHC.AnnOpenC
      markInside GHC.AnnSemi
      markWithLayout (GHC.L (getLocalBindsSrcSpan binds) binds)
      mark GHC.AnnCloseC
      mark GHC.AnnIn
      markLocated e)

  markAST l (GHC.HsDo cts es _) = do
    mark GHC.AnnDo
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
        markListWithLayout l es
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
    mark GHC.AnnDotdot
    mapM_ markLocated fs
    mark GHC.AnnCloseC

  markAST _ (GHC.RecordUpd e (GHC.HsRecFields fs _) _cons _ _) = do
    markLocated e
    mark GHC.AnnOpenC
    mark GHC.AnnDotdot
    mapM_ markLocated fs
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
    markWithString GHC.AnnVal (GHC.unpackFS csFStr)
    markWithString GHC.AnnValStr ("\"" ++ GHC.unpackFS csFStr ++ "\"")
    markWithString GHC.AnnClose "#-}"
    markLocated e

  markAST _ (GHC.HsCoreAnn src csFStr e) = do
    markWithString GHC.AnnOpen src -- "{-# CORE"
    markWithString GHC.AnnVal (GHC.unpackFS csFStr)
    markWithString GHC.AnnClose "#-}"
    markLocated e
  -- TODO: make monomorphic
  markAST l (GHC.HsBracket (GHC.VarBr single v)) =
    let str =
          if single then ("'"  ++ showGhc v)
                    else ("''" ++ showGhc v)
    in
    markExternal l GHC.AnnVal str
  markAST _ (GHC.HsBracket (GHC.DecBrL ds)) = do
    markWithString GHC.AnnOpen "[d|"
    mark GHC.AnnOpenC
    mapM_ markLocated ds
    mark GHC.AnnCloseC
    markWithString GHC.AnnClose "|]"
  markAST _ (GHC.HsBracket (GHC.ExpBr e)) = do
    markWithString GHC.AnnOpen "[|"
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

  markAST _ (GHC.HsRnBracketOut _ _) = return ()
  markAST _ (GHC.HsTcBracketOut _ _) = return ()

  markAST _ (GHC.HsSpliceE False (GHC.HsSplice _ e)) = do
    markWithString GHC.AnnOpen "$("
    markLocated e
    markWithString GHC.AnnClose ")"

  markAST _ (GHC.HsSpliceE True (GHC.HsSplice _ e)) = do
    markWithString GHC.AnnOpen "$$("
    markLocated e
    markWithString GHC.AnnClose ")"

  markAST l (GHC.HsQuasiQuoteE (GHC.HsQuasiQuote n _ q)) = do
    markExternal l GHC.AnnVal
      ("[" ++ (showGhc n) ++ "|" ++ (GHC.unpackFS q) ++ "|]")


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

  markAST _ (GHC.HsWrap _ _) = return ()
  markAST _ (GHC.HsUnboundVar _) = return ()

instance Annotate GHC.HsLit where
  markAST l lit = markExternal l GHC.AnnVal (hsLit2String lit)
-- ---------------------------------------------------------------------

-- |Used for declarations that need to be aligned together, e.g. in a
-- do or let .. in statement/expr
instance (GHC.DataId name,GHC.OutputableBndr name,Annotate name)
  => Annotate ([GHC.ExprLStmt name]) where
  markAST _ ls = mapM_ markLocated ls

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,Annotate name)
  => Annotate (GHC.HsTupArg name) where
  markAST _ (GHC.Present e) = do
    markLocated e

  markAST _ (GHC.Missing _) = do
    mark GHC.AnnComma

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,Annotate name)
  => Annotate (GHC.HsCmdTop name) where
  markAST _ (GHC.HsCmdTop cmd _ _ _) = markLocated cmd

instance (GHC.DataId name,GHC.OutputableBndr name,Annotate name)
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
    markLocated e
    mapM_ markLocated cs
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
    markMatchGroup l matches
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
    markWithLayout (GHC.L (getLocalBindsSrcSpan binds) binds)
    mark GHC.AnnCloseC
    mark GHC.AnnIn
    markLocated e

  markAST l (GHC.HsCmdDo es _) = do
    mark GHC.AnnDo
    mark GHC.AnnOpenC
    -- mapM_ markLocated es
    markListWithLayout l es
    mark GHC.AnnCloseC

  markAST _ (GHC.HsCmdCast {}) = error $ "markP.HsCmdCast: only valid after type checker"


-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,Annotate name)
  => Annotate [GHC.Located (GHC.StmtLR name name (GHC.LHsCmd name))] where
  markAST _ ls = mapM_ markLocated ls

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,Annotate name)
     => Annotate (GHC.TyClDecl name) where

  markAST l (GHC.FamDecl famdecl) = markAST l famdecl

  markAST _ (GHC.SynDecl ln (GHC.HsQTvs _ tyvars) typ _) = do
    mark GHC.AnnType
    markLocated ln
    mapM_ markLocated tyvars
    mark GHC.AnnEqual
    markLocated typ

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
    mapM_ markLocated cons
    markMaybe mderivs

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

-- ---------------------------------------------------------------------

markTyClass :: (Annotate a, Annotate ast)
                => GHC.Located a -> [GHC.Located ast] -> Annotated ()
markTyClass ln tyVars = do
    markMany GHC.AnnOpenP
    applyListAnnotations (prepareListAnnotation [ln]
                      ++ prepareListAnnotation (take 2 tyVars))
    markMany GHC.AnnCloseP
    mapM_ markLocated (drop 2 tyVars)

-- ---------------------------------------------------------------------

instance (GHC.DataId name,Annotate name, GHC.OutputableBndr name)
   => Annotate (GHC.FamilyDecl name) where
  markAST _ (GHC.FamilyDecl info ln (GHC.HsQTvs _ tyvars) mkind) = do
    mark GHC.AnnType
    mark GHC.AnnData
    mark GHC.AnnFamily
    markLocated ln
    mapM_ markLocated tyvars
    mark GHC.AnnDcolon
    markMaybe mkind
    mark GHC.AnnWhere
    mark GHC.AnnOpenC -- {
    case info of
      GHC.ClosedTypeFamily eqns -> mapM_ markLocated eqns
      _ -> return ()
    case info of
      GHC.ClosedTypeFamily eqns -> mapM_ markLocated eqns
      _ -> return ()
    mark GHC.AnnCloseC -- }

-- ---------------------------------------------------------------------

instance (GHC.DataId name,Annotate name,GHC.OutputableBndr name)
   => Annotate (GHC.TyFamInstEqn name) where
  markAST _ (GHC.TyFamEqn ln (GHC.HsWB pats _ _ _) typ) = do
    markLocated ln
    mapM_ markLocated pats
    mark GHC.AnnEqual
    markLocated typ


-- ---------------------------------------------------------------------

instance (GHC.DataId name,Annotate name,GHC.OutputableBndr name)
  => Annotate (GHC.TyFamDefltEqn name) where
  markAST _ (GHC.TyFamEqn ln (GHC.HsQTvs _ns bndrs) typ) = do
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

markDataDefn :: (GHC.DataId name,GHC.OutputableBndr name,Annotate name)
  => GHC.SrcSpan -> GHC.HsDataDefn name -> Annotated ()
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
instance (GHC.DataId name,GHC.OutputableBndr name,Annotate name)
     => Annotate [GHC.LHsType name] where
  markAST l ts = do
    return () `debug` ("markP.HsContext:l=" ++ showGhc l)
    mark GHC.AnnDeriving
    mark GHC.AnnOpenP
    mapM_ markLocated ts
    mark GHC.AnnCloseP
    mark GHC.AnnDarrow

-- ---------------------------------------------------------------------

instance (GHC.DataId name,Annotate name,GHC.OutputableBndr name)
      => Annotate (GHC.ConDecl name) where
  markAST _ (GHC.ConDecl lns _expr (GHC.HsQTvs _ns bndrs) ctx
                         dets res _ _) = do
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

        markHsConDeclDetails lns dets

        mark GHC.AnnDcolon

        markLocated (GHC.L ls (ResTyGADTHook bndrs))

        markLocated ctx
        mark GHC.AnnDarrow

        markLocated ty


    mark GHC.AnnVbar

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,Annotate name) =>
              Annotate (ResTyGADTHook name) where
  markAST _ (ResTyGADTHook bndrs) = do
    mark GHC.AnnForall
    mapM_ markLocated bndrs
    mark GHC.AnnDot

-- ---------------------------------------------------------------------

instance (Annotate name,Annotate a)
  => Annotate (GHC.HsRecField name (GHC.Located a)) where
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
      Just (GHC.Header h) ->
         markWithString GHC.AnnHeader ("\"" ++ GHC.unpackFS h ++ "\"")
    markWithString GHC.AnnVal ("\"" ++ GHC.unpackFS f ++ "\"")
    markWithString GHC.AnnClose "#-}"

-- ---------------------------------------------------------------------
