{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Language.Haskell.GHC.ExactPrint.Common
       ( AnnotationF(..)
       , annotatePC
       , Wrapped
       , AnnotateGen(..)) where

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
  WithAST  :: Data a => GHC.Located a -> LayoutFlag -> Wrapped b -> (b -> next) -> AnnotationF next
  CountAnns ::  GHC.AnnKeywordId -> (Int -> next) -> AnnotationF next
  -- Abstraction breakers
  SetLayoutFlag ::  next -> AnnotationF next
  OutputKD :: (DeltaPos, (GHC.SrcSpan, KeywordId)) -> next -> AnnotationF next

deriving instance Functor (AnnotationF)


type Marked = Free AnnotationF

-- ---------------------------------------------------------------------

type Wrapped a = Marked a

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

mark :: GHC.AnnKeywordId -> Wrapped ()
mark kwid = markPrim kwid Nothing

markWithString :: GHC.AnnKeywordId -> String -> Wrapped ()
markWithString kwid s = markPrim kwid (Just s)

markOffsetWithString :: GHC.AnnKeywordId -> Int -> String -> Wrapped ()
markOffsetWithString kwid n s = markOffsetPrim kwid n (Just s)

markOffset :: GHC.AnnKeywordId -> Int -> Wrapped ()
markOffset kwid n = markOffsetPrim kwid n Nothing

withAST :: Data a => GHC.Located a -> LayoutFlag -> Wrapped b -> Wrapped b
withAST lss layout action = liftF (WithAST lss layout prog id)
  where
    prog = do
      r <- action
      -- Automatically add any trailing comma or semi
      markAfter GHC.AnnComma
      markOutside GHC.AnnSemi AnnSemiSep
      return r
-- ---------------------------------------------------------------------


-- |First move to the given location, then call exactP
annotatePC :: (AnnotateGen ast) => GHC.Located ast -> Wrapped ()
annotatePC a = withLocated a NoLayoutRules annotateG

withLocated :: Data a => GHC.Located a -> LayoutFlag -> (GHC.SrcSpan -> a -> Wrapped ()) -> Wrapped ()
withLocated a@(GHC.L l ast) layoutFlag action = do
  withAST a layoutFlag (action l ast)

annotateMaybe :: (AnnotateGen ast) => Maybe (GHC.Located ast) -> Wrapped ()
annotateMaybe Nothing    = return ()
annotateMaybe (Just ast) = annotatePC ast

annotateList :: (AnnotateGen ast) => [GHC.Located ast] -> Wrapped ()
annotateList xs = mapM_ annotatePC xs

-- | Flag the item to be annotated as requiring layout.
annotateWithLayout :: AnnotateGen ast => GHC.Located ast -> Wrapped ()
annotateWithLayout a = do
  withLocated a LayoutRules (\l ast -> annotateG l ast)

annotateListWithLayout :: AnnotateGen [GHC.Located ast] => GHC.SrcSpan -> [GHC.Located ast] -> Wrapped ()
annotateListWithLayout l ls = do
  let ss = getListSrcSpan ls
  outputKD $ ((DP (0,0)), (l,AnnList ss))
  annotateWithLayout (GHC.L ss ls)

-- ---------------------------------------------------------------------
-- Managing lists which have been separated, e.g. Sigs and Binds

prepareListAnnotation :: AnnotateGen a => [GHC.Located a] -> [(GHC.SrcSpan,Wrapped ())]
prepareListAnnotation ls = map (\b@(GHC.L l _) -> (l,annotatePC b)) ls

applyListAnnotations :: [(GHC.SrcSpan, Wrapped ())] -> Wrapped ()
applyListAnnotations ls
  = mapM_ snd $ sortBy (\(a,_) (b,_) -> compare a b) ls

-- ---------------------------------------------------------------------

class Data ast => AnnotateGen ast where
  annotateG :: GHC.SrcSpan -> ast -> Wrapped ()

-- ---------------------------------------------------------------------

instance AnnotateGen (GHC.HsModule GHC.RdrName) where
  annotateG _ (GHC.HsModule mmn mexp imps decs mdepr _haddock) = do


    case mmn of
      Nothing -> return ()
      Just (GHC.L ln mn) -> do
        mark GHC.AnnModule
        markExternal ln GHC.AnnVal (GHC.moduleNameString mn)

    case mdepr of
      Nothing -> return ()
      Just depr -> annotatePC depr

    case mexp of
      Nothing   -> return ()
      Just expr -> annotatePC expr

    mark GHC.AnnWhere
    mark GHC.AnnOpenC -- Possible '{'
    markMany GHC.AnnSemi -- possible leading semis
    mapM_ annotatePC imps

    annotateList decs

    mark GHC.AnnCloseC -- Possible '}'

    markEOF

-- ---------------------------------------------------------------------

instance AnnotateGen GHC.WarningTxt where
  annotateG _ (GHC.WarningTxt (GHC.L ls txt) lss) = do
    markExternal ls GHC.AnnOpen txt
    mark GHC.AnnOpenS
    mapM_ annotatePC lss
    mark GHC.AnnCloseS
    markWithString GHC.AnnClose "#-}"

  annotateG _ (GHC.DeprecatedTxt (GHC.L ls txt) lss) = do
    markExternal ls GHC.AnnOpen txt
    mark GHC.AnnOpenS
    mapM_ annotatePC lss
    mark GHC.AnnCloseS
    markWithString GHC.AnnClose "#-}"

-- ---------------------------------------------------------------------

instance (GHC.DataId name,AnnotateGen name)
  => AnnotateGen [GHC.LIE name] where
   annotateG _ ls = do
     mark GHC.AnnHiding -- in an import decl
     mark GHC.AnnOpenP -- '('
     mapM_ annotatePC ls
     mark GHC.AnnCloseP -- ')'

instance (GHC.DataId name,AnnotateGen name)
  => AnnotateGen (GHC.IE name) where
  annotateG _ ie = do

    case ie of
        (GHC.IEVar ln) -> do
          mark GHC.AnnPattern
          mark GHC.AnnType
          annotatePC ln

        (GHC.IEThingAbs ln) -> do
          mark GHC.AnnType
          annotatePC ln

        (GHC.IEThingWith ln ns) -> do
          annotatePC ln
          mark GHC.AnnOpenP
          mapM_ annotatePC ns
          mark GHC.AnnCloseP

        (GHC.IEThingAll ln) -> do
          annotatePC ln
          mark GHC.AnnOpenP
          mark GHC.AnnDotdot
          mark GHC.AnnCloseP

        (GHC.IEModuleContents (GHC.L lm mn)) -> do
          mark GHC.AnnModule
          markExternal lm GHC.AnnVal (GHC.moduleNameString mn)


-- ---------------------------------------------------------------------

instance AnnotateGen GHC.RdrName where
  annotateG l n = do
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
          x -> error $ "annotateP.RdrName: too many AnnVal :" ++ showGhc (l,x)
        mark GHC.AnnTildehsh
        mark GHC.AnnTilde
        mark GHC.AnnRarrow
        markOffset GHC.AnnBackquote 1
        mark GHC.AnnCloseP -- ')'

-- ---------------------------------------------------------------------

-- TODO: What is this used for? Not in ExactPrint
instance AnnotateGen GHC.Name where
  annotateG l n = do
    markExternal l GHC.AnnVal (showGhc n)

-- ---------------------------------------------------------------------

instance (GHC.DataId name,AnnotateGen name)
  => AnnotateGen (GHC.ImportDecl name) where
 annotateG _ imp@(GHC.ImportDecl msrc (GHC.L ln _) _pkg src safeflag _qual _impl _as hiding) = do

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
       annotatePC lie

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateGen name)
  => AnnotateGen (GHC.HsDecl name) where
  annotateG l decl = do
    case decl of
      GHC.TyClD d       -> annotateG l d
      GHC.InstD d       -> annotateG l d
      GHC.DerivD d      -> annotateG l d
      GHC.ValD d        -> annotateG l d
      GHC.SigD d        -> annotateG l d
      GHC.DefD d        -> annotateG l d
      GHC.ForD d        -> annotateG l d
      GHC.WarningD d    -> annotateG l d
      GHC.AnnD d        -> annotateG l d
      GHC.RuleD d       -> annotateG l d
      GHC.VectD d       -> annotateG l d
      GHC.SpliceD d     -> annotateG l d
      GHC.DocD d        -> annotateG l d
      GHC.QuasiQuoteD d -> annotateG l d
      GHC.RoleAnnotD d  -> annotateG l d

-- ---------------------------------------------------------------------

instance (AnnotateGen name)
   => AnnotateGen (GHC.RoleAnnotDecl name) where
  annotateG _ (GHC.RoleAnnotDecl ln mr) = do
    mark GHC.AnnType
    mark GHC.AnnRole
    annotatePC ln
    mapM_ annotatePC mr

instance AnnotateGen (Maybe GHC.Role) where
  annotateG l Nothing  = markExternal l GHC.AnnVal "_"
  annotateG l (Just r) = markExternal l GHC.AnnVal (GHC.unpackFS $ GHC.fsFromRole r)

-- ---------------------------------------------------------------------

instance (AnnotateGen name)
   => AnnotateGen (GHC.HsQuasiQuote name) where
  annotateG _ (GHC.HsQuasiQuote _n _ss _fs) = assert False undefined

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateGen name)
   => AnnotateGen (GHC.SpliceDecl name) where
  annotateG _ (GHC.SpliceDecl (GHC.L _ (GHC.HsSplice _n e)) flag) = do
    case flag of
      GHC.ExplicitSplice ->
        markWithString GHC.AnnOpen "$("
      GHC.ImplicitSplice ->
        markWithString GHC.AnnOpen "$$("
    annotatePC e
    markWithString GHC.AnnClose ")"

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateGen name)
   => AnnotateGen (GHC.VectDecl name) where
  annotateG _ (GHC.HsVect src ln e) = do
    markWithString GHC.AnnOpen src -- "{-# VECTORISE"
    annotatePC ln
    mark GHC.AnnEqual
    annotatePC e
    markWithString GHC.AnnClose "#-}" -- "#-}"

  annotateG _ (GHC.HsNoVect src ln) = do
    markWithString GHC.AnnOpen src -- "{-# NOVECTORISE"
    annotatePC ln
    markWithString GHC.AnnClose "#-}" -- "#-}"

  annotateG _ (GHC.HsVectTypeIn src _b ln mln) = do
    markWithString GHC.AnnOpen src -- "{-# VECTORISE" or "{-# VECTORISE SCALAR"
    mark GHC.AnnType
    annotatePC ln
    mark GHC.AnnEqual
    annotateMaybe mln
    markWithString GHC.AnnClose "#-}" -- "#-}"

  annotateG _ (GHC.HsVectTypeOut {}) = error $ "annotateP.HsVectTypeOut: only valid after type checker"

  annotateG _ (GHC.HsVectClassIn src ln) = do
    markWithString GHC.AnnOpen src -- "{-# VECTORISE"
    mark GHC.AnnClass
    annotatePC ln
    markWithString GHC.AnnClose "#-}" -- "#-}"

  annotateG _ (GHC.HsVectClassOut {}) = error $ "annotateP.HsVectClassOut: only valid after type checker"
  annotateG _ (GHC.HsVectInstIn {})   = error $ "annotateP.HsVectInstIn: not supported?"
  annotateG _ (GHC.HsVectInstOut {})   = error $ "annotateP.HsVectInstOut: not supported?"

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateGen name)
   => AnnotateGen (GHC.RuleDecls name) where
   annotateG _ (GHC.HsRules src rules) = do
     markWithString GHC.AnnOpen src
     mapM_ annotatePC rules
     markWithString GHC.AnnClose "#-}"

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateGen name)
   => AnnotateGen (GHC.RuleDecl name) where
  annotateG _ (GHC.HsRule ln act bndrs lhs _ rhs _) = do
    annotatePC ln
    -- activation
    mark GHC.AnnOpenS -- "["
    mark GHC.AnnTilde
    case act of
      GHC.ActiveBefore n -> markWithString GHC.AnnVal (show n)
      GHC.ActiveAfter n  -> markWithString GHC.AnnVal (show n)
      _                  -> return ()
    mark GHC.AnnCloseS -- "]"

    mark GHC.AnnForall
    mapM_ annotatePC bndrs
    mark GHC.AnnDot

    annotatePC lhs
    mark GHC.AnnEqual
    annotatePC rhs

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateGen name)
   => AnnotateGen (GHC.RuleBndr name) where
  annotateG _ (GHC.RuleBndr ln) = annotatePC ln
  annotateG _ (GHC.RuleBndrSig ln (GHC.HsWB thing _ _ _)) = do
    mark GHC.AnnOpenP -- "("
    annotatePC ln
    mark GHC.AnnDcolon
    annotatePC thing
    mark GHC.AnnCloseP -- ")"

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateGen name)
   => AnnotateGen (GHC.AnnDecl name) where
   annotateG _ (GHC.HsAnnotation src prov e) = do
     markWithString GHC.AnnOpen src
     mark GHC.AnnType
     mark GHC.AnnModule
     case prov of
       (GHC.ValueAnnProvenance n) -> annotatePC n
       (GHC.TypeAnnProvenance n) -> annotatePC n
       (GHC.ModuleAnnProvenance) -> return ()

     annotatePC e
     markWithString GHC.AnnClose "#-}"

-- ---------------------------------------------------------------------

instance AnnotateGen name => AnnotateGen (GHC.WarnDecls name) where
   annotateG _ (GHC.Warnings src warns) = do
     markWithString GHC.AnnOpen src
     mapM_ annotatePC warns
     markWithString GHC.AnnClose "#-}"

-- ---------------------------------------------------------------------

instance (AnnotateGen name)
   => AnnotateGen (GHC.WarnDecl name) where
   annotateG _ (GHC.Warning lns txt) = do
     mapM_ annotatePC lns
     mark GHC.AnnOpenS -- "["
     case txt of
       GHC.WarningTxt    _src ls -> mapM_ annotatePC ls
       GHC.DeprecatedTxt _src ls -> mapM_ annotatePC ls
     mark GHC.AnnCloseS -- "]"

instance AnnotateGen GHC.FastString where
  annotateG l fs = markExternal l GHC.AnnVal (show (GHC.unpackFS fs))

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateGen name)
   => AnnotateGen (GHC.ForeignDecl name) where

  annotateG _ (GHC.ForeignImport ln typ _
               (GHC.CImport cconv safety@(GHC.L ll _) _mh _imp (GHC.L ls src))) = do
    mark GHC.AnnForeign
    mark GHC.AnnImport
    annotatePC cconv
    if ll == GHC.noSrcSpan
      then return ()
      else annotatePC safety
    -- annotateMaybe mh
    markExternal ls GHC.AnnVal ("\"" ++ src ++ "\"")
    annotatePC ln
    mark GHC.AnnDcolon
    annotatePC typ


  annotateG _l (GHC.ForeignExport ln typ _ (GHC.CExport spec (GHC.L ls src))) = do
    mark GHC.AnnForeign
    mark GHC.AnnExport
    annotatePC spec
    markExternal ls GHC.AnnVal ("\"" ++ src ++ "\"")
    annotatePC ln
    mark GHC.AnnDcolon
    annotatePC typ


-- ---------------------------------------------------------------------

instance (AnnotateGen GHC.CExportSpec) where
  annotateG l (GHC.CExportStatic _ cconv) = annotateG l cconv

-- ---------------------------------------------------------------------

instance (AnnotateGen GHC.CCallConv) where
  annotateG l GHC.StdCallConv        =  markExternal l  GHC.AnnVal "stdcall"
  annotateG l GHC.CCallConv          =  markExternal l GHC.AnnVal "ccall"
  annotateG l GHC.CApiConv           =  markExternal l GHC.AnnVal "capi"
  annotateG l GHC.PrimCallConv       =  markExternal l GHC.AnnVal "prim"
  annotateG l GHC.JavaScriptCallConv =  markExternal l GHC.AnnVal "javascript"

-- ---------------------------------------------------------------------

instance (AnnotateGen GHC.Safety) where
  annotateG l GHC.PlayRisky         = markExternal l GHC.AnnVal "unsafe"
  annotateG l GHC.PlaySafe          = markExternal l GHC.AnnVal "safe"
  annotateG l GHC.PlayInterruptible = markExternal l GHC.AnnVal "interruptible"

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateGen name)
   => AnnotateGen (GHC.DerivDecl name) where

  annotateG _ (GHC.DerivDecl typ mov) = do
    mark GHC.AnnDeriving
    mark GHC.AnnInstance
    annotateMaybe mov
    annotatePC typ

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateGen name)
   => AnnotateGen (GHC.DefaultDecl name) where

  annotateG _ (GHC.DefaultDecl typs) = do
    mark GHC.AnnDefault
    mark GHC.AnnOpenP -- '('
    mapM_ annotatePC typs
    mark GHC.AnnCloseP -- ')'

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateGen name)
   => AnnotateGen (GHC.InstDecl name) where

  annotateG l (GHC.ClsInstD      cid) = annotateG l  cid
  annotateG l (GHC.DataFamInstD dfid) = annotateG l dfid
  annotateG l (GHC.TyFamInstD   tfid) = annotateG l tfid

-- ---------------------------------------------------------------------

instance AnnotateGen GHC.OverlapMode where
  annotateG _ (GHC.NoOverlap src) = do
    markWithString GHC.AnnOpen src
    markWithString GHC.AnnClose "#-}"

  annotateG _ (GHC.Overlappable src) = do
    markWithString GHC.AnnOpen src
    markWithString GHC.AnnClose "#-}"

  annotateG _ (GHC.Overlapping src) = do
    markWithString GHC.AnnOpen src
    markWithString GHC.AnnClose "#-}"

  annotateG _ (GHC.Overlaps src) = do
    markWithString GHC.AnnOpen src
    markWithString GHC.AnnClose "#-}"

  annotateG _ (GHC.Incoherent src) = do
    markWithString GHC.AnnOpen src
    markWithString GHC.AnnClose "#-}"

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateGen name)
   => AnnotateGen (GHC.ClsInstDecl name) where

  annotateG _ (GHC.ClsInstDecl poly binds sigs tyfams datafams mov) = do
    mark GHC.AnnInstance
    annotateMaybe mov
    annotatePC poly
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

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateGen name)
   => AnnotateGen (GHC.TyFamInstDecl name) where

  annotateG _ (GHC.TyFamInstDecl eqn _) = do
    mark GHC.AnnType
    mark GHC.AnnInstance
    annotatePC eqn

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateGen name)
   => AnnotateGen (GHC.DataFamInstDecl name) where

  annotateG l (GHC.DataFamInstDecl ln (GHC.HsWB pats _ _ _) defn _) = do
    mark GHC.AnnData
    mark GHC.AnnNewtype
    mark GHC.AnnInstance
    annotatePC ln
    mapM_ annotatePC pats
    mark GHC.AnnWhere
    mark GHC.AnnEqual
    annotateDataDefn l defn

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateGen name) =>
                                                  AnnotateGen (GHC.HsBind name) where
  annotateG _ (GHC.FunBind (GHC.L _ln _n) _ (GHC.MG matches _ _ _) _ _ _) = do
    mapM_ annotatePC matches

  annotateG _ (GHC.PatBind lhs (GHC.GRHSs grhs lb) _typ _fvs _ticks) = do
    annotatePC lhs
    mark GHC.AnnEqual
    mapM_ annotatePC grhs
    mark GHC.AnnWhere

    -- TODO: Store the following SrcSpan in an AnnList instance for exactPC
    annotatePC (GHC.L (getLocalBindsSrcSpan lb) lb)

  annotateG _ (GHC.VarBind _n rhse _) =
    -- Note: this bind is introduced by the typechecker
    annotatePC rhse

  annotateG l (GHC.PatSynBind (GHC.PSB ln _fvs args def dir)) = do
    mark GHC.AnnPattern
    annotatePC ln
    case args of
      GHC.InfixPatSyn la lb -> do
        annotatePC la
        annotatePC lb
      GHC.PrefixPatSyn ns -> do
        mapM_ annotatePC ns
    mark GHC.AnnEqual
    mark GHC.AnnLarrow
    annotatePC def
    case dir of
      GHC.Unidirectional           -> return ()
      GHC.ImplicitBidirectional    -> return ()
      GHC.ExplicitBidirectional mg -> annotateMatchGroup l mg

    mark GHC.AnnWhere
    mark GHC.AnnOpenC  -- '{'
    mark GHC.AnnCloseC -- '}'

    return ()

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateGen name)
    => AnnotateGen (GHC.IPBind name) where
  annotateG _ (GHC.IPBind en e) = do
    case en of
      Left n -> annotatePC n
      Right _i -> error $ "annotateP.IPBind:should not happen"
    mark GHC.AnnEqual
    annotatePC e

-- ---------------------------------------------------------------------

instance AnnotateGen GHC.HsIPName where
  annotateG l (GHC.HsIPName n) = markExternal l (GHC.AnnVal) ("?" ++ GHC.unpackFS n)

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateGen name,
                                                  AnnotateGen body)
  => AnnotateGen (GHC.Match name (GHC.Located body)) where

  annotateG _ (GHC.Match mln pats _typ (GHC.GRHSs grhs lb)) = do
    let
      get_infix Nothing = False
      get_infix (Just (_,f)) = f
    case (get_infix mln,pats) of
      (True,[a,b]) -> do
        annotatePC a
        case mln of
          Nothing -> do
            markWithString GHC.AnnOpen "`" -- possible '`'
            mark GHC.AnnFunId
            markWithString GHC.AnnClose "`"-- possible '`'
          Just (n,_) -> annotatePC n
        annotatePC b
      _ -> do
        case mln of
          Nothing -> mark GHC.AnnFunId
          Just (n,_) -> annotatePC n
        mapM_ annotatePC pats

    mark GHC.AnnEqual
    mark GHC.AnnRarrow -- For HsLam

    mapM_ annotatePC grhs

    mark GHC.AnnWhere
    mark GHC.AnnOpenC -- '{'
    markInside GHC.AnnSemi
    -- annotateHsLocalBinds lb
    annotateWithLayout (GHC.L (getLocalBindsSrcSpan lb) lb)
    mark GHC.AnnCloseC -- '}'

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateGen name,
                                                  AnnotateGen body)
  => AnnotateGen (GHC.GRHS name (GHC.Located body)) where
  annotateG _ (GHC.GRHS guards expr) = do

    mark GHC.AnnVbar
    mapM_ annotatePC guards
    mark GHC.AnnEqual
    mark GHC.AnnRarrow -- in case alts
    annotatePC expr

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateGen name)
  => AnnotateGen (GHC.Sig name) where

  annotateG _ (GHC.TypeSig lns typ _) = do
    mapM_ annotatePC lns
    mark GHC.AnnDcolon
    annotatePC typ

  annotateG _ (GHC.PatSynSig ln (_,GHC.HsQTvs _ns bndrs) ctx1 ctx2 typ) = do
    mark GHC.AnnPattern
    annotatePC ln
    mark GHC.AnnDcolon

    -- Note: The 'forall' bndrs '.' may occur multiple times
    mark GHC.AnnForall
    mapM_ annotatePC bndrs
    mark GHC.AnnDot

    annotatePC ctx1
    markOffset GHC.AnnDarrow 0
    annotatePC ctx2
    markOffset GHC.AnnDarrow 1
    annotatePC typ


  annotateG _ (GHC.GenericSig ns typ) = do
    mark GHC.AnnDefault
    mapM_ annotatePC ns
    mark GHC.AnnDcolon
    annotatePC typ

  annotateG _ (GHC.IdSig _) = return ()

  -- FixSig (FixitySig name)
  annotateG _ (GHC.FixSig (GHC.FixitySig lns (GHC.Fixity v fdir))) = do
    let fixstr = case fdir of
         GHC.InfixL -> "infixl"
         GHC.InfixR -> "infixr"
         GHC.InfixN -> "infix"
    markWithString GHC.AnnInfix fixstr
    markWithString GHC.AnnVal (show v)
    mapM_ annotatePC lns

  -- InlineSig (Located name) InlinePragma
  -- '{-# INLINE' activation qvar '#-}'
  annotateG _ (GHC.InlineSig ln inl) = do
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
    annotatePC ln
    markWithString GHC.AnnClose "#-}" -- '#-}'


  annotateG _ (GHC.SpecSig ln typs inl) = do
    markWithString GHC.AnnOpen (GHC.inl_src inl)
    mark GHC.AnnOpenS --  '['
    mark GHC.AnnTilde -- ~
    markWithString GHC.AnnVal  "TODO: What here"

    mark GHC.AnnCloseS -- ']'
    annotatePC ln
    mark GHC.AnnDcolon -- '::'
    mapM_ annotatePC typs
    markWithString GHC.AnnClose "#-}" -- '#-}'


  -- '{-# SPECIALISE' 'instance' inst_type '#-}'
  annotateG _ (GHC.SpecInstSig src typ) = do
    markWithString GHC.AnnOpen src
    mark GHC.AnnInstance
    annotatePC typ
    markWithString GHC.AnnClose "#-}" -- '#-}'


  -- MinimalSig (BooleanFormula (Located name))
  annotateG _ (GHC.MinimalSig src  formula) = do
    markWithString GHC.AnnOpen src
    annotateBooleanFormula formula
    markWithString GHC.AnnClose "#-}"


-- ---------------------------------------------------------------------

annotateBooleanFormula :: GHC.BooleanFormula (GHC.Located name) -> Wrapped ()
annotateBooleanFormula = assert False undefined

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateGen name) =>
                     AnnotateGen (GHC.HsTyVarBndr name) where
  annotateG l (GHC.UserTyVar n) = do
    annotateG l n

  annotateG _ (GHC.KindedTyVar n ty) = do
    mark GHC.AnnOpenP  -- '('
    annotatePC n
    mark GHC.AnnDcolon -- '::'
    annotatePC ty
    mark GHC.AnnCloseP -- '('

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateGen name)
   => AnnotateGen (GHC.HsType name) where

  annotateG _ (GHC.HsForAllTy _f mwc (GHC.HsQTvs _kvs tvs) ctx@(GHC.L lc ctxs) typ) = do
    mark GHC.AnnOpenP -- "("
    mark GHC.AnnForall
    mapM_ annotatePC tvs
    mark GHC.AnnDot

    case mwc of
      Nothing -> if lc /= GHC.noSrcSpan then annotatePC ctx else return ()
      Just lwc -> annotatePC (GHC.L lc (GHC.sortLocated ((GHC.L lwc GHC.HsWildcardTy):ctxs)))

    mark GHC.AnnDarrow
    annotatePC typ
    mark GHC.AnnCloseP -- ")"

  annotateG l (GHC.HsTyVar n) = do
    mark GHC.AnnDcolon -- for HsKind, alias for HsType
    annotateG l n

  annotateG _ (GHC.HsAppTy t1 t2) = do
    mark GHC.AnnDcolon -- for HsKind, alias for HsType
    annotatePC t1
    annotatePC t2

  annotateG _ (GHC.HsFunTy t1 t2) = do
    mark GHC.AnnDcolon -- for HsKind, alias for HsType
    annotatePC t1
    mark GHC.AnnRarrow
    annotatePC t2

  annotateG _ (GHC.HsListTy t) = do
    mark GHC.AnnDcolon -- for HsKind, alias for HsType
    mark GHC.AnnOpenS -- '['
    annotatePC t
    mark GHC.AnnCloseS -- ']'

  annotateG _ (GHC.HsPArrTy t) = do
    markWithString GHC.AnnOpen "[:" -- '[:'
    annotatePC t
    markWithString GHC.AnnClose ":]" -- ':]'

  annotateG _ (GHC.HsTupleTy _tt ts) = do
    mark GHC.AnnDcolon -- for HsKind, alias for HsType
    markWithString GHC.AnnOpen "(#" -- '(#'
    mark GHC.AnnOpenP  -- '('
    mapM_ annotatePC ts
    mark GHC.AnnCloseP -- ')'
    markWithString GHC.AnnClose "#)" --  '#)'

  annotateG _ (GHC.HsOpTy t1 (_,lo) t2) = do
    annotatePC t1
    annotatePC lo
    annotatePC t2

  annotateG _ (GHC.HsParTy t) = do
    mark GHC.AnnDcolon -- for HsKind, alias for HsType
    mark GHC.AnnOpenP  -- '('
    annotatePC t
    mark GHC.AnnCloseP -- ')'

  annotateG _ (GHC.HsIParamTy (GHC.HsIPName n) t) = do
    markWithString GHC.AnnVal ("?" ++ (GHC.unpackFS n))
    mark GHC.AnnDcolon
    annotatePC t

  annotateG _ (GHC.HsEqTy t1 t2) = do
    annotatePC t1
    mark GHC.AnnTilde
    annotatePC t2

  annotateG _ (GHC.HsKindSig t k) = do
    mark GHC.AnnOpenP  -- '('
    annotatePC t
    mark GHC.AnnDcolon -- '::'
    annotatePC k
    mark GHC.AnnCloseP -- ')'

  -- HsQuasiQuoteTy (HsQuasiQuote name)
  -- TODO: Probably wrong
  annotateG l (GHC.HsQuasiQuoteTy (GHC.HsQuasiQuote n _ss q)) = do
    markExternal l GHC.AnnVal
      ("[" ++ (showGhc n) ++ "|" ++ (GHC.unpackFS q) ++ "|]")

  -- HsSpliceTy (HsSplice name) (PostTc name Kind)
  annotateG _ (GHC.HsSpliceTy (GHC.HsSplice _is e) _) = do
    markWithString GHC.AnnOpen "$(" -- '$('
    annotatePC e
    markWithString GHC.AnnClose ")" -- ')'

  annotateG _ (GHC.HsDocTy t ds) = do
    annotatePC t
    annotatePC ds

  annotateG _ (GHC.HsBangTy b t) = do
    case b of
      (GHC.HsSrcBang ms (Just True) _) -> do
        markWithString GHC.AnnOpen  (maybe "{-# UNPACK" id ms)
        markWithString GHC.AnnClose "#-}"
      (GHC.HsSrcBang ms (Just False) _) -> do
        markWithString GHC.AnnOpen  (maybe "{-# NOUNPACK" id ms)
        markWithString GHC.AnnClose "#-}"
      _ -> return ()
    mark GHC.AnnBang
    annotatePC t

  -- HsRecTy [LConDeclField name]
  annotateG _ (GHC.HsRecTy cons) = do
    mark GHC.AnnOpenC  -- '{'
    mapM_ annotatePC cons
    mark GHC.AnnCloseC -- '}'

  -- HsCoreTy Type
  annotateG _ (GHC.HsCoreTy _t) = return ()

  annotateG _ (GHC.HsExplicitListTy _ ts) = do
    -- TODO: what about SIMPLEQUOTE?
    markWithString GHC.AnnOpen "'[" -- "'["
    mapM_ annotatePC ts
    mark GHC.AnnCloseS -- ']'

  annotateG _ (GHC.HsExplicitTupleTy _ ts) = do
    markWithString GHC.AnnOpen "'(" -- "'("
    mapM_ annotatePC ts
    markWithString GHC.AnnClose ")" -- ')'

  -- HsTyLit HsTyLit
  annotateG l (GHC.HsTyLit lit) = do
    case lit of
      (GHC.HsNumTy s _) ->
        markExternal l GHC.AnnVal s
      (GHC.HsStrTy s _) ->
        markExternal l GHC.AnnVal s

  -- HsWrapTy HsTyWrapped (HsType name)
  annotateG _ (GHC.HsWrapTy _ _) = return ()

  annotateG l (GHC.HsWildcardTy) = do
    markExternal l GHC.AnnVal "_"
    mark GHC.AnnDarrow -- if only part of a partial type signature context
-- TODO: Probably wrong
  annotateG l (GHC.HsNamedWildcardTy n) = do
    markExternal l GHC.AnnVal  (showGhc n)

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateGen name) =>
                             AnnotateGen (GHC.ConDeclField name) where
  annotateG _ (GHC.ConDeclField ns ty mdoc) = do
    mapM_ annotatePC ns
    mark GHC.AnnDcolon
    annotatePC ty
    annotateMaybe mdoc

-- ---------------------------------------------------------------------

instance AnnotateGen GHC.HsDocString where
  annotateG l (GHC.HsDocString s) = do
    markExternal l GHC.AnnVal (GHC.unpackFS s)

-- ---------------------------------------------------------------------

instance (GHC.DataId name,AnnotateGen name,GHC.OutputableBndr name)
  => AnnotateGen (GHC.Pat name) where
  annotateG l (GHC.WildPat _) = markExternal l GHC.AnnVal "_"
  -- TODO: probably wrong
  annotateG l (GHC.VarPat n)  = markExternal l GHC.AnnVal (showGhc n)
  annotateG _ (GHC.LazyPat p) = do
    mark GHC.AnnTilde
    annotatePC p

  annotateG _ (GHC.AsPat ln p) = do
    annotatePC ln
    mark GHC.AnnAt
    annotatePC p

  annotateG _ (GHC.ParPat p) = do
    mark GHC.AnnOpenP
    annotatePC p
    mark GHC.AnnCloseP

  annotateG _ (GHC.BangPat p) = do
    mark GHC.AnnBang
    annotatePC p

  annotateG _ (GHC.ListPat ps _ _) = do
    mark GHC.AnnOpenS
    mapM_ annotatePC ps
    mark GHC.AnnCloseS

  annotateG _ (GHC.TuplePat pats b _) = do
    if b == GHC.Boxed then mark GHC.AnnOpenP
                      else markWithString GHC.AnnOpen "(#"
    mapM_ annotatePC pats
    if b == GHC.Boxed then mark GHC.AnnCloseP
                      else markWithString GHC.AnnClose "#)"

  annotateG _ (GHC.PArrPat ps _) = do
    markWithString GHC.AnnOpen "[:"
    mapM_ annotatePC ps
    markWithString GHC.AnnClose ":]"

  annotateG _ (GHC.ConPatIn n dets) = do
    annotateHsConPatDetails n dets

  annotateG _ (GHC.ConPatOut {}) = return ()

  -- ViewPat (LHsExpr id) (LPat id) (PostTc id Type)
  annotateG _ (GHC.ViewPat e pat _) = do
    annotatePC e
    mark GHC.AnnRarrow
    annotatePC pat

  -- SplicePat (HsSplice id)
  annotateG _ (GHC.SplicePat (GHC.HsSplice _ e)) = do
    markWithString GHC.AnnOpen "$(" -- '$('
    annotatePC e
    markWithString GHC.AnnClose ")" -- ')'

  -- QuasiQuotePat (HsQuasiQuote id)
  -- TODO
  annotateG l (GHC.QuasiQuotePat (GHC.HsQuasiQuote n _ q)) = do
    markExternal l GHC.AnnVal
      ("[" ++ (showGhc n) ++ "|" ++ (GHC.unpackFS q) ++ "|]")

  -- LitPat HsLit
  annotateG l (GHC.LitPat lp) = markExternal l GHC.AnnVal (hsLit2String lp)

  -- NPat (HsOverLit id) (Maybe (SyntaxExpr id)) (SyntaxExpr id)
  annotateG _ (GHC.NPat ol _ _) = do
    mark GHC.AnnMinus
    annotatePC ol

  -- NPlusKPat (Located id) (HsOverLit id) (SyntaxExpr id) (SyntaxExpr id)
  annotateG _ (GHC.NPlusKPat ln ol _ _) = do
    annotatePC ln
    markWithString GHC.AnnVal "+"  -- "+"
    annotatePC ol

  annotateG l (GHC.SigPatIn pat ty) = do
    annotatePC pat
    mark GHC.AnnDcolon
    annotateG l ty

  annotateG _ (GHC.SigPatOut {}) = return ()

  -- CoPat HsWrapped (Pat id) Type
  annotateG _ (GHC.CoPat {}) = return ()

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

annotateHsConPatDetails :: (GHC.DataId name,GHC.OutputableBndr name,AnnotateGen name)
                      => GHC.Located name -> GHC.HsConPatDetails name -> Wrapped ()
annotateHsConPatDetails ln dets = do
  case dets of
    GHC.PrefixCon args -> do
      annotatePC ln
      mapM_ annotatePC args
    GHC.RecCon (GHC.HsRecFields fs _) -> do
      annotatePC ln
      mark GHC.AnnOpenC -- '{'
      mapM_ annotatePC fs
      mark GHC.AnnDotdot
      mark GHC.AnnCloseC -- '}'
    GHC.InfixCon a1 a2 -> do
      annotatePC a1
      annotatePC ln
      annotatePC a2

annotateHsConDeclDetails :: (GHC.DataId name,GHC.OutputableBndr name,AnnotateGen name)
                    =>  [GHC.Located name] -> GHC.HsConDeclDetails name -> Wrapped ()
annotateHsConDeclDetails lns dets = do
  case dets of
    GHC.PrefixCon args -> mapM_ annotatePC args
    GHC.RecCon fs -> do
      mark GHC.AnnOpenC
      annotatePC fs
      mark GHC.AnnCloseC
    GHC.InfixCon a1 a2 -> do
      annotatePC a1
      mapM_ annotatePC lns
      annotatePC a2

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateGen name)
   => AnnotateGen [GHC.LConDeclField name] where
  annotateG _ fs = do
       mark GHC.AnnOpenC -- '{'
       mapM_ annotatePC fs
       mark GHC.AnnDotdot
       mark GHC.AnnCloseC -- '}'

-- ---------------------------------------------------------------------

instance (GHC.DataId name) => AnnotateGen (GHC.HsOverLit name) where
  annotateG l ol =
    let str = case GHC.ol_val ol of
                GHC.HsIntegral src _ -> src
                GHC.HsFractional l2   -> (GHC.fl_text l2)
                GHC.HsIsString src _ -> src
    in
    markExternal l GHC.AnnVal str

-- ---------------------------------------------------------------------

instance (GHC.DataId name,AnnotateGen arg)
    => AnnotateGen (GHC.HsWithBndrs name (GHC.Located arg)) where
  annotateG _ (GHC.HsWB thing _ _ _) = annotatePC thing

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateGen name,AnnotateGen body) =>
                            AnnotateGen (GHC.Stmt name (GHC.Located body)) where

  annotateG _ (GHC.LastStmt body _) = annotatePC body

  annotateG _ (GHC.BindStmt pat body _ _) = do
    annotatePC pat
    mark GHC.AnnLarrow
    annotatePC body
    mark GHC.AnnVbar -- possible in list comprehension

  annotateG _ (GHC.BodyStmt body _ _ _) = do
    annotatePC body

  annotateG _ (GHC.LetStmt lb) = do
    -- return () `debug` ("annotateP.LetStmt entered")
    mark GHC.AnnLet
    mark GHC.AnnOpenC -- '{'
    annotateWithLayout (GHC.L (getLocalBindsSrcSpan lb) lb)
    mark GHC.AnnCloseC -- '}'
    -- return () `debug` ("annotateP.LetStmt done")

  annotateG _ (GHC.ParStmt pbs _ _) = do
    mapM_ annotateParStmtBlock pbs

  annotateG _ (GHC.TransStmt form stmts _b using by _ _ _) = do
    mapM_ annotatePC stmts
    case form of
      GHC.ThenForm -> do
        mark GHC.AnnThen
        annotatePC using
        mark GHC.AnnBy
        case by of
          Just b -> annotatePC b
          Nothing -> return ()
      GHC.GroupForm -> do
        mark GHC.AnnThen
        mark GHC.AnnGroup
        mark GHC.AnnBy
        case by of
          Just b -> annotatePC b
          Nothing -> return ()
        mark GHC.AnnUsing
        annotatePC using

  annotateG _ (GHC.RecStmt stmts _ _ _ _ _ _ _ _) = do
    mark GHC.AnnRec
    mark GHC.AnnOpenC
    markInside GHC.AnnSemi
    mapM_ annotatePC stmts
    mark GHC.AnnCloseC

-- ---------------------------------------------------------------------

annotateParStmtBlock :: (GHC.DataId name,GHC.OutputableBndr name, AnnotateGen name)
  =>  GHC.ParStmtBlock name name -> Wrapped ()
annotateParStmtBlock (GHC.ParStmtBlock stmts _ns _) =
  mapM_ annotatePC stmts

-- ---------------------------------------------------------------------

-- | Local binds need to be indented as a group, and thus need to have a
-- SrcSpan around them so they can be processed via the normal
-- annotatePC / exactPC machinery.
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

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateGen name)
  => AnnotateGen (GHC.HsLocalBinds name) where
  annotateG _ lb = annotateHsLocalBinds lb

-- ---------------------------------------------------------------------

annotateHsLocalBinds :: (GHC.DataId name,GHC.OutputableBndr name,AnnotateGen name)
                     => (GHC.HsLocalBinds name) -> Wrapped ()
annotateHsLocalBinds (GHC.HsValBinds (GHC.ValBindsIn binds sigs)) = do
    applyListAnnotations (prepareListAnnotation (GHC.bagToList binds)
                       ++ prepareListAnnotation sigs
                         )
annotateHsLocalBinds (GHC.HsValBinds (GHC.ValBindsOut {}))
   = error $ "annotateHsLocalBinds: only valid after type checking"

annotateHsLocalBinds (GHC.HsIPBinds (GHC.IPBinds binds _)) = mapM_ annotatePC binds
annotateHsLocalBinds (GHC.EmptyLocalBinds)                 = return ()

-- ---------------------------------------------------------------------

annotateMatchGroup :: (GHC.DataId name,GHC.OutputableBndr name,AnnotateGen name,
                                               AnnotateGen body)
                   => GHC.SrcSpan -> (GHC.MatchGroup name (GHC.Located body))
                   -> Wrapped ()
annotateMatchGroup l (GHC.MG matches _ _ _)
  = annotateListWithLayout l matches

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateGen name,
                                               AnnotateGen body)
  => AnnotateGen [GHC.Located (GHC.Match name (GHC.Located body))] where
  annotateG _ ls = mapM_ annotatePC ls

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateGen name)
  => AnnotateGen (GHC.HsExpr name) where
  annotateG l (GHC.HsVar n)           = annotateG l n
  annotateG l (GHC.HsIPVar (GHC.HsIPName v))         =
    markExternal l GHC.AnnVal ("?" ++ GHC.unpackFS v)
  annotateG l (GHC.HsOverLit ov)     = annotateG l ov
  annotateG l (GHC.HsLit lit)           = annotateG l lit

  annotateG l (GHC.HsLam match)       = do
    mark GHC.AnnLam
    annotateMatchGroup l match

  annotateG l (GHC.HsLamCase _ match) = annotateMatchGroup l match

  annotateG _ (GHC.HsApp e1 e2) = do
    annotatePC e1
    annotatePC e2

  annotateG _ (GHC.OpApp e1 e2 _ e3) = do
    annotatePC e1
    annotatePC e2
    annotatePC e3

  annotateG _ (GHC.NegApp e _) = do
    mark GHC.AnnMinus
    annotatePC e

  annotateG _ (GHC.HsPar e) = do
    mark GHC.AnnOpenP -- '('
    annotatePC e
    mark GHC.AnnCloseP -- ')'

  annotateG _ (GHC.SectionL e1 e2) = do
    annotatePC e1
    annotatePC e2

  annotateG _ (GHC.SectionR e1 e2) = do
    annotatePC e1
    annotatePC e2

  annotateG _ (GHC.ExplicitTuple args b) = do
    if b == GHC.Boxed then mark GHC.AnnOpenP
                      else markWithString GHC.AnnOpen "(#"

    mapM_ annotatePC args

    if b == GHC.Boxed then mark GHC.AnnCloseP
                      else markWithString GHC.AnnClose "#)"

  annotateG l (GHC.HsCase e1 matches) = do
    mark GHC.AnnCase
    annotatePC e1
    mark GHC.AnnOf
    mark GHC.AnnOpenC
    markInside GHC.AnnSemi
    annotateMatchGroup l matches
    mark GHC.AnnCloseC

  annotateG _ (GHC.HsIf _ e1 e2 e3) = do
    mark GHC.AnnIf
    annotatePC e1
    markOffset GHC.AnnSemi 0
    mark GHC.AnnThen
    annotatePC e2
    markOffset GHC.AnnSemi 1
    mark GHC.AnnElse
    annotatePC e3

  annotateG _ (GHC.HsMultiIf _ rhs) = do
    mark GHC.AnnIf
    mapM_ annotatePC rhs

  annotateG _ (GHC.HsLet binds e) = do
    setLayoutFlag -- Make sure the 'in' gets indented too
    mark GHC.AnnLet
    mark GHC.AnnOpenC
    markInside GHC.AnnSemi
    annotateWithLayout (GHC.L (getLocalBindsSrcSpan binds) binds)
    mark GHC.AnnCloseC
    mark GHC.AnnIn
    annotatePC e

  annotateG l (GHC.HsDo cts es _) = do
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
        annotatePC (last es)
        mark GHC.AnnVbar
        mapM_ annotatePC (init es)
      else do
        annotateListWithLayout l es
    mark GHC.AnnCloseS
    mark GHC.AnnCloseC
    markWithString GHC.AnnClose cstr

  annotateG _ (GHC.ExplicitList _ _ es) = do
    mark GHC.AnnOpenS
    mapM_ annotatePC es
    mark GHC.AnnCloseS

  annotateG _ (GHC.ExplicitPArr _ es)   = do
    markWithString GHC.AnnOpen "[:"
    mapM_ annotatePC es
    markWithString GHC.AnnClose ":]"

  annotateG _ (GHC.RecordCon n _ (GHC.HsRecFields fs _)) = do
    annotatePC n
    mark GHC.AnnOpenC
    mark GHC.AnnDotdot
    mapM_ annotatePC fs
    mark GHC.AnnCloseC

  annotateG _ (GHC.RecordUpd e (GHC.HsRecFields fs _) _cons _ _) = do
    annotatePC e
    mark GHC.AnnOpenC
    mark GHC.AnnDotdot
    mapM_ annotatePC fs
    mark GHC.AnnCloseC

  annotateG _ (GHC.ExprWithTySig e typ _) = do
    annotatePC e
    mark GHC.AnnDcolon
    annotatePC typ

  annotateG _ (GHC.ExprWithTySigOut e typ) = do
    annotatePC e
    mark GHC.AnnDcolon
    annotatePC typ

  annotateG _ (GHC.ArithSeq _ _ seqInfo) = do
    mark GHC.AnnOpenS -- '['
    case seqInfo of
        GHC.From e -> do
          annotatePC e
          mark GHC.AnnDotdot
        GHC.FromTo e1 e2 -> do
          annotatePC e1
          mark GHC.AnnDotdot
          annotatePC e2
        GHC.FromThen e1 e2 -> do
          annotatePC e1
          mark GHC.AnnComma
          annotatePC e2
          mark GHC.AnnDotdot
        GHC.FromThenTo e1 e2 e3 -> do
          annotatePC e1
          mark GHC.AnnComma
          annotatePC e2
          mark GHC.AnnDotdot
          annotatePC e3
    mark GHC.AnnCloseS -- ']'

  annotateG _ (GHC.PArrSeq _ seqInfo) = do
    markWithString GHC.AnnOpen "[:" -- '[:'
    case seqInfo of
        GHC.From e -> do
          annotatePC e
          mark GHC.AnnDotdot
        GHC.FromTo e1 e2 -> do
          annotatePC e1
          mark GHC.AnnDotdot
          annotatePC e2
        GHC.FromThen e1 e2 -> do
          annotatePC e1
          mark GHC.AnnComma
          annotatePC e2
          mark GHC.AnnDotdot
        GHC.FromThenTo e1 e2 e3 -> do
          annotatePC e1
          mark GHC.AnnComma
          annotatePC e2
          mark GHC.AnnDotdot
          annotatePC e3
    markWithString GHC.AnnClose ":]" -- ':]'

  annotateG _ (GHC.HsSCC src csFStr e) = do
    markWithString GHC.AnnOpen src -- "{-# SCC"
    markWithString GHC.AnnVal (GHC.unpackFS csFStr)
    markWithString GHC.AnnValStr ("\"" ++ GHC.unpackFS csFStr ++ "\"")
    markWithString GHC.AnnClose "#-}"
    annotatePC e

  annotateG _ (GHC.HsCoreAnn src csFStr e) = do
    markWithString GHC.AnnOpen src -- "{-# CORE"
    markWithString GHC.AnnVal (GHC.unpackFS csFStr)
    markWithString GHC.AnnClose "#-}"
    annotatePC e
  -- TODO: make monomorphic
  annotateG l (GHC.HsBracket (GHC.VarBr single v)) =
    let str =
          if single then ("'"  ++ showGhc v)
                    else ("''" ++ showGhc v)
    in
    markExternal l GHC.AnnVal str
  annotateG _ (GHC.HsBracket (GHC.DecBrL ds)) = do
    markWithString GHC.AnnOpen "[d|"
    mark GHC.AnnOpenC
    mapM_ annotatePC ds
    mark GHC.AnnCloseC
    markWithString GHC.AnnClose "|]"
  annotateG _ (GHC.HsBracket (GHC.ExpBr e)) = do
    markWithString GHC.AnnOpen "[|"
    annotatePC e
    markWithString GHC.AnnClose "|]"
  annotateG _ (GHC.HsBracket (GHC.TExpBr e)) = do
    markWithString GHC.AnnOpen "[||"
    annotatePC e
    markWithString GHC.AnnClose "||]"
  annotateG _ (GHC.HsBracket (GHC.TypBr e)) = do
    markWithString GHC.AnnOpen "[t|"
    annotatePC e
    markWithString GHC.AnnClose "|]"
  annotateG _ (GHC.HsBracket (GHC.PatBr e)) = do
    markWithString GHC.AnnOpen  "[p|"
    annotatePC e
    markWithString GHC.AnnClose "|]"

  annotateG _ (GHC.HsRnBracketOut _ _) = return ()
  annotateG _ (GHC.HsTcBracketOut _ _) = return ()

  annotateG _ (GHC.HsSpliceE False (GHC.HsSplice _ e)) = do
    markWithString GHC.AnnOpen "$("
    annotatePC e
    markWithString GHC.AnnClose ")"

  annotateG _ (GHC.HsSpliceE True (GHC.HsSplice _ e)) = do
    markWithString GHC.AnnOpen "$$("
    annotatePC e
    markWithString GHC.AnnClose ")"

  annotateG l (GHC.HsQuasiQuoteE (GHC.HsQuasiQuote n _ q)) = do
    markExternal l GHC.AnnVal
      ("[" ++ (showGhc n) ++ "|" ++ (GHC.unpackFS q) ++ "|]")


  annotateG _ (GHC.HsProc p c) = do
    mark GHC.AnnProc
    annotatePC p
    mark GHC.AnnRarrow
    annotatePC c

  annotateG _ (GHC.HsStatic e) = do
    mark GHC.AnnStatic
    annotatePC e

  annotateG _ (GHC.HsArrApp e1 e2 _ _ _) = do
    annotatePC e1
    -- only one of the next 4 will be resent
    mark GHC.Annlarrowtail
    mark GHC.Annrarrowtail
    mark GHC.AnnLarrowtail
    mark GHC.AnnRarrowtail

    annotatePC e2

  annotateG _ (GHC.HsArrForm e _ cs) = do
    markWithString GHC.AnnOpen "(|"
    annotatePC e
    mapM_ annotatePC cs
    markWithString GHC.AnnClose "|)"

  annotateG _ (GHC.HsTick _ _) = return ()
  annotateG _ (GHC.HsBinTick _ _ _) = return ()

  annotateG _ (GHC.HsTickPragma src (str,(v1,v2),(v3,v4)) e) = do
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
    annotatePC e

  annotateG l (GHC.EWildPat) = do
    markExternal l GHC.AnnVal "_"

  annotateG _ (GHC.EAsPat ln e) = do
    annotatePC ln
    mark GHC.AnnAt
    annotatePC e

  annotateG _ (GHC.EViewPat e1 e2) = do
    annotatePC e1
    mark GHC.AnnRarrow
    annotatePC e2

  annotateG _ (GHC.ELazyPat e) = do
    mark GHC.AnnTilde
    annotatePC e

  annotateG _ (GHC.HsType ty) = annotatePC ty

  annotateG _ (GHC.HsWrap _ _) = return ()
  annotateG _ (GHC.HsUnboundVar _) = return ()

instance AnnotateGen GHC.HsLit where
  annotateG l lit = markExternal l GHC.AnnVal (hsLit2String lit)
-- ---------------------------------------------------------------------

-- |Used for declarations that need to be aligned together, e.g. in a
-- do or let .. in statement/expr
instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateGen name)
  => AnnotateGen ([GHC.ExprLStmt name]) where
  annotateG _ ls = mapM_ annotatePC ls

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateGen name)
  => AnnotateGen (GHC.HsTupArg name) where
  annotateG _ (GHC.Present e) = do
    annotatePC e

  annotateG _ (GHC.Missing _) = do
    mark GHC.AnnComma

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateGen name)
  => AnnotateGen (GHC.HsCmdTop name) where
  annotateG _ (GHC.HsCmdTop cmd _ _ _) = annotatePC cmd

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateGen name)
   => AnnotateGen (GHC.HsCmd name) where
  annotateG _ (GHC.HsCmdArrApp e1 e2 _ _ _) = do
    annotatePC e1
    -- only one of the next 4 will be resent
    mark GHC.Annlarrowtail
    mark GHC.Annrarrowtail
    mark GHC.AnnLarrowtail
    mark GHC.AnnRarrowtail

    annotatePC e2

  annotateG _ (GHC.HsCmdArrForm e _mf cs) = do
    markWithString GHC.AnnOpen "(|"
    annotatePC e
    mapM_ annotatePC cs
    markWithString GHC.AnnClose "|)"

  annotateG _ (GHC.HsCmdApp e1 e2) = do
    annotatePC e1
    annotatePC e2

  annotateG l (GHC.HsCmdLam match) = do
    mark GHC.AnnLam
    annotateMatchGroup l match

  annotateG _ (GHC.HsCmdPar e) = do
    mark GHC.AnnOpenP
    annotatePC e
    mark GHC.AnnCloseP -- ')'

  annotateG l (GHC.HsCmdCase e1 matches) = do
    mark GHC.AnnCase
    annotatePC e1
    mark GHC.AnnOf
    mark GHC.AnnOpenC
    annotateMatchGroup l matches
    mark GHC.AnnCloseC

  annotateG _ (GHC.HsCmdIf _ e1 e2 e3) = do
    mark GHC.AnnIf
    annotatePC e1
    markOffset GHC.AnnSemi 0
    mark GHC.AnnThen
    annotatePC e2
    markOffset GHC.AnnSemi 1
    mark GHC.AnnElse
    annotatePC e3

  annotateG _ (GHC.HsCmdLet binds e) = do
    mark GHC.AnnLet
    mark GHC.AnnOpenC
    annotateWithLayout (GHC.L (getLocalBindsSrcSpan binds) binds)
    mark GHC.AnnCloseC
    mark GHC.AnnIn
    annotatePC e

  annotateG l (GHC.HsCmdDo es _) = do
    mark GHC.AnnDo
    mark GHC.AnnOpenC
    -- mapM_ annotatePC es
    annotateListWithLayout l es
    mark GHC.AnnCloseC

  annotateG _ (GHC.HsCmdCast {}) = error $ "annotateP.HsCmdCast: only valid after type checker"


-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateGen name)
  => AnnotateGen [GHC.Located (GHC.StmtLR name name (GHC.LHsCmd name))] where
  annotateG _ ls = mapM_ annotatePC ls

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateGen name)
     => AnnotateGen (GHC.TyClDecl name) where

  annotateG l (GHC.FamDecl famdecl) = annotateG l famdecl

  annotateG _ (GHC.SynDecl ln (GHC.HsQTvs _ tyvars) typ _) = do
    mark GHC.AnnType
    annotatePC ln
    mapM_ annotatePC tyvars
    mark GHC.AnnEqual
    annotatePC typ

  annotateG _ (GHC.DataDecl ln (GHC.HsQTvs _ns tyVars)
                (GHC.HsDataDefn _ ctx mctyp mk cons mderivs) _) = do
    mark GHC.AnnData
    mark GHC.AnnNewtype
    annotateMaybe mctyp
    annotatePC ctx
    mark GHC.AnnDarrow
    annotateTyClass ln tyVars
    mark GHC.AnnDcolon
    annotateMaybe mk
    mark GHC.AnnEqual
    mark GHC.AnnWhere
    mapM_ annotatePC cons
    annotateMaybe mderivs

  -- -----------------------------------

  annotateG _ (GHC.ClassDecl ctx ln (GHC.HsQTvs _ns tyVars) fds
                          sigs meths ats atdefs docs _) = do
    mark GHC.AnnClass
    annotatePC ctx

    annotateTyClass ln tyVars

    mark GHC.AnnVbar
    mapM_ annotatePC fds
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

annotateTyClass :: (AnnotateGen a, AnnotateGen ast)
                => GHC.Located a -> [GHC.Located ast] -> Wrapped ()
annotateTyClass ln tyVars = do
    markMany GHC.AnnOpenP
    applyListAnnotations (prepareListAnnotation [ln]
                      ++ prepareListAnnotation (take 2 tyVars))
    markMany GHC.AnnCloseP
    mapM_ annotatePC (drop 2 tyVars)

-- ---------------------------------------------------------------------

instance (GHC.DataId name,AnnotateGen name, GHC.OutputableBndr name)
   => AnnotateGen (GHC.FamilyDecl name) where
  annotateG _ (GHC.FamilyDecl info ln (GHC.HsQTvs _ tyvars) mkind) = do
    mark GHC.AnnType
    mark GHC.AnnData
    mark GHC.AnnFamily
    annotatePC ln
    mapM_ annotatePC tyvars
    mark GHC.AnnDcolon
    annotateMaybe mkind
    mark GHC.AnnWhere
    mark GHC.AnnOpenC -- {
    case info of
      GHC.ClosedTypeFamily eqns -> mapM_ annotatePC eqns
      _ -> return ()
    case info of
      GHC.ClosedTypeFamily eqns -> mapM_ annotatePC eqns
      _ -> return ()
    mark GHC.AnnCloseC -- }

-- ---------------------------------------------------------------------

instance (GHC.DataId name,AnnotateGen name,GHC.OutputableBndr name)
   => AnnotateGen (GHC.TyFamInstEqn name) where
  annotateG _ (GHC.TyFamEqn ln (GHC.HsWB pats _ _ _) typ) = do
    annotatePC ln
    mapM_ annotatePC pats
    mark GHC.AnnEqual
    annotatePC typ


-- ---------------------------------------------------------------------

instance (GHC.DataId name,AnnotateGen name,GHC.OutputableBndr name)
  => AnnotateGen (GHC.TyFamDefltEqn name) where
  annotateG _ (GHC.TyFamEqn ln (GHC.HsQTvs _ns bndrs) typ) = do
    annotatePC ln
    mapM_ annotatePC bndrs
    mark GHC.AnnEqual
    annotatePC typ

-- ---------------------------------------------------------------------

-- TODO: modify lexer etc, in the meantime to not set haddock flag
instance AnnotateGen GHC.DocDecl where
  annotateG l v =
    let str =
          case v of
            (GHC.DocCommentNext (GHC.HsDocString fs)) -> (GHC.unpackFS fs)
            (GHC.DocCommentPrev (GHC.HsDocString fs)) -> (GHC.unpackFS fs)
            (GHC.DocCommentNamed _s (GHC.HsDocString fs)) -> (GHC.unpackFS fs)
            (GHC.DocGroup _i (GHC.HsDocString fs)) -> (GHC.unpackFS fs)
    in
      markExternal l (GHC.AnnVal) str

-- ---------------------------------------------------------------------

annotateDataDefn :: (GHC.DataId name,GHC.OutputableBndr name,AnnotateGen name)
  => GHC.SrcSpan -> GHC.HsDataDefn name -> Wrapped ()
annotateDataDefn _ (GHC.HsDataDefn _ ctx typ mk cons mderivs) = do
  annotatePC ctx
  annotateMaybe typ
  annotateMaybe mk
  mapM_ annotatePC cons
  case mderivs of
    Nothing -> return ()
    Just d -> annotatePC d

-- ---------------------------------------------------------------------

-- Note: GHC.HsContext name aliases to here too
instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateGen name)
     => AnnotateGen [GHC.LHsType name] where
  annotateG l ts = do
    return () `debug` ("annotateP.HsContext:l=" ++ showGhc l)
    mark GHC.AnnDeriving
    mark GHC.AnnOpenP
    mapM_ annotatePC ts
    mark GHC.AnnCloseP
    mark GHC.AnnDarrow

-- ---------------------------------------------------------------------

instance (GHC.DataId name,AnnotateGen name,GHC.OutputableBndr name)
      => AnnotateGen (GHC.ConDecl name) where
  annotateG _ (GHC.ConDecl lns _expr (GHC.HsQTvs _ns bndrs) ctx
                         dets res _ _) = do
    case res of
      GHC.ResTyH98 -> do
        mark GHC.AnnForall
        mapM_ annotatePC bndrs
        mark GHC.AnnDot

        annotatePC ctx
        mark GHC.AnnDarrow

        case dets of
          GHC.InfixCon _ _ -> return ()
          _ -> mapM_ annotatePC lns

        annotateHsConDeclDetails lns dets

      GHC.ResTyGADT ls ty -> do
        -- only print names if not infix
        case dets of
          GHC.InfixCon _ _ -> return ()
          _ -> mapM_ annotatePC lns

        annotateHsConDeclDetails lns dets

        mark GHC.AnnDcolon

        annotatePC (GHC.L ls (ResTyGADTHook bndrs))

        annotatePC ctx
        mark GHC.AnnDarrow

        annotatePC ty


    mark GHC.AnnVbar

-- ---------------------------------------------------------------------

instance (GHC.DataId name,GHC.OutputableBndr name,AnnotateGen name) =>
              AnnotateGen (ResTyGADTHook name) where
  annotateG _ (ResTyGADTHook bndrs) = do
    mark GHC.AnnForall
    mapM_ annotatePC bndrs
    mark GHC.AnnDot

-- ---------------------------------------------------------------------

instance (AnnotateGen name,AnnotateGen a)
  => AnnotateGen (GHC.HsRecField name (GHC.Located a)) where
  annotateG _ (GHC.HsRecField n e _) = do
    annotatePC n
    mark GHC.AnnEqual
    annotatePC e

-- ---------------------------------------------------------------------

instance (GHC.DataId name,AnnotateGen name)
    => AnnotateGen (GHC.FunDep (GHC.Located name)) where

  annotateG _ (ls,rs) = do
    mapM_ annotatePC ls
    mark GHC.AnnRarrow
    mapM_ annotatePC rs

-- ---------------------------------------------------------------------

instance AnnotateGen (GHC.CType) where
  annotateG _ (GHC.CType src mh f) = do
    markWithString GHC.AnnOpen src
    case mh of
      Nothing -> return ()
      Just (GHC.Header h) ->
         markWithString GHC.AnnHeader ("\"" ++ GHC.unpackFS h ++ "\"")
    markWithString GHC.AnnVal ("\"" ++ GHC.unpackFS f ++ "\"")
    markWithString GHC.AnnClose "#-}"

-- ---------------------------------------------------------------------
