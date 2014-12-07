{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Language.Haskell.GHC.ExactPrint.Utils
  (
    annotateLHsModule

  , ghcIsComment
  , ghcIsMultiLine

  , srcSpanStartLine
  , srcSpanEndLine
  , srcSpanStartColumn
  , srcSpanEndColumn

  , ss2span
  , ss2pos
  , ss2posEnd
  , undelta
  , undeltaComment
  , rdrName2String
  , isSymbolRdrName

  , showGhc

  , merge

  -- * For tests
  , runAP
  , AP(..)
  , getSrcSpanAP, pushSrcSpanAP, popSrcSpanAP
  , getAnnotationAP
  , addAnnotationsAP, addAnnValue

  ) where

import Control.Applicative (Applicative(..))
import Control.Monad (when, liftM, ap)
import Control.Exception
import Data.Data
import Data.List
import Data.Maybe
import Data.Monoid

import Language.Haskell.GHC.ExactPrint.Types

import qualified Bag            as GHC
import qualified BasicTypes     as GHC
import qualified BooleanFormula as GHC
import qualified DynFlags       as GHC
import qualified FastString     as GHC
import qualified ForeignCall    as GHC
import qualified GHC            as GHC
import qualified GHC.Paths      as GHC
import qualified Lexer          as GHC
import qualified Name           as GHC
import qualified NameSet        as GHC
import qualified Outputable     as GHC
import qualified RdrName        as GHC
import qualified SrcLoc         as GHC
import qualified StringBuffer   as GHC
import qualified UniqSet        as GHC
import qualified Unique         as GHC
import qualified Var            as GHC

import qualified Data.Map as Map

import Debug.Trace

debug :: c -> String -> c
debug = flip trace

-- ---------------------------------------------------------------------

-- | Type used in the AP Monad. The state variables maintain
--    - the current SrcSpan and the TypeRep of the thing it encloses
--      as a stack to the root of the AST as it is traversed,
--    - the srcspan of the last thing annotated, to calculate delta's from
--    - the annotations provided by GHC

{- -}
newtype AP x = AP ([(GHC.SrcSpan,TypeRep)] -> GHC.SrcSpan -> GHC.ApiAnns
            -> (x, [(GHC.SrcSpan,TypeRep)],   GHC.SrcSpan,   GHC.ApiAnns,
                  ([(AnnKey,Annotation)],[(AnnKey,Value)],[(AnnKeyF,[DeltaPos])])
                 ))

instance Functor AP where
  fmap = liftM

instance Applicative AP where
  pure = return
  (<*>) = ap

instance Monad AP where
  return x = AP $ \l pe ga -> (x, l, pe, ga, ([],[],[]))

  AP m >>= k = AP $ \l0 p0 ga0 -> let
        (a, l1, p1, ga1, s1) = m l0 p0 ga0
        AP f = k a
        (b, l2, p2, ga2, s2) = f l1 p1 ga1
    in (b, l2, p2, ga2, s1 <> s2)


runAP :: AP () -> GHC.ApiAnns -> Anns
runAP (AP f) ga
 = let (_,_,_,_,(se,su,sa)) = f [] GHC.noSrcSpan ga
   in (Map.fromList se,Map.fromList su,Map.fromListWith (++) sa)

-- -------------------------------------

-- |Note: assumes the SrcSpan stack is nonempty
getSrcSpanAP :: AP GHC.SrcSpan
getSrcSpanAP = AP (\l pe ga -> (fst $ head l,l,pe,ga,([],[],[])))

pushSrcSpanAP :: (Typeable a) => (GHC.Located a) -> AP ()
pushSrcSpanAP (GHC.L l a) = AP (\ls pe ga -> ((),(l,typeOf a):ls,pe,ga,([],[],[])))

popSrcSpanAP :: AP ()
popSrcSpanAP = AP (\(l:ls) pe ga -> ((),ls,pe,ga,([],[],[])))

-- ---------------------------------------------------------------------

-- |Note: assumes the prior end SrcSpan stack is nonempty
getPriorEnd :: AP GHC.SrcSpan
getPriorEnd = AP (\l pe ga -> (pe,l,pe,ga,([],[],[])))

setPriorEnd :: GHC.SrcSpan -> AP ()
setPriorEnd pe = AP (\ls _ ga  -> ((),ls,pe,ga,([],[],[])))

-- Deprecated, remove
popPriorEnd :: AP ()
popPriorEnd = AP (\ls pe ga -> ((),ls,pe,ga,([],[],[]))
 `debug` ("popPriorEnd: old stack :" ++ showGhc pe))
-- -------------------------------------

getAnnotationAP :: GHC.SrcSpan -> GHC.AnnKeywordId -> AP [GHC.SrcSpan]
getAnnotationAP sp an = AP (\l pe ga
    -> (GHC.getAnnotation ga sp an, l,pe,ga,([],[],[])))


-- -------------------------------------

getCommentsForSpan :: GHC.SrcSpan -> AP [Comment]
getCommentsForSpan s = AP (\l pe ga ->
  let
    gcs = GHC.getAnnotationComments ga s
    cs = reverse $ map tokComment gcs
    tokComment :: GHC.Located GHC.AnnotationComment -> Comment
    tokComment t@(GHC.L l _) = Comment (ghcIsMultiLine t) (ss2span l) (ghcCommentText t)
  in (cs,l,pe,ga,([],[],[])))

-- -------------------------------------

-- |Add some annotation to the currently active SrcSpan
addAnnotationsAP :: Annotation -> AP ()
addAnnotationsAP ann = AP (\l pe ga ->
                       ( (),l,pe,ga,
                 ([((head l),ann)],[],[])))

-- -------------------------------------

-- Deprecated, delete later
addAnnValue :: (Typeable a,Show a,Eq a) => a -> AP ()
addAnnValue v = AP (\l                 pe ga ->
                ( (),l,pe,ga,
                 ([],[( ((fst $ head l),typeOf (Just v)),newValue v)],[])))

-- -------------------------------------

addAnnDeltaPos :: (GHC.SrcSpan,GHC.AnnKeywordId) -> DeltaPos -> AP ()
addAnnDeltaPos (s,kw) dp = AP (\l pe ga -> ( (),
                                 l,pe,ga,
                               ([],[],
                               [ ((s,kw),[dp]) ])  ))

-- -------------------------------------

-- | Enter a new AST element. Maintain SrcSpan stack
enterAST :: (Typeable a) => GHC.Located a -> AP ()
enterAST lss = do
  return () `debug` ("enterAST entered for " ++ show (ss2span $ GHC.getLoc lss))
  pushSrcSpanAP lss
  return ()


-- | Pop up the SrcSpan stack, capture the annotations, and work the
-- comments in belonging to the span
-- Assumption: the annotations belong to the immediate sub elements of
-- the AST, hence relate to the current SrcSpan. They can thus be used
-- to decide which comments belong at this level,
-- The assumption is made valid by matching enterAST/leaveAST calls.
leaveAST :: AP ()
leaveAST = do
  ss <- getSrcSpanAP
  priorEnd <- getPriorEnd

  newCs <- getCommentsForSpan ss
  let (lcs,_) = localComments (ss2span ss) newCs []

  let dp = deltaFromSrcSpans priorEnd ss
  addAnnotationsAP (Ann lcs dp)
  popSrcSpanAP
  return () `debug` ("leaveAST:(ss,dp,priorEnd)=" ++ show (ss2span ss,dp,ss2span priorEnd))

-- ---------------------------------------------------------------------

class (Typeable ast) => AnnotateP ast where
  annotateP :: GHC.SrcSpan -> ast -> AP ()

-- |First move to the given location, then call exactP
annotatePC :: (AnnotateP ast) => GHC.Located ast -> AP ()
annotatePC a@(GHC.L l ast) = do
  enterAST a `debug` ("annotatePC:entering " ++ showGhc l)
  _ <- annotateP l ast
  leaveAST `debug` ("annotatePC:leaving " ++ showGhc (l))


annotateMaybe :: (AnnotateP ast) => Maybe (GHC.Located ast) -> AP ()
annotateMaybe Nothing    = return ()
annotateMaybe (Just ast) = annotatePC ast

annotateList :: (AnnotateP ast) => [GHC.Located ast] -> AP ()
annotateList xs = mapM_ annotatePC xs

-- ---------------------------------------------------------------------

addFinalComments :: AP ()
addFinalComments = do
  cs <- getCommentsForSpan GHC.noSrcSpan
  let (dcs,_) = localComments ((1,1),(1,1)) cs []
  pushSrcSpanAP (GHC.L GHC.noSrcSpan ())
  addAnnotationsAP (Ann dcs (DP (0,0)))
   -- `debug` ("leaveAST:dcs=" ++ show dcs)
  return () -- `debug` ("addFinalComments:dcs=" ++ show dcs)

-- | Look up and add a Delta annotation at the current position, and
-- advance the position to the end of the annotation
addDeltaAnnotation :: GHC.AnnKeywordId -> AP ()
addDeltaAnnotation ann = do
  pe <- getPriorEnd
  ss <- getSrcSpanAP
  ma <- getAnnotationAP ss ann
  case ma of
    [] -> return ()
    [ap] -> do
      let p = deltaFromSrcSpans pe ap
      addAnnDeltaPos (ss,ann) p
      setPriorEnd ap
        `debug` ("addDeltaAnnotation:(ss,pe,ma,p)=" ++ show (ss2span ss,ss2span pe,fmap ss2span ma,p,ann))
    _ -> error $ "addDeltaAnnotation:(ann,ma)=" ++ showGhc (ann,ma)

-- | Look up and add a Delta annotation at the current position, and
-- advance the position to the end of the annotation
addDeltaAnnotationLs :: GHC.AnnKeywordId -> Int -> AP ()
addDeltaAnnotationLs ann off = do
  pe <- getPriorEnd
  ss <- getSrcSpanAP
  ma <- getAnnotationAP ss ann
  case (drop off ma) of
    [] -> return ()
        `debug` ("addDeltaAnnotationLs:missed:(off,pe,ann,ma)=" ++ show (off,ss2span pe,ann,fmap ss2span ma))
    (ap:_) -> do
      let p = deltaFromSrcSpans pe ap
      addAnnDeltaPos (ss,ann) p
      setPriorEnd ap
        `debug` ("addDeltaAnnotationLs:(off,pe,ap,p,ann,ma)=" ++ show (off,ss2span pe,ss2span ap,p,ann,fmap ss2span ma))

-- | Look up and add possibly multiple Delta annotation at the current
-- position, and advance the position to the end of the annotations
addDeltaAnnotations :: GHC.AnnKeywordId -> AP ()
addDeltaAnnotations ann = do
  ss <- getSrcSpanAP
  ma <- getAnnotationAP ss ann
  let do_one ap' = do
        pe <- getPriorEnd
        let p = deltaFromSrcSpans pe ap'
        addAnnDeltaPos (ss,ann) p
        setPriorEnd ap'
          `debug` ("addDeltaAnnotations:(ss,pe,ma,p)=" ++ show (ss2span ss,ss2span pe,fmap ss2span ma,p,ann))
  case ma of
    [] -> return ()
    [ap] -> do_one ap
    as -> do
      mapM_ do_one (sort as)

-- | Add a Delta annotation at the current position, and advance the
-- position to the end of the annotation
addDeltaAnnotationExt :: GHC.SrcSpan -> GHC.AnnKeywordId -> AP ()
addDeltaAnnotationExt s ann = do
  pe <- getPriorEnd
  ss <- getSrcSpanAP
  let p = deltaFromSrcSpans pe s
  addAnnDeltaPos (ss,ann) p
  setPriorEnd s

addEofAnnotation :: AP ()
addEofAnnotation = do
  pe <- getPriorEnd
  ss <- getSrcSpanAP
  ma <- getAnnotationAP GHC.noSrcSpan GHC.AnnEofPos
  case ma of
    [] -> return ()
    [ap] -> do
      let p = deltaFromSrcSpans pe ap
      addAnnDeltaPos (ss,GHC.AnnEofPos) p
      setPriorEnd ap

countAnnsAP :: GHC.AnnKeywordId -> AP Int
countAnnsAP ann = do
  ss <- getSrcSpanAP
  ma <- getAnnotationAP ss ann
  return (length ma)

-- ---------------------------------------------------------------------
-- Start of application specific part

-- ---------------------------------------------------------------------

annotateLHsModule :: GHC.Located (GHC.HsModule GHC.RdrName) -> GHC.ApiAnns
                  -> Anns
annotateLHsModule modu ghcAnns
   = runAP (annotatePC modu >> addFinalComments) ghcAnns

-- ---------------------------------------------------------------------

instance AnnotateP (GHC.HsModule GHC.RdrName) where
  annotateP lm (GHC.HsModule mmn mexp imps decs _depr _haddock) = do
    return () `debug` ("annotateP.HsModule entered")
    setPriorEnd lm

   -- 'module' modid maybemodwarning maybeexports 'where' header_body
    addDeltaAnnotation GHC.AnnModule

    case mmn of
      Nothing -> return ()
      Just (GHC.L ln _) -> addDeltaAnnotationExt ln GHC.AnnVal

    case mexp of
      Nothing -> return ()
      Just exp -> annotatePC exp

    addDeltaAnnotation GHC.AnnWhere
    addDeltaAnnotation GHC.AnnOpen -- Possible '{'

    mapM_ annotatePC imps

    addDeltaAnnotation GHC.AnnSemi -- Possible ';'

    annotateList decs

    addDeltaAnnotation GHC.AnnClose -- Possible '}'

    addEofAnnotation


-- ---------------------------------------------------------------------

instance AnnotateP [GHC.LIE GHC.RdrName] where
   annotateP l ls = do
     addDeltaAnnotation GHC.AnnHiding -- in an import decl
     addDeltaAnnotation GHC.AnnOpen -- '('
     mapM_ annotatePC ls
     addDeltaAnnotation GHC.AnnClose -- ')'

instance AnnotateP (GHC.IE GHC.RdrName) where
  annotateP l ie = do

    case ie of
        (GHC.IEVar (GHC.L ln _)) -> do
          addDeltaAnnotation GHC.AnnPattern
          addDeltaAnnotation GHC.AnnType
          addDeltaAnnotationExt ln GHC.AnnVal

        (GHC.IEThingAbs _) -> do
          addDeltaAnnotation GHC.AnnType
          addDeltaAnnotation GHC.AnnVal

        (GHC.IEThingWith (GHC.L ln n) ns) -> do
          addDeltaAnnotationExt ln GHC.AnnVal
          addDeltaAnnotation GHC.AnnOpen
          mapM_ annotatePC ns
          addDeltaAnnotation GHC.AnnClose

        (GHC.IEThingAll (GHC.L ln n)) -> do
          addDeltaAnnotationExt ln GHC.AnnVal
          addDeltaAnnotation GHC.AnnOpen
          addDeltaAnnotation GHC.AnnDotdot
          addDeltaAnnotation GHC.AnnClose

        (GHC.IEModuleContents (GHC.L lm n)) -> do
          addDeltaAnnotation GHC.AnnModule
          addDeltaAnnotationExt lm GHC.AnnVal

        x -> error $ "annotateP.IE: notimplemented for " ++ showGhc x

    addDeltaAnnotation GHC.AnnComma

-- ---------------------------------------------------------------------

instance AnnotateP GHC.RdrName where
  annotateP l n = do
    addDeltaAnnotationExt l GHC.AnnVal
    addDeltaAnnotation GHC.AnnComma

-- ---------------------------------------------------------------------

instance AnnotateP GHC.Name where
  annotateP l n = do
    addDeltaAnnotationExt l GHC.AnnVal
    addDeltaAnnotation GHC.AnnComma

-- ---------------------------------------------------------------------

instance AnnotateP (GHC.ImportDecl GHC.RdrName) where
 annotateP l (GHC.ImportDecl _msrc (GHC.L ln _) _pkg _src _safe qual _impl as hiding) = do

   -- 'import' maybe_src maybe_safe optqualified maybe_pkg modid maybeas maybeimpspec
   addDeltaAnnotation GHC.AnnImport

   -- "{-# SOURCE" and "#-}"
   addDeltaAnnotation GHC.AnnOpen
   addDeltaAnnotation GHC.AnnClose
   addDeltaAnnotation GHC.AnnSafe
   addDeltaAnnotation GHC.AnnQualified
   addDeltaAnnotation GHC.AnnPackageName

   addDeltaAnnotationExt ln GHC.AnnVal -- modid

   addDeltaAnnotation GHC.AnnAs
   addDeltaAnnotation GHC.AnnVal -- as modid

   case hiding of
     Nothing -> return ()
     Just (_isHiding,lie) -> do
       addDeltaAnnotation GHC.AnnHiding
       annotatePC lie

   -- There may be multiple ';'s
   addDeltaAnnotations GHC.AnnSemi

-- ---------------------------------------------------------------------

instance (GHC.OutputableBndr name,AnnotateP name) => AnnotateP (GHC.HsDecl name) where
  annotateP l decl = do
    case decl of
      GHC.TyClD d -> annotateP l d
      GHC.InstD d -> error $ "annotateLHsDecl:unimplemented " ++ "InstD"
      GHC.DerivD d -> error $ "annotateLHsDecl:unimplemented " ++ "DerivD"
      GHC.ValD d -> annotateP l d
      GHC.SigD d -> annotateP l d
      GHC.DefD d -> error $ "annotateLHsDecl:unimplemented " ++ "DefD"
      GHC.ForD d -> error $ "annotateLHsDecl:unimplemented " ++ "ForD"
      GHC.WarningD d -> error $ "annotateLHsDecl:unimplemented " ++ "WarningD"
      GHC.AnnD d -> error $ "annotateLHsDecl:unimplemented " ++ "AnnD"
      GHC.RuleD d -> error $ "annotateLHsDecl:unimplemented " ++ "RuleD"
      GHC.VectD d -> error $ "annotateLHsDecl:unimplemented " ++ "VectD"
      GHC.SpliceD d -> error $ "annotateLHsDecl:unimplemented " ++ "SpliceD"
      GHC.DocD d -> error $ "annotateLHsDecl:unimplemented " ++ "DocD"
      GHC.QuasiQuoteD d -> error $ "annotateLHsDecl:unimplemented " ++ "QuasiQuoteD"
      GHC.RoleAnnotD d -> error $ "annotateLHsDecl:unimplemented " ++ "RoleAnnotD"

-- ---------------------------------------------------------------------

instance (Typeable name,GHC.OutputableBndr name,AnnotateP name) =>
                                              AnnotateP (GHC.HsBind name) where
  annotateP l (GHC.FunBind (GHC.L ln n) isInfix (GHC.MG matches _ _ _) _ _ _) = do
    -- addDeltaAnnotation GHC.AnnEqual
    mapM_ annotatePC matches

  annotateP l (GHC.PatBind lhs@(GHC.L ll _) grhss@(GHC.GRHSs grhs lb) _typ _fvs _ticks) = do
    annotatePC lhs
    mapM_ annotatePC grhs
    annotateHsLocalBinds lb

  annotateP l (GHC.VarBind n rhse _) = do
    -- Note: this bind is introduced by the typechecker
    annotatePC rhse

  annotateP l (GHC.PatSynBind (GHC.PSB (GHC.L ln _) _fvs args def dir)) = do
    addDeltaAnnotation GHC.AnnPattern
    addDeltaAnnotationExt ln GHC.AnnVal
    case args of
      GHC.InfixPatSyn (GHC.L la _) (GHC.L lb _) -> do
        addDeltaAnnotationExt la GHC.AnnVal
        addDeltaAnnotationExt lb GHC.AnnVal
      GHC.PrefixPatSyn ns -> do
        mapM_ (\(GHC.L lc _) -> addDeltaAnnotationExt lc GHC.AnnVal) ns
    addDeltaAnnotation GHC.AnnEqual
    addDeltaAnnotation GHC.AnnLarrow
    annotatePC def
    case dir of
      GHC.Unidirectional           -> return ()
      GHC.ImplicitBidirectional    -> return ()
      GHC.ExplicitBidirectional mg -> annotateMatchGroup mg

    addDeltaAnnotation GHC.AnnWhere
    addDeltaAnnotation GHC.AnnOpen  -- '{'
    addDeltaAnnotation GHC.AnnClose -- '}'

    return ()

-- ---------------------------------------------------------------------

instance (Typeable name,GHC.OutputableBndr name,AnnotateP name) =>
                          AnnotateP (GHC.Match name (GHC.LHsExpr name)) where
  annotateP l (GHC.Match pats _typ grhss@(GHC.GRHSs grhs lb)) = do

    addDeltaAnnotation GHC.AnnFunId

    mapM_ annotatePC pats

    addDeltaAnnotation GHC.AnnEqual

    mapM_ annotatePC grhs

    addDeltaAnnotation GHC.AnnWhere
    addDeltaAnnotation GHC.AnnOpen -- '{'
    annotateHsLocalBinds lb
    addDeltaAnnotation GHC.AnnClose -- '}'



-- ---------------------------------------------------------------------
{-
rhs     :: { Located (GRHSs RdrName) }
        : '=' exp wherebinds    { sL (comb3 $1 $2 $3) $ GRHSs (unguardedRHS $2) (unLoc $3) }
        | gdrhs wherebinds      { LL $ GRHSs (reverse (unLoc $1)) (unLoc $2) }

gdrhs :: { Located [LGRHS RdrName] }
        : gdrhs gdrh            { LL ($2 : unLoc $1) }
        | gdrh                  { L1 [$1] }

gdrh :: { LGRHS RdrName }
        : '|' guardquals '=' exp        { sL (comb2 $1 $>) $ GRHS (unLoc $2) $4 }

-}

instance (Typeable name,GHC.OutputableBndr name,AnnotateP name) =>
                        AnnotateP (GHC.GRHS name (GHC.LHsExpr name)) where
  annotateP l (GHC.GRHS guards expr) = do

    addDeltaAnnotation GHC.AnnVbar
    mapM_ annotatePC guards
    addDeltaAnnotation GHC.AnnEqual
    annotatePC expr

-- ---------------------------------------------------------------------

instance (Typeable name,GHC.OutputableBndr name,AnnotateP name) =>
                                         AnnotateP (GHC.Sig name) where

  annotateP l (GHC.TypeSig lns typ _) = do
    mapM_ annotatePC lns
    addDeltaAnnotation GHC.AnnDcolon
    annotatePC typ
    addDeltaAnnotation GHC.AnnComma

  -- PatSynSig (Located name) (HsExplicitFlag, LHsTyVarBndrs name)
  --           (LHsContext name) (LHsContext name) (LHsType name)
  annotateP l (GHC.PatSynSig (GHC.L ln _) (_,GHC.HsQTvs _ns bndrs) ctx1 ctx2 typ) = do
    addDeltaAnnotation GHC.AnnPattern
    addDeltaAnnotationExt ln GHC.AnnVal
    addDeltaAnnotation GHC.AnnDcolon

    -- Note: The 'forall' bndrs '.' may occur multiple times
    addDeltaAnnotation GHC.AnnForall
    mapM_ annotatePC bndrs
    addDeltaAnnotation GHC.AnnDot

    annotatePC ctx1
    addDeltaAnnotationLs GHC.AnnDarrow 0
    annotatePC ctx2
    addDeltaAnnotationLs GHC.AnnDarrow 1
    annotatePC typ
    addDeltaAnnotation GHC.AnnComma


  -- GenericSig [Located name] (LHsType name)
  annotateP l (GHC.GenericSig ns typ) = do
    addDeltaAnnotation GHC.AnnDefault
    mapM_ annotatePC ns
    addDeltaAnnotation GHC.AnnDcolon
    annotatePC typ
    addDeltaAnnotation GHC.AnnComma

  annotateP l (GHC.IdSig _) = return ()

  -- FixSig (FixitySig name)
  annotateP l (GHC.FixSig (GHC.FixitySig lns (GHC.Fixity v fdir))) = do
    addDeltaAnnotation GHC.AnnInfix
    addDeltaAnnotation GHC.AnnVal
    mapM_ annotatePC lns
    addDeltaAnnotation GHC.AnnComma

  -- InlineSig (Located name) InlinePragma
  -- '{-# INLINE' activation qvar '#-}'
  annotateP l (GHC.InlineSig (GHC.L ln _) inl) = do
    addDeltaAnnotations GHC.AnnOpen -- '{-# INLINE', '['
    addDeltaAnnotation  GHC.AnnTilde -- ~
    addDeltaAnnotation  GHC.AnnVal   -- e.g. 34
    cnt <- countAnnsAP GHC.AnnClose
    case cnt of
      2 -> do
        addDeltaAnnotationLs GHC.AnnClose 0 -- ']'
        addDeltaAnnotationExt ln GHC.AnnVal
        addDeltaAnnotationLs GHC.AnnClose  1 -- '#-}'
      _ -> do
        addDeltaAnnotationExt ln GHC.AnnVal
        addDeltaAnnotationLs GHC.AnnClose  0 -- '#-}'

    addDeltaAnnotation GHC.AnnComma

  -- SpecSig (Located name) [LHsType name] InlinePragma
  annotateP l (GHC.SpecSig (GHC.L ln _) typs inl) = do
    addDeltaAnnotations GHC.AnnOpen -- '{-# SPECIALISE', '['
    addDeltaAnnotation  GHC.AnnTilde -- ~
    addDeltaAnnotation  GHC.AnnVal   -- e.g. 34
    addDeltaAnnotation  GHC.AnnClose -- ']'
    addDeltaAnnotationExt ln GHC.AnnVal
    addDeltaAnnotation GHC.AnnDcolon -- '::'
    mapM_ annotatePC typs
    addDeltaAnnotation GHC.AnnClose -- '#-}'

    addDeltaAnnotation GHC.AnnComma

  -- SpecInstSig (LHsType name)
  -- '{-# SPECIALISE' 'instance' inst_type '#-}'
  annotateP l (GHC.SpecInstSig _ typ) = do
    addDeltaAnnotation GHC.AnnOpen -- '{-# SPECIALISE'
    addDeltaAnnotation GHC.AnnInstance
    annotatePC typ
    addDeltaAnnotation GHC.AnnClose -- '#-}'

    addDeltaAnnotation GHC.AnnComma

  -- MinimalSig (BooleanFormula (Located name))
  annotateP l (GHC.MinimalSig _ formula) = do
    addDeltaAnnotation GHC.AnnOpen -- '{-# MINIMAL'
    annotateBooleanFormula formula
    addDeltaAnnotation GHC.AnnClose -- '#-}'

    addDeltaAnnotation GHC.AnnComma

-- ---------------------------------------------------------------------

-- data BooleanFormula a = Var a | And [BooleanFormula a] | Or [BooleanFormula a]
--  deriving (Eq, Data, Typeable, Functor, Foldable, Traversable)

annotateBooleanFormula :: (Typeable name,GHC.OutputableBndr name) =>
                             GHC.BooleanFormula (GHC.Located name) -> AP ()
annotateBooleanFormula = assert False undefined

-- ---------------------------------------------------------------------

instance (Typeable name,GHC.OutputableBndr name,AnnotateP name) =>
                     AnnotateP (GHC.HsTyVarBndr name) where
  annotateP l (GHC.UserTyVar n) = do
    addDeltaAnnotationExt l GHC.AnnVal

  annotateP l (GHC.KindedTyVar n ty) = do
    addDeltaAnnotation GHC.AnnOpen -- '('
    addDeltaAnnotationExt l GHC.AnnVal -- n
    addDeltaAnnotation GHC.AnnDcolon -- '::'
    annotatePC ty
    addDeltaAnnotation GHC.AnnClose -- '('

-- ---------------------------------------------------------------------

instance (Typeable name,GHC.OutputableBndr name,AnnotateP name) =>
                           AnnotateP (GHC.HsContext name) where
  annotateP l ts = do
    addDeltaAnnotation GHC.AnnUnit
    mapM_ annotatePC ts >> return ()

-- ---------------------------------------------------------------------

instance (Typeable name,GHC.OutputableBndr name,AnnotateP name) =>
                                      AnnotateP (GHC.HsType name) where

  annotateP l (GHC.HsForAllTy f mwc bndrs ctx@(GHC.L lc cc) typ) = do
    addDeltaAnnotation GHC.AnnForall
    mapM_ annotatePC cc
    case mwc of
      Nothing -> return ()
      Just wcs -> addDeltaAnnotationExt wcs GHC.AnnVal -- '_' location
    addDeltaAnnotation GHC.AnnDot
    addDeltaAnnotation GHC.AnnDarrow
    annotatePC typ


  annotateP l (GHC.HsTyVar n) = do
    addDeltaAnnotationExt l GHC.AnnVal

  annotateP l (GHC.HsAppTy t1 t2) = do
    annotatePC t1
    annotatePC t2

  annotateP l (GHC.HsFunTy t1 t2) = do
    annotatePC t1
    addDeltaAnnotation GHC.AnnRarrow
    annotatePC t2

  annotateP l (GHC.HsListTy t) = do
    addDeltaAnnotation GHC.AnnOpen -- '['
    annotatePC t
    addDeltaAnnotation GHC.AnnClose -- ']'

  annotateP l (GHC.HsPArrTy t) = do
    addDeltaAnnotation GHC.AnnOpen  -- '[:'
    annotatePC t
    addDeltaAnnotation GHC.AnnClose -- ':]'

  annotateP l (GHC.HsTupleTy tt ts) = do
    addDeltaAnnotation GHC.AnnOpen  -- '(' or '(#'
    mapM_ annotatePC ts
    addDeltaAnnotation GHC.AnnClose -- ')' or '#)'

  annotateP l (GHC.HsOpTy t1 (_,GHC.L lo _) t2) = do
    annotatePC t1
    addDeltaAnnotationExt lo GHC.AnnVal
    annotatePC t2

  annotateP l (GHC.HsParTy t) = do
    addDeltaAnnotation GHC.AnnOpen  -- '('
    annotatePC t
    addDeltaAnnotation GHC.AnnClose -- ')'

  -- HsIParamTy HsIPName (LHsType name)
  annotateP l (GHC.HsIParamTy n t) = do
    addDeltaAnnotation GHC.AnnVal
    addDeltaAnnotation GHC.AnnDcolon
    annotatePC t

  -- HsEqTy (LHsType name) (LHsType name)
  annotateP l (GHC.HsEqTy t1 t2) = do
    annotatePC t1
    addDeltaAnnotation GHC.AnnTilde
    annotatePC t2

  -- HsKindSig (LHsType name) (LHsKind name)
  annotateP l (GHC.HsKindSig t k) = do
    addDeltaAnnotation GHC.AnnOpen  -- '('
    annotatePC t
    addDeltaAnnotation GHC.AnnDcolon -- '::'
    annotatePC k
    addDeltaAnnotation GHC.AnnClose -- ')'

  -- HsQuasiQuoteTy (HsQuasiQuote name)
  annotateP l (GHC.HsQuasiQuoteTy qq) = do
    addDeltaAnnotationExt l GHC.AnnVal

  -- HsSpliceTy (HsSplice name) (PostTc name Kind)
  annotateP l (GHC.HsSpliceTy (GHC.HsSplice _is e) _) = do
    addDeltaAnnotation GHC.AnnOpen  -- '$('
    annotatePC e
    addDeltaAnnotation GHC.AnnClose -- ')'

  -- HsDocTy (LHsType name) LHsDocString
  annotateP l (GHC.HsDocTy t ds) = do
    annotatePC t
    annotatePC ds

  -- HsBangTy HsBang (LHsType name)
  annotateP l (GHC.HsBangTy _b t) = do
    addDeltaAnnotation GHC.AnnOpen  -- '{-# UNPACK' or '{-# NOUNPACK'
    addDeltaAnnotation GHC.AnnClose -- '#-}'
    addDeltaAnnotation GHC.AnnBang  -- '!'
    annotatePC t

  -- HsRecTy [LConDeclField name]
  annotateP l (GHC.HsRecTy cons) = do
    addDeltaAnnotation GHC.AnnOpen  -- '{'
    mapM_ annotatePC cons
    addDeltaAnnotation GHC.AnnClose -- '}'

  -- HsCoreTy Type
  annotateP l (GHC.HsCoreTy _t) = return ()

  -- HsExplicitListTy (PostTc name Kind) [LHsType name]
  annotateP l (GHC.HsExplicitListTy _ ts) = do
    -- TODO: what about SIMPLEQUOTE?
    addDeltaAnnotation GHC.AnnOpen  -- "'["
    mapM_ annotatePC ts
    addDeltaAnnotation GHC.AnnClose -- ']'

  -- HsExplicitTupleTy [PostTc name Kind] [LHsType name]
  annotateP l (GHC.HsExplicitTupleTy _ ts) = do
    addDeltaAnnotation GHC.AnnOpen  -- "'("
    mapM_ annotatePC ts
    addDeltaAnnotation GHC.AnnClose -- ')'

  -- HsTyLit HsTyLit
  annotateP l (GHC.HsTyLit tl) = do
    addDeltaAnnotationExt l GHC.AnnVal

  -- HsWrapTy HsTyWrapper (HsType name)
  annotateP l (GHC.HsWrapTy _ _) = return ()

  annotateP l (GHC.HsWildcardTy) = do
    addDeltaAnnotationExt l GHC.AnnVal

  annotateP l (GHC.HsNamedWildcardTy n) = do
    addDeltaAnnotationExt l GHC.AnnVal

-- ---------------------------------------------------------------------

instance (Typeable name,GHC.OutputableBndr name,AnnotateP name) =>
                             AnnotateP (GHC.ConDeclField name) where
  annotateP l (GHC.ConDeclField ns ty mdoc) = do
    mapM_ annotatePC ns
    addDeltaAnnotation GHC.AnnDcolon
    annotatePC ty
    annotateMaybe mdoc

-- ---------------------------------------------------------------------

instance AnnotateP GHC.HsDocString where
  annotateP l (GHC.HsDocString _s) = do
    addDeltaAnnotationExt l GHC.AnnVal

-- ---------------------------------------------------------------------

instance (Typeable name,AnnotateP name) => AnnotateP (GHC.Pat name) where
  annotateP l (GHC.WildPat _) = addDeltaAnnotation GHC.AnnVal
  annotateP l (GHC.VarPat _) = addDeltaAnnotation GHC.AnnVal
  annotateP l (GHC.LazyPat p) = do
    addDeltaAnnotation GHC.AnnTilde
    annotatePC p

  annotateP l (GHC.AsPat ln p) = do
    annotatePC ln
    addDeltaAnnotation GHC.AnnAt
    annotatePC p

  annotateP l (GHC.ParPat p) = do
    addDeltaAnnotation GHC.AnnOpen
    annotatePC p
    addDeltaAnnotation GHC.AnnClose

  annotateP l (GHC.BangPat p) = do
    addDeltaAnnotation GHC.AnnBang
    annotatePC p

  annotateP l (GHC.ListPat ps _ _) = do
    addDeltaAnnotation GHC.AnnOpen
    mapM_ annotatePC ps
    addDeltaAnnotation GHC.AnnClose

  annotateP l (GHC.TuplePat ps _ _) = do
    addDeltaAnnotation GHC.AnnOpen
    mapM_ annotatePC ps
    addDeltaAnnotation GHC.AnnClose

  annotateP l (GHC.PArrPat ps _) = do
    addDeltaAnnotation GHC.AnnOpen
    mapM_ annotatePC ps
    addDeltaAnnotation GHC.AnnClose

  annotateP l (GHC.ConPatIn n dets) = do
    annotatePC n
    -- error $ "ConPatIn unfinished"
    `debug` "ConPatIn unfinished Utils"



  annotateP l p = do
    return () -- `debug` ("annotateP.Pat:" ++ showGhc (l,p))

-- ---------------------------------------------------------------------

instance (Typeable name,GHC.OutputableBndr name) =>
                            AnnotateP (GHC.Stmt name (GHC.LHsExpr name)) where
  annotateP l stmt = do
    return () `debug` ("annotateP.Stmt:not implemented for " ++ (showGhc (l,stmt)))

-- ---------------------------------------------------------------------

annotateHsLocalBinds :: (Typeable name,GHC.OutputableBndr name,AnnotateP name) =>
                                      (GHC.HsLocalBinds name) -> AP ()
annotateHsLocalBinds (GHC.HsValBinds (GHC.ValBindsIn binds sigs)) = do
    mapM_ annotatePC (GHC.bagToList binds)
    mapM_ annotatePC sigs

annotateHsLocalBinds (GHC.HsValBinds _) = assert False undefined
annotateHsLocalBinds (GHC.HsIPBinds vb) = assert False undefined
annotateHsLocalBinds (GHC.EmptyLocalBinds) = return ()

-- ---------------------------------------------------------------------

annotateMatchGroup :: (Typeable name,GHC.OutputableBndr name,AnnotateP name) =>
                      (GHC.MatchGroup name (GHC.LHsExpr name))
                   -> AP ()
annotateMatchGroup (GHC.MG matches _ _ _)
  = mapM_ annotatePC matches

-- ---------------------------------------------------------------------

instance (Typeable name,GHC.OutputableBndr name,AnnotateP name) =>
                               AnnotateP (GHC.HsExpr name) where
  annotateP l (GHC.HsVar _)           = addDeltaAnnotationExt l GHC.AnnVal
  annotateP l (GHC.HsIPVar _)         = addDeltaAnnotationExt l GHC.AnnVal
  annotateP l (GHC.HsOverLit ov)      = addDeltaAnnotationExt l GHC.AnnVal
  annotateP l (GHC.HsLit _)           = addDeltaAnnotationExt l GHC.AnnVal
  annotateP l (GHC.HsLam match)       = annotateMatchGroup match
  annotateP l (GHC.HsLamCase _ match) = annotateMatchGroup match

  annotateP l (GHC.HsApp e1 e2) = do
    annotatePC e1
    annotatePC e2

  annotateP l (GHC.OpApp e1 e2 _ e3) = do
    annotatePC e1
    annotatePC e2
    annotatePC e3

  annotateP l (GHC.NegApp e _) = do
    addDeltaAnnotation GHC.AnnMinus
    annotatePC e

  annotateP l (GHC.HsPar e) = do
    addDeltaAnnotation GHC.AnnOpen -- '('
    annotatePC e
    addDeltaAnnotation GHC.AnnClose -- ')'

  annotateP l (GHC.SectionL e1 e2) = do
    annotatePC e1
    annotatePC e2

  annotateP l (GHC.SectionR e1 e2) = do
    annotatePC e1
    annotatePC e2

  annotateP l (GHC.ExplicitTuple args boxity) = do
    addDeltaAnnotation GHC.AnnOpen
    mapM_ annotatePC args
    addDeltaAnnotation GHC.AnnClose

  annotateP l (GHC.HsCase e1 matches) = do
    addDeltaAnnotation GHC.AnnCase
    annotatePC e1
    addDeltaAnnotation GHC.AnnOf
    addDeltaAnnotation GHC.AnnOpen
    annotateMatchGroup matches
    addDeltaAnnotation GHC.AnnClose

  annotateP l (GHC.HsIf _ e1 e2 e3) = do
    addDeltaAnnotation GHC.AnnIf
    annotatePC e1
    addDeltaAnnotationLs GHC.AnnSemi 0
    addDeltaAnnotation GHC.AnnThen
    annotatePC e2
    addDeltaAnnotationLs GHC.AnnSemi 1
    addDeltaAnnotation GHC.AnnElse
    annotatePC e3

  annotateP l (GHC.HsMultiIf _ rhs) = do
    addDeltaAnnotation GHC.AnnIf
    mapM_ annotatePC rhs

  annotateP l (GHC.HsLet binds e) = do
    addDeltaAnnotation GHC.AnnLet
    addDeltaAnnotation GHC.AnnOpen
    annotateHsLocalBinds binds
    addDeltaAnnotation GHC.AnnClose
    addDeltaAnnotation GHC.AnnIn
    annotatePC e

  annotateP l (GHC.HsDo _ es _)         = do
    addDeltaAnnotation GHC.AnnDo
    addDeltaAnnotation GHC.AnnOpen
    mapM_ annotatePC es
    addDeltaAnnotation GHC.AnnClose

  annotateP l (GHC.ExplicitList _ _ es) = do
    addDeltaAnnotation GHC.AnnOpen
    mapM_ annotatePC es
    addDeltaAnnotation GHC.AnnClose

  annotateP l (GHC.ExplicitPArr _ es)   = do
    addDeltaAnnotation GHC.AnnOpen
    mapM_ annotatePC es
    addDeltaAnnotation GHC.AnnClose

  annotateP l (GHC.RecordCon (GHC.L ln _) _ (GHC.HsRecFields fs _)) = do
    addDeltaAnnotationExt ln GHC.AnnVal
    addDeltaAnnotation GHC.AnnOpen
    addDeltaAnnotation GHC.AnnDotdot
    mapM_ annotatePC fs
    addDeltaAnnotation GHC.AnnClose

  annotateP l (GHC.RecordUpd e (GHC.HsRecFields fs _) cons _ _) = do
    annotatePC e
    addDeltaAnnotation GHC.AnnOpen
    addDeltaAnnotation GHC.AnnDotdot
    mapM_ annotatePC fs
    addDeltaAnnotation GHC.AnnClose

  annotateP l (GHC.ExprWithTySig e typ _) = do
    annotatePC e
    addDeltaAnnotation GHC.AnnDcolon
    annotatePC typ

  annotateP l (GHC.ExprWithTySigOut e typ) = do
    annotatePC e
    addDeltaAnnotation GHC.AnnDcolon
    annotatePC typ

  annotateP l (GHC.ArithSeq _ _ seqInfo) = do
    addDeltaAnnotation GHC.AnnOpen -- '['
    case seqInfo of
        GHC.From e -> do
          annotatePC e
          addDeltaAnnotation GHC.AnnDotdot
        GHC.FromTo e1 e2 -> do
          annotatePC e1
          addDeltaAnnotation GHC.AnnDotdot
          annotatePC e2
        GHC.FromThen e1 e2 -> do
          annotatePC e1
          addDeltaAnnotation GHC.AnnComma
          annotatePC e2
          addDeltaAnnotation GHC.AnnDotdot
        GHC.FromThenTo e1 e2 e3 -> do
          annotatePC e1
          addDeltaAnnotation GHC.AnnComma
          annotatePC e2
          addDeltaAnnotation GHC.AnnDotdot
          annotatePC e3
    addDeltaAnnotation GHC.AnnClose -- ']'

  annotateP l (GHC.PArrSeq _ seqInfo) = do
    addDeltaAnnotation GHC.AnnOpen -- '[:'
    case seqInfo of
        GHC.From e -> do
          annotatePC e
          addDeltaAnnotation GHC.AnnDotdot
        GHC.FromTo e1 e2 -> do
          annotatePC e1
          addDeltaAnnotation GHC.AnnDotdot
          annotatePC e2
        GHC.FromThen e1 e2 -> do
          annotatePC e1
          addDeltaAnnotation GHC.AnnComma
          annotatePC e2
          addDeltaAnnotation GHC.AnnDotdot
        GHC.FromThenTo e1 e2 e3 -> do
          annotatePC e1
          addDeltaAnnotation GHC.AnnComma
          annotatePC e2
          addDeltaAnnotation GHC.AnnDotdot
          annotatePC e3
    addDeltaAnnotation GHC.AnnClose -- ':]'

  annotateP l (GHC.HsSCC _ csFStr e) = do
    addDeltaAnnotation GHC.AnnOpen -- '{-# SCC'
    addDeltaAnnotation GHC.AnnVal
    addDeltaAnnotation GHC.AnnValStr
    addDeltaAnnotation GHC.AnnClose -- '#-}'
    annotatePC e

  annotateP l (GHC.HsCoreAnn _ csFStr e) = do
    addDeltaAnnotation GHC.AnnOpen -- '{-# CORE'
    addDeltaAnnotation GHC.AnnVal
    addDeltaAnnotation GHC.AnnClose -- '#-}'
    annotatePC e

  annotateP l (GHC.HsBracket (GHC.VarBr _ _)) = do
    addDeltaAnnotationExt l GHC.AnnVal
  annotateP l (GHC.HsBracket (GHC.DecBrL ds)) = do
    addDeltaAnnotationLs GHC.AnnOpen 0
    addDeltaAnnotationLs GHC.AnnOpen 1
    mapM_ annotatePC ds
    addDeltaAnnotationLs GHC.AnnClose 0
    addDeltaAnnotationLs GHC.AnnClose 1
  annotateP l (GHC.HsBracket _) = do
    addDeltaAnnotation GHC.AnnOpen
    addDeltaAnnotationExt l GHC.AnnVal
    addDeltaAnnotation GHC.AnnClose

  annotateP l (GHC.HsRnBracketOut _ _) = return ()
  annotateP l (GHC.HsTcBracketOut _ _) = return ()

  annotateP l (GHC.HsSpliceE typed (GHC.HsSplice _ e)) = do
    addDeltaAnnotation GHC.AnnOpen -- possible '$('
    annotatePC e
    addDeltaAnnotation GHC.AnnClose -- possible ')'

  annotateP l (GHC.HsQuasiQuoteE (GHC.HsQuasiQuote _ _ _)) = do
    addDeltaAnnotationExt l GHC.AnnVal

  annotateP l (GHC.HsProc p c) = do
    addDeltaAnnotation GHC.AnnProc
    annotatePC p
    addDeltaAnnotation GHC.AnnRarrow
    annotatePC c

  annotateP l (GHC.HsArrApp e1 e2 _ _ _) = do
    annotatePC e1
    -- only one of the next 4 will be resent
    addDeltaAnnotation GHC.Annlarrowtail
    addDeltaAnnotation GHC.Annrarrowtail
    addDeltaAnnotation GHC.AnnLarrowtail
    addDeltaAnnotation GHC.AnnRarrowtail

    annotatePC e1

  annotateP l (GHC.HsArrForm e _ cs) = do
    addDeltaAnnotation GHC.AnnOpen -- '(|'
    annotatePC e
    mapM_ annotatePC cs
    addDeltaAnnotation GHC.AnnClose -- '|)'

  annotateP l (GHC.HsTick _ _) = return ()
  annotateP l (GHC.HsBinTick _ _ _) = return ()

  annotateP l (GHC.HsTickPragma _ (str,(v1,v2),(v3,v4)) e) = do
    -- '{-# GENERATED' STRING INTEGER ':' INTEGER '-' INTEGER ':' INTEGER '#-}'
    addDeltaAnnotation   GHC.AnnOpen     -- '{-# GENERATED'
    addDeltaAnnotationLs GHC.AnnVal   0 -- STRING
    addDeltaAnnotationLs GHC.AnnVal   1 -- INTEGER
    addDeltaAnnotationLs GHC.AnnColon 0 -- ':'
    addDeltaAnnotationLs GHC.AnnVal   2 -- INTEGER
    addDeltaAnnotation   GHC.AnnMinus   -- '-'
    addDeltaAnnotationLs GHC.AnnVal   3 -- INTEGER
    addDeltaAnnotationLs GHC.AnnColon 1 -- ':'
    addDeltaAnnotationLs GHC.AnnVal   4 -- INTEGER
    addDeltaAnnotation   GHC.AnnClose   -- '#-}'
    annotatePC e

  annotateP l (GHC.EWildPat) = do
    addDeltaAnnotationExt l GHC.AnnVal

  annotateP l (GHC.EAsPat (GHC.L ln _) e) = do
    addDeltaAnnotationExt ln GHC.AnnVal
    addDeltaAnnotation GHC.AnnAt
    annotatePC e

  annotateP l (GHC.EViewPat e1 e2) = do
    annotatePC e1
    addDeltaAnnotation GHC.AnnRarrow
    annotatePC e2

  annotateP l (GHC.ELazyPat e) = do
    addDeltaAnnotation GHC.AnnTilde
    annotatePC e

  annotateP l (GHC.HsType ty) = annotatePC ty

  annotateP l (GHC.HsWrap _ _) = return ()
  annotateP l (GHC.HsUnboundVar _) = return ()


-- ---------------------------------------------------------------------

instance (Typeable name,GHC.OutputableBndr name,AnnotateP name) =>
                             AnnotateP (GHC.HsTupArg name) where
  annotateP l (GHC.Present e@(GHC.L le _)) = do
    annotatePC e
    mcp <- getAnnotationAP l GHC.AnnComma
    let commaPos = deltaFromMaybeSrcSpans [le] mcp
    addAnnValue (AnnListItem commaPos)

  annotateP l (GHC.Missing _) = do
    mcp <- getAnnotationAP l GHC.AnnComma
    let commaPos = deltaFromMaybeSrcSpans [l] mcp
    addAnnValue (AnnListItem commaPos)

-- ---------------------------------------------------------------------

instance (Typeable name) => AnnotateP (GHC.HsCmdTop name) where
  annotateP = assert False undefined

-- ---------------------------------------------------------------------

instance (Typeable name,GHC.OutputableBndr name) => AnnotateP (GHC.TyClDecl name) where
  annotateP l x = do
    return () -- `debug` ("annotateP.TyClDecl:unimplemented for " ++ showGhc x)

{-
annotateLTyClDecl :: GHC.LTyClDecl GHC.RdrName -> AP ()
annotateLTyClDecl (GHC.L l (GHC.DataDecl _ln (GHC.HsQTvs _ns _tyVars) defn _)) = do
  enterAST l
  toksIn <- getToks
  let
    Just eqPos = findTokenWrtPrior ghcIsEqual l toksIn

  annotateHsDataDefn defn
  leaveAST (AnnDataDecl eqPos)
-}
-- ---------------------------------------------------------------------

instance (Typeable name,GHC.OutputableBndr name,AnnotateP name) =>
              AnnotateP (GHC.HsRecField name (GHC.LHsExpr name)) where
  annotateP l (GHC.HsRecField _ e _) = annotatePC e

-- ---------------------------------------------------------------------
{-
annotateHsDataDefn :: (GHC.HsDataDefn GHC.RdrName) -> AP ()
annotateHsDataDefn (GHC.HsDataDefn nOrD ctx mtyp mkind cons mderivs) = do
  mapM_ annotateLConDecl cons

-- ---------------------------------------------------------------------

annotateLConDecl :: (GHC.LConDecl GHC.RdrName) -> AP ()
annotateLConDecl (GHC.L l (GHC.ConDecl ln exp qvars ctx dets res _ _)) = do
  enterAST l
  toksIn <- getToks
  cs <- getComments
  let
    mc = getListAnnInfo l ghcIsVbar (const False) cs toksIn
  leaveAST (AnnConDecl mc)
-}
-- ---------------------------------------------------------------------

getHsLocalBindsSrcSpan :: (GHC.HsLocalBinds name) -> Maybe GHC.SrcSpan
getHsLocalBindsSrcSpan(GHC.HsValBinds (GHC.ValBindsIn binds sigs)) = r
  where
    r = end (GHC.bagToList binds) sigs

    end [] [] = Nothing
    end [] ys = Just $ GHC.getLoc (last ys)
    end xs [] = Just $ GHC.getLoc (last xs)
    end xs ys = if GHC.getLoc (last xs) < GHC.getLoc (last ys)
                  then Just $ GHC.getLoc (last ys)
                  else Just $ GHC.getLoc (last xs)

getHsLocalBindsSrcSpan (GHC.HsValBinds _) = Nothing
getHsLocalBindsSrcSpan (GHC.HsIPBinds vb) = Nothing
getHsLocalBindsSrcSpan (GHC.EmptyLocalBinds) = Nothing

-- ---------------------------------------------------------------------

getListSpan :: [GHC.Located e] -> [Span]
getListSpan xs = map ss2span $ getListSrcSpan xs

getListSrcSpan :: [GHC.Located e] -> [GHC.SrcSpan]
getListSrcSpan [] = []
getListSrcSpan xs = [GHC.mkSrcSpan (GHC.srcSpanStart (GHC.getLoc (head xs)))
                                   (GHC.srcSpanEnd   (GHC.getLoc (last xs)))
                    ]

getListSpans :: [GHC.Located e] -> [Span]
getListSpans xs = map (ss2span . GHC.getLoc) xs


commentPos :: Comment -> (Pos,Pos)
commentPos (Comment _ p _) = p

dcommentPos :: DComment -> (DeltaPos,DeltaPos)
dcommentPos (DComment _ p _) = p


-- ---------------------------------------------------------------------

-- | Given an enclosing Span @(p,e)@, and a list of sub SrcSpans @ds@,
-- identify all comments that are in @(p,e)@ but not in @ds@, and convert
-- them to be DComments relative to @p@
localComments :: Span -> [Comment] -> [Span] -> ([DComment],[Comment])
localComments pin cs ds = r
  -- `debug` ("localComments:(p,ds,r):" ++ show ((p,e),ds,map commentPos matches,map dcommentPos (fst r)))
  `debug` ("localComments:(p,ds,r):" ++ show ((p,e),ds,r))
  where
    r = (map (\c -> deltaComment p c) matches,misses ++ missesRest)
    (p,e) = if pin == ((1,1),(1,1))
               then  ((1,1),(99999999,1))
               else pin

    (matches,misses) = partition notSub cs'
    (cs',missesRest) = partition (\(Comment _ com _) -> isSubPos com (p,e)) cs

    notSub :: Comment -> Bool
    notSub (Comment _ com _) = not $ any (\sub -> isSubPos com sub) ds

    isSubPos (subs,sube) (parents,parente)
      = parents <= subs && parente >= sube

-- ---------------------------------------------------------------------
{-
findTokenSrcSpan :: (PosToken -> Bool) -> GHC.SrcSpan -> [PosToken] -> Maybe GHC.SrcSpan
findTokenSrcSpan isToken ss toks =
  case findToken isToken ss toks of
      Nothing -> Nothing
      Just t  -> Just (tokenSpan t)
-}
-- ---------------------------------------------------------------------
{-
findTokenSrcSpanReverse :: (PosToken -> Bool) -> GHC.SrcSpan -> [PosToken] -> Maybe GHC.SrcSpan
findTokenSrcSpanReverse isToken ss toks =
  case findTokenReverse isToken ss toks of
      Nothing -> Nothing
      Just t  -> Just (tokenSpan t)
-}
-- ---------------------------------------------------------------------
{-
findToken :: (PosToken -> Bool) -> GHC.SrcSpan -> [PosToken] -> Maybe PosToken
findToken isToken ss toks = r
  where
    (_,middle,_) = splitToksForSpan ss toks
    r = case filter isToken middle of
      [] -> Nothing
      (t:_) -> Just t
-}
-- ---------------------------------------------------------------------
{-
findTokenReverse :: (PosToken -> Bool) -> GHC.SrcSpan -> [PosToken] -> Maybe PosToken
findTokenReverse isToken ss toks = r
  -- `debug` ("findTokenReverse:(ss,r,middle):" ++ show (ss2span ss,r,middle))
  where
    (_,middle,_) = splitToksForSpan ss toks
    r = case filter isToken (reverse middle) of
      [] -> Nothing
      (t:_) -> Just t
-}
-- ---------------------------------------------------------------------
{-
findPreceding :: (PosToken -> Bool) -> GHC.SrcSpan -> [PosToken] -> Maybe GHC.SrcSpan
findPreceding isToken ss toks = r
  where
    (toksBefore,_,_) = splitToksForSpan ss toks
    r = case filter isToken (reverse toksBefore) of
      [] -> Nothing
      (t:_) -> Just (tokenSpan t)
-}
-- ---------------------------------------------------------------------
{-
findPrecedingMaybeDelta :: (PosToken -> Bool) -> GHC.SrcSpan -> [PosToken]
 -> Pos -> Maybe DeltaPos
findPrecedingMaybeDelta isToken ln toks p =
  case findPreceding isToken ln toks of
    Nothing -> Nothing
    Just ss -> Just (ss2delta p ss)
-}
-- ---------------------------------------------------------------------
{-
findPrecedingDelta :: (PosToken -> Bool) -> GHC.SrcSpan -> [PosToken]
 -> Pos -> DeltaPos
findPrecedingDelta isToken ln toks p =
  case findPrecedingMaybeDelta isToken ln toks p of
    Nothing -> error $ "findPrecedingDelta: No matching token preceding :" ++ show (ss2span ln)
    Just d  -> d
-}
-- ---------------------------------------------------------------------
{-
findTrailingMaybeDelta :: (PosToken -> Bool) -> GHC.SrcSpan -> [PosToken]
 -> Pos -> Maybe DeltaPos
findTrailingMaybeDelta isToken ln toks p =
  case findTrailing isToken ln toks of
    Nothing -> Nothing
    Just t -> Just (ss2delta p (tokenSpan t))
-}
-- ---------------------------------------------------------------------
{-
findTrailingDelta :: (PosToken -> Bool) -> GHC.SrcSpan -> [PosToken]
 -> Pos -> DeltaPos
findTrailingDelta isToken ln toks p =
  case findTrailingMaybeDelta isToken ln toks p of
    Nothing -> error $ "findTrailingDelta: No matching token trailing :" ++ show (ss2span ln)
    Just d -> d
-}
-- ---------------------------------------------------------------------
{-
findDelta :: (PosToken -> Bool) -> GHC.SrcSpan -> [PosToken]
 -> Pos -> DeltaPos
findDelta isToken ln toks p =
  case findTokenSrcSpan isToken ln toks of
    Nothing -> error $ "findPrecedingDelta: No matching token preceding :" ++ show (ss2span ln)
    Just ss -> ss2delta p ss
-}
-- ---------------------------------------------------------------------
{-
findDeltaReverse :: (PosToken -> Bool) -> GHC.SrcSpan -> [PosToken]
 -> Pos -> DeltaPos
findDeltaReverse isToken ln toks p =
  case findTokenSrcSpanReverse isToken ln toks of
    Nothing -> error $ "findPrecedingDelta: No matching token preceding :" ++ show (ss2span ln)
    Just ss -> ss2delta p ss
-}
-- ---------------------------------------------------------------------
{-
findTrailingComma :: GHC.SrcSpan -> [PosToken] -> Maybe DeltaPos
findTrailingComma ss toks = r
  where
    (_,_,toksAfter) = splitToksForSpan ss toks
    r = case filter ghcIsComma toksAfter of
      [] -> Nothing
      (t:_) -> Just (ss2delta (ss2posEnd ss) $ tokenSpan t)
-}

-- ---------------------------------------------------------------------
{-
findTrailingSrcSpan :: (PosToken -> Bool) -> GHC.SrcSpan -> [PosToken] -> Maybe GHC.SrcSpan
findTrailingSrcSpan isToken ss toks = r
  where
    (_,_,toksAfter) = splitToksForSpan ss toks
    r = case filter isToken toksAfter of
      [] -> Nothing
      (t:_) -> Just (tokenSpan t)
-}
-- ---------------------------------------------------------------------
{-
findTrailing :: (PosToken -> Bool) -> GHC.SrcSpan -> [PosToken] -> Maybe PosToken
findTrailing isToken ss toks = r
  where
    (_,_,toksAfter) = splitToksForSpan ss toks
    r = case filter isToken toksAfter of
      [] -> Nothing
      (t:_) -> Just t
-}

-- ---------------------------------------------------------------------

undeltaComment :: Pos -> DComment -> Comment
undeltaComment l (DComment b (dps,dpe) s) = Comment b ((undelta l dps),(undelta l dpe)) s

deltaComment :: Pos -> Comment -> DComment
deltaComment l (Comment b (s,e) str)
  = DComment b ((ss2deltaP l s),(ss2deltaP l e)) str

-- ---------------------------------------------------------------------

deriving instance Eq GHC.Token
{-
ghcIsTok :: PosToken -> GHC.Token -> Bool
ghcIsTok ((GHC.L _ t),_s) tp = t == tp

ghcIsModule :: PosToken -> Bool
ghcIsModule t = ghcIsTok t GHC.ITmodule

ghcIsLet :: PosToken -> Bool
ghcIsLet t = ghcIsTok t GHC.ITlet

ghcIsAt :: PosToken -> Bool
ghcIsAt t = ghcIsTok t GHC.ITat

ghcIsElse :: PosToken -> Bool
ghcIsElse t = ghcIsTok t GHC.ITelse

ghcIsThen :: PosToken -> Bool
ghcIsThen t = ghcIsTok t GHC.ITthen

ghcIsOf :: PosToken -> Bool
ghcIsOf t = ghcIsTok t GHC.ITof

ghcIsDo :: PosToken -> Bool
ghcIsDo t = ghcIsTok t GHC.ITdo

ghcIsIn :: PosToken -> Bool
ghcIsIn t = ghcIsTok t GHC.ITin

ghcIsOParen :: PosToken -> Bool
ghcIsOParen t = ghcIsTok t GHC.IToparen

ghcIsCParen :: PosToken -> Bool
ghcIsCParen t = ghcIsTok t GHC.ITcparen

ghcIsOBrack :: PosToken -> Bool
ghcIsOBrack t = ghcIsTok t GHC.ITobrack

ghcIsCBrack :: PosToken -> Bool
ghcIsCBrack t = ghcIsTok t GHC.ITcbrack

ghcIsOubxparen :: PosToken -> Bool
ghcIsOubxparen t = ghcIsTok t GHC.IToubxparen

ghcIsCubxparen :: PosToken -> Bool
ghcIsCubxparen t = ghcIsTok t GHC.ITcubxparen

ghcIsComma :: PosToken -> Bool
ghcIsComma t = ghcIsTok t GHC.ITcomma

ghcIsImport :: PosToken -> Bool
ghcIsImport t = ghcIsTok t GHC.ITimport

ghcIsQualified :: PosToken -> Bool
ghcIsQualified t = ghcIsTok t GHC.ITqualified

ghcIsAs :: PosToken -> Bool
ghcIsAs t = ghcIsTok t GHC.ITas

ghcIsDcolon :: PosToken -> Bool
ghcIsDcolon t = ghcIsTok t GHC.ITdcolon

ghcIsTilde :: PosToken -> Bool
ghcIsTilde t = ghcIsTok t GHC.ITtilde

ghcIsBang :: PosToken -> Bool
ghcIsBang t = ghcIsTok t GHC.ITbang

ghcIsRarrow :: PosToken -> Bool
ghcIsRarrow t = ghcIsTok t GHC.ITrarrow

ghcIsDarrow :: PosToken -> Bool
ghcIsDarrow t = ghcIsTok t GHC.ITdarrow

ghcIsDotdot :: PosToken -> Bool
ghcIsDotdot t = ghcIsTok t GHC.ITdotdot

ghcIsConid :: PosToken -> Bool
ghcIsConid ((GHC.L _ t),_) = case t of
       GHC.ITconid _ -> True
       _             -> False

ghcIsQConid :: PosToken -> Bool
ghcIsQConid((GHC.L _ t),_) = case t of
       GHC.ITqconid _ -> True
       _              -> False

ghcIsVarid :: PosToken -> Bool
ghcIsVarid ((GHC.L _ t),_) = case t of
       GHC.ITvarid _ -> True
       _             -> False

ghcIsVarsym :: PosToken -> Bool
ghcIsVarsym ((GHC.L _ t),_) = case t of
       GHC.ITvarsym _ -> True
       _              -> False

ghcIsBackquote :: PosToken -> Bool
ghcIsBackquote t = ghcIsTok t GHC.ITbackquote

ghcIsFunName :: PosToken -> Bool
ghcIsFunName t = ghcIsVarid t || ghcIsVarsym t || ghcIsBackquote t

ghcIsAnyConid :: PosToken -> Bool
ghcIsAnyConid t = ghcIsConid t || ghcIsQConid t


ghcIsHiding :: PosToken -> Bool
ghcIsHiding t = ghcIsTok t GHC.IThiding

ghcIsEqual :: PosToken -> Bool
ghcIsEqual t = ghcIsTok t GHC.ITequal

ghcIsVbar :: PosToken -> Bool
ghcIsVbar t = ghcIsTok t GHC.ITvbar


ghcIsInteger :: PosToken -> Bool
ghcIsInteger ((GHC.L _ t),_)  = case t of
      GHC.ITinteger _ _ -> True
      _                 -> False

ghcIsRational :: PosToken -> Bool
ghcIsRational ((GHC.L _ t),_) = case t of
      GHC.ITrational _ -> True
      _                -> False

ghcIsOverLit :: PosToken -> Bool
ghcIsOverLit t = ghcIsInteger t || ghcIsRational t
-}


ghcIsComment :: PosToken -> Bool
ghcIsComment ((GHC.L _ (GHC.ITdocCommentNext _)),_s)  = True
ghcIsComment ((GHC.L _ (GHC.ITdocCommentPrev _)),_s)  = True
ghcIsComment ((GHC.L _ (GHC.ITdocCommentNamed _)),_s) = True
ghcIsComment ((GHC.L _ (GHC.ITdocSection _ _)),_s)    = True
ghcIsComment ((GHC.L _ (GHC.ITdocOptions _)),_s)      = True
ghcIsComment ((GHC.L _ (GHC.ITdocOptionsOld _)),_s)   = True
ghcIsComment ((GHC.L _ (GHC.ITlineComment _)),_s)     = True
ghcIsComment ((GHC.L _ (GHC.ITblockComment _)),_s)    = True
ghcIsComment ((GHC.L _ _),_s)                         = False

ghcIsMultiLine :: GHC.Located GHC.AnnotationComment -> Bool
ghcIsMultiLine (GHC.L _ (GHC.AnnDocCommentNext _))  = False
ghcIsMultiLine (GHC.L _ (GHC.AnnDocCommentPrev _))  = False
ghcIsMultiLine (GHC.L _ (GHC.AnnDocCommentNamed _)) = False
ghcIsMultiLine (GHC.L _ (GHC.AnnDocSection _ _))    = False
ghcIsMultiLine (GHC.L _ (GHC.AnnDocOptions _))      = False
ghcIsMultiLine (GHC.L _ (GHC.AnnDocOptionsOld _))   = False
ghcIsMultiLine (GHC.L _ (GHC.AnnLineComment _))     = False
ghcIsMultiLine (GHC.L _ (GHC.AnnBlockComment _))    = True

ghcCommentText :: GHC.Located GHC.AnnotationComment -> String
ghcCommentText (GHC.L _ (GHC.AnnDocCommentNext s))  = s
ghcCommentText (GHC.L _ (GHC.AnnDocCommentPrev s))  = s
ghcCommentText (GHC.L _ (GHC.AnnDocCommentNamed s)) = s
ghcCommentText (GHC.L _ (GHC.AnnDocSection _ s))    = s
ghcCommentText (GHC.L _ (GHC.AnnDocOptions s))      = s
ghcCommentText (GHC.L _ (GHC.AnnDocOptionsOld s))   = s
ghcCommentText (GHC.L _ (GHC.AnnLineComment s))     = s
ghcCommentText (GHC.L _ (GHC.AnnBlockComment s))    = "{-" ++ s ++ "-}"

ghcIsBlank :: PosToken -> Bool
ghcIsBlank (_,s)  = s == ""

ghcIsBlankOrComment :: PosToken -> Bool
ghcIsBlankOrComment t = ghcIsBlank t || ghcIsComment t

-- ---------------------------------------------------------------------

-- | The '[SrcSpan]' should have zero or one element
maybeSrcSpan :: Maybe (GHC.Located a) -> [GHC.SrcSpan]
maybeSrcSpan (Just (GHC.L ss _)) = [ss]
maybeSrcSpan _ = []

-- | The '[SrcSpan]' should have zero or one element
deltaFromMaybeSrcSpans :: [GHC.SrcSpan] -> [GHC.SrcSpan] -> Maybe DeltaPos
deltaFromMaybeSrcSpans [ss1] [ss2] = Just (deltaFromSrcSpans ss1 ss2)
deltaFromMaybeSrcSpans _ _ = Nothing

deltaFromLastSrcSpan :: [GHC.SrcSpan] -> [GHC.SrcSpan] -> Maybe DeltaPos
deltaFromLastSrcSpan [] _ = error $ "deltaFromLastSrcSpan: no last SrcSpan"
deltaFromLastSrcSpan sss ms = deltaFromMaybeSrcSpans [last sss] ms

-- | Create a delta covering the gap between the end of the first
-- @SrcSpan@ and the start of the second.
deltaFromSrcSpans :: GHC.SrcSpan -> GHC.SrcSpan -> DeltaPos
deltaFromSrcSpans ss1 ss2 = ss2delta (ss2posEnd ss1) ss2

ss2delta :: Pos -> GHC.SrcSpan -> DeltaPos
ss2delta ref ss = ss2deltaP ref (ss2pos ss)

-- | Convert the start of the second @Pos@ to be an offset from the
-- first. The assumption is the reference starts before the second @Pos@
ss2deltaP :: Pos -> Pos -> DeltaPos
ss2deltaP (refl,refc) (l,c) = DP (lo,co)
  where
    lo = l - refl
    co = if lo == 0 then c - refc
                    else c

undelta :: Pos -> DeltaPos -> Pos
undelta (l,c) (DP (dl,dc)) = (fl,fc)
  where
    fl = l + dl
    fc = if dl == 0 then c + dc else dc

-- prop_delta :: TODO

ss2pos :: GHC.SrcSpan -> Pos
ss2pos ss = (srcSpanStartLine ss,srcSpanStartColumn ss)

ss2posEnd :: GHC.SrcSpan -> Pos
ss2posEnd ss = (srcSpanEndLine ss,srcSpanEndColumn ss)

ss2span :: GHC.SrcSpan -> Span
ss2span ss = (ss2pos ss,ss2posEnd ss)

srcSpanStart :: GHC.SrcSpan -> Pos
srcSpanStart ss = (srcSpanStartLine ss,srcSpanStartColumn ss)

srcSpanEnd :: GHC.SrcSpan -> Pos
srcSpanEnd ss = (srcSpanEndLine ss,srcSpanEndColumn ss)


srcSpanEndColumn :: GHC.SrcSpan -> Int
srcSpanEndColumn (GHC.RealSrcSpan s) = GHC.srcSpanEndCol s
srcSpanEndColumn _ = 0

srcSpanStartColumn :: GHC.SrcSpan -> Int
srcSpanStartColumn (GHC.RealSrcSpan s) = GHC.srcSpanStartCol s
srcSpanStartColumn _ = 0

srcSpanEndLine :: GHC.SrcSpan -> Int
srcSpanEndLine (GHC.RealSrcSpan s) = GHC.srcSpanEndLine s
srcSpanEndLine _ = 0

srcSpanStartLine :: GHC.SrcSpan -> Int
srcSpanStartLine (GHC.RealSrcSpan s) = GHC.srcSpanStartLine s
srcSpanStartLine _ = 0

nullSpan :: Span
nullSpan = ((0,0),(0,0))

-- ---------------------------------------------------------------------

tokenSpan :: PosToken -> GHC.SrcSpan
tokenSpan ((GHC.L l _),_s) = l

tokenPos :: PosToken -> Pos
tokenPos ((GHC.L l _),_s) = srcSpanStart l

tokenPosEnd :: PosToken -> Pos
tokenPosEnd ((GHC.L l _),_s) = srcSpanEnd l

tokenString :: PosToken -> String
tokenString (_,s) = s

-- ---------------------------------------------------------------------

splitToks:: (GHC.SrcSpan,GHC.SrcSpan) -> [PosToken]->([PosToken],[PosToken],[PosToken])
splitToks (startPos, endPos) toks =
  let (toks1,toks2)   = break (\t -> tokenSpan t >= startPos) toks
      (toks21,toks22) = break (\t -> tokenSpan t >=   endPos) toks2
  in
    (toks1,toks21,toks22)

-- ---------------------------------------------------------------------

splitToksForSpan:: GHC.SrcSpan -> [PosToken] -> ([PosToken],[PosToken],[PosToken])
splitToksForSpan ss toks =
  let (toks1,toks2)   = break (\t -> tokenPos t >= srcSpanStart ss) toks
      (toks21,toks22) = break (\t -> tokenPos t >= srcSpanEnd   ss) toks2
  in
    (toks1,toks21,toks22)

-- ---------------------------------------------------------------------

isSymbolRdrName :: GHC.RdrName -> Bool
isSymbolRdrName n = GHC.isSymOcc $ GHC.rdrNameOcc n

rdrName2String :: GHC.RdrName -> String
rdrName2String r =
  case GHC.isExact_maybe r of
    Just n  -> name2String n
    Nothing ->  GHC.occNameString $ GHC.rdrNameOcc r

name2String :: GHC.Name -> String
name2String name = showGhc name

-- |Show a GHC API structure
showGhc :: (GHC.Outputable a) => a -> String
#if __GLASGOW_HASKELL__ > 706
showGhc x = GHC.showPpr GHC.unsafeGlobalDynFlags x
#else
#if __GLASGOW_HASKELL__ > 704
showGhc x = GHC.showSDoc GHC.tracingDynFlags $ GHC.ppr x
#else
showGhc x = GHC.showSDoc                     $ GHC.ppr x
#endif
#endif


-- |Show a GHC API structure
showGhcDebug :: (GHC.Outputable a) => a -> String
#if __GLASGOW_HASKELL__ > 706
showGhcDebug x = GHC.showSDocDebug GHC.unsafeGlobalDynFlags (GHC.ppr x)
#else
#if __GLASGOW_HASKELL__ > 704
showGhcDebug x = GHC.showSDoc GHC.tracingDynFlags $ GHC.ppr x
#else
showGhcDebug x = GHC.showSDoc                     $ GHC.ppr x
#endif
#endif

-- ---------------------------------------------------------------------

instance Show (GHC.GenLocated GHC.SrcSpan GHC.Token) where
  show t@(GHC.L l tok) = show ((srcSpanStart l, srcSpanEnd l),tok)

-- ---------------------------------------------------------------------

pp a = GHC.showPpr GHC.unsafeGlobalDynFlags a

-- -------------------------------------------------------------------..
-- Copied from MissingH, does not compile with HEAD


{- | Merge two sorted lists into a single, sorted whole.

Example:

> merge [1,3,5] [1,2,4,6] -> [1,1,2,3,4,5,6]

QuickCheck test property:

prop_merge xs ys =
    merge (sort xs) (sort ys) == sort (xs ++ ys)
          where types = xs :: [Int]
-}
merge ::  (Ord a) => [a] -> [a] -> [a]
merge = mergeBy (compare)

{- | Merge two sorted lists using into a single, sorted whole,
allowing the programmer to specify the comparison function.

QuickCheck test property:

prop_mergeBy xs ys =
    mergeBy cmp (sortBy cmp xs) (sortBy cmp ys) == sortBy cmp (xs ++ ys)
          where types = xs :: [ (Int, Int) ]
                cmp (x1,_) (x2,_) = compare x1 x2
-}
mergeBy :: (a -> a -> Ordering) -> [a] -> [a] -> [a]
mergeBy cmp [] ys = ys
mergeBy cmp xs [] = xs
mergeBy cmp (allx@(x:xs)) (ally@(y:ys))
        -- Ordering derives Eq, Ord, so the comparison below is valid.
        -- Explanation left as an exercise for the reader.
        -- Someone please put this code out of its misery.
    | (x `cmp` y) <= EQ = x : mergeBy cmp xs ally
    | otherwise = y : mergeBy cmp allx ys


maybeL :: Show a => a -> [a] -> a
maybeL def [] = def
maybeL _ [x] = x
maybeL def xs = error $ "maybeL " ++ show (def,xs)
