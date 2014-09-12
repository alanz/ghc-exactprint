{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.GHC.ExactPrint
-- Based on
-- --------------------------------------------------------------------------
-- Module      :  Language.Haskell.Exts.Annotated.ExactPrint
-- Copyright   :  (c) Niklas Broberg 2009
-- License     :  BSD-style (see the file LICENSE.txt)
--
-- Maintainer  :  Niklas Broberg, d00nibro@chalmers.se
-- Stability   :  stable
-- Portability :  portable
--
-- Exact-printer for Haskell abstract syntax. The input is a (semi-concrete)
-- abstract syntax tree, annotated with exact source information to enable
-- printing the tree exactly as it was parsed.
--
-----------------------------------------------------------------------------
module Language.Haskell.GHC.ExactPrint
        ( annotate
        , exactPrintAnnotated
        , exactPrintAnnotation

        , exactPrint
        , ExactP

        , toksToComments
        ) where

import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Utils

import Control.Monad (when, liftM, ap)
import Control.Applicative (Applicative(..))
import Control.Arrow ((***), (&&&))
import Data.Data
import Data.List (intersperse)
import Data.List.Utils
import Data.Maybe

import qualified Bag           as GHC
import qualified DynFlags      as GHC
import qualified FastString    as GHC
import qualified ForeignCall   as GHC
import qualified GHC           as GHC
import qualified GHC.Paths     as GHC
import qualified Lexer         as GHC
import qualified Name          as GHC
import qualified NameSet       as GHC
import qualified Outputable    as GHC
import qualified RdrName       as GHC
import qualified SrcLoc        as GHC
import qualified StringBuffer  as GHC
import qualified UniqSet       as GHC
import qualified Unique        as GHC
import qualified Var           as GHC

import qualified Data.Map as Map

import Debug.Trace

debug :: c -> String -> c
debug = flip trace

-- ---------------------------------------------------------------------

-- Compatibiity types, from HSE

-- | A portion of the source, extended with information on the position of entities within the span.
data SrcSpanInfo = SrcSpanInfo
    { srcInfoSpan    :: GHC.SrcSpan
    , srcInfoPoints  :: [GHC.SrcSpan]    -- Marks the location of specific entities inside the span
    }
  deriving (Eq,Ord,Show,Typeable,Data)


-- | A class to work over all kinds of source location information.
class SrcInfo si where
  toSrcInfo   :: GHC.SrcLoc -> [GHC.SrcSpan] -> GHC.SrcLoc -> si
  fromSrcInfo :: SrcSpanInfo -> si
  getPointLoc :: si -> GHC.SrcLoc
  fileName    :: si -> String
  startLine   :: si -> Int
  startColumn :: si -> Int

  getPointLoc si = GHC.mkSrcLoc (GHC.mkFastString $ fileName si) (startLine si) (startColumn si)


instance SrcInfo GHC.SrcSpan where
  toSrcInfo   = error "toSrcInfo GHC.SrcSpan undefined"
  fromSrcInfo = error "toSrcInfo GHC.SrcSpan undefined"

  getPointLoc = GHC.srcSpanStart

  fileName (GHC.RealSrcSpan s) = GHC.unpackFS $ GHC.srcSpanFile s
  fileName _                   = "bad file name for SrcSpan"

  startLine   = srcSpanStartLine
  startColumn = srcSpanStartColumn



class Annotated a where
  ann :: a -> GHC.SrcSpan

instance Annotated (GHC.Located a) where
  ann (GHC.L l _) = l


-- | Test if a given span starts and ends at the same location.
isNullSpan :: GHC.SrcSpan -> Bool
isNullSpan ss = spanSize ss == (0,0)

spanSize :: GHC.SrcSpan -> (Int, Int)
spanSize ss = (srcSpanEndLine ss - srcSpanStartLine ss,
               max 0 (srcSpanEndColumn ss - srcSpanStartColumn ss))

toksToComments :: [PosToken] -> [Comment]
toksToComments toks = map tokToComment $ filter ghcIsComment toks
  where
    tokToComment t@(GHC.L l _,s) = Comment (ghcIsMultiLine t) ((ss2pos l),(ss2posEnd l)) s


------------------------------------------------------
-- The EP monad and basic combinators

pos :: (SrcInfo loc) => loc -> Pos
pos ss = (startLine ss, startColumn ss)

newtype EP x = EP (Pos -> [Comment] -> Anns -> (x, Pos, [Comment], Anns, ShowS))

instance Functor EP where
  fmap = liftM

instance Applicative EP where
  pure = return
  (<*>) = ap

instance Monad EP where
  return x = EP $ \l cs an -> (x, l, cs, an, id)

  EP m >>= k = EP $ \l0 c0 an0 -> let
        (a, l1, c1, an1, s1) = m l0 c0 an0
        EP f = k a
        (b, l2, c2, an2, s2) = f l1 c1 an1
    in (b, l2, c2, an2, s1 . s2)

runEP :: EP () -> [Comment] -> Anns -> String
runEP (EP f) cs ans = let (_,_,_,_,s) = f (1,1) cs ans in s ""

getPos :: EP Pos
getPos = EP (\l cs an -> (l,l,cs,an,id))

setPos :: Pos -> EP ()
setPos l = EP (\_ cs an -> ((),l,cs,an,id))

getAnnotation :: GHC.SrcSpan -> EP (Maybe Annotation)
getAnnotation ss = EP (\l cs an -> (Map.lookup ss an,l,cs,an,id))

printString :: String -> EP ()
printString str = EP (\(l,c) cs an -> ((), (l,c+length str), cs, an, showString str))

getComment :: EP (Maybe Comment)
getComment = EP $ \l cs an ->
    let x = case cs of
             c:_ -> Just c
             _   -> Nothing
     in (x, l, cs, an, id)

dropComment :: EP ()
dropComment = EP $ \l cs an ->
    let cs' = case cs of
               (_:cs) -> cs
               _      -> cs
     in ((), l, cs', an, id)

mergeComments :: [DComment] -> EP ()
mergeComments dcs = EP $ \l cs an ->
    let acs = map (undeltaComment l) dcs
        cs' = merge acs cs
    in ((), l, cs', an, id)

newLine :: EP ()
newLine = do
    (l,_) <- getPos
    printString "\n"
    setPos (l+1,1)

padUntil :: Pos -> EP ()
padUntil (l,c) = do
    (l1,c1) <- getPos
    case  {- trace (show ((l,c), (l1,c1))) -} () of
     _ {-()-} | l1 >= l && c1 <= c -> printString $ replicate (c - c1) ' '
              | l1 < l             -> newLine >> padUntil (l,c)
              | otherwise          -> return ()

padDelta :: DeltaPos -> EP ()
padDelta (DP (dl,dc)) = do
    (l1,c1) <- getPos
    let (l,c) = (l1+dl,c1+dc)
    case  {- trace (show ((l,c), (l1,c1))) -} () of
     _ {-()-} | l1 >= l && c1 <= c -> printString $ replicate (c - c1) ' '
              | l1 < l             -> newLine >> padUntil (l,c)
              | otherwise          -> return ()


mPrintComments :: Pos -> EP ()
mPrintComments p = do
    mc <- getComment
    case mc of
     Nothing -> return ()
     Just (Comment multi (s,e) str) ->
        when (s < p) $ do
            dropComment
            padUntil s
            printComment multi str
            setPos e
            mPrintComments p

printComment :: Bool -> String -> EP ()
printComment b str
    | b         = printString str
    | otherwise = printString str

printWhitespace :: Pos -> EP ()
printWhitespace p = mPrintComments p >> padUntil p

printStringAt :: Pos -> String -> EP ()
printStringAt p str = printWhitespace p >> printString str

printStringAtDelta :: DeltaPos -> String -> EP ()
printStringAtDelta (DP (dl,dc)) str = do
  (l1,c1) <- getPos
  let (l,c) = (l1 + dl, c1 + dc)
  printWhitespace (l,c) >> printString str

printStringAtMaybe :: Maybe Pos -> String -> EP ()
printStringAtMaybe mc s =
  case mc of
    Nothing -> return ()
    Just cl -> printStringAt cl s

printStringAtMaybeDelta :: Maybe DeltaPos -> String -> EP ()
printStringAtMaybeDelta mc s =
  case mc of
    Nothing -> return ()
    Just cl -> do
      p <- getPos
      printStringAt (undelta p cl) s

printStringAtMaybeDeltaP :: Pos -> Maybe DeltaPos -> String -> EP ()
printStringAtMaybeDeltaP p mc s =
  case mc of
    Nothing -> return ()
    Just cl -> do
      printStringAt (undelta p cl) s

errorEP :: String -> EP a
errorEP = fail

------------------------------------------------------------------------------
-- Printing of source elements

-- | Print an AST exactly as specified by the annotations on the nodes in the tree.
-- exactPrint :: (ExactP ast) => ast -> [Comment] -> String
exactPrint :: (ExactP ast) => GHC.Located ast -> [Comment] -> [PosToken] -> String
exactPrint ast cs toks = runEP (exactPC ast) cs Map.empty


exactPrintAnnotated ::
     GHC.Located (GHC.HsModule GHC.RdrName)
  -> [Comment] -> [PosToken] -> String
exactPrintAnnotated ast cs toks = runEP (exactPC ast) [] ann
  where
    ann = Map.fromList $ annotateLHsModule ast cs toks

exactPrintAnnotation :: ExactP ast =>
  GHC.Located ast -> [Comment] -> Anns -> String
exactPrintAnnotation ast cs ann = runEP (exactPC ast) cs ann

annotate :: GHC.Located (GHC.HsModule GHC.RdrName) -> [Comment] -> [PosToken] -> Anns
annotate ast cs toks = Map.fromList $ annotateLHsModule ast cs toks

-- |First move to the given location, then call exactP
exactPC :: (ExactP ast) => GHC.Located ast -> EP ()
exactPC (GHC.L l ast) =
 let p = pos l
 in do ma <- getAnnotation l
       mPrintComments p
       padUntil p
       exactP ma ast

printSeq :: [(Pos, EP ())] -> EP ()
printSeq [] = return ()
printSeq ((p,pr):xs) = printWhitespace p >> pr >> printSeq xs

printStrs :: SrcInfo loc => [(loc, String)] -> EP ()
printStrs = printSeq . map (pos *** printString)

printPoints :: SrcSpanInfo -> [String] -> EP ()
printPoints l = printStrs . zip (srcInfoPoints l)

printInterleaved :: (Annotated ast, SrcInfo loc, ExactP ast) => [(loc, String)] -> [ast] -> EP ()
printInterleaved sistrs asts = printSeq $
    interleave (map (pos *** printString ) sistrs)
           --  (map (pos . ann &&& exactP) asts)
               (map (pos . ann &&& exactP') asts)
  where
    exactP' ast = do
      ma <- getAnnotation (ann ast)
      exactP ma ast

-- so, f ast = pos $ ann ast
--     g ast = exactP ast

{-

The default definition may be overridden with a more efficient version if desired.

(***) :: a b c -> a b' c' -> a (b, b') (c, c') -- infixr 3
  Split the input between the two argument arrows and combine their output.
  Note that this is in general not a functor.
f *** g = first f >>> second g


(&&&) :: a b c -> a b c' -> a b (c, c') -- infixr 3
  Fanout: send the input to both argument arrows and combine their output.
f &&& g = arr (\b -> (b,b)) >>> f *** g


-- | Lift a function to an arrow.
    arr :: (b -> c) -> a b c

-- | Send the first component of the input through the argument
    --   arrow, and copy the rest unchanged to the output.
    first :: a b c -> a (b,d) (c,d)

-}

printInterleaved' sistrs (a:asts) = exactPC a >> printInterleaved sistrs asts
printInterleaved' _ _ = internalError "printInterleaved'"

printStreams :: [(Pos, EP ())] -> [(Pos, EP ())] -> EP ()
printStreams [] ys = printSeq ys
printStreams xs [] = printSeq xs
printStreams (x@(p1,ep1):xs) (y@(p2,ep2):ys)
    | p1 <= p2 = printWhitespace p1 >> ep1 >> printStreams xs (y:ys)
    | otherwise = printWhitespace p2 >> ep2 >> printStreams (x:xs) ys


interleave :: [a] -> [a] -> [a]
interleave [] ys = ys
interleave xs [] = xs
interleave (x:xs) (y:ys) = x:y: interleave xs ys

maybeEP :: (a -> EP ()) -> Maybe a -> EP ()
maybeEP = maybe (return ())

-- bracketList :: (ExactP ast) => (String, String, String) -> [GHC.SrcSpan] -> [ast] -> EP ()
bracketList :: (Annotated b1, SrcInfo b, ExactP b1) => (String, String, String) -> [b] -> [b1] -> EP ()
bracketList (a,b,c) poss asts = printInterleaved (pList poss (a,b,c)) asts

pList (p:ps) (a,b,c) = (p,a) : pList' ps (b,c)
pList _ _ = internalError "pList"
pList' [] _ = []
pList' [p] (_,c) = [(p,c)]
pList' (p:ps) (b,c) = (p, b) : pList' ps (b,c)

parenList, squareList, curlyList, parenHashList :: (Annotated ast,ExactP ast) => [GHC.SrcSpan] -> [ast] -> EP ()
parenList = bracketList ("(",",",")")
squareList = bracketList ("[",",","]")
curlyList = bracketList ("{",",","}")
parenHashList = bracketList ("(#",",","#)")

-- layoutList :: (Functor ast, Show (ast ()), ExactP ast) => [GHC.SrcSpan] -> [ast] -> EP ()
layoutList :: (Annotated ast, ExactP ast) => [GHC.SrcSpan] -> [ast] -> EP ()
layoutList poss asts = printStreams
        (map (pos *** printString) $ lList poss)
        (map (pos . ann &&& exactP') asts)
  where
    exactP' ast = do
      ma <- getAnnotation (ann ast)
      exactP ma ast

lList (p:ps) = (if isNullSpan p then (p,"") else (p,"{")) : lList' ps
lList _ = internalError "lList"
lList' [] = []
lList' [p] = [if isNullSpan p then (p,"") else (p,"}")]
lList' (p:ps) = (if isNullSpan p then (p,"") else (p,";")) : lList' ps

printSemi :: GHC.SrcSpan -> EP ()
printSemi p = do
  printWhitespace (pos p)
  when (not $ isNullSpan p) $ printString ";"


--------------------------------------------------
-- Exact printing for GHC

class ExactP ast where
  -- | Print an AST fragment, possibly having an annotation. The
  -- correct position in output is already established.
  exactP :: (Maybe Annotation) -> ast -> EP ()

instance ExactP (GHC.HsModule GHC.RdrName) where
  exactP ma (GHC.HsModule Nothing exps imps decls deprecs haddock) = do
    printSeq $ map (pos . ann &&& exactPC) decls
    printString "foo"

  exactP ma (GHC.HsModule (Just lmn@(GHC.L l mn)) mexp imps decls deprecs haddock) = do
    mAnn <- getAnnotation l
    -- p <- getPos -- starting position is bogus
    let p = (1,0)
    case mAnn of
      Just (Ann cs _ (AnnModuleName pm _pn po pc pw)) -> do
        mergeComments cs -- TODO: make this part of getAnnotation, or perhaps activateAnnotation
        printStringAt (undelta p pm) "module"
        exactPC lmn
        case mexp of
          Just exps -> do
            printStringAt (undelta p po) "("
            mapM_ exactPC exps
            p2 <- getPos
            printStringAt (undelta p2 pc) ")"
          Nothing -> return ()
        printStringAt (undelta p pw) "where"
        mapM_ exactPC imps
      _ -> return ()

    printSeq $ map (pos . ann &&& exactPC) decls
    printString "foo"

-- ---------------------------------------------------------------------

instance ExactP (GHC.ModuleName) where
  exactP ma mn = do
    printString (GHC.moduleNameString mn)

-- ---------------------------------------------------------------------

instance ExactP (GHC.IE GHC.RdrName) where
  exactP ma (GHC.IEVar n) = do
    let Just (Ann cs ll (AnnIEVar mc)) = ma
    mergeComments cs
    printStringAtMaybeDelta mc ","
    printStringAtDelta ll (rdrName2String n) -- `debug` ("exactP LIE.Var:(l,cs,mc,ll)=" ++ show (ss2pos l,cs,mc,ll))
    return ()

  exactP ma (GHC.IEThingAbs n) = do
    let Just (Ann cs ll (AnnIEThingAbs mc)) = ma `debug` ("blah:" ++ show ma)
    mergeComments cs
    printStringAtMaybeDelta mc ","
    printStringAtDelta ll (rdrName2String n) -- `debug` ("exactP LIE.ThingAbs:(l,cs,mc,ll)=" ++ show (ss2pos l,cs,mc,ll))
    return ()

  exactP ma _ = printString ("no exactP for " ++ show (ma))

-- ---------------------------------------------------------------------

instance ExactP (GHC.ImportDecl GHC.RdrName) where
  exactP ma imp = do
    let Just (Ann cs ll an) = ma
    mergeComments cs
    p <- getPos
    printString "import"
    printStringAtMaybeDeltaP p (id_qualified an) "qualified"
    exactPC (GHC.ideclName imp)
    printStringAtMaybeDeltaP p (id_as an) "as"
    case GHC.ideclAs imp of
      Nothing -> return ()
      Just mn -> printStringAtMaybeDeltaP p (id_as_pos an) (GHC.moduleNameString mn)
    printStringAtMaybeDeltaP p (id_hiding an) "hiding"
    printStringAtMaybeDeltaP p (id_op an) "("
    case GHC.ideclHiding imp of
      Nothing -> return ()
      Just (_,ies) -> mapM_ exactPC ies
    printStringAtMaybeDelta (id_cp an) ")"

-- ---------------------------------------------------------------------

doMaybe :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
doMaybe ma f = case ma of
                 Nothing -> return ()
                 Just a -> f a

instance ExactP (GHC.HsDecl GHC.RdrName) where
  exactP ma decl = case decl of
    GHC.TyClD d -> printString "TyCld"
    GHC.InstD d -> printString "InstD"
    GHC.DerivD d -> printString "DerivD"
    GHC.ValD d -> exactP ma d
    GHC.SigD d -> printString "SigD"
    GHC.DefD d -> printString "DefD"
    GHC.ForD d -> printString "ForD"
    GHC.WarningD d -> printString "WarningD"
    GHC.AnnD d -> printString "AnnD"
    GHC.RuleD d -> printString "RuleD"
    GHC.VectD d -> printString "VectD"
    GHC.SpliceD d -> printString "SpliceD"
    GHC.DocD d -> printString "DocD"
    GHC.QuasiQuoteD d -> printString "QuasiQuoteD"
    GHC.RoleAnnotD d -> printString "RoleAnnotD"

instance ExactP (GHC.HsBind GHC.RdrName) where
  exactP ma (GHC.FunBind n _  (GHC.MG matches _ _ _) _fun_co_fn _fvs _tick) = do
    exactPC n
    mapM_ exactPC matches

  exactP ma (GHC.PatBind pat_lhs pat_rhs pat_rhs_ty bind_fvs pat_ticks) = printString "PatBind"
  exactP ma (GHC.VarBind var_id var_rhs var_inline ) = printString "VarBind"
  exactP ma (GHC.AbsBinds abs_tvs abs_ev_vars abs_exports abs_ev_binds abs_binds) = printString "AbsBinds"
  exactP ma (GHC.PatSynBind patsyn_id bind_fvs patsyn_args patsyn_def patsyn_dir) = printString "PatSynBind"

instance ExactP (GHC.Match GHC.RdrName (GHC.LHsExpr GHC.RdrName)) where
  exactP ma (GHC.Match pats typ (GHC.GRHSs grhs lb)) = do
    mapM_ exactPC pats
    doMaybe typ exactPC
    mapM_ exactPC grhs
    -- exactPC lb

instance ExactP (GHC.Pat GHC.RdrName) where
  exactP _ _ = printString "Pat"

instance ExactP (GHC.HsType GHC.RdrName) where
  exactP _ _ = printString "HsType"

instance ExactP (GHC.GRHS GHC.RdrName (GHC.LHsExpr GHC.RdrName)) where
  exactP _ (GHC.GRHS rhs lb) = do
    mapM_ exactPC rhs
    exactPC lb

instance ExactP (GHC.StmtLR GHC.RdrName GHC.RdrName (GHC.LHsExpr GHC.RdrName)) where
  exactP _ _ = printString "StmtLR"

instance ExactP (GHC.HsExpr GHC.RdrName) where
  exactP ma  (GHC.HsLet lb e)    = do
    let Just (Ann cs dp an) = ma
    p <- getPos
    printStringAtMaybeDelta (hsl_let an) "let"
    exactP Nothing lb
    printStringAtMaybeDeltaP p (hsl_in an) "in"
    exactPC e
  exactP ma (GHC.HsOverLit lit) = exactP ma lit `debug` ("GHC.HsOverLit:" ++ show ma)
  exactP _  (GHC.OpApp e1 op _f e2) = exactPC e1 >> exactPC op >> exactPC e2
  exactP ma  (GHC.HsVar v)          = exactP ma v
  exactP _ _ = printString "HsExpr"

instance ExactP GHC.RdrName where
  exactP _ n = printString (rdrName2String n)

instance ExactP (GHC.HsLocalBinds GHC.RdrName) where
  exactP _ (GHC.HsValBinds (GHC.ValBindsIn binds sigs)) = do
    mapM_ exactPC (GHC.bagToList binds)
    mapM_ exactPC sigs
  exactP _ (GHC.HsValBinds (GHC.ValBindsOut binds sigs)) = printString "ValBindsOut"
  exactP _ (GHC.HsIPBinds binds) = printString "HsIPBinds"
  exactP _ (GHC.EmptyLocalBinds) = return ()

instance ExactP (GHC.Sig GHC.RdrName) where
  exactP _ _ = printString "Sig"

instance ExactP (GHC.HsOverLit GHC.RdrName) where
  exactP (Just (Ann cs p an)) _ = printString (ol_str an)
  exactP Nothing            lit = printString "overlit no ann"

instance ExactP GHC.HsLit where
  exactP ma lit = case lit of
    GHC.HsChar       rw -> printString ('\'':rw:"\'")
{-
    String     _ _ rw -> printString ('\"':rw ++ "\"")
    Int        _ _ rw -> printString (rw)
    Frac       _ _ rw -> printString (rw)
    PrimInt    _ _ rw -> printString (rw ++ "#" )
    PrimWord   _ _ rw -> printString (rw ++ "##")
    PrimFloat  _ _ rw -> printString (rw ++ "#" )
    PrimDouble _ _ rw -> printString (rw ++ "##")
    PrimChar   _ _ rw -> printString ('\'':rw ++ "\'#" )
    PrimString _ _ rw -> printString ('\"':rw ++ "\"#" )
-}

{-
data HsLit
  = HsChar	    Char		-- Character
  | HsCharPrim	    Char		-- Unboxed character
  | HsString	    FastString		-- String
  | HsStringPrim    FastString		-- Packed string
  | HsInt	    Integer		-- Genuinely an Int; arises from TcGenDeriv, 
					--	and from TRANSLATION
  | HsIntPrim       Integer             -- literal Int#
  | HsWordPrim      Integer             -- literal Word#
  | HsInt64Prim     Integer             -- literal Int64#
  | HsWord64Prim    Integer             -- literal Word64#
  | HsInteger	    Integer  Type	-- Genuinely an integer; arises only from TRANSLATION
					-- 	(overloaded literals are done with HsOverLit)
  | HsRat	    FractionalLit Type	-- Genuinely a rational; arises only from TRANSLATION
					-- 	(overloaded literals are done with HsOverLit)
  | HsFloatPrim	    FractionalLit	-- Unboxed Float
  | HsDoublePrim    FractionalLit	-- Unboxed Double
  deriving (Data, Typeable)
-}











-- ---------------------------------------------------------------------

-- Hopefully, this will never fire.
-- If it does, hopefully by that time https://github.com/sol/rewrite-with-location
-- will be implemented.
-- If not, then removing all calls to internalError should give a better
-- idea where the error comes from.
-- So far, it's necessary to eliminate non-exhaustive patterns warnings.
-- We don't want to turn them off, as we want unhandled AST nodes to be
-- reported.
internalError :: String -> a
internalError loc = error $ unlines
    [ "haskell-src-exts: ExactPrint: internal error (non-exhaustive pattern)"
    , "Location: " ++ loc
    , "This is either caused by supplying incorrect location information or by"
    , "a bug in haskell-src-exts. If this happens on an unmodified AST obtained"
    , "by the haskell-src-exts Parser it is a bug, please it report it at"
    , "https://github.com/haskell-suite/haskell-src-exts"]


