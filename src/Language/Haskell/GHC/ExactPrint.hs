{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
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
        ( annotateAST
        , exactPrintAnnotated
        , exactPrintAnnotation

        , exactPrint
        , ExactP

        ) where

import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Utils

import Control.Monad (when, liftM, ap)
import Control.Applicative (Applicative(..))
import Control.Arrow ((***), (&&&))
import Control.Exception
import Data.Data
import Data.List (intersperse,sortBy)
-- import Data.List.Utils
import Data.Maybe

import qualified Bag           as GHC
import qualified BasicTypes    as GHC
import qualified Class         as GHC
import qualified CoAxiom        as GHC
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

------------------------------------------------------
-- The EP monad and basic combinators

pos :: (SrcInfo loc) => loc -> Pos
pos ss = (startLine ss, startColumn ss)

newtype EP x = EP (Pos -> DeltaPos -> [GHC.SrcSpan] -> [Comment] -> Extra -> Anns
            -> (x, Pos,   DeltaPos,   [GHC.SrcSpan],   [Comment],   Extra,   Anns, ShowS))

data Extra = E { eFunId :: (Bool,String) -- (isSymbol,name)
               , eFunIsInfix :: Bool
               , eInhibitTrailing :: Bool
               }
initExtra = E (False,"") False False

instance Functor EP where
  fmap = liftM

instance Applicative EP where
  pure = return
  (<*>) = ap

instance Monad EP where
  return x = EP $ \l dp s cs st an -> (x, l, dp, s, cs, st, an, id)

  EP m >>= k = EP $ \l0 ss0 dp0 c0 st0 an0 -> let
        (a, l1, ss1, dp1, c1, st1, an1, s1) = m l0 ss0 dp0 c0 st0 an0
        EP f = k a
        (b, l2, ss2, dp2, c2, st2, an2, s2) = f l1 ss1 dp1 c1 st1 an1
    in (b, l2, ss2, dp2, c2, st2, an2, s1 . s2)

runEP :: EP () -> GHC.SrcSpan -> [Comment] -> Anns -> String
runEP (EP f) ss cs ans = let (_,_,_,_,_,_,_,s) = f (1,1) (DP (0,0)) [ss] cs initExtra ans in s ""

getPos :: EP Pos
getPos = EP (\l dp s cs st an -> (l,l,dp,s,cs,st,an,id))

setPos :: Pos -> EP ()
setPos l = EP (\_ dp s cs st an -> ((),l,dp,s,cs,st,an,id))


getOffset :: EP DeltaPos
getOffset = EP (\l dp s cs st an -> (dp,l,dp,s,cs,st,an,id))

addOffset :: DeltaPos -> EP ()
addOffset (DP (r,c)) = EP (\l (DP (ro,co)) s cs st an -> ((),l,(DP (r+ro,c+co)),s,cs,st,an,id))

setOffset :: DeltaPos -> EP ()
setOffset dp = EP (\l _ s cs st an -> ((),l,dp,s,cs,st,an,id))

getSrcSpan :: EP GHC.SrcSpan
getSrcSpan = EP (\l dp (s:ss) cs st an -> (s,l,dp,(s:ss),cs,st,an,id))

-- | Replace the current head value
setSrcSpan :: GHC.SrcSpan -> EP ()
setSrcSpan ss = EP (\l dp (s:sss) cs st an -> ((),l,dp,(ss:sss),cs,st,an,id))

pushSrcSpan :: GHC.SrcSpan -> EP ()
pushSrcSpan ss = EP (\l dp sss cs st an -> ((),l,dp,(ss:sss),cs,st,an,id))

popSrcSpan :: EP ()
popSrcSpan = EP (\l dp (_:sss) cs st an -> ((),l,dp,sss,cs,st,an,id))


getAnnotation :: (Typeable a) => GHC.Located a -> EP (Maybe Annotation)
getAnnotation a@(GHC.L ss _) = EP (\l dp s cs st an -> (getAnnotationEP (anEP an) a
                       ,l,dp,s,cs,st,an,id))

-- |destructive get, hence use an annotation once only
getAnnFinal :: GHC.AnnKeywordId -> EP [DeltaPos]
getAnnFinal kw = EP (\l dp (s:ss) cs st (ane,anf) ->
     let
       (r,anf') = case Map.lookup (s,kw) anf of
             Nothing -> ([],anf)
             Just ds -> ([d],f')
               where
                 (d,f') = case reverse ds of
                   [h]   -> (h,Map.delete (s,kw) anf)
                   (h:t) -> (h,Map.insert (s,kw) (reverse t) anf)
     in (r         ,l,dp,(s:ss),cs,st,(ane,anf'),id))

-- |non-destructive get, hence use an annotation once only
peekAnnFinal :: GHC.AnnKeywordId -> EP [DeltaPos]
peekAnnFinal kw = EP (\l dp (s:ss) cs st (ane,anf) ->
     let
       r = case Map.lookup (s,kw) anf of
             Nothing -> []
             Just ds -> ds
     in (r         ,l,dp,(s:ss),cs,st,(ane,anf),id))

getFunId :: EP (Bool,String)
getFunId = EP (\l dp s cs st an -> (eFunId st,l,dp,s,cs,st,an,id))

setFunId :: (Bool,String) -> EP ()
setFunId st = EP (\l dp s cs e an -> ((),l,dp,s,cs,e { eFunId = st},an,id))

getFunIsInfix :: EP Bool
getFunIsInfix = EP (\l dp s cs e an -> (eFunIsInfix e,l,dp,s,cs,e,an,id))

setFunIsInfix :: Bool -> EP ()
setFunIsInfix b = EP (\l dp s cs e an -> ((),l,dp,s,cs,e { eFunIsInfix = b},an,id))

-- ---------------------------------------------------------------------

inhibitTrailing :: EP ()
inhibitTrailing = EP (\l dp s cs e an -> ((),l,dp,s,cs,e { eInhibitTrailing = True},an,id))

resetTrailing :: EP ()
resetTrailing = EP (\l dp s cs e an -> ((),l,dp,s,cs,e { eInhibitTrailing = False},an,id))

trailingInhibited :: EP Bool
trailingInhibited = EP (\l dp s cs e an -> (eInhibitTrailing e,l,dp,s,cs,e,an,id))

-- ---------------------------------------------------------------------

printString :: String -> EP ()
printString str = EP (\(l,c) dp s cs st an -> ((), (l,c+length str), dp, s, cs, st, an, showString str))

getComment :: EP (Maybe Comment)
getComment = EP $ \l dp s cs st an ->
    let x = case cs of
             c:_ -> Just c
             _   -> Nothing
     in (x, l, dp, s, cs, st, an, id)

dropComment :: EP ()
dropComment = EP $ \l dp s cs st an ->
    let cs' = case cs of
               (_:cs) -> cs
               _      -> cs
     in ((), l, dp, s, cs', st, an, id)

mergeComments :: [DComment] -> EP ()
mergeComments dcs = EP $ \l dp s cs st an ->
    let ll = ss2pos $ head s
        acs = map (undeltaComment ll) dcs
        cs' = merge acs cs
    in ((), l, dp, s, cs', st, an, id) `debug` ("mergeComments:(l,acs,dcs)=" ++ show (l,acs,dcs))

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
        (
        when (s < p) $ do
            dropComment
            padUntil s
            printComment multi str
            setPos e
            mPrintComments p
         ) -- `debug` ("mPrintComments:(s,p):" ++ show (s,p))

printComment :: Bool -> String -> EP ()
printComment b str
    | b         = printString str
    | otherwise = printString str

-- Single point of delta application
printWhitespace :: Pos -> EP ()
printWhitespace (r,c) = do
  DP (dr,dc)  <- getOffset
  let p = (r + dr, c + dc) -- `debug` ("printWhiteSpace:offset=" ++ (show (dr,dc)))
  mPrintComments p >> padUntil p

printStringAt :: Pos -> String -> EP ()
printStringAt p str = printWhitespace p >> printString str

printStringAtDelta :: DeltaPos -> String -> EP ()
printStringAtDelta (DP (dl,dc)) str = do
  (l1,c1) <- getPos
  let (l,c) = if dl == 0 then (l1 + dl, c1 + dc)
                         else (l1 + dl, dc)
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

-- ---------------------------------------------------------------------

printStringAtLsDelta :: [DeltaPos] -> String -> EP ()
printStringAtLsDelta mc s =
  case reverse mc of
    (cl:_) -> do
      p <- getPos
      printStringAt (undelta p cl) s
    _ -> return ()

printStringAtMaybeAnn :: GHC.AnnKeywordId -> String -> EP ()
printStringAtMaybeAnn ann str = do
  ma <- getAnnFinal ann
  ss <- getSrcSpan
  printStringAtLsDelta ma str
    `debug` ("printStringAtMaybeAnn:(ss,ann,ma,str)=" ++ show (ss2span ss,ann,ma,str))

printStringAtMaybeAnnAll :: GHC.AnnKeywordId -> String -> EP ()
printStringAtMaybeAnnAll ann str = go
  where
    go = do
      ma <- getAnnFinal ann
      case ma of
        [] -> return ()
        [d]  -> printStringAtLsDelta [d] str >> go

-- ---------------------------------------------------------------------

countAnns :: GHC.AnnKeywordId -> EP Int
countAnns ann = do
  ma <- peekAnnFinal ann
  return (length ma)

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
exactPrint ast@(GHC.L l _) cs toks = runEP (exactPC ast) l cs (Map.empty,Map.empty)


exactPrintAnnotated ::
     GHC.Located (GHC.HsModule GHC.RdrName) -> GHC.ApiAnns -> String
exactPrintAnnotated ast@(GHC.L l _) ghcAnns = runEP (loadInitialComments >> exactPC ast) l [] ann
  where
    ann = annotateLHsModule ast ghcAnns

exactPrintAnnotation :: ExactP ast =>
  GHC.Located ast -> [Comment] -> Anns -> String
exactPrintAnnotation ast@(GHC.L l _) cs ann = runEP (loadInitialComments >> exactPC ast) l cs ann
  -- `debug` ("exactPrintAnnotation:ann=" ++ (concatMap (\(l,a) -> show (ss2span l,a)) $ Map.toList ann ))

annotateAST :: GHC.Located (GHC.HsModule GHC.RdrName) -> GHC.ApiAnns -> Anns
annotateAST ast ghcAnns = annotateLHsModule ast ghcAnns

loadInitialComments :: EP ()
loadInitialComments = do
  -- return () `debug` ("loadInitialComments entered")
  Just (Ann cs _) <- getAnnotation (GHC.L GHC.noSrcSpan ())
  mergeComments cs -- `debug` ("loadInitialComments cs=" ++ show cs)
  -- return () `debug` ("loadInitialComments exited")
  return ()

-- |First move to the given location, then call exactP
exactPC :: (ExactP ast) => GHC.Located ast -> EP ()
exactPC a@(GHC.L l ast) =
    do pushSrcSpan l `debug` ("exactPC entered for:" ++ showGhc l)
       ma <- getAnnotation a
       off@(DP (r,c)) <- case ma of
         Nothing -> return (DP (0,0))
           `debug` ("exactPC:no annotation for " ++ show (ss2span l,typeOf ast))
         Just (Ann lcs dp) -> do
             mergeComments lcs
             return dp
       pe <- getPos
       let p = undelta pe off

       let negOff = DP (-r,-c)
       -- addOffset off `debug` ("addOffset:push:" ++ show (ss2span l,off))
       exactP ast
       -- automatically print any trailing commas or semis
       printStringAtMaybeAnn GHC.AnnComma ","
       printStringAtMaybeAnnAll GHC.AnnSemi ";"

       -- addOffset negOff `debug` ("addOffset:pop:" ++ show (ss2span l,negOff))
       popSrcSpan



printSeq :: [(Pos, EP ())] -> EP ()
printSeq [] = return ()
printSeq ((p,pr):xs) = printWhitespace p >> pr >> printSeq xs

printStrs :: SrcInfo loc => [(loc, String)] -> EP ()
printStrs = printSeq . map (pos *** printString)

printPoints :: SrcSpanInfo -> [String] -> EP ()
printPoints l = printStrs . zip (srcInfoPoints l)

printInterleaved :: (Annotated ast, SrcInfo loc, ExactP ast)
                 => [(loc, String)] -> [ast] -> EP ()
printInterleaved sistrs asts = printSeq $
    interleave (map (pos *** printString ) sistrs)
               (map (pos . ann &&& exactP) asts)

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

-- printMerged :: [a] -> [b] -> EP ()
printMerged :: (ExactP a, ExactP b) => [GHC.Located a] -> [GHC.Located b] -> EP ()
printMerged [] [] = return ()
printMerged [] bs = mapM_ exactPC bs
printMerged as [] = mapM_ exactPC as
printMerged (a@(GHC.L l1 _):as) (b@(GHC.L l2 _):bs) =
  if l1 < l2
    then exactPC a >> printMerged    as (b:bs)
    else exactPC b >> printMerged (a:as)   bs

interleave :: [a] -> [a] -> [a]
interleave [] ys = ys
interleave xs [] = xs
interleave (x:xs) (y:ys) = x:y: interleave xs ys

-- ---------------------------------------------------------------------

prepareListPrint ls = map (\b@(GHC.L l _) -> (l,exactPC b)) ls

applyListPrint ls = mapM_ (\(_,b) -> b) $ sortBy (\(a,_) (b,_) -> compare a b) ls

-- ---------------------------------------------------------------------

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
        (map (pos . ann &&& exactP) asts)

lList (p:ps) = (if isNullSpan p then (p,"") else (p,"{")) : lList' ps
lList _ = internalError "lList"
lList' [] = []
lList' [p] = [if isNullSpan p then (p,"") else (p,"}")]
lList' (p:ps) = (if isNullSpan p then (p,"") else (p,";")) : lList' ps

printSemi :: GHC.SrcSpan -> EP ()
printSemi p = do
  printWhitespace (pos p)
  when (not $ isNullSpan p) $ printString ";"

-- ---------------------------------------------------------------------

getAnn :: (Annotation -> Bool) -> Maybe [Annotation] -> String -> [Annotation]
getAnn isAnn ma str =
  case ma of
    Nothing -> error $ "getAnn expecting an annotation:" ++ str
    Just as -> filter isAnn as


--------------------------------------------------
-- Exact printing for GHC

class (Typeable ast) => ExactP ast where
  -- | Print an AST fragment. The correct position in output is
  -- already established.
  exactP :: ast -> EP ()

instance ExactP (GHC.HsModule GHC.RdrName) where
  exactP (GHC.HsModule mmn mexp imps decls mdepr haddock) = do

    case mmn of
      Just lmn@(GHC.L l mn) -> do
        printStringAtMaybeAnn GHC.AnnModule "module" -- `debug` ("exactP.HsModule:cs=" ++ show cs)
        printStringAtMaybeAnn GHC.AnnVal (GHC.moduleNameString mn)
      Nothing -> return ()

    case mdepr of
      Nothing -> return ()
      Just depr -> exactPC depr

    case mexp of
      Just lexps -> do
        return () `debug` ("about to exactPC lexps")
        exactPC lexps
        return ()
      Nothing -> return ()

    printStringAtMaybeAnn GHC.AnnWhere "where"
    printStringAtMaybeAnn GHC.AnnOpen  "{"
    printStringAtMaybeAnnAll GHC.AnnSemi ";" -- possible leading semis
    exactP imps

    mapM_ exactPC decls

    printStringAtMaybeAnn GHC.AnnClose "}"

    -- put the end of file whitespace in
    printStringAtMaybeAnn GHC.AnnEofPos ""

-- ---------------------------------------------------------------------

instance ExactP GHC.WarningTxt where
  exactP (GHC.WarningTxt (GHC.L _ ls) lss) = do
    printStringAtMaybeAnn GHC.AnnOpen ls
    printStringAtMaybeAnn GHC.AnnOpen "["
    mapM_ exactPC lss
    printStringAtMaybeAnn GHC.AnnClose "]"
    printStringAtMaybeAnn GHC.AnnClose "#-}"

  exactP (GHC.DeprecatedTxt (GHC.L _ ls) lss) = do
    printStringAtMaybeAnn GHC.AnnOpen ls
    printStringAtMaybeAnn GHC.AnnOpen "["
    mapM_ exactPC lss
    printStringAtMaybeAnn GHC.AnnClose "]"
    printStringAtMaybeAnn GHC.AnnClose "#-}"

-- ---------------------------------------------------------------------

instance ExactP (GHC.ModuleName) where
  exactP mn = do
    printString (GHC.moduleNameString mn)

-- ---------------------------------------------------------------------

instance ExactP [GHC.LIE GHC.RdrName] where
  exactP ies = do
    printStringAtMaybeAnn GHC.AnnHiding "hiding"
    printStringAtMaybeAnn GHC.AnnOpen "("
    mapM_ exactPC ies
    printStringAtMaybeAnn GHC.AnnClose ")"

-- ---------------------------------------------------------------------

instance ExactP (GHC.IE GHC.RdrName) where
  exactP (GHC.IEVar ln) = do
    printStringAtMaybeAnn GHC.AnnPattern "pattern"
    printStringAtMaybeAnn GHC.AnnType    "type"
    exactPC ln

  exactP (GHC.IEThingAbs n) = do
    printStringAtMaybeAnn GHC.AnnType    "type"
    exactPC n

  exactP (GHC.IEThingWith n ns) = do
    exactPC n
    printStringAtMaybeAnn GHC.AnnOpen    "("
    mapM_ exactPC ns
    printStringAtMaybeAnn GHC.AnnClose   ")"

  exactP (GHC.IEThingAll n) = do
    exactPC n
    printStringAtMaybeAnn GHC.AnnOpen    "("
    printStringAtMaybeAnn GHC.AnnDotdot  ".."
    printStringAtMaybeAnn GHC.AnnClose   ")"

  exactP (GHC.IEModuleContents (GHC.L _ mn)) = do
    printStringAtMaybeAnn GHC.AnnModule  "module"
    printStringAtMaybeAnn GHC.AnnVal     (GHC.moduleNameString mn)

  exactP x = printString ("no exactP.IE for " ++ showGhc (x))

-- ---------------------------------------------------------------------

instance ExactP [GHC.LImportDecl GHC.RdrName] where
  exactP imps = mapM_ exactPC imps

-- ---------------------------------------------------------------------

instance ExactP (GHC.ImportDecl GHC.RdrName) where
  exactP imp = do
    printStringAtMaybeAnn GHC.AnnImport "import"

    printStringAtMaybeAnn GHC.AnnOpen  "{-# SOURCE"
    printStringAtMaybeAnn GHC.AnnClose  "#-}"

    printStringAtMaybeAnn GHC.AnnSafe      "safe"
    printStringAtMaybeAnn GHC.AnnQualified "qualified"
    printStringAtMaybeAnn GHC.AnnVal (GHC.moduleNameString $ GHC.unLoc $ GHC.ideclName imp)

    case GHC.ideclAs imp of
      Nothing -> return ()
      Just mn -> do
        printStringAtMaybeAnn GHC.AnnAs "as"
        printStringAtMaybeAnn GHC.AnnVal (GHC.moduleNameString mn)

    case GHC.ideclHiding imp of
      Nothing -> return ()
      Just (_,lie) -> do
        -- printStringAtMaybeAnn GHC.AnnHiding "hiding"
        exactPC lie

-- ---------------------------------------------------------------------

doMaybe :: (Monad m) => (a -> m ()) -> Maybe a -> m ()
doMaybe f ma = case ma of
                 Nothing -> return ()
                 Just a -> f a

instance ExactP (GHC.HsDecl GHC.RdrName) where
  exactP decl = case decl of
    GHC.TyClD d       -> exactP d
    GHC.InstD d       -> exactP d
    GHC.DerivD d      -> exactP d
    GHC.ValD d        -> exactP d
    GHC.SigD d        -> exactP d
    GHC.DefD d        -> exactP d
    GHC.ForD d        -> exactP d
    GHC.WarningD d    -> exactP d
    GHC.AnnD d        -> exactP d
    GHC.RuleD d       -> exactP d
    GHC.VectD d       -> exactP d
    GHC.SpliceD d     -> exactP d
    GHC.DocD d        -> exactP d
    GHC.QuasiQuoteD d -> exactP d
    GHC.RoleAnnotD d  -> exactP d

-- ---------------------------------------------------------------------

instance ExactP (GHC.RoleAnnotDecl GHC.RdrName) where
  exactP (GHC.RoleAnnotDecl ln mr) = do
    printStringAtMaybeAnn GHC.AnnType "type"
    printStringAtMaybeAnn GHC.AnnRole "role"
    exactPC ln
    mapM_ exactPC mr

instance ExactP (Maybe GHC.Role) where
  exactP Nothing  = printStringAtMaybeAnn GHC.AnnVal "_"
  exactP (Just r) = printStringAtMaybeAnn GHC.AnnVal (GHC.unpackFS $ GHC.fsFromRole r)

-- ---------------------------------------------------------------------

instance ExactP (GHC.HsQuasiQuote GHC.RdrName) where
  exactP = assert False undefined

-- ---------------------------------------------------------------------

instance ExactP (GHC.SpliceDecl GHC.RdrName) where
  exactP (GHC.SpliceDecl (GHC.L ls (GHC.HsSplice n e)) flag) = do
    case flag of
      GHC.ExplicitSplice ->
        printStringAtMaybeAnn GHC.AnnOpen "$("
      GHC.ImplicitSplice ->
        printStringAtMaybeAnn GHC.AnnOpen "$$("
    exactPC e
    printStringAtMaybeAnn GHC.AnnClose ")"

-- ---------------------------------------------------------------------

instance ExactP (GHC.VectDecl GHC.RdrName) where
  exactP (GHC.HsVect src ln e) = do
    printStringAtMaybeAnn GHC.AnnOpen src -- "{-# VECTORISE"
    exactPC ln
    printStringAtMaybeAnn GHC.AnnEqual "="
    exactPC e
    printStringAtMaybeAnn GHC.AnnClose "#-}"

  exactP (GHC.HsNoVect src ln) = do
    printStringAtMaybeAnn GHC.AnnOpen src -- "{-# NOVECTORISE"
    exactPC ln
    printStringAtMaybeAnn GHC.AnnClose "#-}"

  exactP (GHC.HsVectTypeIn src b ln mln) = do
    printStringAtMaybeAnn GHC.AnnOpen src -- "{-# VECTORISE" or "{-# VECTORISE SCALAR"
    printStringAtMaybeAnn GHC.AnnType "type"
    exactPC ln
    printStringAtMaybeAnn GHC.AnnEqual "="
    case mln of
      Nothing -> return ()
      Just n -> exactPC n
    printStringAtMaybeAnn GHC.AnnClose "#-}"

  exactP (GHC.HsVectTypeOut {}) = error $ "exactP.HsVectTypeOut: only valid after type checker"

  exactP (GHC.HsVectClassIn src ln) = do
    printStringAtMaybeAnn GHC.AnnOpen src -- "{-# VECTORISE"
    printStringAtMaybeAnn GHC.AnnClass "class"
    exactPC ln
    printStringAtMaybeAnn GHC.AnnClose "#-}"

  exactP (GHC.HsVectClassOut {}) = error $ "exactP.HsVectClassOut: only valid after type checker"
  exactP (GHC.HsVectInstIn {})   = error $ "exactP.HsVectInstIn: not supported?"
  exactP (GHC.HsVectInstOut {})  = error $ "exactP.HsVectInstOut: not supported?"


-- ---------------------------------------------------------------------

instance ExactP (GHC.RuleDecls GHC.RdrName) where
  exactP (GHC.HsRules src rules) = do
    printStringAtMaybeAnn GHC.AnnOpen src
    mapM_ exactPC rules
    printStringAtMaybeAnn GHC.AnnClose "#-}"

-- ---------------------------------------------------------------------

instance ExactP (GHC.AnnDecl GHC.RdrName) where
  exactP (GHC.HsAnnotation src prov e) = do
    printStringAtMaybeAnn GHC.AnnOpen src
    printStringAtMaybeAnn GHC.AnnType "type"
    printStringAtMaybeAnn GHC.AnnModule "module"
    case prov of
      (GHC.ValueAnnProvenance n) -> exactPC n
      (GHC.TypeAnnProvenance n) -> exactPC n
      (GHC.ModuleAnnProvenance) -> return ()
    exactPC e
    printStringAtMaybeAnn GHC.AnnClose "#-}"

-- ---------------------------------------------------------------------

instance ExactP (GHC.RuleDecl GHC.RdrName) where
  exactP (GHC.HsRule ln act bndrs lhs _ rhs _) = do
    exactPC ln
    -- activation
    printStringAtMaybeAnn GHC.AnnOpen "["
    printStringAtMaybeAnn GHC.AnnTilde "~"
    case act of
      GHC.ActiveBefore n -> printStringAtMaybeAnn GHC.AnnVal (show n)
      GHC.ActiveAfter n  -> printStringAtMaybeAnn GHC.AnnVal (show n)
      _                  -> return ()
    printStringAtMaybeAnn GHC.AnnClose "]"

    printStringAtMaybeAnn GHC.AnnForall "forall"
    mapM_ exactPC bndrs
    printStringAtMaybeAnn GHC.AnnDot "."

    exactPC lhs
    printStringAtMaybeAnn GHC.AnnEqual "="
    exactPC rhs

-- ---------------------------------------------------------------------

instance ExactP (GHC.RuleBndr GHC.RdrName) where
  exactP (GHC.RuleBndr ln) = exactPC ln
  exactP (GHC.RuleBndrSig ln (GHC.HsWB thing _ _ _)) = do
    printStringAtMaybeAnn GHC.AnnOpen "("
    exactPC ln
    printStringAtMaybeAnn GHC.AnnDcolon "::"
    exactPC thing
    printStringAtMaybeAnn GHC.AnnClose ")"

-- ---------------------------------------------------------------------

instance ExactP (GHC.WarnDecls GHC.RdrName) where
  exactP (GHC.Warnings src warns) = do
    printStringAtMaybeAnn GHC.AnnOpen src
    mapM_ exactPC warns
    printStringAtMaybeAnn GHC.AnnClose "#-}"

-- ---------------------------------------------------------------------

instance ExactP (GHC.WarnDecl GHC.RdrName) where
  exactP (GHC.Warning lns txt) = do
     mapM_ exactPC lns
     printStringAtMaybeAnn GHC.AnnOpen "["
     case txt of
       GHC.WarningTxt    src ls -> mapM_ exactPC ls
       GHC.DeprecatedTxt src ls -> mapM_ exactPC ls
     printStringAtMaybeAnn GHC.AnnClose "]"


instance ExactP GHC.FastString where
  exactP fs = printStringAtMaybeAnn GHC.AnnVal ("\"" ++ GHC.unpackFS fs ++ "\"")

-- ---------------------------------------------------------------------

instance ExactP (GHC.ForeignDecl GHC.RdrName) where
  exactP (GHC.ForeignImport ln typ _
               (GHC.CImport cconv safety@(GHC.L ll _) mh imp (GHC.L ls src))) = do
    printStringAtMaybeAnn GHC.AnnForeign "foreign"
    printStringAtMaybeAnn GHC.AnnImport "import"

    exactPC cconv

    if ll == GHC.noSrcSpan
      then return ()
      else exactPC safety

    printStringAtMaybeAnn GHC.AnnVal ("\"" ++ src ++ "\"")
    exactPC ln
    printStringAtMaybeAnn GHC.AnnDcolon "::"
    exactPC typ

  exactP (GHC.ForeignExport ln typ _ (GHC.CExport spec (GHC.L ls src))) = do
    printStringAtMaybeAnn GHC.AnnForeign "foreign"
    printStringAtMaybeAnn GHC.AnnExport "export"
    exactPC spec
    printStringAtMaybeAnn GHC.AnnVal ("\"" ++ src ++ "\"")
    exactPC ln
    printStringAtMaybeAnn GHC.AnnDcolon "::"
    exactPC typ

-- ---------------------------------------------------------------------

instance (ExactP GHC.CExportSpec) where
  exactP (GHC.CExportStatic _ cconv) = exactP cconv

-- ---------------------------------------------------------------------

instance ExactP GHC.CCallConv where
  exactP GHC.StdCallConv        = printStringAtMaybeAnn GHC.AnnVal "stdcall"
  exactP GHC.CCallConv          = printStringAtMaybeAnn GHC.AnnVal "ccall"
  exactP GHC.CApiConv           = printStringAtMaybeAnn GHC.AnnVal "capi"
  exactP GHC.PrimCallConv       = printStringAtMaybeAnn GHC.AnnVal "prim"
  exactP GHC.JavaScriptCallConv = printStringAtMaybeAnn GHC.AnnVal "javascript"

-- ---------------------------------------------------------------------

instance ExactP GHC.Safety where
  exactP GHC.PlayRisky         = printStringAtMaybeAnn GHC.AnnVal "unsafe"
  exactP GHC.PlaySafe          = printStringAtMaybeAnn GHC.AnnVal "safe"
  exactP GHC.PlayInterruptible = printStringAtMaybeAnn GHC.AnnVal "interruptible"


-- ---------------------------------------------------------------------

instance ExactP (GHC.DerivDecl GHC.RdrName) where
  exactP (GHC.DerivDecl typ mov) = do
    printStringAtMaybeAnn GHC.AnnDeriving "deriving"
    printStringAtMaybeAnn GHC.AnnInstance "instance"
    case mov of
      Nothing -> return ()
      Just ov -> exactPC ov
    exactPC typ

-- ---------------------------------------------------------------------

instance ExactP (GHC.DefaultDecl GHC.RdrName) where
  exactP (GHC.DefaultDecl typs) = do
    printStringAtMaybeAnn GHC.AnnDefault "default"
    printStringAtMaybeAnn GHC.AnnOpen "("
    mapM_ exactPC typs
    printStringAtMaybeAnn GHC.AnnClose ")"

-- ---------------------------------------------------------------------

instance ExactP (GHC.InstDecl GHC.RdrName) where
  exactP (GHC.ClsInstD      cid) = exactP  cid
  exactP (GHC.DataFamInstD dfid) = exactP dfid
  exactP (GHC.TyFamInstD   tfid) = exactP tfid

-- ---------------------------------------------------------------------

instance ExactP GHC.OverlapMode where
  exactP (GHC.NoOverlap src) = do
    printStringAtMaybeAnn GHC.AnnOpen src
    printStringAtMaybeAnn GHC.AnnClose "#-}"

  exactP (GHC.Overlappable src) = do
    printStringAtMaybeAnn GHC.AnnOpen src
    printStringAtMaybeAnn GHC.AnnClose "#-}"

  exactP (GHC.Overlapping src) = do
    printStringAtMaybeAnn GHC.AnnOpen src
    printStringAtMaybeAnn GHC.AnnClose "#-}"

  exactP (GHC.Overlaps src) = do
    printStringAtMaybeAnn GHC.AnnOpen src
    printStringAtMaybeAnn GHC.AnnClose "#-}"

  exactP (GHC.Incoherent src) = do
    printStringAtMaybeAnn GHC.AnnOpen src
    printStringAtMaybeAnn GHC.AnnClose "#-}"


-- ---------------------------------------------------------------------

instance ExactP (GHC.ClsInstDecl GHC.RdrName) where
  exactP (GHC.ClsInstDecl poly binds sigs tyfams datafams mov) = do
    printStringAtMaybeAnn GHC.AnnInstance "instance"
    case mov of
      Nothing -> return ()
      Just ov -> exactPC ov
    exactPC poly
    printStringAtMaybeAnn GHC.AnnWhere "where"
    printStringAtMaybeAnn GHC.AnnOpen "{"
    printStringAtMaybeAnnAll GHC.AnnSemi ";"

    applyListPrint (prepareListPrint (GHC.bagToList binds)
                 ++ prepareListPrint sigs
                 ++ prepareListPrint tyfams
                 ++ prepareListPrint datafams
                    )
    printStringAtMaybeAnn GHC.AnnClose "}"

-- ---------------------------------------------------------------------

instance ExactP (GHC.TyFamInstDecl GHC.RdrName) where
   exactP (GHC.TyFamInstDecl eqn _) = do
     printStringAtMaybeAnn GHC.AnnType     "type"
     printStringAtMaybeAnn GHC.AnnInstance "instance"
     exactPC eqn

-- ---------------------------------------------------------------------

instance ExactP (GHC.DataFamInstDecl GHC.RdrName) where
   exactP (GHC.DataFamInstDecl ln (GHC.HsWB pats _ _ _) defn _) = do
    printStringAtMaybeAnn GHC.AnnData     "data"
    printStringAtMaybeAnn GHC.AnnNewtype  "newtype"
    printStringAtMaybeAnn GHC.AnnInstance "instance"
    exactPC ln
    mapM_ exactPC pats
    printStringAtMaybeAnn GHC.AnnEqual "="
    exactP defn
-- ---------------------------------------------------------------------

instance ExactP (GHC.HsBind GHC.RdrName) where
  exactP (GHC.FunBind (GHC.L _ n) isInfix  (GHC.MG matches _ _ _) _ _ _) = do
    setFunId (isSymbolRdrName n,rdrName2String n)
    setFunIsInfix isInfix
    mapM_ exactPC matches

  exactP (GHC.PatBind lhs (GHC.GRHSs grhs lb) _ty _fvs _ticks) = do
    exactPC lhs
    printStringAtMaybeAnn GHC.AnnEqual "="
    mapM_ exactPC grhs
    printStringAtMaybeAnn GHC.AnnWhere "where"
    exactP lb

  exactP (GHC.VarBind var_id var_rhs var_inline ) = printString "VarBind"
  exactP (GHC.AbsBinds abs_tvs abs_ev_vars abs_exports abs_ev_binds abs_binds) = printString "AbsBinds"

  exactP (GHC.PatSynBind (GHC.PSB n fvs args def dir)) = do
    printStringAtMaybeAnn GHC.AnnPattern "pattern"
    exactPC n
    case args of
      GHC.InfixPatSyn na nb -> do
        exactPC na
        exactPC nb
      GHC.PrefixPatSyn ns -> do
        mapM_ exactPC ns

    printStringAtMaybeAnn GHC.AnnEqual   "="
    printStringAtMaybeAnn GHC.AnnLarrow  "<-"

    exactPC def
    case dir of
      GHC.Unidirectional           -> return ()
      GHC.ImplicitBidirectional    -> return ()
      GHC.ExplicitBidirectional mg -> exactPMatchGroup mg

    printStringAtMaybeAnn GHC.AnnWhere   "where"
    printStringAtMaybeAnn GHC.AnnOpen    "{"
    printStringAtMaybeAnn GHC.AnnClose   "}"

-- ---------------------------------------------------------------------

instance ExactP (GHC.IPBind GHC.RdrName) where
  exactP (GHC.IPBind en e) = do
    case en of
      Left n -> exactPC n
      Right i -> error $ "annotateP.IPBind:should not happen"
    printStringAtMaybeAnn GHC.AnnEqual "="
    exactPC e

-- ---------------------------------------------------------------------

instance (ExactP body) => ExactP (GHC.Match GHC.RdrName (GHC.Located body)) where
  exactP (GHC.Match mln pats typ (GHC.GRHSs grhs lb)) = do
    (isSym,funid) <- getFunId
    isInfix <- getFunIsInfix
    let
      get_infix Nothing = isInfix
      get_infix (Just (_,f)) = f
    case (get_infix mln,pats) of
      (True,[a,b]) -> do
        exactPC a
        case mln of
          Nothing -> do
            if isSym
              then printStringAtMaybeAnn GHC.AnnFunId funid
              else printStringAtMaybeAnn GHC.AnnFunId ("`"++ funid ++ "`")
          Just (n,_) -> exactPC n
        exactPC b
      _ -> do
        case mln of
          Nothing -> printStringAtMaybeAnn GHC.AnnFunId funid
          Just (n,_)  -> exactPC n
        mapM_ exactPC pats
    printStringAtMaybeAnn GHC.AnnEqual  "="
    printStringAtMaybeAnn GHC.AnnRarrow "->" -- for HsLam
    mapM_ exactPC typ
    mapM_ exactPC grhs
    printStringAtMaybeAnn GHC.AnnWhere "where"
    printStringAtMaybeAnn GHC.AnnOpen "{"
    printStringAtMaybeAnnAll GHC.AnnSemi ";"
    exactP lb
    printStringAtMaybeAnn GHC.AnnClose "}"

-- ---------------------------------------------------------------------

instance ExactP (GHC.Pat GHC.RdrName) where

  exactP (GHC.WildPat _) = printStringAtMaybeAnn GHC.AnnVal "_"

  exactP (GHC.VarPat n) = printStringAtMaybeAnn GHC.AnnVal (rdrName2String n)

  exactP (GHC.LazyPat p)    = do
    printStringAtMaybeAnn GHC.AnnTilde "~"
    exactPC p

  exactP (GHC.AsPat n p) = do
    exactPC n
    printStringAtMaybeAnn GHC.AnnAt "@"
    exactPC p

  exactP (GHC.ParPat p) = do
    return () `debug` ("in exactP.ParPat")
    printStringAtMaybeAnn GHC.AnnOpen "("
    exactPC p
    printStringAtMaybeAnn GHC.AnnClose ")"

  exactP (GHC.BangPat p) = do
    printStringAtMaybeAnn GHC.AnnBang "!"
    exactPC p

  exactP (GHC.ListPat ps _ _) = do
    printStringAtMaybeAnn GHC.AnnOpen "["
    mapM_ exactPC ps
    printStringAtMaybeAnn GHC.AnnClose "]"

  exactP (GHC.TuplePat pats b _) = do
    if b == GHC.Boxed then printStringAtMaybeAnn GHC.AnnOpen "("
                      else printStringAtMaybeAnn GHC.AnnOpen "(#"
    mapM_ exactPC pats
    if b == GHC.Boxed then printStringAtMaybeAnn GHC.AnnClose ")"
                      else printStringAtMaybeAnn GHC.AnnClose "#)"

  exactP (GHC.PArrPat ps _) = do
    printStringAtMaybeAnn GHC.AnnOpen "[:"
    mapM_ exactPC ps
    printStringAtMaybeAnn GHC.AnnClose ":]"

  exactP (GHC.ConPatIn n dets) = do
    case dets of
      GHC.PrefixCon args -> do
        exactPC n
        mapM_ exactPC args
      GHC.RecCon (GHC.HsRecFields fs _) -> do
        exactPC n
        printStringAtMaybeAnn GHC.AnnOpen "{"
        mapM_ exactPC fs
        printStringAtMaybeAnn GHC.AnnDotdot ".."
        printStringAtMaybeAnn GHC.AnnClose "}"
      GHC.InfixCon a1 a2 -> do
        exactPC a1
        exactPC n
        exactPC a2

  exactP (GHC.ConPatOut {}) = return ()

  exactP (GHC.ViewPat e pat _) = do
    exactPC e
    printStringAtMaybeAnn GHC.AnnRarrow "->"
    exactPC pat

  exactP (GHC.SplicePat (GHC.HsSplice _ e)) = do
    printStringAtMaybeAnn GHC.AnnOpen "$("
    exactPC e
    printStringAtMaybeAnn GHC.AnnClose ")"

  exactP (GHC.QuasiQuotePat (GHC.HsQuasiQuote n _ q)) = do
    printStringAtMaybeAnn GHC.AnnVal
                    ("[" ++ (rdrName2String n) ++ "|" ++ (GHC.unpackFS q) ++ "|]")

  exactP (GHC.LitPat lp) = do
    printStringAtMaybeAnn GHC.AnnVal (hsLit2String lp)

  exactP (GHC.NPat ol _ _)  = exactP ol

  exactP (GHC.NPlusKPat ln ol _ _) = do
    exactPC ln
    printStringAtMaybeAnn GHC.AnnVal "+"
    exactPC ol

  exactP (GHC.SigPatIn pat (GHC.HsWB ty _ _ _)) = do
    exactPC pat
    printStringAtMaybeAnn GHC.AnnDcolon "::"
    exactPC ty

  exactP (GHC.SigPatOut {}) = return ()
  exactP (GHC.CoPat {}) = return ()

-- ---------------------------------------------------------------------

instance ExactP (GHC.HsType GHC.Name) where
  exactP typ = do
    return () `debug` ("exactP.HsType not implemented for " ++ showGhc (typ))
    printString "HsType.Name"
    assert False undefined

-- ---------------------------------------------------------------------

hstylit2str :: GHC.HsTyLit -> String
hstylit2str (GHC.HsNumTy src _) = src
hstylit2str (GHC.HsStrTy src _) = src

-- ---------------------------------------------------------------------

instance ExactP (GHC.HsType GHC.RdrName) where
  exactP (GHC.HsForAllTy f mwc bndrs ctx typ) = do
    printStringAtMaybeAnn GHC.AnnForall "forall"
    exactPC ctx
    case mwc of
      Nothing -> return ()
      Just _  -> printStringAtMaybeAnn GHC.AnnVal "_"
    printStringAtMaybeAnn GHC.AnnDot    "."
    printStringAtMaybeAnn GHC.AnnDarrow "=>"
    exactPC typ

  exactP (GHC.HsTyVar n) = do
    printStringAtMaybeAnn GHC.AnnDcolon "::" -- for HsKind, aliased to HsType
    printStringAtMaybeAnn GHC.AnnVal (rdrName2String n)

  exactP (GHC.HsAppTy t1 t2) = do
    printStringAtMaybeAnn GHC.AnnDcolon "::" -- for HsKind, aliased to HsType
    exactPC t1
    exactPC t2

  exactP (GHC.HsFunTy t1 t2) = do
    printStringAtMaybeAnn GHC.AnnDcolon "::" -- for HsKind, aliased to HsType
    exactPC t1
    printStringAtMaybeAnn GHC.AnnRarrow "->"
    exactPC t2

  exactP (GHC.HsListTy t) = do
    printStringAtMaybeAnn GHC.AnnDcolon "::" -- for HsKind, aliased to HsType
    printStringAtMaybeAnn GHC.AnnOpen  "["
    exactPC t
    printStringAtMaybeAnn GHC.AnnClose "]"

  exactP (GHC.HsPArrTy t) = do
    printStringAtMaybeAnn GHC.AnnOpen  "[:"
    exactPC t
    printStringAtMaybeAnn GHC.AnnClose ":]"

  exactP (GHC.HsTupleTy sort ts) = do
    printStringAtMaybeAnn GHC.AnnDcolon "::" -- for HsKind, aliased to HsType
    let (ostr,cstr) = case sort of
          GHC.HsUnboxedTuple -> ("(#","#)")
          _ -> ("(",")")
    printStringAtMaybeAnn GHC.AnnOpen  ostr
    mapM_ exactPC ts
    printStringAtMaybeAnn GHC.AnnClose cstr

  exactP (GHC.HsOpTy t1 (_,op) t2) = do
    exactPC t1
    exactPC op
    exactPC t2

  exactP (GHC.HsParTy t1) = do
    printStringAtMaybeAnn GHC.AnnDcolon "::" -- for HsKind, aliased to HsType
    printStringAtMaybeAnn GHC.AnnOpen  "("
    exactPC t1
    printStringAtMaybeAnn GHC.AnnClose ")"

  exactP (GHC.HsIParamTy (GHC.HsIPName n) t) = do
    printStringAtMaybeAnn GHC.AnnVal ("?" ++ (GHC.unpackFS n))
    printStringAtMaybeAnn GHC.AnnDcolon "::"
    exactPC t

  exactP (GHC.HsEqTy t1 t2) = do
    exactPC t1
    printStringAtMaybeAnn GHC.AnnTilde "~"
    exactPC t2

  exactP (GHC.HsKindSig t k) = do
    printStringAtMaybeAnn GHC.AnnOpen  "("
    exactPC t
    printStringAtMaybeAnn GHC.AnnDcolon "::"
    exactPC k
    printStringAtMaybeAnn GHC.AnnClose ")"

  exactP (GHC.HsQuasiQuoteTy (GHC.HsQuasiQuote n _ss q)) = do
    printStringAtMaybeAnn GHC.AnnVal
                    ("[" ++ (rdrName2String n) ++ "|" ++ (GHC.unpackFS q) ++ "|]")

  exactP (GHC.HsSpliceTy (GHC.HsSplice _is e) _) = do
    printStringAtMaybeAnn GHC.AnnOpen  "$("
    exactPC e
    printStringAtMaybeAnn GHC.AnnClose ")"

  exactP (GHC.HsDocTy t d) = do
    exactPC t
    exactPC d

  exactP (GHC.HsBangTy b t) = do
    case b of
      (GHC.HsUserBang ms (Just True) _) -> do
        printStringAtMaybeAnn GHC.AnnOpen  (maybe "{-# UNPACK" id ms)
        printStringAtMaybeAnn GHC.AnnClose "#-}"
        printStringAtMaybeAnn GHC.AnnBang  "!"
      (GHC.HsUserBang ms (Just False) _) -> do
        printStringAtMaybeAnn GHC.AnnOpen  (maybe "{-# NOUNPACK" id ms)
        printStringAtMaybeAnn GHC.AnnClose "#-}"
        printStringAtMaybeAnn GHC.AnnBang  "!"
      _ -> do
        printStringAtMaybeAnn GHC.AnnBang  "!"
    exactPC t

  exactP (GHC.HsRecTy cons) = do
    printStringAtMaybeAnn GHC.AnnOpen  "{"
    mapM_ exactPC cons
    printStringAtMaybeAnn GHC.AnnClose "}"

  exactP (GHC.HsCoreTy _t) = return ()

  exactP (GHC.HsExplicitListTy _ ts) = do
    printStringAtMaybeAnn GHC.AnnDcolon "::" -- for HsKind, aliased to HsType
    printStringAtMaybeAnn GHC.AnnOpen  "'["
    mapM_ exactPC ts
    printStringAtMaybeAnn GHC.AnnClose "]"

  exactP (GHC.HsExplicitTupleTy _ ts) = do
    printStringAtMaybeAnn GHC.AnnDcolon "::" -- for HsKind, aliased to HsType
    printStringAtMaybeAnn GHC.AnnOpen  "'("
    mapM_ exactPC ts
    printStringAtMaybeAnn GHC.AnnClose ")"

  exactP (GHC.HsTyLit lit) = do
    printStringAtMaybeAnn GHC.AnnDcolon "::" -- for HsKind, aliased to HsType
    case lit of
      (GHC.HsNumTy s _) -> printStringAtMaybeAnn GHC.AnnVal s
      (GHC.HsStrTy s _) -> printStringAtMaybeAnn GHC.AnnVal s

  exactP (GHC.HsWrapTy _ _) = return ()

  exactP GHC.HsWildcardTy = do
    printStringAtMaybeAnn GHC.AnnVal "_"

  exactP (GHC.HsNamedWildcardTy n) = do
    printStringAtMaybeAnn GHC.AnnVal (rdrName2String n)

-- ---------------------------------------------------------------------

instance ExactP GHC.HsDocString where
  exactP (GHC.HsDocString s) = do
    printStringAtMaybeAnn GHC.AnnVal (GHC.unpackFS s)

instance ExactP (GHC.ConDeclField GHC.RdrName) where
  exactP (GHC.ConDeclField ns ty mdoc) = do
    mapM_ exactPC ns
    printStringAtMaybeAnn GHC.AnnDcolon "::"
    exactPC ty
    case mdoc of
      Just doc -> exactPC doc
      Nothing -> return ()

instance ExactP (GHC.HsContext GHC.RdrName) where
  exactP typs = do
    -- printStringAtMaybeAnn GHC.AnnUnit "()"

    printStringAtMaybeAnn GHC.AnnDeriving "deriving"
    printStringAtMaybeAnn GHC.AnnOpen "("
    mapM_ exactPC typs
    printStringAtMaybeAnn GHC.AnnClose ")"
    printStringAtMaybeAnn GHC.AnnDarrow "=>"

instance (ExactP body) => ExactP (GHC.GRHS GHC.RdrName (GHC.Located body)) where
  exactP (GHC.GRHS guards expr) = do
    printStringAtMaybeAnn GHC.AnnVbar "|"
    mapM_ exactPC guards
    printStringAtMaybeAnn GHC.AnnEqual "="
    exactPC expr

instance (ExactP body)
  => ExactP (GHC.Stmt GHC.RdrName (GHC.Located body)) where

  exactP (GHC.LastStmt body _) = exactPC body
    `debug` ("exactP.LastStmt")

  exactP (GHC.BindStmt pat body _ _) = do
    exactPC pat
    printStringAtMaybeAnn GHC.AnnLarrow "<-"
    exactPC body
    printStringAtMaybeAnn GHC.AnnVbar  "|" -- possible in list comprehension

  exactP (GHC.BodyStmt e _ _ _) = do
    exactPC e

  exactP (GHC.LetStmt lb) = do
    printStringAtMaybeAnn GHC.AnnLet "let"
    printStringAtMaybeAnn GHC.AnnOpen "{"
    exactP lb
    printStringAtMaybeAnn GHC.AnnClose "}"

  exactP (GHC.ParStmt pbs _ _) = do
    mapM_ exactPParStmtBlock pbs

  exactP (GHC.TransStmt form stmts _b using by _ _ _) = do
    mapM_ exactPC stmts
    case form of
      GHC.ThenForm -> do
        printStringAtMaybeAnn GHC.AnnThen "then"
        exactPC using
        printStringAtMaybeAnn GHC.AnnBy "by"
        case by of
          Just b -> exactPC b
          Nothing -> return ()
      GHC.GroupForm -> do
        printStringAtMaybeAnn GHC.AnnThen "then"
        printStringAtMaybeAnn GHC.AnnGroup "group"
        printStringAtMaybeAnn GHC.AnnBy "by"
        case by of
          Just b -> exactPC b
          Nothing -> return ()
        printStringAtMaybeAnn GHC.AnnUsing "using"
        exactPC using

  exactP (GHC.RecStmt stmts _ _ _ _ _ _ _ _) = do
    printStringAtMaybeAnn GHC.AnnRec "rec"
    printStringAtMaybeAnn GHC.AnnOpen "{"
    printStringAtMaybeAnnAll GHC.AnnSemi ";"
    mapM_ exactPC stmts
    printStringAtMaybeAnn GHC.AnnClose "}"

-- ---------------------------------------------------------------------

exactPParStmtBlock :: GHC.ParStmtBlock GHC.RdrName GHC.RdrName -> EP ()
exactPParStmtBlock (GHC.ParStmtBlock stmts ns _) = do
  mapM_ exactPC stmts

-- ---------------------------------------------------------------------

instance ExactP (GHC.HsExpr GHC.RdrName) where
  exactP (GHC.HsVar v)  = exactP v
  exactP (GHC.HsIPVar (GHC.HsIPName v)) = do
    printStringAtMaybeAnn GHC.AnnVal ("?" ++ GHC.unpackFS v)
  exactP (GHC.HsOverLit lit)     = exactP lit
  exactP (GHC.HsLit lit)         = exactP lit
  exactP (GHC.HsLam match)       = do
    printStringAtMaybeAnn GHC.AnnLam "\\"
    exactPMatchGroup match
  exactP (GHC.HsLamCase _ match) = exactPMatchGroup match
  exactP (GHC.HsApp e1 e2)       = exactPC e1 >> exactPC e2
  exactP (GHC.OpApp e1 op _f e2) = exactPC e1 >> exactPC op >> exactPC e2

  exactP (GHC.NegApp e _)        = do
    printStringAtMaybeAnn GHC.AnnMinus "-"
    exactPC e

  exactP (GHC.HsPar e) = do
    printStringAtMaybeAnn GHC.AnnOpen  "("
    exactPC e
    printStringAtMaybeAnn GHC.AnnClose ")"

  exactP (GHC.SectionL e1 e2)    = exactPC e1 >> exactPC e2
  exactP (GHC.SectionR e1 e2)    = exactPC e1 >> exactPC e2

  exactP (GHC.ExplicitTuple args b) = do
    if b == GHC.Boxed then printStringAtMaybeAnn GHC.AnnOpen "("
                      else printStringAtMaybeAnn GHC.AnnOpen "(#"

    mapM_ exactPC args `debug` ("exactP.ExplicitTuple")

    if b == GHC.Boxed then printStringAtMaybeAnn GHC.AnnClose ")"
                      else printStringAtMaybeAnn GHC.AnnClose "#)"

  exactP (GHC.HsCase e1 matches) = do
    printStringAtMaybeAnn GHC.AnnCase "case"
    exactPC e1
    printStringAtMaybeAnn GHC.AnnOf "of"
    printStringAtMaybeAnn GHC.AnnOpen "{"
    exactPMatchGroup matches
    printStringAtMaybeAnn GHC.AnnClose "}"

  exactP (GHC.HsIf _ e1 e2 e3)   = do
    printStringAtMaybeAnn GHC.AnnIf "if"
    exactPC e1
    printStringAtMaybeAnn GHC.AnnSemi ";"
    printStringAtMaybeAnn GHC.AnnThen "then"
    exactPC e2
    printStringAtMaybeAnn GHC.AnnSemi ";"
    printStringAtMaybeAnn GHC.AnnElse "else"
    exactPC e3

  exactP (GHC.HsMultiIf _ rhs)   = do
    printStringAtMaybeAnn GHC.AnnIf "if"
    mapM_ exactPC rhs

  exactP (GHC.HsLet lb e)    = do
    printStringAtMaybeAnn GHC.AnnLet "let"
    printStringAtMaybeAnn GHC.AnnOpen "{"
    printStringAtMaybeAnnAll GHC.AnnSemi ";"
    exactP lb
    printStringAtMaybeAnn GHC.AnnClose "}"
    printStringAtMaybeAnn GHC.AnnIn "in"
    exactPC e

  exactP (GHC.HsDo cts stmts _typ)    = do
    printStringAtMaybeAnn GHC.AnnDo "do"
    let (ostr,cstr,isComp) =
          if isListComp cts
            then case cts of
                   GHC.PArrComp -> ("[:",":]",True)
                   _            -> ("[",  "]",True)
            else ("{","}",False)

    printStringAtMaybeAnn GHC.AnnOpen ostr
    printStringAtMaybeAnnAll GHC.AnnSemi ";"
    if isComp
      then do
        exactPC(last stmts)
        printStringAtMaybeAnn GHC.AnnVbar "|"
        mapM_ exactPC (init stmts)
      else do
        mapM_ exactPC stmts
    printStringAtMaybeAnn GHC.AnnClose cstr

  exactP (GHC.ExplicitList _ _ es) = do
    printStringAtMaybeAnn GHC.AnnOpen "["
    mapM_ exactPC es
    printStringAtMaybeAnn GHC.AnnClose "]"

  exactP (GHC.ExplicitPArr _ es)   = do
    printStringAtMaybeAnn GHC.AnnOpen "[:"
    mapM_ exactPC es
    printStringAtMaybeAnn GHC.AnnClose ":]"

  exactP (GHC.RecordCon n _ (GHC.HsRecFields fs _)) = do
    exactPC n
    printStringAtMaybeAnn GHC.AnnOpen "{"
    printStringAtMaybeAnn GHC.AnnDotdot ".."
    mapM_ exactPC fs
    printStringAtMaybeAnn GHC.AnnClose "}"

  exactP (GHC.RecordUpd e (GHC.HsRecFields fs _) cons _ _)  = do
    exactPC e
    printStringAtMaybeAnn GHC.AnnOpen "{"
    printStringAtMaybeAnn GHC.AnnDotdot ".."
    mapM_ exactPC fs
    printStringAtMaybeAnn GHC.AnnClose "}"

  exactP (GHC.ExprWithTySig e typ _) = do
    exactPC e
    printStringAtMaybeAnn GHC.AnnDcolon "::"
    exactPC typ

  exactP (GHC.ExprWithTySigOut e typ) = do
    exactPC e
    printStringAtMaybeAnn GHC.AnnDcolon "::"
    exactPC typ

  exactP (GHC.ArithSeq _ _ seqInfo) = do
    printStringAtMaybeAnn GHC.AnnOpen "["
    case seqInfo of
      GHC.From e1 -> exactPC e1 >> printStringAtMaybeAnn GHC.AnnDotdot ".."
      GHC.FromTo e1 e2 -> do
        exactPC e1
        printStringAtMaybeAnn GHC.AnnDotdot ".."
        exactPC e2
      GHC.FromThen e1 e2 -> do
        exactPC e1
        printStringAtMaybeAnn GHC.AnnComma ","
        exactPC e2
        printStringAtMaybeAnn GHC.AnnDotdot ".."
      GHC.FromThenTo e1 e2 e3 -> do
        exactPC e1
        printStringAtMaybeAnn GHC.AnnComma ","
        exactPC e2
        printStringAtMaybeAnn GHC.AnnDotdot ".."
        exactPC e3
    printStringAtMaybeAnn GHC.AnnClose "]"

  exactP (GHC.PArrSeq _ seqInfo) = do
    printStringAtMaybeAnn GHC.AnnOpen "[:"
    case seqInfo of
      GHC.From e1 -> exactPC e1 >> printStringAtMaybeAnn GHC.AnnDotdot ".."
      GHC.FromTo e1 e2 -> do
        exactPC e1
        printStringAtMaybeAnn GHC.AnnDotdot ".."
        exactPC e2
      GHC.FromThen e1 e2 -> do
        exactPC e1
        printStringAtMaybeAnn GHC.AnnComma ","
        exactPC e2
        printStringAtMaybeAnn GHC.AnnDotdot ".."
      GHC.FromThenTo e1 e2 e3 -> do
        exactPC e1
        printStringAtMaybeAnn GHC.AnnComma ","
        exactPC e2
        printStringAtMaybeAnn GHC.AnnDotdot ".."
        exactPC e3
    printStringAtMaybeAnn GHC.AnnClose ":]"

  exactP (GHC.HsSCC src str e) = do
    printStringAtMaybeAnn GHC.AnnOpen src -- "{-# SCC"
    printStringAtMaybeAnn GHC.AnnVal (GHC.unpackFS str)
    printStringAtMaybeAnn GHC.AnnValStr ("\"" ++ GHC.unpackFS str ++ "\"")
    printStringAtMaybeAnn GHC.AnnClose "#-}"
    exactPC e

  exactP (GHC.HsCoreAnn src str e) = do
    printStringAtMaybeAnn GHC.AnnOpen src -- "{-# CORE"
    printStringAtMaybeAnn GHC.AnnVal (GHC.unpackFS str)
    printStringAtMaybeAnn GHC.AnnClose "#-}"
    exactPC e

  exactP (GHC.HsBracket (GHC.VarBr single v)) = do
    if single then printStringAtMaybeAnn GHC.AnnVal ("'"  ++ rdrName2String v)
              else printStringAtMaybeAnn GHC.AnnVal ("''" ++ rdrName2String v)
  exactP (GHC.HsBracket (GHC.DecBrL ds)) = do
    cnt <- countAnns GHC.AnnOpen
    case cnt of
      1 -> do
        printStringAtMaybeAnn GHC.AnnOpen  "[d|"
        mapM_ exactPC ds
        printStringAtMaybeAnn GHC.AnnClose "|]"
      _ -> do
        printStringAtMaybeAnn GHC.AnnOpen  "[d|"
        printStringAtMaybeAnn GHC.AnnOpen  "{"
        mapM_ exactPC ds
        printStringAtMaybeAnn GHC.AnnClose "}"
        printStringAtMaybeAnn GHC.AnnClose "|]"
  exactP (GHC.HsBracket (GHC.ExpBr e)) = do
        printStringAtMaybeAnn GHC.AnnOpen  "[|"
        exactPC e
        printStringAtMaybeAnn GHC.AnnClose "|]"
  exactP (GHC.HsBracket (GHC.TExpBr e)) = do
        printStringAtMaybeAnn GHC.AnnOpen  "[||"
        exactPC e
        printStringAtMaybeAnn GHC.AnnClose "||]"
  exactP (GHC.HsBracket (GHC.TypBr e)) = do
        printStringAtMaybeAnn GHC.AnnOpen  "[t|"
        exactPC e
        printStringAtMaybeAnn GHC.AnnClose "|]"
  exactP (GHC.HsBracket (GHC.PatBr e)) = do
        printStringAtMaybeAnn GHC.AnnOpen  "[p|"
        exactPC e
        printStringAtMaybeAnn GHC.AnnClose "|]"

  exactP (GHC.HsRnBracketOut _ _) = return ()
  exactP (GHC.HsTcBracketOut _ _) = return ()

  exactP (GHC.HsSpliceE False (GHC.HsSplice _ e)) = do
    printStringAtMaybeAnn GHC.AnnOpen "$("
    exactPC e
    printStringAtMaybeAnn GHC.AnnClose ")"
  exactP (GHC.HsSpliceE True (GHC.HsSplice _ e)) = do
    printStringAtMaybeAnn GHC.AnnOpen "$$("
    exactPC e
    printStringAtMaybeAnn GHC.AnnClose ")"

  exactP (GHC.HsQuasiQuoteE (GHC.HsQuasiQuote n _ str)) = do
    printStringAtMaybeAnn GHC.AnnVal
          ("[" ++ (rdrName2String n) ++ "|" ++ (GHC.unpackFS str) ++ "|]")

  exactP (GHC.HsProc p c) = do
    printStringAtMaybeAnn GHC.AnnProc "proc"
    exactPC p
    printStringAtMaybeAnn GHC.AnnRarrow "->"
    exactPC c

  exactP (GHC.HsArrApp e1 e2 _ _ _) = do
    exactPC e1
    -- only one of the next 4 will be resent
    printStringAtMaybeAnn GHC.Annlarrowtail "-<"
    printStringAtMaybeAnn GHC.Annrarrowtail ">-"
    printStringAtMaybeAnn GHC.AnnLarrowtail "-<<"
    printStringAtMaybeAnn GHC.AnnRarrowtail ">>-"

    exactPC e1

  exactP (GHC.HsArrForm e _ cs) = do
    printStringAtMaybeAnn GHC.AnnOpen "(|"
    exactPC e
    mapM_ exactPC cs
    printStringAtMaybeAnn GHC.AnnClose  "|)"

  exactP (GHC.HsTick _ _) = return ()
  exactP (GHC.HsBinTick _ _ _) = return ()

  exactP (GHC.HsTickPragma src (str,(v1,v2),(v3,v4)) e) = do
    -- '{-# GENERATED' STRING INTEGER ':' INTEGER '-' INTEGER ':' INTEGER '#-}'
    printStringAtMaybeAnn GHC.AnnOpen  src -- "{-# GENERATED"
    printStringAtMaybeAnn GHC.AnnVal   (show $ GHC.unpackFS str)
    printStringAtMaybeAnn GHC.AnnVal   (show v1)
    printStringAtMaybeAnn GHC.AnnColon ":"
    printStringAtMaybeAnn GHC.AnnVal   (show v2)
    printStringAtMaybeAnn GHC.AnnMinus "-"
    printStringAtMaybeAnn GHC.AnnVal   (show v3)
    printStringAtMaybeAnn GHC.AnnColon ":"
    printStringAtMaybeAnn GHC.AnnVal   (show v4)
    printStringAtMaybeAnn GHC.AnnClose "#-}"
    exactPC e

  exactP (GHC.EWildPat) = printStringAtMaybeAnn GHC.AnnVal "_"

  exactP (GHC.EAsPat n e) = do
    exactPC n
    printStringAtMaybeAnn GHC.AnnAt "@"
    exactPC e

  exactP (GHC.EViewPat e1 e2) = do
    exactPC e1
    printStringAtMaybeAnn GHC.AnnRarrow "->"
    exactPC e2

  exactP (GHC.ELazyPat e) = do
    printStringAtMaybeAnn GHC.AnnTilde "~"
    exactPC e

  exactP (GHC.HsType ty) = exactPC ty
  exactP (GHC.HsWrap _ _) = return ()
  exactP (GHC.HsUnboundVar _) = return ()

  exactP e = printString "HsExpr"
    `debug` ("exactP.HsExpr:not processing " ++ (showGhc e) )

-- ---------------------------------------------------------------------

-- instance ExactP (GHC.HsRecField GHC.RdrName (GHC.LHsExpr GHC.RdrName)) where
--   exactP (GHC.HsRecField _ e _) = exactPC e

instance (ExactP arg) => ExactP (GHC.HsRecField GHC.RdrName (GHC.Located arg)) where
  exactP (GHC.HsRecField _ e _) = exactPC e

-- ---------------------------------------------------------------------

instance ExactP (GHC.HsCmdTop GHC.RdrName) where
  exactP (GHC.HsCmdTop cmd _ _ _) = exactPC cmd

-- ---------------------------------------------------------------------

instance ExactP (GHC.HsCmd GHC.RdrName) where
  exactP (GHC.HsCmdArrApp e1 e2 _ _ _) = do
    exactPC e1
    -- only one of the next 4 will be resent
    printStringAtMaybeAnn GHC.Annlarrowtail "-<"
    printStringAtMaybeAnn GHC.Annrarrowtail ">-"
    printStringAtMaybeAnn GHC.AnnLarrowtail "-<<"
    printStringAtMaybeAnn GHC.AnnRarrowtail ">>-"

    exactPC e1

  exactP (GHC.HsCmdArrForm e _ cs) = do
    printStringAtMaybeAnn GHC.AnnOpen "(|"
    exactPC e
    mapM_ exactPC cs
    printStringAtMaybeAnn GHC.AnnClose  "|)"

  exactP (GHC.HsCmdApp e1 e2) = exactPC e1 >> exactPC e2

  exactP (GHC.HsCmdLam match) = do
    printStringAtMaybeAnn GHC.AnnLam "\\"
    exactPMatchGroup match

  exactP (GHC.HsCmdPar e) = do
    printStringAtMaybeAnn GHC.AnnOpen  "("
    exactPC e
    printStringAtMaybeAnn GHC.AnnClose ")"

  exactP (GHC.HsCmdCase e1 matches) = do
    printStringAtMaybeAnn GHC.AnnCase "case"
    exactPC e1
    printStringAtMaybeAnn GHC.AnnOf "of"
    printStringAtMaybeAnn GHC.AnnOpen "{"
    exactPMatchGroup matches
    printStringAtMaybeAnn GHC.AnnClose "}"

  exactP (GHC.HsCmdIf _ e1 e2 e3)   = do
    printStringAtMaybeAnn GHC.AnnIf "if"
    exactPC e1
    printStringAtMaybeAnn GHC.AnnSemi ";"
    printStringAtMaybeAnn GHC.AnnThen "then"
    exactPC e2
    printStringAtMaybeAnn GHC.AnnSemi ";"
    printStringAtMaybeAnn GHC.AnnElse "else"
    exactPC e3

  exactP (GHC.HsCmdLet lb e)    = do
    printStringAtMaybeAnn GHC.AnnLet "let"
    printStringAtMaybeAnn GHC.AnnOpen "{"
    exactP lb
    printStringAtMaybeAnn GHC.AnnClose "}"
    printStringAtMaybeAnn GHC.AnnIn "in"
    exactPC e

  exactP (GHC.HsCmdDo stmts _typ)    = do
    printStringAtMaybeAnn GHC.AnnDo "do"
    printStringAtMaybeAnn GHC.AnnOpen "{"
    mapM_ exactPC stmts
    printStringAtMaybeAnn GHC.AnnClose "}"

  exactP (GHC.HsCmdCast {}) = error $ "exactP.HsCmdCast: only valid after type checker"

-- ---------------------------------------------------------------------

instance ExactP GHC.RdrName where
  exactP n = do
    case rdrName2String n of
      "[]" -> do
        printStringAtMaybeAnn GHC.AnnOpen "["
        printStringAtMaybeAnn GHC.AnnClose "]"
      "()" -> do
        printStringAtMaybeAnn GHC.AnnOpen "("
        printStringAtMaybeAnn GHC.AnnClose ")"
      str ->  do
        printStringAtMaybeAnn GHC.AnnType      "type"
        printStringAtMaybeAnn GHC.AnnOpen      "("
        printStringAtMaybeAnn GHC.AnnBackquote  "`"
        printStringAtMaybeAnn GHC.AnnTildehsh  "~#"
        printStringAtMaybeAnn GHC.AnnTilde     "~"
        printStringAtMaybeAnn GHC.AnnVal       str
        printStringAtMaybeAnn GHC.AnnBackquote "`"
        printStringAtMaybeAnn GHC.AnnClose     ")"

instance ExactP GHC.HsIPName where
  exactP (GHC.HsIPName n) = do
    printStringAtMaybeAnn GHC.AnnVal ("?" ++ GHC.unpackFS n)

-- ---------------------------------------------------------------------

exactPMatchGroup :: (ExactP body) => (GHC.MatchGroup GHC.RdrName (GHC.Located body))
                   -> EP ()
exactPMatchGroup (GHC.MG matches _ _ _)
  = mapM_ exactPC matches

-- ---------------------------------------------------------------------

instance ExactP (GHC.HsTupArg GHC.RdrName) where
  exactP (GHC.Missing _) = do
    printStringAtMaybeAnn GHC.AnnComma ","
    return ()
  exactP (GHC.Present e) = do
    exactPC e
    printStringAtMaybeAnn GHC.AnnComma ","

instance ExactP (GHC.HsLocalBinds GHC.RdrName) where
  exactP (GHC.HsValBinds (GHC.ValBindsIn binds sigs)) = do
    printMerged (GHC.bagToList binds) sigs
  exactP (GHC.HsValBinds (GHC.ValBindsOut binds sigs)) = printString "ValBindsOut"
  exactP (GHC.HsIPBinds (GHC.IPBinds binds _)) = mapM_ exactPC binds
  exactP (GHC.EmptyLocalBinds) = return ()

-- ---------------------------------------------------------------------

instance ExactP (GHC.Sig GHC.RdrName) where
  exactP (GHC.TypeSig lns typ _) = do
    mapM_ exactPC lns
    printStringAtMaybeAnn GHC.AnnDcolon "::"
    exactPC typ

  exactP (GHC.PatSynSig n (_,GHC.HsQTvs _ns bndrs) ctx1 ctx2 typ) = do
    printStringAtMaybeAnn GHC.AnnPattern "pattern"
    exactPC n
    printStringAtMaybeAnn GHC.AnnDcolon "::"

    -- Note: The 'forall' bndrs '.' may occur multiple times
    printStringAtMaybeAnn GHC.AnnForall "forall"
    mapM_ exactPC bndrs
    printStringAtMaybeAnn GHC.AnnDot "."

    exactPC ctx1
    printStringAtMaybeAnn GHC.AnnDarrow "=>"
    exactPC ctx2
    printStringAtMaybeAnn GHC.AnnDarrow "=>"
    exactPC typ

  exactP (GHC.GenericSig ns typ) = do
    printStringAtMaybeAnn GHC.AnnDefault "default"
    mapM_ exactPC ns
    printStringAtMaybeAnn GHC.AnnDcolon "::"
    exactPC typ

  exactP (GHC.IdSig _) = return ()

  exactP (GHC.FixSig (GHC.FixitySig lns (GHC.Fixity v fdir))) = do
    let fixstr = case fdir of
         GHC.InfixL -> "infixl"
         GHC.InfixR -> "infixr"
         GHC.InfixN -> "infix"
    printStringAtMaybeAnn GHC.AnnInfix fixstr
    printStringAtMaybeAnn GHC.AnnVal (show v)
    mapM_ exactPC lns

  exactP (GHC.InlineSig n inl) = do
    cnt <- countAnns GHC.AnnOpen
    return () `debug` ("exactP.InlineSig:cnt=" ++ show cnt)
    case cnt of
      2 -> do
        let actStr = case GHC.inl_act inl of
              GHC.AlwaysActive -> ""
              GHC.ActiveBefore np -> show np
              GHC.ActiveAfter  np -> show np
        printStringAtMaybeAnn GHC.AnnOpen (GHC.inl_src inl) -- "{-# INLINE"
        printStringAtMaybeAnn GHC.AnnOpen  "["
        printStringAtMaybeAnn GHC.AnnTilde "~"
        printStringAtMaybeAnn GHC.AnnVal   actStr
        printStringAtMaybeAnn GHC.AnnClose "]"
        exactPC n
        printStringAtMaybeAnn GHC.AnnClose "#-}"
      _ -> do
        return () `debug` ("exactP.InlineSig.2:cnt=" ++ show cnt)
        printStringAtMaybeAnn GHC.AnnOpen (GHC.inl_src inl) -- "{-# INLINE"
        exactPC n
        printStringAtMaybeAnn GHC.AnnClose "#-}"


  exactP (GHC.SpecSig n typs inl) = do
    cnt <- countAnns GHC.AnnOpen
    case cnt of
      2 -> do
        printStringAtMaybeAnn GHC.AnnOpen  (GHC.inl_src inl) -- "{-# SPECIALISE"
        printStringAtMaybeAnn GHC.AnnOpen  "["
        printStringAtMaybeAnn GHC.AnnTilde  "~"
        printStringAtMaybeAnn GHC.AnnVal   "TODO:what here?" -- e.g. 34
        printStringAtMaybeAnn GHC.AnnClose "]"
        exactPC n
        printStringAtMaybeAnn GHC.AnnDcolon "::"
        mapM_ exactPC typs
        printStringAtMaybeAnn GHC.AnnClose "#-}"
      _ -> do
        printStringAtMaybeAnn GHC.AnnOpen  (GHC.inl_src inl) -- "{-# SPECIALISE"
        exactPC n
        printStringAtMaybeAnn GHC.AnnDcolon "::"
        mapM_ exactPC typs
        printStringAtMaybeAnn GHC.AnnClose "#-}"


  exactP _ = printString "Sig"

-- ---------------------------------------------------------------------

instance ExactP (GHC.HsOverLit GHC.RdrName) where
  exactP ol = do
    case GHC.ol_val ol of
      GHC.HsIntegral src _ -> printStringAtMaybeAnn GHC.AnnVal src
      GHC.HsFractional l   -> printStringAtMaybeAnn GHC.AnnVal (GHC.fl_text l)
      GHC.HsIsString src _ -> printStringAtMaybeAnn GHC.AnnVal src

-- ---------------------------------------------------------------------

instance ExactP GHC.HsLit where
  exactP lit = printStringAtMaybeAnn GHC.AnnVal (hsLit2String lit)

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

-- ---------------------------------------------------------------------


instance ExactP (GHC.TyClDecl GHC.RdrName) where
  exactP (GHC.FamDecl famdecl) = exactP famdecl

  exactP (GHC.SynDecl ln (GHC.HsQTvs _ tyvars) typ _) = do
    printStringAtMaybeAnn GHC.AnnType "type"
    exactPC ln
    mapM_ exactPC tyvars
    printStringAtMaybeAnn GHC.AnnEqual "="
    exactPC typ

  exactP (GHC.DataDecl ln (GHC.HsQTvs ns tyVars) defn _) = do
    printStringAtMaybeAnn GHC.AnnData    "data"
    printStringAtMaybeAnn GHC.AnnNewtype "newtype"
    exactPC ln
    mapM_ exactPC tyVars
    printStringAtMaybeAnn GHC.AnnEqual "="
    printStringAtMaybeAnn GHC.AnnWhere "where"
    exactP defn

  exactP (GHC.ClassDecl ctx ln (GHC.HsQTvs ns tyVars) fds
                          sigs meths ats atdefs docs _) = do
    printStringAtMaybeAnn GHC.AnnClass "class"
    exactPC ctx
    exactPC ln
    mapM_ exactPC tyVars
    printStringAtMaybeAnn GHC.AnnVbar "|"
    mapM_ exactPC fds
    printStringAtMaybeAnn GHC.AnnWhere "where"
    printStringAtMaybeAnn GHC.AnnOpen  "{"
    printStringAtMaybeAnnAll GHC.AnnSemi ";"

    applyListPrint (prepareListPrint sigs
                 ++ prepareListPrint (GHC.bagToList meths)
                 ++ prepareListPrint ats
                 ++ prepareListPrint atdefs
                 ++ prepareListPrint docs
                    )

    printStringAtMaybeAnn GHC.AnnClose "}"

-- ---------------------------------------------------------------------

instance ExactP (GHC.FamilyDecl GHC.RdrName) where
  exactP (GHC.FamilyDecl info ln (GHC.HsQTvs _ tyvars) mkind) = do
    printStringAtMaybeAnn GHC.AnnType   "type"
    printStringAtMaybeAnn GHC.AnnData   "data"
    printStringAtMaybeAnn GHC.AnnFamily "family"
    exactPC ln
    mapM_ exactPC tyvars
    case mkind of
      Nothing -> return ()
      Just k -> exactPC k
    printStringAtMaybeAnn GHC.AnnWhere "where"
    printStringAtMaybeAnn GHC.AnnOpen "{"
    case info of
      GHC.ClosedTypeFamily eqns -> mapM_ exactPC eqns
      _ -> return ()
    printStringAtMaybeAnn GHC.AnnClose "}"

-- ---------------------------------------------------------------------

instance ExactP (GHC.TyFamDefltEqn GHC.RdrName) where
   exactP = assert False undefined

-- ---------------------------------------------------------------------

instance ExactP (GHC.TyFamInstEqn GHC.RdrName) where
  exactP (GHC.TyFamEqn ln (GHC.HsWB pats _ _ _) typ) = do
    exactPC ln
    mapM_ exactPC pats
    printStringAtMaybeAnn GHC.AnnEqual "="
    exactPC typ

-- ---------------------------------------------------------------------

instance ExactP GHC.DocDecl where
  exactP (GHC.DocCommentNext (GHC.HsDocString fs))
    = printStringAtMaybeAnn GHC.AnnVal (GHC.unpackFS fs)
  exactP (GHC.DocCommentPrev (GHC.HsDocString fs))
    = printStringAtMaybeAnn GHC.AnnVal (GHC.unpackFS fs)
  exactP (GHC.DocCommentNamed s (GHC.HsDocString fs))
    = printStringAtMaybeAnn GHC.AnnVal (GHC.unpackFS fs)
  exactP (GHC.DocGroup i (GHC.HsDocString fs))
    = printStringAtMaybeAnn GHC.AnnVal (GHC.unpackFS fs)

{-
DocCommentNext HsDocString
DocCommentPrev HsDocString
DocCommentNamed String HsDocString
DocGroup Int HsDocString
-}

-- ---------------------------------------------------------------------

instance ExactP (GHC.FunDep (GHC.Located GHC.RdrName)) where
  exactP (ls,rs) = do
    mapM_ exactPC ls
    printStringAtMaybeAnn GHC.AnnRarrow "->"
    mapM_ exactPC rs

-- ---------------------------------------------------------------------

instance ExactP (GHC.HsTyVarBndr GHC.RdrName) where
  exactP (GHC.UserTyVar n) = printStringAtMaybeAnn GHC.AnnVal (rdrName2String n)
  exactP (GHC.KindedTyVar n ty) = do
    printStringAtMaybeAnn GHC.AnnOpen "("
    exactPC n
    printStringAtMaybeAnn GHC.AnnDcolon "::"
    exactPC ty
    printStringAtMaybeAnn GHC.AnnClose ")"

-- ---------------------------------------------------------------------

instance ExactP (GHC.HsDataDefn GHC.RdrName) where
  exactP (GHC.HsDataDefn nOrD ctx mtyp mkind cons mderivs) = do
    exactPC ctx
    doMaybe exactPC mtyp
    doMaybe exactPC mkind
    mapM_ exactPC cons
    doMaybe exactPC mderivs

-- ---------------------------------------------------------------------

instance ExactP [GHC.LConDecl GHC.RdrName] where
  exactP cons = mapM_ exactPC cons

-- ---------------------------------------------------------------------

instance ExactP (GHC.ConDecl GHC.RdrName) where
  exactP (GHC.ConDecl lns exp (GHC.HsQTvs _ns bndrs) ctx dets res _ _) = do
    case dets of
      GHC.InfixCon _ _ -> return ()
      _ -> mapM_ exactPC lns

    printStringAtMaybeAnn GHC.AnnForall "forall"
    mapM_ exactPC bndrs
    printStringAtMaybeAnn GHC.AnnDot "."

    exactPC ctx

    case dets of
      GHC.PrefixCon args -> mapM_ exactPC args
      GHC.RecCon fs -> exactPC fs
      GHC.InfixCon a1 a2 -> do
        exactPC a1
        mapM_ exactPC lns
        exactPC a2


    printStringAtMaybeAnn GHC.AnnVbar "|"

-- ---------------------------------------------------------------------

instance ExactP [GHC.LConDeclField GHC.RdrName] where
  exactP fs = do
    printStringAtMaybeAnn GHC.AnnOpen "{"
    mapM_ exactPC fs
    printStringAtMaybeAnn GHC.AnnDotdot ".."
    printStringAtMaybeAnn GHC.AnnClose "}"

-- ---------------------------------------------------------------------

instance ExactP (GHC.CType) where
  exactP (GHC.CType src _ _) = printStringAtMaybeAnn GHC.AnnVal src

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


