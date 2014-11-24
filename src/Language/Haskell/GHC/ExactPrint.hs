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
import Data.Data
import Data.List (intersperse)
-- import Data.List.Utils
import Data.Maybe

import qualified Bag           as GHC
import qualified BasicTypes    as GHC
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

newtype EP x = EP (Pos -> DeltaPos -> [GHC.SrcSpan] -> [Comment] -> Anns
            -> (x, Pos,   DeltaPos,   [GHC.SrcSpan],   [Comment],   Anns, ShowS))

instance Functor EP where
  fmap = liftM

instance Applicative EP where
  pure = return
  (<*>) = ap

instance Monad EP where
  return x = EP $ \l dp s cs an -> (x, l, dp, s, cs, an, id)

  EP m >>= k = EP $ \l0 ss0 dp0 c0 an0 -> let
        (a, l1, ss1, dp1, c1, an1, s1) = m l0 ss0 dp0 c0 an0
        EP f = k a
        (b, l2, ss2, dp2, c2, an2, s2) = f l1 ss1 dp1 c1 an1
    in (b, l2, ss2, dp2, c2, an2, s1 . s2)

runEP :: EP () -> GHC.SrcSpan -> [Comment] -> Anns -> String
runEP (EP f) ss cs ans = let (_,_,_,_,_,_,s) = f (1,1) (DP (0,0)) [ss] cs ans in s ""

getPos :: EP Pos
getPos = EP (\l dp s cs an -> (l,l,dp,s,cs,an,id))

setPos :: Pos -> EP ()
setPos l = EP (\_ dp s cs an -> ((),l,dp,s,cs,an,id))


getOffset :: EP DeltaPos
getOffset = EP (\l dp s cs an -> (dp,l,dp,s,cs,an,id))

addOffset :: DeltaPos -> EP ()
addOffset (DP (r,c)) = EP (\l (DP (ro,co)) s cs an -> ((),l,(DP (r+ro,c+co)),s,cs,an,id))

setOffset :: DeltaPos -> EP ()
setOffset dp = EP (\l _ s cs an -> ((),l,dp,s,cs,an,id))

getSrcSpan :: EP GHC.SrcSpan
getSrcSpan = EP (\l dp (s:ss) cs an -> (s,l,dp,(s:ss),cs,an,id))

-- | Replace the current head value
setSrcSpan :: GHC.SrcSpan -> EP ()
setSrcSpan ss = EP (\l dp (s:sss) cs an -> ((),l,dp,(ss:sss),cs,an,id))

pushSrcSpan :: GHC.SrcSpan -> EP ()
pushSrcSpan ss = EP (\l dp sss cs an -> ((),l,dp,(ss:sss),cs,an,id))

popSrcSpan :: EP ()
popSrcSpan = EP (\l dp (_:sss) cs an -> ((),l,dp,sss,cs,an,id))


getAnnotation :: (Typeable a) => GHC.Located a -> EP (Maybe Annotation)
getAnnotation a@(GHC.L ss _) = EP (\l dp s cs an -> (getAnnotationEP (anEP an) a
                       ,l,dp,s,cs,an,id))

getAnnValue :: (Typeable b) => EP (Maybe b)
getAnnValue = EP (\l dp (s:ss) cs an -> (getAnnotationValue (anU an) s
                  ,l,dp,(s:ss),cs,an,id))

getAnnFinal :: GHC.AnnKeywordId -> EP [DeltaPos]
getAnnFinal kw = EP (\l dp (s:ss) cs an ->
     let
       r = case Map.lookup (s,kw) (anF an) of
             Nothing -> []
             Just ds -> ds
     in (r         ,l,dp,(s:ss),cs,an,id))



{- Should not be needed anymore due to storing TypeRep
putAnnotation :: GHC.SrcSpan -> (Annotation,Value) -> EP ()
putAnnotation ss anns = EP (\l dp cs an ->
  let
    an' = putAnnotationValue an ss anns
  in ((),l,dp, cs,an',id))
-}

printString :: String -> EP ()
printString str = EP (\(l,c) dp s cs an -> ((), (l,c+length str), dp, s, cs, an, showString str))

getComment :: EP (Maybe Comment)
getComment = EP $ \l dp s cs an ->
    let x = case cs of
             c:_ -> Just c
             _   -> Nothing
     in (x, l, dp, s, cs, an, id)

dropComment :: EP ()
dropComment = EP $ \l dp s cs an ->
    let cs' = case cs of
               (_:cs) -> cs
               _      -> cs
     in ((), l, dp, s, cs', an, id)

mergeComments :: [DComment] -> EP ()
mergeComments dcs = EP $ \l dp s cs an ->
    let acs = map (undeltaComment l) dcs
        cs' = merge acs cs
    in ((), l, dp, s, cs', an, id) -- `debug` ("mergeComments:(l,acs,dcs)=" ++ show (l,acs,dcs))

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

printStringAtLsDelta :: [DeltaPos] -> String -> EP ()
printStringAtLsDelta mc s =
  case mc of
    [cl] -> do
      p <- getPos
      printStringAt (undelta p cl) s
    _ -> return ()


printStringAtMaybeAnn :: GHC.AnnKeywordId -> String -> EP ()
printStringAtMaybeAnn ann str = do
  ma <- getAnnFinal ann
  ss <- getSrcSpan
  printStringAtLsDelta ma str
    `debug` ("printStringAtMaybeAnn:(ss,ann,ma,str)=" ++ show (ss2span ss,ann,ma,str))

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
exactPrint ast@(GHC.L l _) cs toks = runEP (exactPC ast) l cs (Map.empty,Map.empty,Map.empty)


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
 -- let p = pos l
    do pushSrcSpan l `debug` ("exactPC entered for:" ++ showGhc l)
       ma <- getAnnotation a
       (off@(DP (r,c)),cs) <- case ma of
         Nothing -> return ((DP (0,0)),[])
           `debug` ("exactPC:no annotation for " ++ show (ss2span l,typeOf ast))
         Just ann -> do
             -- mergeComments lcs
             return (dp,lcs)
           where lcs = ann_comments ann
                 dp = ann_delta ann
       pe <- getPos
       let p = undelta pe off
       -- mPrintComments p -- `debug` ("exactPC:(p,off)=" ++ show (p,off))
       -- padUntil p
       mergeComments cs

       let negOff = DP (-r,-c)
       -- addOffset off `debug` ("addOffset:push:" ++ show (ss2span l,off))
       exactP ast
       -- addOffset negOff `debug` ("addOffset:pop:" ++ show (ss2span l,negOff))
       popSrcSpan

{-

Two approaches:

getAnn2 anns span = res
  where res = case  Map.lookup (span,typeOf res) anns of
                       Nothing -> Nothing
                       Just d -> fromDynamic d

Or:

getAnn2 :: forall a. Map.Map (SrcSpan,TypeRep) Dynamic -> SrcSpan -> Maybe a
... typeOf (undefined :: a) ...

-}


{-
exactPCTrailingComma :: (ExactP ast) => GHC.Located ast -> EP ()
exactPCTrailingComma a@(GHC.L l _) = do
  exactPC a
  ma <- getAnnotation l
  case getAnn isAnnListItem ma "ListItem" of
    [Ann _ _ (AnnListItem commaPos)] -> do
      printStringAtMaybeDelta commaPos ","
    _ -> return ()
-}

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
  exactP (GHC.HsModule Nothing exps imps decls deprecs haddock) = do
    Just (AnnHsModule mm mn mw mob ms mcb ep) <- getAnnValue :: EP (Maybe AnnHsModule)
    -- let Just [Ann _ _ (AnnHsModule mm mn mo mc mw ep)] = ma
    printSeq $ map (pos . ann &&& exactPC) decls

    -- put the end of file whitespace in
    pe <- getPos
    padUntil (undelta pe ep) `debug` ("exactP.HsModule:(pe,ep)=" ++ show (pe,ep))
    printString ""

  exactP (GHC.HsModule (Just lmn@(GHC.L l mn)) mexp limps decls deprecs haddock) = do
    ss <- getSrcSpan
    return () `debug` ("exactP.HsModule:ss=" ++ showGhc ss)

    printStringAtMaybeAnn GHC.AnnModule "module" -- `debug` ("exactP.HsModule:cs=" ++ show cs)
    printStringAtMaybeAnn GHC.AnnVal (GHC.moduleNameString mn)

    case mexp of
      Just lexps -> do
        exactPC lexps
        return ()
      Nothing -> return ()

    printStringAtMaybeAnn GHC.AnnWhere "where"
    printStringAtMaybeAnn GHC.AnnOpen  "{"
    exactP limps

    printStringAtMaybeAnn GHC.AnnSemi ";"

    -- printSeq $ map (pos . ann &&& exactPC) decls

    printStringAtMaybeAnn GHC.AnnClose "}"

    -- put the end of file whitespace in
{-
    pe <- getPos
    padUntil (undelta pe ep) `debug` ("exactP.HsModule:(pe,ep)=" ++ show (pe,ep))
    printString ""
-}

-- ---------------------------------------------------------------------

instance ExactP (GHC.ModuleName) where
  exactP mn = do
    printString (GHC.moduleNameString mn)

-- ---------------------------------------------------------------------

instance ExactP [GHC.LIE GHC.RdrName] where
  exactP ies = do
    printStringAtMaybeAnn GHC.AnnOpen "("
    mapM_ exactPC ies
    printStringAtMaybeAnn GHC.AnnClose ")"

-- ---------------------------------------------------------------------

instance ExactP (GHC.IE GHC.RdrName) where
  exactP (GHC.IEVar (GHC.L l n)) = do
    printStringAtMaybeAnn GHC.AnnPattern "pattern"
    printStringAtMaybeAnn GHC.AnnType    "type"
    printStringAtMaybeAnn GHC.AnnVal     (rdrName2String n)
    printStringAtMaybeAnn GHC.AnnComma   ","

  exactP (GHC.IEThingAbs n) = do
    printStringAtMaybeAnn GHC.AnnType    "type"
    printStringAtMaybeAnn GHC.AnnVal     (rdrName2String n)
    printStringAtMaybeAnn GHC.AnnComma   ","

  exactP (GHC.IEThingWith (GHC.L _ n) ns) = do
    printStringAtMaybeAnn GHC.AnnVal     (rdrName2String n)
    printStringAtMaybeAnn GHC.AnnOpen    "("
    mapM_ exactPC ns
    printStringAtMaybeAnn GHC.AnnClose   ")"
    printStringAtMaybeAnn GHC.AnnComma   ","

  exactP (GHC.IEThingAll (GHC.L _ n)) = do
    printStringAtMaybeAnn GHC.AnnVal     (rdrName2String n)
    printStringAtMaybeAnn GHC.AnnOpen    "("
    printStringAtMaybeAnn GHC.AnnDotdot  ".."
    printStringAtMaybeAnn GHC.AnnClose   ")"
    printStringAtMaybeAnn GHC.AnnComma   ","

  exactP (GHC.IEModuleContents (GHC.L _ mn)) = do
    printStringAtMaybeAnn GHC.AnnModule  "module"
    printStringAtMaybeAnn GHC.AnnVal     (GHC.moduleNameString mn)
    printStringAtMaybeAnn GHC.AnnComma   ","

  exactP x = printString ("no exactP.IE for " ++ showGhc (x))

-- ---------------------------------------------------------------------

instance ExactP [GHC.LImportDecl GHC.RdrName] where
  exactP imps = mapM_ exactPC imps
  -- Trailing semis?

-- ---------------------------------------------------------------------

instance ExactP (GHC.ImportDecl GHC.RdrName) where
  exactP imp = do
    Just an <- getAnnValue :: EP (Maybe AnnImportDecl)
    printString "import" `debug` ("exactP.ImportDecl: an=" ++ show an)
    case id_source an of
      Nothing -> return ()
      Just (od,cd) -> do
        printStringAtDelta od "{-# SOURCE"
        printStringAtDelta cd "#-}"

    printStringAtMaybeDelta (id_safe an) "safe"
    printStringAtMaybeDelta (id_qualified an) "qualified"
    printStringAtDelta (id_modulename an) ""
    exactPC (GHC.ideclName imp)
    case id_as an of
      Just (ap,np) -> do
        printStringAtDelta ap "as"
        printStringAtDelta np ""
        exactP (fromJust $ GHC.ideclAs imp)

      Nothing -> return ()
    case GHC.ideclHiding imp of
      Nothing -> return ()
      Just (isHiding,lie) -> do
        printStringAtMaybeDelta (id_hiding an) "hiding"
        exactPC lie

-- ---------------------------------------------------------------------

doMaybe :: (Monad m) => Maybe a -> (a -> m ()) -> m ()
doMaybe ma f = case ma of
                 Nothing -> return ()
                 Just a -> f a

instance ExactP (GHC.HsDecl GHC.RdrName) where
  exactP decl = case decl of
    GHC.TyClD d -> exactP d
    GHC.InstD d -> printString "InstD"
    GHC.DerivD d -> printString "DerivD"
    GHC.ValD d -> exactP d
    GHC.SigD d -> exactP d
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
  exactP (GHC.FunBind n _  (GHC.MG matches _ _ _) _fun_co_fn _fvs _tick) = do
    Just (AnnFunBind eqPos) <- getAnnValue
    printString (showGhc (GHC.unLoc n))
    printStringAtMaybeDelta eqPos "="
    mapM_ exactPC matches

  exactP (GHC.PatBind lhs (GHC.GRHSs grhs lb) _ty _fvs _ticks) = do
    Just (AnnPatBind eqPos wherePos) <- getAnnValue
    exactPC lhs
    printStringAtMaybeDelta eqPos "="
    mapM_ exactPC grhs
    printStringAtMaybeDelta wherePos "where"
    exactP lb

  exactP (GHC.VarBind var_id var_rhs var_inline ) = printString "VarBind"
  exactP (GHC.AbsBinds abs_tvs abs_ev_vars abs_exports abs_ev_binds abs_binds) = printString "AbsBinds"
  exactP (GHC.PatSynBind (GHC.PSB patsyn_id bind_fvs patsyn_args patsyn_def patsyn_dir)) = printString "PatSynBind"

instance ExactP (GHC.Match GHC.RdrName (GHC.LHsExpr GHC.RdrName)) where
  exactP (GHC.Match pats typ (GHC.GRHSs grhs lb)) = do
    (Just (AnnMatch {- nPos n isInfix -} eqPos wherePos)) <- getAnnValue
{-
    if isInfix
      then do
        exactPC (head pats)
        if isSymbolRdrName n
          then printStringAtDelta nPos (rdrName2String n)
          else printStringAtDelta nPos ("`" ++ (rdrName2String n) ++ "`")
        mapM_ exactPC (tail pats)
      else do
        printStringAtDelta nPos (rdrName2String n)
        mapM_ exactPC pats
-}
    printStringAtMaybeDelta eqPos "="
    -- doMaybe typ exactPC
    mapM_ exactPC typ
    mapM_ exactPC grhs
    printStringAtMaybeDelta wherePos "where"
    exactP lb

instance ExactP (GHC.Pat GHC.RdrName) where
  exactP (GHC.VarPat n)     = printString (rdrName2String n)
  exactP (GHC.NPat ol _ _)  = exactP ol
  exactP (GHC.ConPatIn e _) = exactPC e
  exactP (GHC.WildPat _)    = printString "_"
  exactP (GHC.AsPat n p) = do
    Just (AnnAsPat asPos) <- getAnnValue :: EP (Maybe AnnPat)
    -- let [(Ann _ _ (AnnAsPat asPos))] = getAnn isAnnAsPat ma "AsPat"
    exactPC n
    printStringAtDelta asPos "@"
    exactPC p

  exactP (GHC.TuplePat pats b _) = do
    Just (AnnTuplePat opPos cpPos) <- getAnnValue :: EP (Maybe AnnPat)
    -- let [(Ann _ _ (AnnTuplePat opPos cpPos))] = getAnn isAnnTuplePat ma "TuplePat"
    if b == GHC.Boxed then printStringAtDelta opPos "("
                      else printStringAtDelta opPos "(#"
    mapM_ exactPC pats
    if b == GHC.Boxed then printStringAtDelta cpPos ")"
                      else printStringAtDelta cpPos "#)"

  exactP p = printString "Pat"
   `debug` ("exactP.Pat:ignoring " ++ (showGhc p))

-- ---------------------------------------------------------------------

instance ExactP (GHC.HsType GHC.Name) where
  exactP typ = do
    return () `debug` ("exactP.HsType not implemented for " ++ showGhc (typ))
    printString "HsType.Name"


instance ExactP (GHC.HsType GHC.RdrName) where
-- HsForAllTy HsExplicitFlag (LHsTyVarBndrs name) (LHsContext name) (LHsType name)
  exactP (GHC.HsForAllTy f bndrs ctx typ) = do
    Just (AnnHsForAllTy opPos darrowPos cpPos) <- getAnnValue :: EP (Maybe AnnHsType)
    -- let [(Ann _ _ (AnnHsForAllTy opPos darrowPos cpPos))] = getAnn isAnnHsForAllTy ma "HsForAllTy"
    printStringAtMaybeDelta opPos "("
    exactPC ctx
    printStringAtMaybeDelta cpPos ")"
    printStringAtMaybeDelta darrowPos "=>"
    exactPC typ

  exactP (GHC.HsTyVar n) = printString (rdrName2String n)

  exactP (GHC.HsAppTy t1 t2) = exactPC t1 >> exactPC t2

  exactP (GHC.HsFunTy t1 t2) = do
    Just (AnnHsFunTy rarrowPos) <- getAnnValue :: EP (Maybe AnnHsType)
    -- let [(Ann _ _ (AnnHsFunTy rarrowPos))] = getAnn isAnnHsFunTy ma "HsFunTy"
    exactPC t1
    printStringAtDelta rarrowPos "->"
    exactPC t2

  exactP (GHC.HsParTy t1) = do
    Just (AnnHsParTy opPos cpPos) <- getAnnValue :: EP (Maybe AnnHsType)
    -- let [(Ann _ _ (AnnHsParTy opPos cpPos))] = getAnn isAnnHsParTy ma "HsParTy"
    printStringAtDelta opPos "("
    exactPC t1
    printStringAtDelta cpPos ")"

  exactP (GHC.HsTupleTy sort ts) = do
    Just (AnnHsTupleTy opPos cpPos) <- getAnnValue :: EP (Maybe AnnHsType)
    -- let [(Ann _ _ (AnnHsTupleTy opPos cpPos))] = getAnn isAnnHsTupleTy ma "HsTupleTy"
    let (ostr,cstr) = case sort of
          GHC.HsUnboxedTuple -> ("(#","#)")
          _ -> ("(",")")
    printStringAtDelta opPos ostr
    mapM_ exactPC ts
    printStringAtDelta cpPos cstr



{-
HsListTy (LHsType name)	 
HsPArrTy (LHsType name)	 
HsTupleTy HsTupleSort [LHsType name]	 
HsOpTy (LHsType name) (LHsTyOp name) (LHsType name)	 
HsIParamTy HsIPName (LHsType name)	 
HsEqTy (LHsType name) (LHsType name)	 
HsKindSig (LHsType name) (LHsKind name)	 
HsQuasiQuoteTy (HsQuasiQuote name)	 
HsSpliceTy (HsSplice name) PostTcKind	 
HsDocTy (LHsType name) LHsDocString	 
HsBangTy HsBang (LHsType name)	 
HsRecTy [ConDeclField name]	 
HsCoreTy Type	 
HsExplicitListTy PostTcKind [LHsType name]	 
HsExplicitTupleTy [PostTcKind] [LHsType name]	 
HsTyLit HsTyLit	 
HsWrapTy HsTyWrapper (HsType name)
-}

  exactP t = printString "HsType" `debug` ("exactP.LHSType:ignoring " ++ (showGhc t))


instance ExactP (GHC.HsContext GHC.RdrName) where
  exactP typs = do
    mapM_ exactPC typs

instance ExactP (GHC.GRHS GHC.RdrName (GHC.LHsExpr GHC.RdrName)) where
  exactP (GHC.GRHS guards expr) = do
    Just (AnnGRHS guardPos eqPos) <- getAnnValue
    printStringAtMaybeDelta guardPos "|"
    mapM_ exactPC guards
    printStringAtMaybeDelta eqPos "="
    exactPC expr

instance ExactP (GHC.StmtLR GHC.RdrName GHC.RdrName (GHC.LHsExpr GHC.RdrName)) where
  exactP (GHC.BodyStmt e _ _ _) = do
    Just an <- getAnnValue :: EP (Maybe AnnStmt)
    -- let [(Ann lcs _ an)] = getAnn isAnnStmtLR ma "StmtLR"
    exactPC e

  exactP (GHC.LetStmt lb) = do
    Just an <- getAnnValue :: EP (Maybe AnnStmt)
    -- let [(Ann lcs _ an)] = getAnn isAnnLetStmt ma "LetStmt"
    p <- getPos
    printStringAtMaybeDelta (ls_let an) "let" `debug` ("exactP.LetStmt:an=" ++ show an)
    exactP lb
    printStringAtMaybeDeltaP p (ls_in an) "in"


  exactP _ = printString "StmtLR"

-- ---------------------------------------------------------------------

instance ExactP (GHC.HsExpr GHC.RdrName) where
  exactP (GHC.HsVar v)           = exactP v
  exactP (GHC.HsIPVar v)         = exactP v
  exactP (GHC.HsOverLit lit)     = exactP lit
  exactP (GHC.HsLit lit)         = exactP lit
  exactP (GHC.HsLam match)       = exactPMatchGroup match
  exactP (GHC.HsLamCase _ match) = exactPMatchGroup match
  exactP (GHC.HsApp e1 e2)       = exactPC e1 >> exactPC e2
  exactP (GHC.OpApp e1 op _f e2) = exactPC e1 >> exactPC op >> exactPC e2
  exactP (GHC.NegApp e _)        = exactPC e

  exactP (GHC.HsPar e) = do
    Just (AnnHsPar od cd) <- getAnnValue

    printStringAtDelta od "("
    exactPC e
    printStringAtDelta cd ")"

  exactP (GHC.SectionL e1 e2)    = exactPC e1 >> exactPC e2
  exactP (GHC.SectionR e1 e2)    = exactPC e1 >> exactPC e2

  exactP (GHC.ExplicitTuple args b) = do
    Just (AnnExplicitTuple op cp) <- getAnnValue
    return () `debug` ("exactP.ExplicitTuple:" ++ show (AnnExplicitTuple op cp))
    if b == GHC.Boxed then printStringAtDelta op "("
                      else printStringAtDelta op "(#"

    mapM_ exactPC args `debug` ("exactP.ExplicitTuple")

    if b == GHC.Boxed then printStringAtDelta cp ")"
                      else printStringAtDelta cp "#)"

  exactP (GHC.HsCase e1 matches) = exactPC e1 >> exactPMatchGroup matches
  exactP (GHC.HsIf _ e1 e2 e3)   = exactPC e1 >> exactPC e2 >> exactPC e3
  exactP (GHC.HsMultiIf _ rhs)   = mapM_ exactPC rhs

  exactP (GHC.HsLet lb e)    = do
    Just an <- getAnnValue :: EP (Maybe AnnHsExpr)
    -- let [(Ann lcs _ an)] = getAnn isAnnHsLet ma "HsLet"
    p <- getPos
    printStringAtMaybeDelta (hsl_let an) "let" `debug` ("exactP.HsLet:an=" ++ show an)
    exactP lb
    printStringAtMaybeDeltaP p (hsl_in an) "in"
    exactPC e

  exactP (GHC.HsDo cts stmts _typ)    = do
    Just an <- getAnnValue :: EP (Maybe AnnHsExpr)
    -- let [(Ann lcs _ an)] = getAnn isAnnHsDo ma "HsDo"
    printStringAtMaybeDelta (hsd_do an) "do" `debug` ("exactP.HsDo:an=" ++ show an)
    mapM_ exactPC stmts

  exactP (GHC.ExplicitList _ _ es) = mapM_ exactPC es
  exactP (GHC.ExplicitPArr _ es)   = mapM_ exactPC es
  exactP (GHC.RecordCon _ _ (GHC.HsRecFields fs _)) = mapM_ exactPC fs
  exactP (GHC.RecordUpd e (GHC.HsRecFields fs _) cons _ _)  = exactPC e >> mapM_ exactPC fs
  exactP (GHC.ExprWithTySig e typ) = exactPC e >> exactPC typ
  exactP (GHC.ExprWithTySigOut e typ) = exactPC e >> exactPC typ

  exactP (GHC.ArithSeq _ _ seqInfo) = do
    Just (AnnArithSeq obPos mcPos ddPos cbPos) <- getAnnValue
    -- let [(Ann lcs _ (AnnArithSeq obPos mcPos ddPos cbPos))] = getAnn isAnnArithSeq ma "ArithSeq"
    printStringAtDelta obPos "["
    case seqInfo of
      GHC.From e1 -> exactPC e1 >> printStringAtDelta ddPos ".."
      GHC.FromTo e1 e2 -> do
        exactPC e1
        printStringAtDelta ddPos ".."
        exactPC e2
      GHC.FromThen e1 e2 -> do
        exactPC e1
        printStringAtMaybeDelta mcPos ","
        exactPC e2
        printStringAtDelta ddPos ".."
      GHC.FromThenTo e1 e2 e3 -> do
        exactPC e1
        printStringAtMaybeDelta mcPos ","
        exactPC e2
        printStringAtDelta ddPos ".."
        exactPC e3

    printStringAtDelta cbPos "]"

  exactP e = printString "HsExpr"
    `debug` ("exactP.HsExpr:not processing " ++ (showGhc e) )

-- ---------------------------------------------------------------------

instance ExactP (GHC.HsRecField GHC.RdrName (GHC.LHsExpr GHC.RdrName)) where
  exactP (GHC.HsRecField _ e _) = exactPC e

-- ---------------------------------------------------------------------

instance ExactP GHC.RdrName where
  exactP n = do
    printString (rdrName2String n)
    ma <- getAnnValue
    case ma of
      Just (AnnListItem mc) ->
        printStringAtMaybeDelta mc ","
      Nothing -> return ()

instance ExactP GHC.HsIPName where
  exactP (GHC.HsIPName n) = do
    printString (GHC.unpackFS n)
    Just (AnnListItem mc) <- getAnnValue
    printStringAtMaybeDelta mc ","

-- ---------------------------------------------------------------------

exactPMatchGroup :: (GHC.MatchGroup GHC.RdrName (GHC.LHsExpr GHC.RdrName))
                   -> EP ()
exactPMatchGroup (GHC.MG matches _ _ _)
  = mapM_ exactPC matches

-- ---------------------------------------------------------------------

instance ExactP (GHC.HsTupArg GHC.RdrName) where
  exactP (GHC.Missing _) = do
    Just (AnnListItem cPos) <- getAnnValue
    printStringAtMaybeDelta cPos ","
    return ()
  exactP (GHC.Present e) = do
    Just (AnnListItem cPos) <- getAnnValue
    exactPC e
    printStringAtMaybeDelta cPos ","

instance ExactP (GHC.HsLocalBinds GHC.RdrName) where
  exactP (GHC.HsValBinds (GHC.ValBindsIn binds sigs)) = do
    printMerged (GHC.bagToList binds) sigs
  exactP (GHC.HsValBinds (GHC.ValBindsOut binds sigs)) = printString "ValBindsOut"
  exactP (GHC.HsIPBinds binds) = printString "HsIPBinds"
  exactP (GHC.EmptyLocalBinds) = return ()


instance ExactP (GHC.Sig GHC.RdrName) where
  exactP (GHC.TypeSig lns typ) = do
    Just (AnnTypeSig dc) <- getAnnValue :: EP (Maybe AnnTypeSig)
    -- let [(Ann _ _ (AnnTypeSig dc))] = getAnn isAnnTypeSig ma "TypeSig"
    mapM_ exactPC lns
    printStringAtDelta dc "::"
    exactPC typ

  exactP _ = printString "Sig"

instance ExactP (GHC.HsOverLit GHC.RdrName) where
  -- exactP (Just [(Ann cs p an)]) _ = printString (ol_str an)
  exactP  l = do
    -- Just an <- getAnnValue :: EP (Maybe AnnOverLit)
    printString (showGhc l) -- ++AZ temporary until D412

-- ---------------------------------------------------------------------

-- ++AZ++ TODO: rework when D412 is accepted
instance ExactP GHC.HsLit where
  exactP lit = case lit of
    GHC.HsChar       src _   -> printString src
    GHC.HsCharPrim   src _   -> printString src
    GHC.HsString     src _   -> printString src
    GHC.HsStringPrim src _   -> printString src
    GHC.HsInt        src _   -> printString src
    GHC.HsIntPrim    src _   -> printString src
    GHC.HsWordPrim   src _   -> printString src
    GHC.HsInt64Prim  src _   -> printString src
    GHC.HsWord64Prim src _   -> printString src
    GHC.HsInteger    src _ _ -> printString src
    GHC.HsRat        (GHC.FL src _) _ -> printString src
    GHC.HsFloatPrim  (GHC.FL src _)   -> printString src
    GHC.HsDoublePrim (GHC.FL src _)   -> printString src

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


instance ExactP (GHC.TyClDecl GHC.RdrName) where
  exactP (GHC.FamDecl  _)         = printString "FamDecl"
  exactP (GHC.SynDecl  _ _ _ _)   = printString "SynDecl"

  exactP (GHC.DataDecl ln (GHC.HsQTvs ns tyVars) defn _) = do
    Just (AnnDataDecl eqDelta) <- getAnnValue :: EP (Maybe AnnTyClDecl)
    -- let [(Ann lcs _ (AnnDataDecl eqDelta))] = getAnn isAnnDataDecl ma "DataDecl"
    printString "data"
    exactPC ln
    printStringAtDelta eqDelta "="
    mapM_ exactPC tyVars
    exactP defn


  exactP (GHC.ClassDecl  _ _ _ _ _ _ _ _ _ _) = printString "ClassDecl"

-- ---------------------------------------------------------------------

instance ExactP (GHC.HsTyVarBndr GHC.RdrName) where
  exactP _ = printString "HsTyVarBndr"

-- ---------------------------------------------------------------------

instance ExactP (GHC.HsDataDefn GHC.RdrName) where
  exactP (GHC.HsDataDefn nOrD ctx mtyp mkind cons mderivs) = do
    mapM_ exactPC cons

-- ---------------------------------------------------------------------

instance ExactP [GHC.LConDecl GHC.RdrName] where
  exactP cons = mapM_ exactPC cons

-- ---------------------------------------------------------------------

instance ExactP (GHC.ConDecl GHC.RdrName) where
  exactP (GHC.ConDecl ln exp qvars ctx dets res _ _) = do
    Just (AnnConDecl mp) <- getAnnValue :: EP (Maybe AnnTyClDecl)
    -- let [(Ann lcs _ (AnnConDecl mp))] = getAnn isAnnConDecl ma "ConDecl"
    mapM_ exactPC ln
    printStringAtMaybeDelta mp "|"


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


