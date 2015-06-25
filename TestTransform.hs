{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Annotate
import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Utils

import Data.Data
import Data.Generics.Schemes

import HsExpr as GHC hiding (Stmt)
import qualified HsExpr as GHC (Stmt)
import HsBinds as GHC
import HsSyn hiding (Pat, Stmt)
import qualified HsSyn as GHC (Pat, Stmt)
import SrcLoc
import qualified SrcLoc as GHC
import qualified RdrName as GHC
import qualified ApiAnnotation as GHC
import qualified DynFlags      as GHC
import qualified FastString    as GHC
import qualified GHC           as GHC hiding (parseModule)
import qualified HeaderInfo    as GHC
import qualified Lexer         as GHC
import qualified MonadUtils    as GHC
import qualified Outputable    as GHC
import qualified Parser        as GHC
import qualified SrcLoc        as GHC
import qualified StringBuffer  as GHC
import qualified OccName as GHC
import Data.Generics
import Control.Monad.State

import qualified Data.Map as Map

import Debug.Trace

import Data.Monoid

import System.IO.Unsafe

import Control.Arrow

import Data.Maybe

import qualified Refact.Types as R
import Refact.Types hiding (SrcSpan)

import System.Environment
import System.Process
import System.Directory
import System.IO

main = do
  args <- getArgs
  case args of
    ["pipe", file] -> runPipe file
    ["test", file] -> runTest file

-- Types
--
type M a = State Anns a

type Module = (GHC.Located (GHC.HsModule GHC.RdrName))

type Expr = GHC.Located (GHC.HsExpr GHC.RdrName)

type Bind = HsBindLR  GHC.RdrName GHC.RdrName

type Type = GHC.Located (GHC.HsType GHC.RdrName)

type Decl = GHC.Located (GHC.HsDecl GHC.RdrName)

type Pat = GHC.LPat GHC.RdrName

type Stmt = ExprLStmt GHC.RdrName

mergeAnns :: Anns -> Anns -> Anns
mergeAnns (a, b) (c,d) = (Map.union a c, Map.union b d)

-- | Replaces an old expression with a new expression
replace :: AnnKey -> AnnKey -> Anns -> Maybe Anns
replace old new (as, keys) = do
  oldan <- Map.lookup old as
  newan <- Map.lookup new as
  return . (, keys) . Map.delete old . Map.insert new (combine oldan newan) $ as

combine :: Annotation -> Annotation -> Annotation
combine oldann newann =
  Ann (annEntryDelta oldann) (annDelta oldann) (annTrueEntryDelta oldann)
      (annPriorComments oldann ++ annPriorComments newann) (annsDP newann)


mkKey :: (Data a) => GHC.Located a -> AnnKey
mkKey (GHC.L l s) = AnnKey l (annGetConstr s) NotNeeded


-- | Shift the first output annotation into the correct place
moveAnns :: [(KeywordId, DeltaPos)] -> [(KeywordId, DeltaPos)] -> [(KeywordId, DeltaPos)]
moveAnns [] xs        = xs
moveAnns ((_, dp): _) ((kw, _):xs) = (kw,dp) : xs

-- Pipe

runPipe :: FilePath -> IO ()
runPipe file = do
  path <- canonicalizePath file
  rawhints <- getHints path
  let inp :: [Refactoring R.SrcSpan] = read rawhints
  print inp
  let inp' = fmap (toGhcSrcSpan file) <$> inp
  (anns, m) <- either (error . show) id <$> parseModule file
  let as = relativiseApiAnns m anns
      -- need a check here to avoid overlap
      (ares, res) = foldl (uncurry runRefactoring) (as, m) inp'
  putStrLn (exactPrintWithAnns res ares)

-- Run HLint to get the commands

makeCmd :: String -> String
makeCmd file = "hlint " ++ file ++ " --serialise"

getHints :: FilePath -> IO String
getHints file = do
  (_, Just hOut, _, hProc) <- createProcess (
                                (shell (makeCmd file))
                                { std_out = CreatePipe }
                              )
  exitCode <- waitForProcess hProc
  hGetContents hOut


toGhcSrcSpan :: FilePath -> R.SrcSpan -> SrcSpan
toGhcSrcSpan file R.SrcSpan{..} = mkSrcSpan (f start) (f end)
  where
    f (x,y) = mkSrcLoc (GHC.mkFastString file) x y

-- Perform the substitutions

runRefactoring :: Anns -> Module -> Refactoring GHC.SrcSpan -> (Anns, Module)
runRefactoring as m r@Replace{}  =
  case rtype r of
    Expr -> replaceWorker as m parseExpr doExprReplacement r
    Decl -> replaceWorker as m parseDecl doGenReplacement r
    Type -> replaceWorker as m parseType doGenReplacement r
    Pattern -> replaceWorker as m parsePattern doGenReplacement r
    Stmt -> replaceWorker as m parseStmt doGenReplacement r
runRefactoring (as,sk) m ModifyComment{..} =
    ((Map.map go as, sk), m)
    where
      go a@(Ann{ annPriorComments, annsDP }) =
        a { annsDP = map changeComment annsDP
          , annPriorComments = map (first change) annPriorComments }
      changeComment (AnnComment d, dp) = (AnnComment (change d), dp)
      changeComment e = e
      change old@(DComment dp s prov) = if s == originalComment
                                          then DComment dp newComment prov
                                          else old

type Repl a = (GHC.Located a -> Bool) -> GHC.Located a -> GHC.Located a -> M (GHC.Located a)

replaceWorker :: (Annotate a, Data a) => Anns -> Module
              -> Parser (GHC.Located a) -> Repl a
              -> Refactoring GHC.SrcSpan -> (Anns, Module)
replaceWorker as m p r Replace{..} =
  let replExprLocation = expr
      (newanns, template) = case (unsafePerformIO $ p orig) of
                              Right xs -> xs
                              Left err -> error (show err)
      relat = relativiseApiAnns template newanns
      (newExpr, newAnns) = runState (substTransform m subts template) (mergeAnns as relat)
      replacementPred (GHC.L l _) = l == replExprLocation
      transformation = everywhereM (mkM (r replacementPred newExpr))
      (final, finalanns) = runState (transformation m) newAnns
   in (finalanns, final)


-- Find the largest expression with a given SrcSpan
findGen :: forall ast . Data ast => Module -> SrcSpan -> GHC.Located ast
findGen m ss = snd $ runState (doTrans m) (error (showGhc ss))
  where
    doTrans :: Module -> State (GHC.Located ast) Module
    doTrans = everywhereM (mkM (findLargestExpression ss))

findExpr :: Module -> SrcSpan -> Expr
findExpr = findGen

findPat :: Module -> SrcSpan -> Pat
findPat = findGen

findType :: Module -> SrcSpan -> Type
findType = findGen

findDecl :: Module -> SrcSpan -> Decl
findDecl = findGen

findStmt :: Module -> SrcSpan -> Stmt
findStmt = findGen



findLargestExpression :: Data ast => SrcSpan -> GHC.Located ast -> State (GHC.Located ast) (GHC.Located ast)
findLargestExpression ss e@(GHC.L l _) =
  if l == ss
    then (e <$ put e)
    else return e

-- Deletion from a list

deleteFromList :: (Stmt -> Bool) -> [Stmt] -> [Stmt]
deleteFromList p xs = trace (showGhc xs) (filter p xs)

doDelete p = everywhere (mkT (deleteFromList p))


-- Substitute variables into templates

substTransform :: Data a => Module -> [(String, GHC.SrcSpan)] -> a -> M a
substTransform m ss = everywhereM (mkM (exprSub m ss)
                                    `extM` typeSub m ss
                                    `extM` patSub m ss
                                    `extM` stmtSub m ss)

stmtSub :: Module -> [(String, GHC.SrcSpan)] -> Stmt -> M Stmt
stmtSub m subs old@(GHC.L l (BodyStmt (GHC.L l2 (HsVar name)) _ _ _) ) =
  resolveRdrName (findStmt m) old subs name
stmtSub _ _ e = return e

patSub :: Module -> [(String, GHC.SrcSpan)] -> Pat -> M Pat
patSub m subs old@(GHC.L l (VarPat name)) =
  (resolveRdrName (findPat m) old subs name)
patSub _ _ e = return e

typeSub :: Module -> [(String, GHC.SrcSpan)] -> Type -> M Type
typeSub m subs old@(GHC.L l (HsTyVar name)) =
  (resolveRdrName (findType m) old subs name)
typeSub _ _ e = return e

exprSub :: Module -> [(String, GHC.SrcSpan)] -> Expr -> M Expr
exprSub m subs old@(GHC.L l (HsVar name)) =
  resolveRdrName (findExpr m) old subs name
exprSub _ _ e = return e

resolveRdrName :: Data a
               => (SrcSpan -> GHC.Located a) -> GHC.Located a
               -> [(String, GHC.SrcSpan)] -> GHC.RdrName -> M (GHC.Located a)
resolveRdrName f old subs name =
  case name of
    -- Todo: this should replace anns as well?
    GHC.Unqual (showGhc -> oname)
      -> case (lookup oname subs) of
              Just (f -> new) -> modifyAnnKey old new
              Nothing -> return old
    _ -> return old


-- Test

runTest :: FilePath -> IO ()
runTest file = do
  Right (anns, m) <- parseModule file
  let as = relativiseApiAnns m anns
      (tm, tanns) = case 6 of
                      1 -> runState (removeBracketsT m) as
                      2 -> replaceExpr "(a + b)" m as
                      3 -> (etaTransform m, as)
                      4 -> runState (transformRedundantDo m) as
                      5 -> (m, (removeComment (const False)) as)
                      6 ->  addImport "import Data.Foldable" m as
  putStrLn  $ showAnnData as 0 m
  putStrLn  $ showAnnData tanns 0 tm
  putStrLn (exactPrintWithAnns m as)
  putStrLn (exactPrintWithAnns tm tanns)
  return ()


-- 1 -- Remove brackets
--
removeBracketsT = everywhereM (mkM $ removeBrackets (const True))


removeBrackets :: (GHC.SrcSpan -> Bool) -> (GHC.Located (GHC.HsExpr GHC.RdrName)) -> M (GHC.Located (GHC.HsExpr GHC.RdrName))
removeBrackets p v@(GHC.L t (GHC.HsPar e)) =
  if p t
    then modifyAnnKey v e
    else return v
removeBrackets _ e = return e

modifyAnnKey e1 e2 = e2 <$ modify (\m -> replaceAnnKey m e1 e2)


replaceAnnKey :: (Data old, Data new)
  => Anns -> GHC.Located old -> GHC.Located new -> Anns
replaceAnnKey a old new =
  case replace (mkAnnKey old) (mkAnnKey new) a  of
    Nothing -> a
    Just a' -> a'

-- 2 -- Replace expression

replaceExpr :: String -> Module -> Anns -> (Module, Anns)
replaceExpr expr ast anns  =
  let Right (newanns, hsexpr) = (unsafePerformIO $ parseExpr expr)
      relat = relativiseApiAnns hsexpr newanns
      transformation = everywhereM (mkM (doExprReplacement (fourliteral) hsexpr))
      (final, finalanns) = runState (transformation ast) anns
  in (final, mergeAnns finalanns relat)

doGenReplacement :: Data ast
              => (GHC.Located ast -> Bool)
              -> GHC.Located ast
              -> GHC.Located ast
              -> M (GHC.Located ast)
doGenReplacement p new old =
  if p old then modifyAnnKey old new else return old

doExprReplacement :: (Expr -> Bool) -> Expr -> Expr -> M Expr
doExprReplacement = doGenReplacement


fourliteral (GHC.L l (GHC.HsOverLit {})) = True
fourliteral _ = False

-- 3
-- eta transformation

etaTransform = everywhere (mkT etaReduce)


etaReduce :: Match GHC.RdrName (LHsExpr GHC.RdrName)
          -> Match GHC.RdrName (LHsExpr GHC.RdrName)
etaReduce f@(GHC.Match { m_pats }) = traceShow (length m_pats) new
  where
    new = f { m_pats = init m_pats, m_grhss = newgrsh}
    newgrsh = (m_grhss f) { grhssGRHSs = getMeat (grhssGRHSs (m_grhss f)) }
    getMeat xs = map (fmap handle) xs
    handle (GRHS xs (GHC.L l (HsApp e _))) = GRHS xs e
    handle _ = error "error"

-- 4
-- redundant do
--
transformRedundantDo = everywhereM (mkM (redundantDo (const True)))

redundantDo :: (Expr -> Bool) -> Expr -> M Expr
redundantDo p e@(GHC.L l (HsDo _ stmts _)) = do
  case stmts of
    [e'@(GHC.L l' (BodyStmt b _ _ _))] -> modifyAnnKey e b
    _ -> return e
redundantDo p e = return e


-- 5
-- remove comments
--
-- This is quite complicated as we must update the entry deltas depending
-- on which comments are deleted.
--
-- We want to maintain the invariants
-- 2. If we delete a non-last comment then we must modify the following
-- comment delta
-- 3. If we delete the final comment then we must modify the
-- annEntryDelta
--
-- Internal comments
-- 1. Internal comments can't appear last
-- 2. So we just need to update the next delta

removeComment :: (DComment -> Bool) -> Anns -> Anns
removeComment p (as, sk) = dropDoubleSpaces (Map.map go as, sk)
  where
    go a@(Ann { annPriorComments, annsDP, annEntryDelta, annTrueEntryDelta})
      = a { annEntryDelta = if (newr == oldr)
                              then DP (newr, newc)
                              else DP (newr, newc)
          , annPriorComments = newPcomments
          , annsDP = newanns annsDP }
      where
        DP (oldr, oldc) = annTrueEntryDelta
        (DP (newr, newc), newPcomments) = newprior annEntryDelta annPriorComments
        newprior :: DeltaPos -> [(DComment, DeltaPos)] -> (DeltaPos, [(DComment, DeltaPos)])
        newprior d [] = (d, [])
        newprior d [(x@(DComment e _ _), k)] = if p x then (d, [(x,k)])
                                                      else traceShowId(addDP e d, [])
        newprior d (e@(comment,o):y:xs) =
          if p comment then let (d', cs) = newprior d (y:xs) in (d', e:cs)
                       else newprior d (traceShowId (fiddle e y: xs))

        newanns :: [(KeywordId, DeltaPos)] -> [(KeywordId, DeltaPos)]
        newanns [] = []
        newanns [x] = [x]
        newanns (x:y:xs) =
            case x of (AnnComment d, start) -> if p d then x : newanns (y:xs)
                                                  else newanns (fiddle (d, start) y:xs)
                      _ -> x : newanns (y:xs)

        fiddle :: (DComment, DeltaPos) -> (a, DeltaPos) -> (a, DeltaPos)
        fiddle (DComment end _ _, start) (ann, off) = (ann, addDP end off)

dropDoubleSpaces :: Anns -> Anns
dropDoubleSpaces (as, sk) = (Map.map go as, sk)
  where
    go a@(Ann { annEntryDelta, annPriorComments, annsDP }) =
      a { annEntryDelta = process annEntryDelta
        , annPriorComments = map (fmap process) annPriorComments
        , annsDP = map (fmap process) annsDP}
    process (DP (l,c)) = if l >= 3 then DP (2, c) else DP (l, c)


-- 6
-- add import

addImport :: String -> Module -> Anns -> (Module, Anns)
addImport s mod@(GHC.L l m@(GHC.HsModule name exp imports decls depr _haddock)) anns =
  let Right (newanns, newimport) = (unsafePerformIO $ parseImport s)
      newimports = imports ++ [newimport]
      relat = mergeAnns anns (relativiseApiAnns newimport newanns)
      final = m { hsmodImports = newimports }
      -- Moving comments
      finalanns =
        if or [isJust name, isJust exp, not . null $ imports , isJust depr]
          then relat
          else case decls of
                 [] -> relat -- Comments associated with top level module
                 (x:xs) -> moveComments x newimport relat

  in (GHC.L l final, setInitialDelta (DP (0,0)) newimport finalanns)

moveComments :: (Data ast, Data ast2) => GHC.Located ast -> GHC.Located ast2 -> Anns -> Anns
moveComments (mkAnnKey -> from) (mkAnnKey -> to) (as, sks) =
  let old = Map.lookup from as
      new = Map.lookup to as
      final = fromMaybe as $ do
                v <- old
                v' <- new
                return . Map.adjust (\a -> a { annPriorComments = [] }) from
                       . Map.adjust
                          (\a -> a { annPriorComments =
                                     annPriorComments v ++ annPriorComments v'} ) to
                       $ as
  in (final, sks)


setInitialDelta :: Data ast => DeltaPos -> GHC.Located ast -> Anns -> Anns
setInitialDelta dp (mkAnnKey -> key) (as,sk) = (Map.adjust set key as, sk)
  where
    set a@(Ann { annEntryDelta } ) =
      let DP (c,l) = annEntryDelta
          newdelta = if c == 0 then DP (1, l) else annEntryDelta
      in a { annEntryDelta = newdelta}

