{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns    #-}
{-# LANGUAGE RecordWildCards #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.GHC.ExactPrint.Transform
--
-----------------------------------------------------------------------------
module Language.Haskell.GHC.ExactPrint.Transform
        (
        -- * The Transform Monad
          Transform
        , runTransform
        , logTr
        , getAnnsT, putAnnsT

        -- * Operations
        , uniqueSrcSpan
        , isUniqueSrcSpan

        , adjustAnnOffset
        , mergeAnns
        , mergeAnnList
        , setLocatedAnns
        , setPrecedingLines
        , addSortKeyBefore

        -- * Utility
        , Parser
        , parseToAnnotated

        , parseModule
        , parseExpr
        , parseImport
        , parseType
        , parseDecl
        , parsePattern
        , parseStmt

        , parseWith
        , withDynFlags
        ) where

import Language.Haskell.GHC.ExactPrint.Annotate
import Language.Haskell.GHC.ExactPrint.Delta
import Language.Haskell.GHC.ExactPrint.Preprocess
import Language.Haskell.GHC.ExactPrint.Internal.Types
import Language.Haskell.GHC.ExactPrint.Utils

import Control.Monad.RWS
import Control.Monad.State
import Data.List

import Data.Data

import GHC.Paths (libdir)

import qualified ApiAnnotation as GHC
import qualified DynFlags      as GHC
import qualified FastString    as GHC
import qualified GHC           as GHC hiding (parseModule)
import qualified HeaderInfo    as GHC
import qualified Lexer         as GHC
import qualified MonadUtils    as GHC
import qualified Outputable    as GHC
import qualified Parser        as GHC
import qualified RdrHsSyn      as GHC ( checkPattern )
import qualified SrcLoc        as GHC
import qualified StringBuffer  as GHC
import qualified Data.Generics as SYB

import qualified OrdList as OL

import qualified Data.Map as Map

import Control.Monad.Trans.Free

import Distribution.Helper

import Debug.Trace

------------------------------------------------------------------------------
-- Transformation of source elements

-- | Monad type for updating the AST and managing the annotations at the same
-- time. The W state is used to generate logging information if required.
type Transform a = RWS () [String] (Anns,Int) a

runTransform :: Anns ->Transform a -> (a,(Anns,Int),[String])
runTransform ans f = runRWS f () (ans,0)

logTr :: String -> Transform ()
logTr str = tell [str]

getAnnsT :: Transform Anns
getAnnsT = gets fst

putAnnsT :: Anns -> Transform ()
putAnnsT ans = do
  (_,col) <- get
  put (ans,col)

-- ---------------------------------------------------------------------

-- TODO: do we have to match the filename for GHC compare functions?
uniqueSrcSpan :: Transform GHC.SrcSpan
uniqueSrcSpan = do
  (an,col) <- get
  put (an,col + 1 )
  let pos = GHC.mkSrcLoc (GHC.mkFastString "ghc-exactprint") (-1) col
  return $ GHC.mkSrcSpan pos pos

isUniqueSrcSpan :: GHC.SrcSpan -> Bool
isUniqueSrcSpan ss = srcSpanStartLine ss == -1

-- ---------------------------------------------------------------------

adjustAnnOffset :: ColDelta -> Annotation -> Annotation
adjustAnnOffset (ColDelta cd) (Ann (DP (ro,co)) (ColDelta ad) _ cs kds) = Ann edp cd' edp cs kds'
  where
    edp = case ro of
      0 -> DP (ro,co)
      _ -> DP (ro,co - cd)
    cd' = ColDelta (ad - cd)
    kds' = fmap adjustEntrySpan kds
    adjustEntrySpan (AnnSpanEntry,dp) =
      case dp of
        DP (0,c) -> (AnnSpanEntry,DP (0,c))
        DP (r,c) -> (AnnSpanEntry,DP (r, c - cd))
    adjustEntrySpan x = x

-- ---------------------------------------------------------------------

-- | Left bias pair union
mergeAnns :: Anns -> Anns -> Anns
mergeAnns (a, b) (c,d) = (Map.union a c, Map.union b d)

mergeAnnList :: [Anns] -> Anns
mergeAnnList [] = error "mergeAnnList must have at lease one entry"
mergeAnnList (x:xs) = foldr mergeAnns x xs

-- ---------------------------------------------------------------------

-- |Update the DeltaPos for the given annotation keys
setLocatedAnns :: (SYB.Data a) => Anns -> [(GHC.Located a,Annotation)] -> Anns
setLocatedAnns anne kvs = foldl' setLocatedAnn anne kvs

setLocatedAnn :: (SYB.Data a) => Anns -> (GHC.Located a, Annotation) ->  Anns
setLocatedAnn aane (loc, annVal) = setAnn aane (mkAnnKey loc,annVal)

-- |Update the DeltaPos for the given annotation key/val
setAnn :: Anns -> (AnnKey, Annotation) -> Anns
setAnn (anne,sortKeys) (k, Ann dp col edp cs _) = case
  Map.lookup k anne of
    Nothing               -> (Map.insert k (Ann dp col edp cs []) anne,sortKeys)
    Just (Ann _ _ _ _ ks) -> (Map.insert k (Ann dp col edp cs ks) anne,sortKeys)

-- | Adjust the entry annotations to provide an `n` line preceding gap
setPrecedingLines :: (SYB.Data a) => Anns -> GHC.Located a -> Int -> Int -> Anns
setPrecedingLines (anne,sk) ast n c =
  case Map.lookup (mkAnnKey ast) anne of
    Nothing                      -> (Map.insert (mkAnnKey ast) (Ann (DP (n,c)) (ColDelta c) (DP (n,c)) []  []) anne,sk)
    Just (Ann ed cd _ted cs dps) -> (Map.insert (mkAnnKey ast) (Ann (DP (n,c)) cd (DP (n,c)) cs dps) anne,sk)

-- ---------------------------------------------------------------------

-- |Add a sort key for the first item to come before the second
addSortKeyBefore :: Anns -> GHC.Located a -> GHC.Located b -> Anns
addSortKeyBefore (anne,sk) (GHC.L l1 _) (GHC.L l2 _) = (anne,sk')
  where
    (other,sk2) = case Map.lookup l2 sk of
      Just k -> (k,sk)
      Nothing -> (ss2SortKey l2,Map.insert l2 (ss2SortKey l2) sk)
    sk' = Map.insert l1 (sortKeyBefore other) sk2

-- ---------------------------------------------------------------------

interpretChange :: GHC.SrcSpan -> GHC.SrcSpan -> Annotated a ->
                   FB a
interpretChange old new = iterTM go
  where
    go :: AnnotationF (FB a) -> FB a
    go (MarkEOF next) =
      change GHC.AnnEofPos >> next
    go (MarkPrim kwid _ next) =
      change kwid  >> next
      -- let annString = fromMaybe (keywordToString kwid) mstr in
      --   printStringAtMaybeAnn (G kwid) annString >> next
    go (MarkOutside _ (G kwid) next) =
      change kwid  >> next
    go (MarkOutside _ _ next) =
      next
    go (MarkInside akwid next) =
      change akwid >> next
    go (MarkMany akwid next) =
      change akwid >> next
    go (MarkOffsetPrim kwid _ _ next) =
      change kwid >> next
    go (MarkAfter akwid next) =
      change akwid >> next
    go (WithAST _ _ _ action next) =
      (interpretChange old new action) >> next
    go (CountAnns _ next) = do
      next 0
    go (SetLayoutFlag action next) =
      (interpretChange old new action) >> next
    go (MarkExternal _ akwid _ next) =
      change akwid  >> next
    go (StoreOriginalSrcSpan ss d next) = next (ss,d)
    go (GetSrcSpanForKw _ next) = next GHC.noSrcSpan
    go (StoreString _ _ next) = next
    go (GetNextDisambiguator next) = return NotNeeded >>= next
    go (AnnotationsToComments _ next) = next

    change :: GHC.AnnKeywordId -> FB ()
    change kwid = do
      (as, cs) <- get
      let r = Map.lookup (old, kwid) as
      case r of
        Nothing -> return ()
        Just v  -> do
          traceShowM (old, kwid)
          let as' = Map.insert (new,kwid) v (Map.delete (old, kwid) as)
          put (as', cs)


-- |There are a number of bugs in GHC 7.10.1 which result in incorrect
-- SrcSpans being allocated to AST elements, such that they do not
-- include all the annotated items in the SrcSpan.
-- See https://ghc.haskell.org/trac/ghc/ticket/10207
-- and https://ghc.haskell.org/trac/ghc/ticket/10209
-- and https://ghc.haskell.org/trac/ghc/ticket/10214
type FB a = State GHC.ApiAnns a
 -- runState  :: State s a -> s -> (a, s)
fixBugsInAst :: (SYB.Data t) => GHC.ApiAnns -> t -> (GHC.ApiAnns,t)
fixBugsInAst anns t = (anns',t')
  where
    (t',anns') = runState f anns

    -- Note: bottom up
    f = SYB.everywhereM (SYB.mkM parStmtBlock `SYB.extM` parStmt
                                              `SYB.extM` hsKind) t

    -- ---------------------------------

    changeAnnSpan :: Annotate ast => GHC.Located ast -> GHC.SrcSpan -> GHC.SrcSpan -> FB ()
    changeAnnSpan ast old new = do
      interpretChange old new (annotate ast)


    addAnnotation :: GHC.SrcSpan -> GHC.SrcSpan -> GHC.AnnKeywordId -> FB ()
    addAnnotation parent loc kw = do
      (anKW,anComments) <- get
      let anKW' = Map.insertWith (++) (parent,kw) [loc] anKW
      put (anKW',anComments)

    -- ---------------------------------

    parStmtBlockSpan :: GHC.ParStmtBlock GHC.RdrName GHC.RdrName -> GHC.SrcSpan
    parStmtBlockSpan (GHC.ParStmtBlock []    _ _) = GHC.noSrcSpan -- Should never happen
    parStmtBlockSpan (GHC.ParStmtBlock stmts _ _) = GHC.combineLocs (head stmts) (last stmts)

    parStmt :: GHC.Located (GHC.StmtLR GHC.RdrName GHC.RdrName (GHC.GenLocated GHC.SrcSpan (GHC.HsExpr GHC.RdrName)))
        -> FB (GHC.Located (GHC.StmtLR GHC.RdrName GHC.RdrName (GHC.GenLocated GHC.SrcSpan (GHC.HsExpr GHC.RdrName))))
    parStmt ps@(GHC.L _(GHC.ParStmt [] _ _)) = return ps
    parStmt (GHC.L _ ps@(GHC.ParStmt pbs _ _)) = do
      let ss = GHC.combineSrcSpans (parStmtBlockSpan $ head pbs) (parStmtBlockSpan $ last pbs)
      return (GHC.L ss ps)
    parStmt a@(GHC.L ss ast@GHC.TransStmt{..}) =
      let ss' = GHC.combineLocs (head trS_stmts) trS_using in
        changeAnnSpan a ss ss' >> return (GHC.L ss' ast)
    parStmt x = return x

    -- ---------------------------------

    parStmtBlock :: GHC.GenLocated GHC.SrcSpan (GHC.ParStmtBlock GHC.RdrName GHC.RdrName)
                 -> FB (GHC.GenLocated GHC.SrcSpan (GHC.ParStmtBlock GHC.RdrName GHC.RdrName))
    parStmtBlock psb@(GHC.L _  (GHC.ParStmtBlock []    _  _ )) = return psb
    parStmtBlock    a@(GHC.L ss (GHC.ParStmtBlock stmts ns se)) = do
      changeAnnSpan a ss ss'
      return (GHC.L ss' (GHC.ParStmtBlock stmts ns se))
      where
        ss' = GHC.combineLocs (head stmts) (last stmts)

    -- ---------------------------------

    hsKind :: (GHC.GenLocated GHC.SrcSpan (GHC.HsType GHC.RdrName))
           -> FB (GHC.GenLocated GHC.SrcSpan (GHC.HsType GHC.RdrName))
    hsKind a@(GHC.L ss k) = do
      changeAnnSpan a ss ss'
      addAnnotation ss' ss GHC.AnnEofPos
      return (GHC.L ss' k)
      where
        ss' = case GHC.getAnnotation anns ss GHC.AnnDcolon of
          []     -> ss
          (ld:_) -> GHC.combineSrcSpans ld ss

-- ---------------------------------------------------------------------

parseToAnnotated :: (Show a, Annotate ast)
                 => GHC.DynFlags
                 -> FilePath
                 -> (GHC.DynFlags -> FilePath -> String -> Either a (GHC.ApiAnns, GHC.Located ast))
                 -- -> Parser
                 -> String
                 -> (GHC.Located ast, Anns)
parseToAnnotated df fp parser src = (ast,anns)
  where
    (ghcAnns, ast) = case (parser df fp src) of
                            Right xs -> xs
                            Left err -> error (show err)
    anns = relativiseApiAnns ast ghcAnns

-- ---------------------------------------------------------------------

runParser :: GHC.P a -> GHC.DynFlags -> FilePath -> String -> GHC.ParseResult a
runParser parser flags filename str = GHC.unP parser parseState
    where
      location = GHC.mkRealSrcLoc (GHC.mkFastString filename) 1 1
      buffer = GHC.stringToStringBuffer str
      parseState = GHC.mkPState flags buffer location

-- ---------------------------------------------------------------------

withDynFlags :: (GHC.DynFlags -> a) -> IO a
withDynFlags action =
  GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut $
    GHC.runGhc (Just libdir) $ do
      dflags <- GHC.getSessionDynFlags
      void $ GHC.setSessionDynFlags dflags
      return (action dflags)

-- ---------------------------------------------------------------------

parseFile :: GHC.DynFlags -> FilePath -> String -> GHC.ParseResult (GHC.Located (GHC.HsModule GHC.RdrName))
parseFile = runParser GHC.parseModule

-- ---------------------------------------------------------------------

parseWith :: GHC.DynFlags
          -> FilePath
          -> GHC.P w
          -> String
          -> Either (GHC.SrcSpan, String) (GHC.ApiAnns, w)
parseWith dflags fileName parser s =
  case runParser parser dflags fileName s of
    GHC.PFailed ss m                    -> Left (ss, GHC.showSDoc dflags m)
    GHC.POk (mkApiAnns -> apianns) pmod -> Right (apianns, pmod)

-- ---------------------------------------------------------------------

type Parser a = GHC.DynFlags -> FilePath -> String
                -> Either (GHC.SrcSpan, String)
                          (GHC.ApiAnns, a)

parseExpr :: Parser (GHC.LHsExpr GHC.RdrName)
parseExpr df fp = parseWith df fp GHC.parseExpression

parseImport :: Parser (GHC.LImportDecl GHC.RdrName)
parseImport df fp = parseWith df fp GHC.parseImport

parseType :: Parser (GHC.LHsType GHC.RdrName)
parseType df fp = parseWith df fp GHC.parseType

-- safe, see D1007
parseDecl :: Parser (GHC.LHsDecl GHC.RdrName)
parseDecl df fp = parseWith df fp (head . OL.fromOL <$> GHC.parseDeclaration)

parseStmt :: Parser (GHC.ExprLStmt GHC.RdrName)
parseStmt df fp = parseWith df fp GHC.parseStatement

-- Interim, see D1005
-- will not parse bang patterns properly
parsePattern :: Parser (GHC.LPat GHC.RdrName)
parsePattern df fp = parseWith df fp (GHC.parseExpression >>= GHC.checkPattern GHC.empty)
-- parsePattern df fp = parseWith df fp GHC.parsePattern

-- ---------------------------------------------------------------------
--
generateMacroFile :: IO ()
generateMacroFile = writeAutogenFiles "dist/"


parseModule :: FilePath -> IO (Either (GHC.SrcSpan, String) (Anns, (GHC.Located (GHC.HsModule GHC.RdrName))))
parseModule file = do
  generateMacroFile
  GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut $
    GHC.runGhc (Just libdir) $ do
      dflags <- initDynFlags file
      let useCpp = GHC.xopt GHC.Opt_Cpp dflags
      (fileContents, injectedComments) <-
        if useCpp
          then do
            let macros = (Just "dist/build/autogen/cabal_macros.h")
            contents <- getPreprocessedSrcDirect macros file
            cppComments <- getCppTokensAsComments macros file
            return (contents,cppComments)
          else do
            txt <- GHC.liftIO $ readFile file
            let (contents1,lp) = stripLinePragmas txt
            return (contents1,lp)
      return $
        case parseFile dflags file fileContents of
          GHC.PFailed ss m -> Left $ (ss, (GHC.showSDoc dflags m))
          GHC.POk (mkApiAnns -> apianns) pmod  ->
            let as = relativiseApiAnnsWithComments injectedComments pmod apianns in
            Right $ (as, pmod)

-- ---------------------------------------------------------------------

initDynFlags :: GHC.GhcMonad m => FilePath -> m GHC.DynFlags
initDynFlags file = do
  dflags0 <- GHC.getSessionDynFlags
  let dflags1 = GHC.gopt_set dflags0 GHC.Opt_KeepRawTokenStream
  src_opts <- GHC.liftIO $ GHC.getOptionsFromFile dflags1 file
  (dflags2, _, _)
    <- GHC.parseDynamicFilePragma dflags1 src_opts
  void $ GHC.setSessionDynFlags dflags2
  return dflags2

-- ---------------------------------------------------------------------

mkApiAnns :: GHC.PState -> GHC.ApiAnns
mkApiAnns pstate
  = ( Map.fromListWith (++) . GHC.annotations $ pstate
    , Map.fromList ((GHC.noSrcSpan, GHC.comment_q pstate) : (GHC.annotations_comments pstate)))
