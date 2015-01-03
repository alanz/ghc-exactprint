{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
-- | Use "runhaskell Setup.hs test" or "cabal test" to run these tests.
module Main where

import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Utils

import GHC.Paths ( libdir )

import qualified DynFlags      as GHC
import qualified FastString    as GHC
import qualified GHC           as GHC
import qualified MonadUtils    as GHC
import qualified Outputable    as GHC

import qualified GHC.SYB.Utils as SYB

import Control.Applicative
import Control.Monad
import Control.Monad.Trans
import Data.Generics
import Data.List
import System.Directory
import System.FilePath
import System.IO

import Test.HUnit

import qualified Data.Map as Map

import Debug.Trace

debug :: c -> String -> c
debug = flip trace


main :: IO ()
main = do
{-
    manipulateAstTest "examples/LetStmt.hs"               "Layout.LetStmt"
    manipulateAstTest "examples/LetExpr.hs"               "LetExpr"
    manipulateAstTest "examples/ExprPragmas.hs"           "ExprPragmas"
    manipulateAstTest "examples/ListComprehensions.hs"    "Main"
    manipulateAstTest "examples/MonadComprehensions.hs"   "Main"
    manipulateAstTest "examples/FunDeps.hs"               "Main"
    manipulateAstTest "examples/ImplicitParams.hs"        "Main"
    manipulateAstTest "examples/RecursiveDo.hs"           "Main"
    manipulateAstTest "examples/TypeFamilies.hs"          "Main"
    manipulateAstTest "examples/MultiParamTypeClasses.hs" "Main"
    manipulateAstTest "examples/DataFamilies.hs"          "DataFamilies"
    manipulateAstTest "examples/Deriving.hs"              "Main"
    manipulateAstTest "examples/Default.hs"               "Main"
    manipulateAstTest "examples/ForeignDecl.hs"           "ForeignDecl"
    manipulateAstTest "examples/Warning.hs"               "Warning"
    manipulateAstTest "examples/Deprecation.hs"           "Deprecation"
    manipulateAstTest "examples/Annotations.hs"           "Annotations"
    manipulateAstTest "examples/DocDecls.hs"              "DocDecls"
    manipulateAstTestTH "examples/QuasiQuote.hs"          "QuasiQuote"
    manipulateAstTest "examples/Roles.hs"                 "Roles"
    manipulateAstTest "examples/Splice.hs"                "Splice"
    manipulateAstTest "examples/ImportsSemi.hs"           "ImportsSemi"
    manipulateAstTest "examples/Stmts.hs"                 "Stmts"
    manipulateAstTest "examples/LetExprSemi.hs"           "LetExprSemi"
    manipulateAstTest "examples/Mixed.hs"                 "Main"
    manipulateAstTest "examples/EmptyMostly.hs"           "EmptyMostly"
    manipulateAstTest "examples/Arrow.hs"                 "Arrow"
    manipulateAstTest "examples/PatSynBind.hs"            "Main"
    manipulateAstTest "examples/HsDo.hs"                  "HsDo"
    manipulateAstTest "examples/ForAll.hs"                "ForAll"
    manipulateAstTest "examples/PArr.hs"                  "PArr"
    manipulateAstTest "examples/DataDecl.hs"              "Main"
    manipulateAstTest "examples/ViewPatterns.hs"          "Main"
    manipulateAstTest "examples/BangPatterns.hs"          "Main"
    manipulateAstTest "examples/Associated.hs"            "Main"
    manipulateAstTest "examples/Infix.hs"                 "Main"
    manipulateAstTest "examples/Move1.hs"                 "Move1"
    manipulateAstTest "examples/Rules.hs"                 "Rules"
    manipulateAstTest "examples/TypeOperators.hs"         "Main"
    manipulateAstTest "examples/NullaryTypeClasses.hs"    "Main"
    manipulateAstTest "examples/FunctionalDeps.hs"        "Main"
    manipulateAstTest "examples/DerivingOC.hs"            "Main"
    manipulateAstTest "examples/GenericDeriving.hs"       "Main"
    manipulateAstTest "examples/OverloadedStrings.hs"     "Main"
    manipulateAstTest "examples/RankNTypes.hs"            "Main"
    manipulateAstTest "examples/Existential.hs"           "Main"
    manipulateAstTest "examples/ScopedTypeVariables.hs"   "Main"
    manipulateAstTest "examples/Arrows.hs"                "Main"
    manipulateAstTest "examples/TH.hs"                    "Main"
    manipulateAstTest "examples/StaticPointers.hs"        "Main"
    manipulateAstTest "examples/DataDecl.hs"              "Main"
    manipulateAstTest "examples/Guards.hs"                "Main"
    manipulateAstTest "examples/RebindableSyntax.hs"      "Main"
    manipulateAstTest "examples/RdrNames.hs"              "RdrNames"
    manipulateAstTest "examples/Vect.hs"                  "Vect"
    manipulateAstTest "examples/Tuple.hs"                 "Main"
    manipulateAstTest "examples/ExtraConstraints1.hs"     "ExtraConstraints1"
    manipulateAstTest "examples/AddAndOr3.hs"             "AddAndOr3"
    manipulateAstTest "examples/Ann01.hs"                 "Ann01"
    manipulateAstTest "examples/StrictLet.hs"             "Main"
    manipulateAstTest "examples/Cg008.hs"                 "Cg008"
    manipulateAstTest "examples/T2388.hs"                 "T2388"
    manipulateAstTest "examples/T3132.hs"                 "T3132"
    manipulateAstTest "examples/Stream.hs"                "Stream"
    manipulateAstTest "examples/Trit.hs"                  "Trit"
    manipulateAstTest "examples/Dead1.hs"                 "Dead1"
    manipulateAstTest "examples/Sigs.hs"                  "Sigs"
    manipulateAstTest "examples/Cpp.hs"                   "Main"
-}
    manipulateAstTest "examples/Lhs.lhs"                  "Main"

{-
    manipulateAstTest "examples/ParensAroundContext.hs"   "ParensAroundContext"
    manipulateAstTest "examples/EmptyMostlyInst.hs"       "EmptyMostlyInst"
    -- manipulateAstTest "examples/Foo.hs"                   "Main"
    manipulateAstTest "examples/EmptyMostly2.hs"          "EmptyMostly2"
-}

-- | Where all the tests are to be found
examplesDir :: FilePath
examplesDir = "tests" </> "examples"

examplesDir2 :: FilePath
examplesDir2 = "examples"

manipulateAstTest :: FilePath -> String -> IO ()
manipulateAstTest file modname = manipulateAstTest' False file modname

manipulateAstTestTH :: FilePath -> String -> IO ()
manipulateAstTestTH file modname = manipulateAstTest' True file modname

manipulateAstTest' :: Bool -> FilePath -> String -> IO ()
manipulateAstTest' useTH file modname = do
  let out    = file <.> "exactprinter" <.> "out"

  contents <- readUTF8File file
  (ghcAnns,t) <- parsedFileGhc file modname useTH
  let
    parsed@(GHC.L l hsmod) = GHC.pm_parsed_source $ GHC.tm_parsed_module t
    parsedAST = SYB.showData SYB.Parser 0 parsed
    -- parsedAST = showGhc parsed
       -- `debug` ("getAnn:=" ++ (show (getAnnotationValue (snd ann) (GHC.getLoc parsed) :: Maybe AnnHsModule)))
    -- try to pretty-print; summarize the test result
    ann = annotateAST parsed ghcAnns
      `debug` ("ghcAnns:" ++ showGhc ghcAnns)

    Just (GHC.L le exps) = GHC.hsmodExports hsmod
    secondExp@(GHC.L l2 _) = ghead "foo" $ tail exps
    -- Just [(Ann cs ll (AnnIEVar mc))] = Map.lookup l2 ann
    -- ann' = Map.insert l2 [(Ann cs ll (AnnIEVar Nothing))] ann
    -- parsed' = (GHC.L l (hsmod { GHC.hsmodExports = Just (tail exps) }))
    -- parsed' = (GHC.L l (hsmod { GHC.hsmodExports = Just (head exps : (drop 2 exps)) }))
    -- parsed' = (GHC.L l (hsmod { GHC.hsmodExports = Just (init exps) }))

    -- parsed' = (GHC.L l (hsmod { GHC.hsmodExports = Just (GHC.L le (head exps:(head $ tail exps):tail exps)) }))
    -- ((16,9),(16,27))
    ss = GHC.mkSrcSpan (GHC.mkSrcLoc (GHC.mkFastString "examples/PatBind.hs") 16 9)
                       (GHC.mkSrcLoc (GHC.mkFastString "examples/PatBind.hs") 16 27)

    -- Just [Ann cs1 dp1 as1,Ann cs2 dp2 as2] = Map.lookup ss ann
    -- ann2 = Map.insert ss [Ann cs1 (DP (0,6)) as1,Ann cs2 dp2 as2] ann
    printed = exactPrintAnnotation parsed [] ann -- `debug` ("ann=" ++ (show $ map (\(s,a) -> (ss2span s, a)) $ Map.toList ann))
       -- `debug` ("ann=" ++ (show (snd ann)))
       -- `debug` ("getAnn:=" ++ (show (getAnnotationValue (snd ann) (GHC.getLoc parsed) :: Maybe AnnHsModule)))
    result =
            if printed == contents
              then "Match\n"
              else printed ++ "\n==============\n"
                    ++ "lengths:" ++ show (length printed,length contents) ++ "\n"
                    ++ parsedAST
  -- putStrLn $ "Test:parsed=" ++ parsedAST
  -- putStrLn $ "Test:ghcAnns:fst=" ++ show (fst ghcAnns)
  -- putStrLn $ "Test:ghcAnns:snd=" ++ showGhc (snd ghcAnns)
  -- putStrLn $ "Test2:empty ann=" ++ show ((Map.empty,Map.empty) :: Anns)
  -- putStrLn $ "Test2:ann=[" ++ show (annotateAST parsed ghcAnns) ++ "]"
  writeFile out $ result
  -- putStrLn $ "Test3:ann=[" ++ show (snd ann) ++ "]"
  return ()
-- }}}


-- ---------------------------------------------------------------------
-- |Result of parsing a Haskell source file. It is simply the
-- TypeCheckedModule produced by GHC.
type ParseResult = GHC.TypecheckedModule

parsedFileGhc :: String -> String -> Bool -> IO (GHC.ApiAnns,ParseResult)
parsedFileGhc fileName modname useTH = do
    putStrLn $ "parsedFileGhc:" ++ show fileName
#if __GLASGOW_HASKELL__ > 704
    GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut $ do
#else
    GHC.defaultErrorHandler GHC.defaultLogAction $ do
#endif
      GHC.runGhc (Just libdir) $ do
        dflags <- GHC.getSessionDynFlags
        let dflags' = foldl GHC.xopt_set dflags
                           [GHC.Opt_Cpp, GHC.Opt_ImplicitPrelude, GHC.Opt_MagicHash]

            dflags'' = dflags' { GHC.importPaths = ["./tests/examples/","../tests/examples/",
                                                    "./src/","../src/"] }

            tgt = if useTH then GHC.HscInterpreted
                           else GHC.HscNothing -- allows FFI
            dflags''' = dflags'' { GHC.hscTarget = tgt,
                                   GHC.ghcLink =  GHC.LinkInMemory
                                  , GHC.packageFlags = [GHC.ExposePackage (GHC.PackageArg "ghc") (GHC.ModRenaming False [])]
                                 }

            dflags4 = if False -- useHaddock
                        then GHC.gopt_set (GHC.gopt_set dflags''' GHC.Opt_Haddock)
                                       GHC.Opt_KeepRawTokenStream
                        else GHC.gopt_set dflags'''
                                       GHC.Opt_KeepRawTokenStream
                        -- else GHC.gopt_set (GHC.gopt_unset dflags''' GHC.Opt_Haddock)
                        --               GHC.Opt_KeepRawTokenStream

        void $ GHC.setSessionDynFlags dflags4
        -- GHC.liftIO $ putStrLn $ "dflags set"

        target <- GHC.guessTarget fileName Nothing
        GHC.setTargets [target]
        GHC.liftIO $ putStrLn $ "target set:" ++ showGhc (GHC.targetId target)
        void $ GHC.load GHC.LoadAllTargets -- Loads and compiles, much as calling make
        -- GHC.liftIO $ putStrLn $ "targets loaded"
        g <- GHC.getModuleGraph
        let showStuff ms = show (GHC.moduleNameString $ GHC.moduleName $ GHC.ms_mod ms,GHC.ms_location ms)
        GHC.liftIO $ putStrLn $ "module graph:" ++ (intercalate "," (map showStuff g))

        modSum <- GHC.getModSummary $ GHC.mkModuleName modname
        GHC.liftIO $ putStrLn $ "got modSum"
        -- let modSum = head g
        p <- GHC.parseModule modSum
        GHC.liftIO $ putStrLn $ "got parsedModule"
        t <- GHC.typecheckModule p
        GHC.liftIO $ putStrLn $ "typechecked"
        -- toks <- GHC.getRichTokenStream (GHC.ms_mod modSum)
        -- GHC.liftIO $ putStrLn $ "toks"
        let anns = GHC.pm_annotations p
        GHC.liftIO $ putStrLn $ "anns"
        return (anns,t)

readUTF8File :: FilePath -> IO String
readUTF8File fp = openFile fp ReadMode >>= \h -> do
        hSetEncoding h utf8
        hGetContents h


-- ---------------------------------------------------------------------
{-
testAP :: AP a -> [Comment] -> GHC.ApiAnns -> IO a
testAP (AP f) cs ga = do
  let st = S [] [] cs ga
      (r,st',(se,su)) = f st
  return r

testAPSrcSpans = do
  let ss1 = mkSs (1,2) (3,4)
      ss2 = mkSs (2,3) (4,5)
      ss3 = mkSs (3,4) (5,6)

      sst :: AP (GHC.SrcSpan,GHC.SrcSpan)
      sst = do
        pushSrcSpan (GHC.L ss1 ())
        pushSrcSpan (GHC.L ss2 ())
        r1 <- getSrcSpanAP
        popSrcSpan
        r2 <- getSrcSpanAP
        return (r1,r2)

  ss <- testAP sst [] (Map.empty,Map.empty)
  putStrLn $ "testAPStuff: ss=" ++ showGhc ss
-}

-- ---------------------------------------------------------------------

pwd :: IO FilePath
pwd = getCurrentDirectory

cd :: FilePath -> IO ()
cd = setCurrentDirectory

-- ---------------------------------------------------------------------

mkSs :: (Int,Int) -> (Int,Int) -> GHC.SrcSpan
mkSs (sr,sc) (er,ec)
  = GHC.mkSrcSpan (GHC.mkSrcLoc (GHC.mkFastString "examples/PatBind.hs") sr sc)
                  (GHC.mkSrcLoc (GHC.mkFastString "examples/PatBind.hs") er ec)
