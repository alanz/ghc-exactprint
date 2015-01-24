{-# LANGUAGE CPP #-}
-- | Use "runhaskell Setup.hs test" or "cabal test" to run these tests.
module Main where

import Language.Haskell.GHC.ExactPrint
-- import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Utils

import GHC.Paths ( libdir )

import qualified DynFlags      as GHC
import qualified FastString    as GHC
import qualified GHC           as GHC
import qualified MonadUtils    as GHC
import qualified Outputable    as GHC

import qualified GHC.SYB.Utils as SYB

import Control.Monad
import System.Directory
import System.FilePath
import System.IO

import Test.HUnit

-- import qualified Data.Map as Map

-- ---------------------------------------------------------------------

main :: IO Counts
main = runTestTT tests

-- tests = TestCase (do r <- manipulateAstTest "examples/LetStmt.hs" "Layout.LetStmt"
--                      assertBool "test" r )

tests = TestList
  [
    mkTestMod "examples/LetStmt.hs"               "Layout.LetStmt"
  , mkTestMod "examples/LetExpr.hs"               "LetExpr"
  , mkTestMod "examples/ExprPragmas.hs"           "ExprPragmas"
  , mkTestMod "examples/ListComprehensions.hs"    "Main"
  , mkTestMod "examples/MonadComprehensions.hs"   "Main"
  , mkTestMod "examples/FunDeps.hs"               "Main"
  , mkTestMod "examples/ImplicitParams.hs"        "Main"
  , mkTestMod "examples/RecursiveDo.hs"           "Main"
  , mkTestMod "examples/TypeFamilies.hs"          "Main"
  , mkTestMod "examples/MultiParamTypeClasses.hs" "Main"
  , mkTestMod "examples/DataFamilies.hs"          "DataFamilies"
  , mkTestMod "examples/Deriving.hs"              "Main"
  , mkTestMod "examples/Default.hs"               "Main"
  , mkTestMod "examples/ForeignDecl.hs"           "ForeignDecl"
  , mkTestMod "examples/Warning.hs"               "Warning"
  , mkTestMod "examples/Annotations.hs"           "Annotations"
  , mkTestMod "examples/DocDecls.hs"              "DocDecls"
  , mkTestModTH "examples/QuasiQuote.hs"          "QuasiQuote"
  , mkTestMod "examples/Roles.hs"                 "Roles"
  , mkTestMod "examples/Splice.hs"                "Splice"
  , mkTestMod "examples/ImportsSemi.hs"           "ImportsSemi"
  , mkTestMod "examples/Stmts.hs"                 "Stmts"
  , mkTestMod "examples/Mixed.hs"                 "Main"
  , mkTestMod "examples/Arrow.hs"                 "Arrow"
  , mkTestMod "examples/PatSynBind.hs"            "Main"
  , mkTestMod "examples/HsDo.hs"                  "HsDo"
  , mkTestMod "examples/ForAll.hs"                "ForAll"
  , mkTestMod "examples/PArr.hs"                  "PArr"
  , mkTestMod "examples/ViewPatterns.hs"          "Main"
  , mkTestMod "examples/BangPatterns.hs"          "Main"
  , mkTestMod "examples/Associated.hs"            "Main"
  , mkTestMod "examples/Move1.hs"                 "Move1"
  , mkTestMod "examples/Rules.hs"                 "Rules"
  , mkTestMod "examples/TypeOperators.hs"         "Main"
  , mkTestMod "examples/NullaryTypeClasses.hs"    "Main"
  , mkTestMod "examples/FunctionalDeps.hs"        "Main"
  , mkTestMod "examples/DerivingOC.hs"            "Main"
  , mkTestMod "examples/GenericDeriving.hs"       "Main"
  , mkTestMod "examples/OverloadedStrings.hs"     "Main"
  , mkTestMod "examples/RankNTypes.hs"            "Main"
  , mkTestMod "examples/Existential.hs"           "Main"
  , mkTestMod "examples/ScopedTypeVariables.hs"   "Main"
  , mkTestMod "examples/Arrows.hs"                "Main"
  , mkTestMod "examples/TH.hs"                    "Main"
  , mkTestMod "examples/StaticPointers.hs"        "Main"
  , mkTestMod "examples/DataDecl.hs"              "Main"
  , mkTestMod "examples/Guards.hs"                "Main"
  , mkTestMod "examples/RebindableSyntax.hs"      "Main"
  , mkTestMod "examples/RdrNames.hs"              "RdrNames"
  , mkTestMod "examples/Vect.hs"                  "Vect"
  , mkTestMod "examples/Tuple.hs"                 "Main"
  , mkTestMod "examples/ExtraConstraints1.hs"     "ExtraConstraints1"
  , mkTestMod "examples/AddAndOr3.hs"             "AddAndOr3"
  , mkTestMod "examples/Ann01.hs"                 "Ann01"
  , mkTestMod "examples/StrictLet.hs"             "Main"
  , mkTestMod "examples/Cg008.hs"                 "Cg008"
  , mkTestMod "examples/T2388.hs"                 "T2388"
  , mkTestMod "examples/T3132.hs"                 "T3132"
  , mkTestMod "examples/Stream.hs"                "Stream"
  , mkTestMod "examples/Trit.hs"                  "Trit"
  , mkTestMod "examples/DataDecl.hs"              "Main"
  , mkTestMod "examples/Zipper.hs"                "Zipper"
  , mkTestMod "examples/Sigs.hs"                  "Sigs"
  , mkTestMod "examples/Utils2.hs"                "Utils2"
  , mkTestMod "examples/EmptyMostlyInst.hs"       "EmptyMostlyInst"
  , mkTestMod "examples/EmptyMostlyNoSemis.hs"    "EmptyMostlyNoSemis"
  , mkTestMod "examples/Dead1.hs"                 "Dead1"
  , mkTestMod "examples/EmptyMostly.hs"           "EmptyMostly"
  , mkTestMod "examples/FromUtils.hs"             "Main"
  , mkTestMod "examples/DocDecls.hs"              "DocDecls"
  , mkTestMod "examples/RecordUpdate.hs"          "Main"
  -- , mkTestMod "examples/Unicode.hs"               "Main"
  , mkTestMod "examples/B.hs"                     "Main"
  , mkTestMod "examples/LayoutWhere.hs"           "Main"
  , mkTestMod "examples/LayoutLet.hs"             "Main"
  , mkTestMod "examples/Deprecation.hs"           "Deprecation"
  , mkTestMod "examples/Infix.hs"                 "Main"
  , mkTestMod "examples/BCase.hs"                 "Main"
  , mkTestMod "examples/AltsSemis.hs"             "Main"

  , mkTestMod "examples/LetExprSemi.hs"           "LetExprSemi"
  ]

mkTestMain :: FilePath -> Test
mkTestMain fileName = TestCase (do r <- manipulateAstTest fileName "Main"
                                   assertBool fileName r )

mkTestMod :: FilePath -> String -> Test
mkTestMod fileName modName
  = TestCase (do r <- manipulateAstTest fileName modName
                 assertBool fileName r )

mkTestModTH :: FilePath -> String -> Test
mkTestModTH fileName modName
  = TestCase (do r <- manipulateAstTestTH fileName modName
                 assertBool fileName r )

-- ---------------------------------------------------------------------

t :: IO Bool
t = do

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
    manipulateAstTest "examples/Annotations.hs"           "Annotations"
    manipulateAstTest "examples/DocDecls.hs"              "DocDecls"
    manipulateAstTestTH "examples/QuasiQuote.hs"          "QuasiQuote"
    manipulateAstTest "examples/Roles.hs"                 "Roles"
    manipulateAstTest "examples/Splice.hs"                "Splice"
    manipulateAstTest "examples/ImportsSemi.hs"           "ImportsSemi"
    manipulateAstTest "examples/Stmts.hs"                 "Stmts"
    manipulateAstTest "examples/Mixed.hs"                 "Main"
    manipulateAstTest "examples/Arrow.hs"                 "Arrow"
    manipulateAstTest "examples/PatSynBind.hs"            "Main"
    manipulateAstTest "examples/HsDo.hs"                  "HsDo"
    manipulateAstTest "examples/ForAll.hs"                "ForAll"
    manipulateAstTest "examples/PArr.hs"                  "PArr"
    manipulateAstTest "examples/ViewPatterns.hs"          "Main"
    manipulateAstTest "examples/BangPatterns.hs"          "Main"
    manipulateAstTest "examples/Associated.hs"            "Main"
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
    manipulateAstTest "examples/DataDecl.hs"              "Main"
    manipulateAstTest "examples/Zipper.hs"                "Zipper"
    manipulateAstTest "examples/Sigs.hs"                  "Sigs"
    manipulateAstTest "examples/Utils2.hs"                "Utils2"
    manipulateAstTest "examples/EmptyMostlyInst.hs"       "EmptyMostlyInst"
    manipulateAstTest "examples/EmptyMostlyNoSemis.hs"    "EmptyMostlyNoSemis"
    manipulateAstTest "examples/Dead1.hs"                 "Dead1"
    manipulateAstTest "examples/EmptyMostly.hs"           "EmptyMostly"
    manipulateAstTest "examples/FromUtils.hs"             "Main"
    manipulateAstTest "examples/DocDecls.hs"              "DocDecls"
    manipulateAstTest "examples/RecordUpdate.hs"          "Main"
    -- manipulateAstTest "examples/Unicode.hs"               "Main"
    manipulateAstTest "examples/B.hs"                     "Main"
    manipulateAstTest "examples/LayoutWhere.hs"           "Main"
    manipulateAstTest "examples/LayoutLet.hs"             "Main"
    manipulateAstTest "examples/Deprecation.hs"           "Deprecation"
    manipulateAstTest "examples/Infix.hs"                 "Main"
    manipulateAstTest "examples/BCase.hs"                 "Main"
    manipulateAstTest "examples/AltsSemis.hs"             "Main"

    manipulateAstTest "examples/LetExprSemi.hs"           "LetExprSemi"
{-
    manipulateAstTest "examples/Cpp.hs"                   "Main"
    manipulateAstTest "examples/Lhs.lhs"                  "Main"
    manipulateAstTest "examples/ParensAroundContext.hs"   "ParensAroundContext"
    manipulateAstTest "examples/EmptyMostly2.hs"          "EmptyMostly2"
    manipulateAstTest "examples/Foo.hs"                   "Main"
-}

-- | Where all the tests are to be found
examplesDir :: FilePath
examplesDir = "tests" </> "examples"

examplesDir2 :: FilePath
examplesDir2 = "examples"

manipulateAstTest :: FilePath -> String -> IO Bool
manipulateAstTest file modname = manipulateAstTest' False file modname

manipulateAstTestTH :: FilePath -> String -> IO Bool
manipulateAstTestTH file modname = manipulateAstTest' True file modname

manipulateAstTest' :: Bool -> FilePath -> String -> IO Bool
manipulateAstTest' useTH file modname = do
  let out    = file <.> "out"
      golden = file <.> "golden"

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
    ss = GHC.mkSrcSpan (GHC.mkSrcLoc (GHC.mkFastString "examples/PatBind.hs") 16 9)
                       (GHC.mkSrcLoc (GHC.mkFastString "examples/PatBind.hs") 16 27)

    printed = exactPrintAnnotation parsed [] ann -- `debug` ("ann=" ++ (show $ map (\(s,a) -> (ss2span s, a)) $ Map.toList ann))
    result =
            if printed == contents
              then "Match\n"
              else printed ++ "\n==============\n"
                    ++ "lengths:" ++ show (length printed,length contents) ++ "\n"
                    ++ parsedAST
  -- putStrLn $ "Test:parsed=" ++ parsedAST
  writeFile out $ result
  -- putStrLn $ "Test:ann organised:" ++ showGhc (organiseAnns ann)
  -- putStrLn $ "Test:showdata:" ++ showAnnData (organiseAnns ann) 0 parsed
  return ("Match\n"  == result)
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

        (dflags5,args,warns) <- GHC.parseDynamicFlagsCmdLine dflags4 [GHC.noLoc "-package ghc"]
        GHC.liftIO $ putStrLn $ "dflags set:(args,warns)" ++ show (map GHC.unLoc args,map GHC.unLoc warns)
        void $ GHC.setSessionDynFlags dflags5
        -- GHC.liftIO $ putStrLn $ "dflags set"

        target <- GHC.guessTarget fileName Nothing
        GHC.setTargets [target]
        GHC.liftIO $ putStrLn $ "target set:" ++ showGhc (GHC.targetId target)
        void $ GHC.load GHC.LoadAllTargets -- Loads and compiles, much as calling make
        -- GHC.liftIO $ putStrLn $ "targets loaded"
        g <- GHC.getModuleGraph
        let showStuff ms = show (GHC.moduleNameString $ GHC.moduleName $ GHC.ms_mod ms,GHC.ms_location ms)
        -- GHC.liftIO $ putStrLn $ "module graph:" ++ (intercalate "," (map showStuff g))

        modSum <- GHC.getModSummary $ GHC.mkModuleName modname
        -- GHC.liftIO $ putStrLn $ "got modSum"
        -- let modSum = head g
        p <- GHC.parseModule modSum
        -- GHC.liftIO $ putStrLn $ "got parsedModule"
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

pwd :: IO FilePath
pwd = getCurrentDirectory

cd :: FilePath -> IO ()
cd = setCurrentDirectory

-- ---------------------------------------------------------------------

mkSs :: (Int,Int) -> (Int,Int) -> GHC.SrcSpan
mkSs (sr,sc) (er,ec)
  = GHC.mkSrcSpan (GHC.mkSrcLoc (GHC.mkFastString "examples/PatBind.hs") sr sc)
                  (GHC.mkSrcLoc (GHC.mkFastString "examples/PatBind.hs") er ec)
