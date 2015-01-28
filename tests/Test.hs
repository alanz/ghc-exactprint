{-# LANGUAGE CPP #-}
-- | Use "runhaskell Setup.hs test" or "cabal test" to run these tests.
module Main where

import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Utils

import GHC.Paths ( libdir )

import qualified DynFlags      as GHC
import qualified FastString    as GHC
import qualified GHC           as GHC
-- import qualified MonadUtils    as GHC
-- import qualified Outputable    as GHC

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

tests :: Test
tests = TestList
  [
    mkTestMod "LetStmt.hs"               "Layout.LetStmt"
  , mkTestMod "LetExpr.hs"               "LetExpr"
  , mkTestMod "ExprPragmas.hs"           "ExprPragmas"
  , mkTestMod "ListComprehensions.hs"    "Main"
  , mkTestMod "MonadComprehensions.hs"   "Main"
  , mkTestMod "FunDeps.hs"               "Main"
  , mkTestMod "ImplicitParams.hs"        "Main"
  , mkTestMod "RecursiveDo.hs"           "Main"
  , mkTestMod "TypeFamilies.hs"          "Main"
  , mkTestMod "MultiParamTypeClasses.hs" "Main"
  , mkTestMod "DataFamilies.hs"          "DataFamilies"
  , mkTestMod "Deriving.hs"              "Main"
  , mkTestMod "Default.hs"               "Main"
  , mkTestMod "ForeignDecl.hs"           "ForeignDecl"
  , mkTestMod "Warning.hs"               "Warning"
  , mkTestMod "Annotations.hs"           "Annotations"
  , mkTestMod "DocDecls.hs"              "DocDecls"
  , mkTestModTH "QuasiQuote.hs"          "QuasiQuote"
  , mkTestMod "Roles.hs"                 "Roles"
  , mkTestMod "Splice.hs"                "Splice"
  , mkTestMod "ImportsSemi.hs"           "ImportsSemi"
  , mkTestMod "Stmts.hs"                 "Stmts"
  , mkTestMod "Mixed.hs"                 "Main"
  , mkTestMod "Arrow.hs"                 "Arrow"
  , mkTestMod "PatSynBind.hs"            "Main"
  , mkTestMod "HsDo.hs"                  "HsDo"
  , mkTestMod "ForAll.hs"                "ForAll"
  , mkTestMod "PArr.hs"                  "PArr"
  , mkTestMod "ViewPatterns.hs"          "Main"
  , mkTestMod "BangPatterns.hs"          "Main"
  , mkTestMod "Associated.hs"            "Main"
  , mkTestMod "Move1.hs"                 "Move1"
  , mkTestMod "Rules.hs"                 "Rules"
  , mkTestMod "TypeOperators.hs"         "Main"
  , mkTestMod "NullaryTypeClasses.hs"    "Main"
  , mkTestMod "FunctionalDeps.hs"        "Main"
  , mkTestMod "DerivingOC.hs"            "Main"
  , mkTestMod "GenericDeriving.hs"       "Main"
  , mkTestMod "OverloadedStrings.hs"     "Main"
  , mkTestMod "RankNTypes.hs"            "Main"
  , mkTestMod "Existential.hs"           "Main"
  , mkTestMod "ScopedTypeVariables.hs"   "Main"
  , mkTestMod "Arrows.hs"                "Main"
  , mkTestMod "TH.hs"                    "Main"
  , mkTestMod "StaticPointers.hs"        "Main"
  , mkTestMod "DataDecl.hs"              "Main"
  , mkTestMod "Guards.hs"                "Main"
  , mkTestMod "RebindableSyntax.hs"      "Main"
  , mkTestMod "RdrNames.hs"              "RdrNames"
  , mkTestMod "Vect.hs"                  "Vect"
  , mkTestMod "Tuple.hs"                 "Main"
  , mkTestMod "ExtraConstraints1.hs"     "ExtraConstraints1"
  , mkTestMod "AddAndOr3.hs"             "AddAndOr3"
  , mkTestMod "Ann01.hs"                 "Ann01"
  , mkTestMod "StrictLet.hs"             "Main"
  , mkTestMod "Cg008.hs"                 "Cg008"
  , mkTestMod "T2388.hs"                 "T2388"
  , mkTestMod "T3132.hs"                 "T3132"
  , mkTestMod "Stream.hs"                "Stream"
  , mkTestMod "Trit.hs"                  "Trit"
  , mkTestMod "DataDecl.hs"              "Main"
  , mkTestMod "Zipper.hs"                "Zipper"
  , mkTestMod "Sigs.hs"                  "Sigs"
  , mkTestMod "Utils2.hs"                "Utils2"
  , mkTestMod "EmptyMostlyInst.hs"       "EmptyMostlyInst"
  , mkTestMod "EmptyMostlyNoSemis.hs"    "EmptyMostlyNoSemis"
  , mkTestMod "Dead1.hs"                 "Dead1"
  , mkTestMod "EmptyMostly.hs"           "EmptyMostly"
  , mkTestMod "FromUtils.hs"             "Main"
  , mkTestMod "DocDecls.hs"              "DocDecls"
  , mkTestMod "RecordUpdate.hs"          "Main"
  -- , mkTestMod "Unicode.hs"               "Main"
  , mkTestMod "B.hs"                     "Main"
  , mkTestMod "LayoutWhere.hs"           "Main"
  , mkTestMod "LayoutLet.hs"             "Main"
  , mkTestMod "Deprecation.hs"           "Deprecation"
  , mkTestMod "Infix.hs"                 "Main"
  , mkTestMod "BCase.hs"                 "Main"
  , mkTestMod "AltsSemis.hs"             "Main"

  , mkTestMod "LetExprSemi.hs"           "LetExprSemi"
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

tt :: IO Bool
tt = do

    manipulateAstTest "LetStmt.hs"               "Layout.LetStmt"
    manipulateAstTest "LetExpr.hs"               "LetExpr"
    manipulateAstTest "ExprPragmas.hs"           "ExprPragmas"
    manipulateAstTest "ListComprehensions.hs"    "Main"
    manipulateAstTest "MonadComprehensions.hs"   "Main"
    manipulateAstTest "FunDeps.hs"               "Main"
    manipulateAstTest "ImplicitParams.hs"        "Main"
    manipulateAstTest "RecursiveDo.hs"           "Main"
    manipulateAstTest "TypeFamilies.hs"          "Main"
    manipulateAstTest "MultiParamTypeClasses.hs" "Main"
    manipulateAstTest "DataFamilies.hs"          "DataFamilies"
    manipulateAstTest "Deriving.hs"              "Main"
    manipulateAstTest "Default.hs"               "Main"
    manipulateAstTest "ForeignDecl.hs"           "ForeignDecl"
    manipulateAstTest "Warning.hs"               "Warning"
    manipulateAstTest "Annotations.hs"           "Annotations"
    manipulateAstTest "DocDecls.hs"              "DocDecls"
    manipulateAstTestTH "QuasiQuote.hs"          "QuasiQuote"
    manipulateAstTest "Roles.hs"                 "Roles"
    manipulateAstTest "Splice.hs"                "Splice"
    manipulateAstTest "ImportsSemi.hs"           "ImportsSemi"
    manipulateAstTest "Stmts.hs"                 "Stmts"
    manipulateAstTest "Mixed.hs"                 "Main"
    manipulateAstTest "Arrow.hs"                 "Arrow"
    manipulateAstTest "PatSynBind.hs"            "Main"
    manipulateAstTest "HsDo.hs"                  "HsDo"
    manipulateAstTest "ForAll.hs"                "ForAll"
    manipulateAstTest "PArr.hs"                  "PArr"
    manipulateAstTest "ViewPatterns.hs"          "Main"
    manipulateAstTest "BangPatterns.hs"          "Main"
    manipulateAstTest "Associated.hs"            "Main"
    manipulateAstTest "Move1.hs"                 "Move1"
    manipulateAstTest "Rules.hs"                 "Rules"
    manipulateAstTest "TypeOperators.hs"         "Main"
    manipulateAstTest "NullaryTypeClasses.hs"    "Main"
    manipulateAstTest "FunctionalDeps.hs"        "Main"
    manipulateAstTest "DerivingOC.hs"            "Main"
    manipulateAstTest "GenericDeriving.hs"       "Main"
    manipulateAstTest "OverloadedStrings.hs"     "Main"
    manipulateAstTest "RankNTypes.hs"            "Main"
    manipulateAstTest "Existential.hs"           "Main"
    manipulateAstTest "ScopedTypeVariables.hs"   "Main"
    manipulateAstTest "Arrows.hs"                "Main"
    manipulateAstTest "TH.hs"                    "Main"
    manipulateAstTest "StaticPointers.hs"        "Main"
    manipulateAstTest "DataDecl.hs"              "Main"
    manipulateAstTest "Guards.hs"                "Main"
    manipulateAstTest "RebindableSyntax.hs"      "Main"
    manipulateAstTest "RdrNames.hs"              "RdrNames"
    manipulateAstTest "Vect.hs"                  "Vect"
    manipulateAstTest "Tuple.hs"                 "Main"
    manipulateAstTest "ExtraConstraints1.hs"     "ExtraConstraints1"
    manipulateAstTest "AddAndOr3.hs"             "AddAndOr3"
    manipulateAstTest "Ann01.hs"                 "Ann01"
    manipulateAstTest "StrictLet.hs"             "Main"
    manipulateAstTest "Cg008.hs"                 "Cg008"
    manipulateAstTest "T2388.hs"                 "T2388"
    manipulateAstTest "T3132.hs"                 "T3132"
    manipulateAstTest "Stream.hs"                "Stream"
    manipulateAstTest "Trit.hs"                  "Trit"
    manipulateAstTest "DataDecl.hs"              "Main"
    manipulateAstTest "Zipper.hs"                "Zipper"
    manipulateAstTest "Sigs.hs"                  "Sigs"
    manipulateAstTest "Utils2.hs"                "Utils2"
    manipulateAstTest "EmptyMostlyInst.hs"       "EmptyMostlyInst"
    manipulateAstTest "EmptyMostlyNoSemis.hs"    "EmptyMostlyNoSemis"
    manipulateAstTest "Dead1.hs"                 "Dead1"
    manipulateAstTest "EmptyMostly.hs"           "EmptyMostly"
    manipulateAstTest "FromUtils.hs"             "Main"
    manipulateAstTest "DocDecls.hs"              "DocDecls"
    manipulateAstTest "RecordUpdate.hs"          "Main"
    -- manipulateAstTest "Unicode.hs"               "Main"
    manipulateAstTest "B.hs"                     "Main"
    manipulateAstTest "LayoutWhere.hs"           "Main"
    manipulateAstTest "LayoutLet.hs"             "Main"
    manipulateAstTest "Deprecation.hs"           "Deprecation"
    manipulateAstTest "Infix.hs"                 "Main"
    manipulateAstTest "BCase.hs"                 "Main"
    manipulateAstTest "AltsSemis.hs"             "Main"

    manipulateAstTest "LetExprSemi.hs"           "LetExprSemi"
{-
    manipulateAstTest "Cpp.hs"                   "Main"
    manipulateAstTest "Lhs.lhs"                  "Main"
    manipulateAstTest "ParensAroundContext.hs"   "ParensAroundContext"
    manipulateAstTest "EmptyMostly2.hs"          "EmptyMostly2"
    manipulateAstTest "Foo.hs"                   "Main"
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
manipulateAstTest' useTH file' modname = do
  let testpath = "./tests/examples/"
      file   = testpath </> file'
      out    = file <.> "out"

  contents <- readUTF8File file
  (ghcAnns,t) <- parsedFileGhc file modname useTH
  let
    parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t
    parsedAST = SYB.showData SYB.Parser 0 parsed
    -- parsedAST = showGhc parsed
       -- `debug` ("getAnn:=" ++ (show (getAnnotationValue (snd ann) (GHC.getLoc parsed) :: Maybe AnnHsModule)))
    -- try to pretty-print; summarize the test result
    ann = annotateAST parsed ghcAnns
      `debug` ("ghcAnns:" ++ showGhc ghcAnns)

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
  return ("Match\n" == result)


-- ---------------------------------------------------------------------
-- |Result of parsing a Haskell source file. It is simply the
-- TypeCheckedModule produced by GHC.
type ParseResult = GHC.TypecheckedModule

parsedFileGhc :: String -> String -> Bool -> IO (GHC.ApiAnns,ParseResult)
parsedFileGhc fileName modname useTH = do
    -- putStrLn $ "parsedFileGhc:" ++ show fileName
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

        (dflags5,_args,_warns) <- GHC.parseDynamicFlagsCmdLine dflags4 [GHC.noLoc "-package ghc"]
        -- GHC.liftIO $ putStrLn $ "dflags set:(args,warns)" ++ show (map GHC.unLoc args,map GHC.unLoc warns)
        void $ GHC.setSessionDynFlags dflags5
        -- GHC.liftIO $ putStrLn $ "dflags set"

        target <- GHC.guessTarget fileName Nothing
        GHC.setTargets [target]
        -- GHC.liftIO $ putStrLn $ "target set:" ++ showGhc (GHC.targetId target)
        void $ GHC.load GHC.LoadAllTargets -- Loads and compiles, much as calling make
        -- GHC.liftIO $ putStrLn $ "targets loaded"
        -- g <- GHC.getModuleGraph
        -- let showStuff ms = show (GHC.moduleNameString $ GHC.moduleName $ GHC.ms_mod ms,GHC.ms_location ms)
        -- GHC.liftIO $ putStrLn $ "module graph:" ++ (intercalate "," (map showStuff g))

        modSum <- GHC.getModSummary $ GHC.mkModuleName modname
        -- GHC.liftIO $ putStrLn $ "got modSum"
        -- let modSum = head g
        p <- GHC.parseModule modSum
        -- GHC.liftIO $ putStrLn $ "got parsedModule"
        t <- GHC.typecheckModule p
        -- GHC.liftIO $ putStrLn $ "typechecked"
        -- toks <- GHC.getRichTokenStream (GHC.ms_mod modSum)
        -- GHC.liftIO $ putStrLn $ "toks"
        let anns = GHC.pm_annotations p
        -- GHC.liftIO $ putStrLn $ "anns"
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
