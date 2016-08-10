{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
-- | Use "runhaskell Setup.hs test" or "cabal test" to run these tests.
module Main where

-- import Language.Haskell.GHC.ExactPrint.Utils ( showGhc )

import Control.Monad
import System.Directory
import System.FilePath
import System.IO
import System.Exit

import Data.List

import System.IO.Silently

import Test.Common
import Test.NoAnnotations
import Test.Transform

import Test.HUnit


-- import Debug.Trace

-- ---------------------------------------------------------------------

data GHCVersion = GHC710 | GHC8 deriving (Eq, Ord, Show)

ghcVersion :: GHCVersion
ghcVersion =
#if __GLASGOW_HASKELL__ >= 711
  GHC8
#else
  GHC710
#endif

-- | Directories to automatically find roundtrip tests
testDirs :: [FilePath]
testDirs =
  case ghcVersion of
    GHC710 -> ["ghc710-only","ghc710"]
    GHC8   -> ["ghc710", "ghc8"]

-- ---------------------------------------------------------------------

main :: IO ()
main = hSilence [stderr] $ do
  print ghcVersion
  tests <- mkTests
  cnts <- fst <$> runTestText (putTextToHandle stdout True) tests
  putStrLn $ show cnts
  if errors cnts > 0 || failures cnts > 0
     then exitFailure
     else return () -- exitSuccess

transform :: IO ()
transform = hSilence [stderr] $ do
  cnts <- fst <$> runTestText (putTextToHandle stdout True) transformTests
  putStrLn $ show cnts
  if errors cnts > 0 || failures cnts > 0
     then exitFailure
     else return () -- exitSuccess

-- ---------------------------------------------------------------------

findTests :: IO Test
findTests = testList "Round-trip tests" <$> mapM (findTestsDir mkParserTest) testDirs

findPrettyTests :: IO Test
findPrettyTests = testList "Default Annotations round-trip tests" <$> mapM (findTestsDir mkPrettyRoundtrip) testDirs

findTestsDir :: (FilePath -> FilePath -> Test) -> FilePath -> IO Test
findTestsDir mkTestFn dir = do
  let fp = testPrefix </> dir
  fs <- getDirectoryContents fp
  let testFiles = sort $ filter (".hs" `isSuffixOf`) fs
  -- return $ testList dir (map (mkTestFn dir) testFiles)
  return $ testList dir (map (\fn -> TestLabel fn (mkTestFn dir fn)) testFiles)

listTests :: IO ()
listTests = do
  let
    ftd dir = do
      let fp = testPrefix </> dir
      fs <- getDirectoryContents fp
      let testFiles = sort $ filter (".hs" `isSuffixOf`) fs
      return (zip [0::Integer ..] testFiles)
  files <- mapM ftd testDirs
  putStrLn $ "round trip tests:" ++ show (zip testDirs files)

mkTests :: IO Test
mkTests = do
  -- listTests
  roundTripTests <- findTests
  prettyRoundTripTests <- findPrettyTests
  return $ TestList [
                      internalTests, roundTripTests, transformTests, failingTests, noAnnotationTests
                    -- , prettyRoundTripTests
                    ]

-- Tests that will fail until https://phabricator.haskell.org/D907 lands in a
-- future GHC
failingTests :: Test
failingTests = testList "Failing tests"
  [
  -- Tests requiring future GHC modifications
    mkTestModBad "InfixOperator.hs"

#if __GLASGOW_HASKELL__ > 710
  , mkTestModBad "overloadedlabelsrun04.hs"
#else
  , mkTestModBad "UnicodeSyntax.hs"
  , mkTestModBad "UnicodeRules.hs"
  , mkTestModBad "Deprecation.hs"
  , mkTestModBad "MultiLineWarningPragma.hs"
#endif


  ]


mkParserTest :: FilePath -> FilePath -> Test
mkParserTest dir fp = mkParsingTest roundTripTest dir fp

-- ---------------------------------------------------------------------

formatTT :: ([([Char], Bool)], [([Char], Bool)]) -> IO ()
formatTT (ts, fs) = do
  when (not . null $ tail ts) (do
    putStrLn "Pass"
    mapM_ (putStrLn . fst) (tail ts)
    )
  when (not . null $ fs) (do
    putStrLn "Fail"
    mapM_ (putStrLn . fst) fs)

tr :: IO (Counts,Int)
tr = hSilence [stderr] $ do
  prettyRoundTripTests <- findPrettyTests
  runTestText (putTextToHandle stdout True) prettyRoundTripTests

tt' :: IO (Counts,Int)
tt' = runTestText (putTextToHandle stdout True) $ TestList [
      -- mkParserTest "ghc710" "Unicode.hs"


      -- mkPrettyRoundtrip "ghc710" "TypeOperators.hs"
      mkPrettyRoundtrip "ghc710" "Sigs.hs"
    -- , mkPrettyRoundtrip "ghc710" "TemplateHaskell.hs"
      -- mkPrettyRoundtrip "ghc710" "NestedDoLambda.hs"
      -- mkPrettyRoundtrip "ghc710" "PuncFunctions.hs"
      -- mkPrettyRoundtrip "ghc710-only" "DataDecl.hs"
      -- mkPrettyRoundtrip "ghc710" "TemplateHaskell.hs"
      -- mkPrettyRoundtrip "ghc710" "Splice.hs"
      -- mkPrettyRoundtrip "ghc710" "Undefined3.hs"
    -- , mkPrettyRoundtrip "ghc710" "Process1.hs"
    -- , mkPrettyRoundtrip "ghc710" "ModuleOnly.hs"
      -- mkPrettyRoundtrip "ghc710" "Simple.hs"
    -- , mkPrettyRoundtrip "ghc710" "ShiftingLambda.hs"
    -- , mkPrettyRoundtrip "ghc710" "Case.hs"
    -- , mkParserTest "ghc710" "Ann01.hs"
    -- , mkParserTest "ghc710" "TypeOperators.hs"
    -- , mkParserTest "ghc710" "PuncFunctions.hs"
    -- , mkParserTest "ghc710" "QuasiQuote.hs"

    -- , mkParserTest "ghc710" "Sigs.hs"
    -- , mkParserTest "ghc710" "Commands.hs"
    -- , mkParserTest "ghc710" "ListComprehensions.hs"


    -- , mkParserTest      "ghc710" "RdrNames.hs"
    -- , mkParserTest      "ghc8" "records-mixing-fields.hs"
    -- , mkParserTest      "ghc8" "records-no-uni-update.hs"
    -- , mkParserTest      "ghc8" "records-poly.hs"
    -- , mkParserTest      "ghc8" "records-run.hs"
    ]

testsTT :: Test
testsTT = TestList
  [
    mkParserTest "ghc710" "Cpp.hs"
  , mkParserTest "ghc710" "DroppedDoSpace.hs"
  ]

tt :: IO ()
-- tt = hSilence [stderr] $ do
tt = do
  cnts <- fst <$> runTestText (putTextToHandle stdout True) testsTT
  putStrLn $ show cnts
  if errors cnts > 0 || failures cnts > 0
     then exitFailure
     else return () -- exitSuccess

-- ---------------------------------------------------------------------

ii :: IO ()
ii = do
  cnts <- fst <$> runTestText (putTextToHandle stdout True) internalTests
  putStrLn $ show cnts
  if errors cnts > 0 || failures cnts > 0
     then exitFailure
     else return () -- exitSuccess

internalTests :: Test
internalTests = testList "Internal tests"
  [
    -- testCleanupOneLine
  ]

{-
testCleanupOneLine :: Test
testCleanupOneLine = do
  let
    makeCase n = (show n
                 ,(T.replicate n " ") <> "\t|" <> T.replicate n " " <> "\t"
                 ,(T.replicate 8 " " <> "|"))
    mkTest n = TestCase $ assertEqual name outp (cleanupOneLine inp)
      where (name,inp,outp) = makeCase n
  testList "cleanupOneLine" $ map mkTest [1..7]
-}

-- ---------------------------------------------------------------------

pwd :: IO FilePath
pwd = getCurrentDirectory

cd :: FilePath -> IO ()
cd = setCurrentDirectory
