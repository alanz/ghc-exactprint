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
import qualified Data.Set as Set
import System.IO.Silently

import Test.Common
import Test.NoAnnotations
import Test.Transform

import Test.HUnit


-- import Debug.Trace

-- ---------------------------------------------------------------------

data GHCVersion = GHC710 | GHC80 | GHC82 | GHC84 | GHC86 | GHC88 | GHC810 | GHC90
     deriving (Eq, Ord, Show)

ghcVersion :: GHCVersion
ghcVersion =
#if __GLASGOW_HASKELL__ >= 900
  GHC90
#elif __GLASGOW_HASKELL__ > 808
  GHC810
#elif __GLASGOW_HASKELL__ > 806
  GHC88
#elif __GLASGOW_HASKELL__ > 804
  GHC86
#elif __GLASGOW_HASKELL__ > 802
  GHC84
#elif __GLASGOW_HASKELL__ > 800
  GHC82
#elif __GLASGOW_HASKELL__ >= 711
  GHC80
#else
  GHC710
#endif

-- | Directories to automatically find roundtrip tests
testDirs :: [FilePath]
testDirs =
  case ghcVersion of
    GHC710 -> ["ghc710-only",             "ghc710", "vect"]
    GHC80  -> [                           "ghc710", "ghc80", "vect"]
    GHC82  -> ["pre-ghc86",               "ghc710", "ghc80", "ghc82", "vect"]
    GHC84  -> ["pre-ghc86",  "pre-ghc88", "ghc710", "ghc80", "ghc82", "ghc84", "vect" ]
    GHC86  -> [              "pre-ghc88", "ghc710", "ghc80", "ghc82", "ghc84", "ghc86" ]
    GHC88  -> [                           "ghc710", "ghc80", "ghc82", "ghc84", "ghc86", "ghc88" ]
    GHC810 -> [                           "ghc710", "ghc80", "ghc82", "ghc84", "ghc86", "ghc88", "ghc810" ]
    GHC90  -> [                           "ghc710", "ghc80", "ghc82", "ghc84", "ghc86", "ghc88", "ghc810", "ghc90"]

    -- GHC90  -> ["ghc90-copied"]
    -- GHC90  -> ["ghc90"]

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
findTests = testList "Round-trip tests" <$> mapM (findTestsDir id mkParserTest) testDirs

findPrettyTests :: IO Test
findPrettyTests =
  testList "Default Annotations round-trip tests"
           <$> mapM (findTestsDir filterPrettyRoundTrip mkPrettyRoundtrip) testDirs

-- | Filter out tests that are known to fail, for particular compilers
filterPrettyRoundTrip :: [FilePath] -> [FilePath]
filterPrettyRoundTrip fps = sort $ Set.toList $ Set.difference (Set.fromList fps) skipped
-- filterPrettyRoundTrip fps = error $ "filterPrettyRoundTrip:fps=" ++ show fps
  where
#if __GLASGOW_HASKELL__ > 800
  -- GHC 8.2
  skipped = Set.empty
#elif __GLASGOW_HASKELL__ >= 711
  -- GHC 8.0
  skipped = Set.fromList
    [
      -- testPrefix </> "ghc80" </> "MultiQuote.hs"
      "MultiQuote.hs"
    , "TestUtils.hs"
    , "T10689a.hs"
    , "Zwaluw.hs"
    , "determ004.hs"
    ]
#else
  -- GHC 7.10
  skipped = Set.empty
#endif

findTestsDir :: ([FilePath] -> [FilePath]) -> (FilePath -> FilePath -> Test) -> FilePath -> IO Test
findTestsDir filterFn mkTestFn dir = do
  let fp = testPrefix </> dir
  fs <- getDirectoryContents fp
  let testFiles = sort $ filter (".hs" `isSuffixOf`) fs
  return $ testList dir (map (\fn -> TestLabel fn (mkTestFn dir fn)) $ filterFn testFiles)

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
                      internalTests,
                      roundTripTests
                    ,
                      transformTests
                    , failingTests
                    , noAnnotationTests
                    ,
                      prettyRoundTripTests
                    ]

-- Tests that will fail until https://phabricator.haskell.org/D907 lands in a
-- future GHC
failingTests :: Test
failingTests = testList "Failing tests"
  [
  -- Tests requiring future GHC modifications
    mkTestModBad "InfixOperator.hs"

#if __GLASGOW_HASKELL__ > 802
#elif __GLASGOW_HASKELL__ > 800
  , mkTestModBad "overloadedlabelsrun04.hs"
#elif __GLASGOW_HASKELL__ > 710
  , mkTestModBad "overloadedlabelsrun04.hs"
  , mkTestModBad "TensorTests.hs" -- Should be fixed in GHC 8.2
  , mkTestModBad "List2.hs"       -- Should be fixed in GHC 8.2
#else
  , mkTestModBad "CtorOp.hs" -- Should be fixed in GHC 8.4
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

    -- mkTestModChange changeRenameCase1 "RenameCase1.hs"

    -- mkParserTest      "ghc710" "Associated.hs"

    -- mkParserTest      "ghc710" "BracesSemiDataDecl.hs"
    -- mkParserTest      "ghc710" "GADTRecords.hs"
    -- mkParserTest      "ghc710" "RdrNames.hs"
    -- mkParserTest      "ghc710" "RdrNames1.hs"

    -- mkParserTest      "ghc80" "T11010.hs"
    -- mkParserTest      "ghc80" "Test10399.hs"
    -- mkParserTest      "ghc90" "Linear12.hs"
    -- mkParserTest      "ghc90" "T17544_kw.hs"

    -- mkParserTest      "ghc90" "FromManual.hs"
    -- mkPrettyRoundtrip  "ghc90" "FromManual.hs"

    -- mkParserTest       "ghc90" "Linear1Rule.hs"
    -- mkPrettyRoundtrip  "ghc90" "Linear1Rule.hs"

    -- mkParserTest       "ghc80" "Test11018.hs"
    -- mkPrettyRoundtrip  "ghc80" "Test11018.hs"

    -- mkParserTest       "ghc86" "UnicodeSyntax.hs"
    -- mkPrettyRoundtrip  "ghc86" "UnicodeSyntax.hs"

    mkParserTest       "ghc86" "empty-foralls.hs"
    -- mkPrettyRoundtrip  "ghc86" "empty-foralls.hs"

    -- mkParserTest       "ghc710" "PatSynBind.hs"
    -- mkPrettyRoundtrip  "ghc710" "PatSynBind.hs"

    -- ---------------------------------------------

    -- mkParserTest       "ghc86" "Webhook.hs"

    -- mkParserTest       "ghc710" "TypeBrackets2.hs"
    -- mkPrettyRoundtrip  "ghc710" "TypeBrackets2.hs"

    -- mkParserTest       "ghc710" "DataDecl.hs"
    -- mkPrettyRoundtrip  "ghc710" "DataDecl.hs"

    -- mkParserTest      "ghc90" "BaseDescriptor.hs"
    -- mkPrettyRoundtrip "ghc90" "BaseDescriptor.hs"

    -- mkParserTest      "ghc90" "BaseDescriptors2.hs"
    -- mkPrettyRoundtrip "ghc90" "BaseDescriptors2.hs"

   -- Needs GHC changes


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
