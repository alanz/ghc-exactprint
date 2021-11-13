{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
-- | Use "runhaskell Setup.hs test" or "cabal test" to run these tests.
module Main where

-- import Language.Haskell.GHC.ExactPrint.Utils ( showGhc )
import qualified GHC.Paths
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

data GHCVersion = GHC710 | GHC80 | GHC82 | GHC84 | GHC86 | GHC88 | GHC810 | GHC90 | GHC92
     deriving (Eq, Ord, Show)

ghcVersion :: GHCVersion
ghcVersion = GHC92

-- | Directories to automatically find roundtrip tests
testDirs :: [FilePath]
testDirs =
  case ghcVersion of
    GHC90  -> ["ghc710", "ghc80", "ghc82", "ghc84", "ghc86", "ghc88", "ghc810", "ghc90"]
    GHC92  -> ["ghc710", "ghc80", "ghc82", "ghc84", "ghc86", "ghc88", "ghc810", "ghc90", "ghc92"]

    -- GHC92  -> ["ghc92-copied"]
    -- GHC92  -> ["ghc92"]

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
  let libdir = GHC.Paths.libdir
  cnts <- fst <$> runTestText (putTextToHandle stdout True) (transformTestsTT libdir)
  putStrLn $ show cnts
  if errors cnts > 0 || failures cnts > 0
     then exitFailure
     else return () -- exitSuccess

-- ---------------------------------------------------------------------

findTests :: LibDir -> IO Test
findTests libdir
  = testList "Round-trip tests" <$> mapM (findTestsDir id (mkParserTest libdir)) testDirs

findTestsBC :: LibDir -> IO Test
findTestsBC libdir
  = testList "Balance comments tests" <$> mapM (findTestsDir id (mkParserTestBC libdir)) testDirs

findTestsMD :: LibDir -> IO Test
findTestsMD libdir
  = testList "Make Delta tests" <$> mapM (findTestsDir id (mkParserTestMD libdir)) testDirs

findPrettyTests :: LibDir -> IO Test
findPrettyTests libdir =
  testList "Default Annotations round-trip tests"
           <$> mapM (findTestsDir filterPrettyRoundTrip (mkPrettyRoundtrip libdir)) testDirs

-- | Filter out tests that are known to fail, for particular compilers
filterPrettyRoundTrip :: [FilePath] -> [FilePath]
filterPrettyRoundTrip fps = sort $ Set.toList $ Set.difference (Set.fromList fps) skipped
-- filterPrettyRoundTrip fps = error $ "filterPrettyRoundTrip:fps=" ++ show fps
  where
  skipped = Set.empty

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
  let libdir = GHC.Paths.libdir
  roundTripTests <- findTests libdir
  roundTripBalanceCommentsTests <- findTestsBC libdir
  roundTripMakeDeltaTests <- findTestsMD libdir
  prettyRoundTripTests <- findPrettyTests libdir
  return $ TestList [
                    --   internalTests,
                    --   roundTripTests
                    -- ,
                    --   (transformTests libdir)
                    -- , (failingTests libdir)
                    -- ,
                      roundTripBalanceCommentsTests
                    -- ,
                    --   roundTripMakeDeltaTests
                    ]
                    -- , noAnnotationTests
                    -- ,
                    --   prettyRoundTripTests
                    -- ,

failingTests :: LibDir -> Test
failingTests libdir = testList "Failing tests"
  [
  -- Tests requiring future GHC modifications

  -- We do not capture EOF location very well any more
    mkTestModBad libdir "T10970a.hs"

  ]


mkParserTest :: LibDir -> FilePath -> FilePath -> Test
mkParserTest libdir dir fp = mkParsingTest (roundTripTest libdir) dir fp

mkParserTestBC :: LibDir -> FilePath -> FilePath -> Test
mkParserTestBC libdir dir fp = mkParsingTest (roundTripTestBC libdir) dir fp

mkParserTestMD :: LibDir -> FilePath -> FilePath -> Test
mkParserTestMD libdir dir fp = mkParsingTest (roundTripTestMD libdir) dir fp

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
  let libdir = GHC.Paths.libdir
  prettyRoundTripTests <- findPrettyTests libdir
  runTestText (putTextToHandle stdout True) prettyRoundTripTests

tt' :: IO (Counts,Int)
tt' = do
  let libdir = GHC.Paths.libdir
  runTestText (putTextToHandle stdout True) $ TestList [

    -- mkTestModChange libdir changeLayoutLet2 "LayoutLet2.hs"
    -- mkParserTestMD libdir      "transform" "LayoutLet2.hs"

    -- mkTestModChange libdir rmTypeSig1 "RmTypeSig1.hs"
    -- mkTestModChange libdir changeLocToName   "LocToName.hs"
    -- mkParserTest libdir      "ghc80" "T10970a.hs"

    -- mkTestModChange libdir changeLocalDecls2  "LocalDecls2.hs"
    -- mkParserTestMD libdir "transform"  "LocalDecls2.hs"
    -- mkParserTest libdir "transform"  "LocalDecls2.hs"

    -- mkTestModChange libdir addLocaLDecl2  "AddLocalDecl2.hs"
    -- mkParserTestMD libdir "transform"  "AddLocalDecl2.hs"
    -- mkParserTest libdir "transform"  "AddLocalDecl2.hs"

    -- mkParserTest libdir "ghc710" "Control.hs"
    -- mkParserTestBC libdir "ghc710" "Control.hs"

    -- mkTestModChange libdir changeAddDecl     "AddDecl.hs"
    -- mkParserTestMD libdir "transform"  "AddDecl.hs"

    -- mkParserTest libdir      "ghc710" "EmptyMostly.hs"

    -- mkParserTest libdir      "ghc92-copied" "AddLocalDecl5.expected.hs"

    -- mkParserTest libdir      "ghc710" "QuasiQuote.hs"
    -- mkParserTestBC libdir      "ghc710" "QuasiQuote.hs"
    -- mkParserTestMD libdir      "ghc92" "DependentStmt.hs"
    -- mkParserTestMD libdir      "ghc710" "AddAndOr3.hs"
    -- mkParserTestMD libdir      "ghc92" "Records2.hs"
    -- mkParserTestMD libdir      "ghc92" "AdhocRule.hs"
    -- mkParserTestMD libdir      "ghc92" "Observer.hs"

    -- mkParserTest libdir      "ghc92" "CommentPlacement.hs"
    -- mkParserTestMD libdir      "ghc92" "CommentPlacement.hs"

    -- mkParserTest libdir      "ghc92" "CommentPlacement3.hs"
    -- mkParserTestMD libdir      "ghc92" "CommentPlacement3.hs"

    -- mkParserTestMD libdir      "ghc92" "proc-do-complex.hs"
    -- mkParserTestMD libdir      "ghc92" "ConstructorComment.hs"

    -- mkParserTest libdir      "ghc92" "Observer1.hs"
    -- mkParserTestMD libdir      "ghc92" "Observer1.hs"

    -- mkParserTest libdir      "ghc92" "ScopesBug.hs"
    -- mkParserTestMD libdir      "ghc92" "ScopesBug.hs"

    -- mkParserTest libdir      "ghc92" "Field.hs"
    -- mkParserTestMD libdir      "ghc92" "Field.hs"

    -- mkParserTest libdir      "ghc88" "EmptyCase008.hs"
    -- mkParserTestMD libdir      "ghc88" "EmptyCase008.hs"

    -- mkParserTest libdir      "ghc86" "HashTab.hs"
    -- mkParserTestMD libdir      "ghc86" "HashTab.hs"
    -- mkParserTestBC libdir      "ghc86" "HashTab.hs"

    -- mkParserTest libdir      "ghc80" "T6018failclosed.hs"
    -- mkParserTestMD libdir      "ghc80" "T6018failclosed.hs"

    -- mkParserTest libdir      "ghc92" "TypeFamilies.hs"
    -- mkParserTestMD libdir      "ghc92" "TypeFamilies.hs"

    -- mkParserTest libdir      "ghc92" "StringRef.hs"
    -- mkParserTestMD libdir      "ghc92" "StringRef.hs"

    -- mkParserTestMD libdir      "ghc92" "proc-do-complex.hs"
    -- mkParserTestMD libdir      "ghc88" "Utils.hs"
    -- mkParserTestMD libdir      "ghc92" "ParensGADT.hs"
    -- mkParserTestMD libdir      "ghc80" "Test10354.hs"

    -- Still to fix, think it is local to ExactPrint.hs
    -- mkParserTestMD libdir      "ghc710" "RdrNames.hs"

    -- mkTestModChange libdir rmDecl4  "RmDecl4.hs"
    -- mkParserTest libdir     "ghc92" "RmDecl4.hs"
    -- mkParserTestMD libdir     "ghc92" "RmDecl4.hs"

    -- mkParserTestMD libdir     "ghc92" "Records2.hs"

    -- mkTestModChange libdir rmTypeSig1  "RmTypeSig1.hs"

    -- mkParserTest libdir      "ghc92" "Observer1.hs"
    -- mkParserTestMD libdir      "ghc92" "Observer1.hs"

    -- mkParserTest libdir     "ghc92" "IndentedComments.hs"
    -- mkParserTestMD libdir     "ghc92" "IndentedComments.hs"

    -- Fixed by diff to make LocatedA Modulename
    -- mkParserTestMD libdir      "ghc710" "LetExprSemi.hs"

    -- mkParserTestMD libdir      "ghc710" "AltsSemis.hs"
    -- mkParserTest libdir      "ghc710" "AltsSemis.hs"
    -- mkParserTest libdir      "ghc80" "Match.hs"
    -- mkTestModBad libdir "n-plus-k-patterns.hs"

    -- mkParserTest libdir "ghc92" "TopLevelSemis.hs"
    -- mkParserTestBC libdir "ghc92" "TopLevelSemis.hs"

    -- mkParserTest libdir "ghc92" "TopLevelSemis1.hs"
    -- mkParserTestBC libdir "ghc92" "TopLevelSemis1.hs"

    -- mkParserTest libdir      "ghc92" "n-plus-k-patterns.hs"
    -- mkParserTestMD libdir      "ghc92" "n-plus-k-patterns.hs"

    mkParserTestMD libdir      "ghc92" "Retrie.hs"

    -- mkParserTest libdir      "ghc92" "BalanceComments1.hs"
    -- mkParserTestBC libdir    "ghc92" "BalanceComments1.hs"

-- ### Failure in: 0:Round-trip tests:0:ghc710:62:HsDo.hs
-- ### Failure in: 0:Round-trip tests:0:ghc710:110:PatBind.hs
-- ### Failure in: 0:Round-trip tests:0:ghc710:120:QuasiQuote.hs
-- ### Failure in: 0:Round-trip tests:0:ghc710:125:RebindableSyntax.hs
-- ### Failure in: 0:Round-trip tests:0:ghc710:129:RecursiveDo.hs
-- ### Failure in: 0:Round-trip tests:0:ghc710:131:Remorse.hs
-- ### Failure in: 0:Round-trip tests:0:ghc710:155:Stream.hs
-- ### Failure in: 0:Round-trip tests:0:ghc710:168:Trit.hs
-- ### Failure in: 0:Round-trip tests:0:ghc710:170:TupleSections.hs
-- ### Failure in: 0:Round-trip tests:0:ghc710:177:TypeSignature.hs
-- ### Failure in: 0:Round-trip tests:1:ghc80:1:AddParams2.hs
-- ### Failure in: 0:Round-trip tests:1:ghc80:12:CheckUtils.hs
-- ### Failure in: 0:Round-trip tests:1:ghc80:18:CmmSwitchTestGen.hs
-- ### Failure in: 0:Round-trip tests:1:ghc80:19:Collapse1.hs
-- ### Failure in: 0:Round-trip tests:1:ghc80:147:SayAnnNames.hs


   -- Needs GHC changes


    ]

testsTT :: LibDir -> Test
testsTT libdir = TestList
  [
    mkParserTest libdir "ghc710" "Cpp.hs"
  , mkParserTest libdir "ghc710" "DroppedDoSpace.hs"
  ]

tt :: IO ()
-- tt = hSilence [stderr] $ do
tt = do
  let libdir = GHC.Paths.libdir
  cnts <- fst <$> runTestText (putTextToHandle stdout True) (testsTT libdir)
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
