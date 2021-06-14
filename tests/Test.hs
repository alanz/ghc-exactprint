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
  prettyRoundTripTests <- findPrettyTests libdir
  return $ TestList [
                      internalTests,
                      roundTripTests
                    ,
                      (transformTests libdir)
                    , (failingTests libdir)
                    -- , noAnnotationTests
                    -- ,
                    --   prettyRoundTripTests
                    ]

-- Tests that will fail until https://phabricator.haskell.org/D907 lands in a
-- future GHC
failingTests :: LibDir -> Test
failingTests libdir = testList "Failing tests"
  [
  -- Tests requiring future GHC modifications
    mkTestModBad libdir "InfixOperator.hs"
  ]


mkParserTest :: LibDir -> FilePath -> FilePath -> Test
mkParserTest libdir dir fp = mkParsingTest (roundTripTest libdir) dir fp

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

    -- mkTestModChange libdir changeRenameCase1 "RenameCase1.hs"

    -- mkParserTest libdir      "ghc710" "Associated.hs"
    -- mkParserTest libdir      "ghc710" "EmptyMostly.hs"
    -- mkParserTest libdir      "ghc710" "EmptyMostlyInst.hs"
    -- mkParserTest libdir      "ghc710" "BracesSemiDataDecl.hs"
    -- mkParserTest libdir      "ghc710" "DataDecl.hs"
    -- mkParserTest libdir      "ghc710" "ForeignDecl.hs"
    -- mkParserTest libdir      "ghc710" "Jon.hs"
    -- mkParserTest libdir      "ghc92" "MatchSemis.hs"
    -- mkParserTest libdir      "ghc710" "LinePragma.hs"
    -- mkParserTest libdir      "ghc92" "BlockComment.hs"
    -- mkParserTest libdir      "ghc710" "Cpp.hs"
    -- mkParserTest libdir      "ghc710" "MultiLineCommentWithPragmas.hs"
    -- mkParserTest libdir      "ghc710" "PatBind.hs"
    -- mkParserTest libdir      "ghc710" "RecordSemi.hs"
    -- mkParserTest libdir      "ghc710" "RecursiveDo.hs"
    -- mkParserTest libdir      "ghc710" "Roles.hs"
    -- mkParserTest libdir      "ghc710" "Stmts.hs"
    -- mkParserTest libdir      "ghc710" "TypeBrackets.hs"
    -- mkParserTest libdir      "ghc710" "TypeBrackets4.hs"
    -- mkParserTest libdir      "ghc710" "TypeSynParens.hs"
    -- mkParserTest libdir      "ghc710" "UnicodeSyntaxFailure.hs"
    -- mkParserTest libdir      "ghc80" "Class.hs"
    -- mkParserTest libdir      "ghc80" "MonadFailErrors.hs"
    -- mkParserTest libdir      "ghc80" "ParenFunBind.hs"
    -- mkParserTest libdir      "ghc80" "SemicolonIf.hs"
    -- mkParserTest libdir      "ghc80" "T10670.hs"
    -- mkParserTest libdir      "ghc80" "T10734.hs"
    -- mkParserTest libdir      "ghc80" "T10836.hs"
    -- mkParserTest libdir      "ghc80" "T11010.hs"
    -- mkParserTest libdir      "ghc80" "T6018failclosed.hs"
    -- mkParserTest libdir      "ghc80" "T8970.hs"
    -- mkParserTest libdir      "ghc80" "TH_abstractFamily.hs"
    -- mkParserTest libdir      "ghc80" "overloadedlabelsfail01.hs"
    -- mkParserTest libdir      "ghc80" "overloadedrecfldsrun05.hs"
    -- mkParserTest libdir      "ghc82" "Completesig03A.hs"
    -- mkParserTest libdir      "ghc82" "brackets.hs"
    -- mkParserTest libdir      "ghc84" "T13747.hs"
    -- mkParserTest libdir      "ghc86" "SlidingTypeSyn.hs"

    -- mkParserTest libdir      "ghc710" "TypeOperators.hs"
    -- mkParserTest libdir      "ghc710" "TypeBrackets.hs"
    -- mkParserTest libdir      "ghc710" "TypeBrackets2.hs"
    -- mkParserTest libdir      "ghc80" "TestUtils.hs"
    -- mkParserTest libdir      "ghc86" "Webhook.hs"
    -- mkParserTest libdir      "ghc86" "dynamic-paper.hs"
    -- mkParserTest libdir      "ghc90" "ArrowLambdaCase.hs"
    -- mkParserTest libdir      "ghc90" "CSETest.hs"
    -- mkParserTest libdir      "ghc90" "ExplicitSpecificity4.hs"
    -- mkParserTest libdir      "ghc90" "FromManual.hs"
    -- mkParserTest libdir      "ghc90" "Linear1Rule.hs"
    -- mkParserTest libdir      "ghc80" "T6018failclosed.hs"
    mkParserTest libdir      "failing" "InfixOperator.hs"

    -- mkParserTest libdir      "ghc92-copied" "AddLocalDecl5.expected.hs"
    -- mkParserTest libdir      "ghc92" "ScopesBug.hs"
    -- mkParserTest libdir      "ghc92-copied" "T10279.hs"
    -- mkParserTest libdir      "ghc92-copied" "T10891.hs"
    -- mkParserTest libdir      "ghc92-copied" "T2632.hs"
    -- mkParserTest libdir      "ghc92-copied" "T4442.hs"
    -- mkParserTest libdir      "ghc92-copied" "TH_reifyExplicitForAllFams.hs"
    -- mkParserTest libdir      "ghc92-copied" "TH_unresolvedInfix.hs"
    -- mkParserTest libdir      "ghc92-copied" "regalloc_unit_tests.hs"

    -- mkTestModChange libdir rmDecl1  "RmDecl1.hs"

    -- mkParserTest libdir      "ghc92" "LinearArrow.hs"
    -- mkParserTest libdir      "transform" "AddLocalDecl5.1.hs"
    -- mkTestModChange libdir addLocaLDecl5  "AddLocalDecl5.hs"
    -- mkTestModChange libdir changeLocalDecls2  "LocalDecls2.hs"
    -- mkTestModChange libdir addLocaLDecl1  "AddLocalDecl1.hs"
    {-
    ### Failure in: 1:Round-trip tests:1:ghc80:27:Decision.hs
    ### Failure in: 2:transformation tests:0:Low level transformations:15
    AddLocalDecl1.hs
    AddLocalDecl4.hs
    AddLocalDecl5.hs
    AddLocalDecl6.hs
    -}

    -- mkParserTest libdir      "ghc710" "EmptyMostly.hs"

    -- comment problem
    -- mkParserTest libdir      "ghc710" "Move1.hs"
    -- mkParserTest libdir      "ghc80" "Decision.hs"
    -- mkParserTest libdir      "ghc80" "RandomPGC.hs"
    -- mkParserTest libdir      "ghc92" "BlockComment.hs"
    -- mkParserTest libdir      "ghc92" "CommentPlacement.hs"
    -- mkParserTest libdir      "ghc92" "CommentPlacement2.hs"
    -- mkParserTest libdir      "ghc80" "Decision.hs"

    -- mkParserTest libdir      "ghc92-copied" "AddLocalDecl5.expected.hs"
    -- mkParserTest libdir      "ghc92-copied" "AtomicPrimops.hs"
    -- mkParserTest libdir      "ghc92-copied" "BinaryLiterals0.hs"
    -- mkParserTest libdir      "ghc92-copied" "CountDeps.hs"
    -- mkParserTest libdir      "ghc92-copied" "regalloc_unit_tests.hs"



-- ### Failure in: 1:Round-trip tests:0:ghc710:20:Control.hs
-- ### Failure in: 1:Round-trip tests:0:ghc710:21:CoreIr.hs
-- ### Failure in: 1:Round-trip tests:0:ghc710:23:Cpp.hs
-- ### Failure in: 1:Round-trip tests:0:ghc710:38:EmptyMostly.hs
-- ### Failure in: 1:Round-trip tests:0:ghc710:39:EmptyMostly2.hs
-- ### Failure in: 1:Round-trip tests:0:ghc710:40:EmptyMostlyInst.hs
-- ### Failure in: 1:Round-trip tests:0:ghc710:41:EmptyMostlyNoSemis.hs
-- ### Error in:   1:Round-trip tests:0:ghc710:50:ForeignDecl.hs

    -- mkParserTest libdir      "ghc710" "BracesSemiDataDecl.hs"
    -- mkParserTest libdir      "ghc710" "GADTRecords.hs"
    -- mkParserTest libdir      "ghc710" "RdrNames.hs"
    -- mkParserTest libdir      "ghc710" "RdrNames1.hs"

    -- mkParserTest libdir      "ghc80" "T11010.hs"
    -- mkParserTest libdir      "ghc80" "Test10399.hs"
    -- mkParserTest libdir      "ghc90" "Linear12.hs"
    -- mkParserTest libdir      "ghc90" "T17544_kw.hs"

    -- mkParserTest libdir      "ghc90" "FromManual.hs"
    -- mkPrettyRoundtrip libdir  "ghc90" "FromManual.hs"

    -- mkParserTest libdir       "ghc90" "Linear1Rule.hs"
    -- mkPrettyRoundtrip libdir  "ghc90" "Linear1Rule.hs"

    -- mkParserTest libdir       "ghc80" "Test11018.hs"
    -- mkPrettyRoundtrip libdir  "ghc80" "Test11018.hs"

    -- mkParserTest libdir       "ghc86" "UnicodeSyntax.hs"
    -- mkPrettyRoundtrip libdir  "ghc86" "UnicodeSyntax.hs"

    -- mkParserTest libdir       "ghc86" "empty-foralls.hs"
    -- mkPrettyRoundtrip libdir  "ghc86" "empty-foralls.hs"

    -- mkParserTest libdir       "ghc710" "PatSynBind.hs"
    -- mkPrettyRoundtrip libdir  "ghc710" "PatSynBind.hs"

    -- ---------------------------------------------

    -- mkParserTest libdir       "ghc86" "Webhook.hs"

    -- mkParserTest libdir       "ghc710" "TypeBrackets2.hs"
    -- mkPrettyRoundtrip libdir  "ghc710" "TypeBrackets2.hs"

    -- mkParserTest libdir       "ghc710" "DataDecl.hs"
    -- mkPrettyRoundtrip libdir  "ghc710" "DataDecl.hs"

    -- mkParserTest libdir      "ghc90" "BaseDescriptor.hs"
    -- mkPrettyRoundtrip libdir "ghc90" "BaseDescriptor.hs"

    -- mkParserTest libdir      "ghc90" "BaseDescriptors2.hs"
    -- mkPrettyRoundtrip libdir "ghc90" "BaseDescriptors2.hs"

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
