{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}
-- | Use "runhaskell Setup.hs test" or "cabal test" to run these tests.
module Main where

import Language.Haskell.GHC.ExactPrint.Utils (showGhc)

-- import qualified FastString     as GHC
-- import qualified GHC            as GHC

-- import qualified Data.Generics as SYB
-- import qualified GHC.SYB.Utils as SYB

import Control.Monad
import System.Directory
import System.FilePath
import System.IO
import System.Exit

import Data.List

import System.IO.Silently

import Test.Common
import Test.Transform

import Test.HUnit


-- import Debug.Trace

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
  "ghc710" : ["ghc8" | ghcVersion >= GHC8]


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
findTests = testList "Round-trip tests" <$> mapM findTestsDir testDirs

findTestsDir :: FilePath -> IO Test
findTestsDir dir = do
  let fp = testPrefix </> dir
  fs <- getDirectoryContents fp
  let testFiles = filter (".hs" `isSuffixOf`) fs
  return $ testList dir (map (mkParserTest dir) testFiles)

mkTests :: IO Test
mkTests = do
  roundTripTests <- findTests
  return $ TestList [roundTripTests, transformTests, failingTests]

-- Tests that will fail until https://phabricator.haskell.org/D907 lands in a
-- future GHC
failingTests :: Test
failingTests = testList "Failing tests"
  [
  -- Require current master #10313 / Phab:D907
    mkTestModBad "Deprecation.hs"
  , mkTestModBad "MultiLineWarningPragma.hs"
  , mkTestModBad "UnicodeRules.hs"

  -- Tests requiring future GHC modifications
  , mkTestModBad "UnicodeSyntax.hs"
  , mkTestModBad "InfixOperator.hs"

  ]


mkParserTest :: FilePath -> FilePath -> Test
mkParserTest dir fp =
  let basename       = testPrefix </> dir </> fp
      writeFailure   = writeFile (basename <.> "out")
      writeHsPP      = writeFile (basename <.> "hspp")
      writeIncons s  = writeFile (basename <.> "incons") (showGhc s)
  in
    TestCase (do r <- either (\(ParseFailure _ s) -> error (s ++ basename)) id
                        <$> roundTripTest basename
                 writeFailure (debugTxt r)
                 forM_ (inconsistent r) writeIncons
                 forM_ (cppStatus r) writeHsPP
                 assertBool fp (status r == Success))


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

tt' :: IO (Counts,Int)
tt' = runTestText (putTextToHandle stdout True) $ TestList [
  -- , mkTestModChange changeLayoutLet2  "LayoutLet2.hs"
  -- , mkTestModChange changeLayoutLet3  "LayoutLet3.hs"
  -- , mkTestModChange changeLayoutLet3  "LayoutLet4.hs"
  -- , mkTestModChange changeRename1     "Rename1.hs"
  -- , mkTestModChange changeRename2     "Rename2.hs"
  -- , mkTestModChange changeLayoutIn1   "LayoutIn1.hs"
  -- , mkTestModChange changeLayoutIn3   "LayoutIn3.hs"
  -- , mkTestModChange changeLayoutIn3   "LayoutIn3a.hs"
  -- , mkTestModChange changeLayoutIn3   "LayoutIn3b.hs"
  -- , mkTestModChange changeLayoutIn4   "LayoutIn4.hs"
  -- , mkTestModChange changeLocToName   "LocToName.hs"
  -- , mkTestModChange changeLetIn1      "LetIn1.hs"
  -- , mkTestModChange changeWhereIn4    "WhereIn4.hs"
  -- , mkTestModChange changeAddDecl     "AddDecl.hs"
    -- mkTestModChange changeLocalDecls  "LocalDecls.hs"
  -- , mkTestModChange changeLocalDecls2 "LocalDecls2.hs"
  -- , mkTestModChange changeWhereIn3a   "WhereIn3a.hs"
    -- , mkTestModChange changeRenameCase1 "RenameCase1.hs"
    -- , mkTestModChange changeRenameCase2 "RenameCase2.hs"
      -- mkTestModBad "QuasiQuote2.hs"
      mkParserTest "ghc710" "Minimal.hs"
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


pwd :: IO FilePath
pwd = getCurrentDirectory

cd :: FilePath -> IO ()
cd = setCurrentDirectory
