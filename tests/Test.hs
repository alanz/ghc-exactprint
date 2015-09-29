{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE CPP #-}
-- | Use "runhaskell Setup.hs test" or "cabal test" to run these tests.
module Main where

import Language.Haskell.GHC.ExactPrint.Utils (showGhc)

import qualified FastString     as GHC
import qualified GHC            as GHC

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

testPrefix :: FilePath
testPrefix = "tests" </> "examples"

testList :: String -> [Test] -> Test
testList s ts = TestLabel s (TestList ts)

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
    mkTestModBad "Deprecation.hs"            "Deprecation"
  , mkTestModBad "MultiLineWarningPragma.hs" "Main"
  , mkTestModBad "UnicodeRules.hs"           "Main"

  -- Tests requiring future GHC modifications
  , mkTestModBad "UnicodeSyntax.hs"          "Tutorial"
  , mkTestModBad "InfixOperator.hs"          "Main"
  ]


mkParserTest :: FilePath -> FilePath -> Test
mkParserTest dir fp =
  let basename       = testPrefix </> dir </> fp
      writeFailure   = writeFile (basename <.> "out")
      writeHsPP      = writeFile (basename <.> "hspp")
      writeIncons s  = writeFile (basename <.> "incons") (showGhc s)
  in
    TestCase (do r <- either (\(ParseFailure _ s) -> error s) id
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

tt' :: IO ()
tt' = formatTT =<< partition snd <$> sequence [ return ("", True)
    , manipulateAstTestWFnameMod addLocaLDecl1 "AddLocalDecl1.hs" "AddLocaLDecl1"
    -- , manipulateAstTestWFnameMod addLocaLDecl2 "AddLocalDecl2.hs" "AddLocaLDecl2"
    -- , manipulateAstTestWFnameMod addLocaLDecl3 "AddLocalDecl3.hs" "AddLocaLDecl3"
    -- , manipulateAstTestWFnameMod addLocaLDecl4 "AddLocalDecl4.hs" "AddLocaLDecl4"
    -- , manipulateAstTestWFnameMod addLocaLDecl5 "AddLocalDecl5.hs" "AddLocaLDecl5"
    -- , manipulateAstTestWFnameMod addLocaLDecl6 "AddLocalDecl6.hs" "AddLocaLDecl6"
    -- , manipulateAstTestWFnameMod rmDecl1       "RmDecl1.hs"       "RmDecl1"
    -- , manipulateAstTestWFname "RmDecl2.hs"                        "RmDecl2"
    -- , manipulateAstTestWFnameMod rmDecl2       "RmDecl2.hs"       "RmDecl2"
    -- , manipulateAstTestWFnameMod rmDecl3       "RmDecl3.hs"       "RmDecl3"
    -- , manipulateAstTestWFnameMod rmDecl4       "RmDecl4.hs"       "RmDecl4"
    -- , manipulateAstTestWFnameMod rmDecl5       "RmDecl5.hs"       "RmDecl5"
    -- , manipulateAstTestWFname "RmDecl5.hs"                        "RmDecl5"
    -- , manipulateAstTestWFnameMod rmDecl6       "RmDecl6.hs"       "RmDecl6"
    -- , manipulateAstTestWFnameMod rmDecl7       "RmDecl7.hs"       "RmDecl7"
    -- , manipulateAstTestWFname "TypeSignature.hs"                  "TypeSignature"
    -- , manipulateAstTestWFnameMod rmTypeSig1    "RmTypeSig1.hs"    "RmTypeSig1"
    -- , manipulateAstTestWFnameMod rmTypeSig2    "RmTypeSig2.hs"    "RmTypeSig2"
    -- , manipulateAstTestWFname "StringGap.hs"                      "StringGap"
    -- , manipulateAstTestWFnameMod addHiding1    "AddHiding1.hs"    "AddHiding1"
    -- , manipulateAstTestWFnameMod addHiding2    "AddHiding2.hs"    "AddHiding2"
    -- , manipulateAstTestWFnameMod cloneDecl1    "CloneDecl1.hs"    "CloneDecl1"
    -- , manipulateAstTestWFname "SimpleDo.hs"                      "Main"
    -- , manipulateAstTestWFnameMod changeRename2    "Rename2.hs"  "Main"
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
