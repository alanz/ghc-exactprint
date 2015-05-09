{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.FilePath

import Data.List hiding (find)

import System.Exit

import System.Directory

import Test.HUnit

import System.FilePath.Find

import Debug.Trace
import Control.Monad
import System.Environment

import qualified Data.Set as S

import Common

import System.IO.Temp
import System.IO (hClose, hPutStr)


data Verbosity = Debug | Status | None deriving (Eq, Show, Ord, Enum)

verb :: Verbosity
verb = Debug

cppFile, parseFailFile, processed :: String
cppFile = "cpp.txt"
parseFailFile = "pfail.txt"
processed = "processed.txt"

writeCPP :: FilePath -> IO ()
writeCPP fp = appendFile cppFile (('\n' : fp))

writeParseFail :: FilePath -> String -> IO ()
writeParseFail fp s = appendFile parseFailFile (('\n' : (fp ++ " " ++ s)))

writeProcessed :: FilePath -> IO ()
writeProcessed fp = appendFile processed (('\n' : fp))



main :: IO ()
main = do
  as <- getArgs
  case as of
    [] -> putStrLn "Must enter directory to process"
    ["failures"] -> do
      fs <- lines <$> readFile "origfailures.txt"
      () <$ runTests (TestList (map mkParserTest fs))
    ["clean"] -> do
      putStrLn "Cleaning..."
      writeFile "processed.txt" ""
      writeFile "pfail.txt" ""
      writeFile "cpp.txt" ""
      removeDirectoryRecursive "tests/roundtrip"
      createDirectory "tests/roundtrip"
      putStrLn "Done."
    ds -> () <$ (runTests =<< (TestList <$> mapM tests ds))

runTests :: Test -> IO Counts
runTests t = do
  let n = testCaseCount t
  putStrLn $ "Running " ++ show n ++ " tests."
  putStrLn $ "Verbosity: " ++ show verb
  runTestTT t

tests :: FilePath -> IO Test
tests dir = do
  done <- S.fromList . lines <$> readFile processed
  roundTripHackage done dir

-- Selection:

-- Given base directory finds all haskell source files
findSrcFiles :: FilePath -> IO [FilePath]
findSrcFiles = find filterDirectory filterFilename

filterDirectory :: FindClause Bool
filterDirectory =
  p <$> fileName
  where
    p x
      | "." `isPrefixOf` x = False
      | otherwise = True

filterFilename :: FindClause Bool
filterFilename = do
  ext <- extension
  fname <- fileName
  return (ext == ".hs" && p fname)
  where
    p x
      | "refactored" `isInfixOf` x = False
      | "Setup.hs" `isInfixOf` x = False
      | "HLint.hs" `isInfixOf` x = False -- HLint config files
      | otherwise                 = True

-- Hackage dir
roundTripHackage :: S.Set String -> FilePath -> IO Test
roundTripHackage done hackageDir = do
  packageDirs <- drop 2 <$> getDirectoryContents hackageDir
  when (verb <= Debug) (traceShowM packageDirs)
  TestList <$> mapM (roundTripPackage done) (zip [0..] (map (hackageDir </>) packageDirs))


roundTripPackage :: S.Set String -> (Int, FilePath) -> IO Test
roundTripPackage done (n, dir) = do
  putStrLn (show n)
  when (verb <= Status) (traceM dir)
  hsFiles <- filter (flip S.notMember done)  <$> findSrcFiles dir

  return (TestLabel (dropFileName dir) (TestList $ map mkParserTest hsFiles))

mkParserTest :: FilePath -> Test
mkParserTest fp =
    TestCase (do r <- either (\(ParseFailure _ s) -> exitFailure) return
                        =<< roundTripTest fp
                 writeProcessed fp
                 unless (status r == Success) (writeFailure fp (debugTxt r))
                 assertBool fp (status r == Success))


writeFailure :: FilePath -> String -> IO ()
writeFailure fp db = do
  let outdir      = "tests" </> "roundtrip"
      outname     = takeFileName fp <.> "out"
  (fname, handle) <- openTempFile outdir outname
  (hPutStr handle db >> hClose handle)
