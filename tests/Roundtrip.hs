{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import System.FilePath
import qualified Data.Text.IO as T
import qualified Data.Text as T


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

data Verbosity = Debug | Status | None deriving (Eq, Show, Ord, Enum)

verb :: Verbosity
verb = Debug

cppFile, parseFailFile, processed :: String
cppFile = "cpp.txt"
parseFailFile = "pfail.txt"
processed = "processed.txt"

writeCPP :: FilePath -> IO ()
writeCPP fp = T.appendFile cppFile (T.pack ('\n' : fp))

writeParseFail :: FilePath -> String -> IO ()
writeParseFail fp s = T.appendFile parseFailFile (T.pack ('\n' : (fp ++ " " ++ s)))

writeProcessed :: FilePath -> IO ()
writeProcessed fp = T.appendFile processed (T.pack ('\n' : fp))


main :: IO ()
main = do
  as <- getArgs
  case as of
    [] -> putStrLn "Must enter directory to process"
    ds -> () <$ (runTests =<< (TestList <$> mapM tests ds))

runTests :: Test -> IO Counts
runTests t = do
  let n = testCaseCount t
  putStrLn $ "Running " ++ show n ++ " tests."
  putStrLn $ "Verbosity: " ++ show verb
  runTestTT t

tests :: FilePath -> IO Test
tests dir = do
  done <- S.fromList . lines . T.unpack <$> T.readFile processed
  roundTripHackage done dir

mkDirTest :: FilePath -> [(FilePath, Report)] -> Test
mkDirTest d ts = TestLabel d (TestList (map (uncurry mkTest) ts))

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

  r <- mapM roundTripTest hsFiles
  return (TestLabel (dropFileName dir) (TestList $ zipWith mkTest (map takeFileName hsFiles) r))

mkTest :: FilePath -> Report -> Test
mkTest file r =
  case r of
    Success -> TestLabel file (TestCase (return ()))
    a -> traceShow a $ TestLabel file (TestCase exitFailure)
