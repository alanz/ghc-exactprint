{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Exception
import System.FilePath

import System.Exit

import System.Directory

import Test.CommonUtils
import Test.HUnit

import Debug.Trace
import Control.Monad
import System.Environment

import qualified Data.Set as S

import Test.Common

import System.IO.Temp
import System.IO


data Verbosity = Debug | Status | None deriving (Eq, Show, Ord, Enum)

verb :: Verbosity
verb = Debug

cppFile, parseFailFile, processed,blackListed,logFile :: String
cppFile       = "cpp.txt"
parseFailFile = "pfail.txt"
processed     = "processed.txt"
blackListed   = "blacklist.txt"
logFile       = "roundtrip.log"

writeCPP :: FilePath -> IO ()
writeCPP fp = appendFileFlush cppFile (('\n' : fp))

writeError = writeCPP

writeParseFail :: FilePath -> String -> IO ()
writeParseFail fp _s = appendFileFlush parseFailFile (('\n' : fp))
-- writeParseFail fp s = appendFileFlush parseFailFile (('\n' : (fp ++ " " ++ s)))

writeProcessed :: FilePath -> IO ()
writeProcessed fp = appendFileFlush processed (('\n' : fp))

writeLog :: String -> IO ()
writeLog msg = appendFileFlush logFile (('\n' : msg))

appendFileFlush      :: FilePath -> String -> IO ()
appendFileFlush f txt = withFile f AppendMode (\ hdl -> hPutStr hdl txt >> hFlush hdl)

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
      writeFile processed ""
      writeFile parseFailFile ""
      writeFile cppFile ""
      writeFile logFile ""
      removeDirectoryRecursive "tests/roundtrip"
      createDirectory "tests/roundtrip"
      putStrLn "Done."
    -- ds -> () <$ (runTests =<< (TestList <$> mapM tests ds))
    ds -> do
      isBlackList <- doesFileExist blackListed
      blackList <- if isBlackList
                      then lines <$> readFile blackListed
                      else return []
      !processedList <- lines <$> readFile processed
      !cppList       <- lines <$> readFile cppFile
      !parseFailList <- lines <$> readFile parseFailFile
      let done = S.fromList (processedList ++ cppList ++ blackList)
      tsts <- TestList <$> mapM (tests done) ds
      runTests tsts
      return ()

runTests :: Test -> IO Counts
runTests t = do
  let n = testCaseCount t
  putStrLn $ "Running " ++ show n ++ " tests."
  putStrLn $ "Verbosity: " ++ show verb
  runTestTT t

tests :: S.Set String ->  FilePath -> IO Test
tests done dir = do
  roundTripHackage done dir

-- Selection:

-- Hackage dir
roundTripHackage :: S.Set String -> FilePath -> IO Test
roundTripHackage done hackageDir = do
  packageDirs <- drop 2 <$> getDirectoryContents hackageDir
  when (verb <= Debug) (traceShowM hackageDir)
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
    TestLabel fp $
    TestCase (do writeLog $ "starting:" ++ fp
                 -- r1 <- roundTripTest fp
                 r1 <- catchAny (roundTripTest fp) $ \e -> do
                   writeError fp
                   throwIO e
    {-
    x <- catchAny dangerous $ \e -> do
        putStrLn $ "Caught an exception: " ++ show e
        return (-1)
    -}
                 case r1 of
                   Left (ParseFailure _ s) -> do
                     writeParseFail fp s
                     exitFailure
                   Right r -> do
                     writeProcessed fp
                     unless (status r == Success) (writeFailure fp (debugTxt r))
                     assertBool fp (status r == Success))
{-
mkParserTest fp =
    TestLabel fp $
    TestCase (do r <- either (\(ParseFailure _ s) -> exitFailure) return
                        =<< roundTripTest fp
                 writeProcessed fp
                 unless (status r == Success) (writeFailure fp (debugTxt r))
                 assertBool fp (status r == Success))
-}

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch


writeFailure :: FilePath -> String -> IO ()
writeFailure fp db = do
  let outdir      = "tests" </> "roundtrip"
      outname     = takeFileName fp <.> "out"
  (_fname, handle) <- openTempFile outdir outname
  (hPutStr handle db >> hClose handle)
