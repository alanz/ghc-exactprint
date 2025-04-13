{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified GHC.Paths
import Control.Exception
import Control.Monad
import Data.IORef
import Data.Time.Clock
import Data.Time.Format
import Debug.Trace
import System.Directory
import System.Environment
import System.Exit
import System.FilePath
import System.IO
import Test.Common
import Test.CommonUtils
import Test.HUnit
import qualified Data.Set as S

import Language.Haskell.GHC.ExactPrint.Parsers (macroIORef)

-- ---------------------------------------------------------------------

data Verbosity = Debug | Status | None deriving (Eq, Show, Ord, Enum)

verb :: Verbosity
verb = Debug

-- ---------------------------------------------------------------------

writeCPP :: FilePath -> IO ()
writeCPP fp = appendFileFlush cppFile (('\n' : fp))

writeError :: FilePath -> IO ()
writeError = writeCPP

writeParseFail :: FilePath -> String -> IO ()
writeParseFail fp _s = appendFileFlush parseFailFile (('\n' : fp))
-- writeParseFail fp s = appendFileFlush parseFailFile (('\n' : (fp ++ " " ++ s)))

writeProcessed :: FilePath -> IO ()
writeProcessed fp = appendFileFlush processed (('\n' : fp))

writeFailed :: FilePath -> IO ()
writeFailed fp = appendFileFlush processedFailFile (('\n' : fp))

writeLog :: String -> IO ()
writeLog msg = appendFileFlush logFile (('\n' : msg))

getTimeStamp :: IO String
getTimeStamp = do
  t <- getCurrentTime
  return $ formatTime defaultTimeLocale (iso8601DateFormat (Just "%H%M%S")) t

writeFailure :: FilePath -> String -> IO ()
writeFailure fp db = do
  ts <- getTimeStamp
  let outname = failuresDir </> takeFileName fp <.> ts <.> "out"
  writeFile outname db

appendFileFlush      :: FilePath -> String -> IO ()
appendFileFlush f txt = withFile f AppendMode (\ hdl -> hPutStr hdl txt >> hFlush hdl)

-- ---------------------------------------------------------------------

readFileIfPresent :: FilePath -> IO [String]
readFileIfPresent fileName = do
  isPresent <- doesFileExist fileName
  if isPresent
    then lines <$> readFile fileName
    else return []

-- ---------------------------------------------------------------------

main :: IO ()
main = do
  let libdir = GHC.Paths.libdir
  createDirectoryIfMissing True workDir
  createDirectoryIfMissing True configDir
  createDirectoryIfMissing True failuresDir
  ms <- getHackageVersionMacros
  writeIORef macroIORef ms
  as <- getArgs
  case as of
    [] -> putStrLn "Must enter directory to process"
    ["failures"] -> do
      -- !knownFailures <- readFileIfPresent knownFailuresFile
      -- let done = S.fromList knownFailures
      fs <- lines <$> readFile origFailuresFile
      () <$ runTests (TestList (map (mkParserTest libdir) fs))
    ["clean"] -> do
      putStrLn "Cleaning..."
      writeFile processed ""
      writeFile parseFailFile ""
      writeFile cppFile ""
      writeFile logFile ""
      writeFile processedFailFile ""
      removeDirectoryRecursive failuresDir
      createDirectory failuresDir
      putStrLn "Done."
    -- ds -> () <$ (runTests =<< (TestList <$> mapM tests ds))
    ds -> do
      !blackList     <- readFileIfPresent blackListed
      !knownFailures <- readFileIfPresent knownFailuresFile
      !processedList <- lines <$> readFile processed
      !cppList       <- lines <$> readFile cppFile
      !parseFailList <- lines <$> readFile parseFailFile
      let done = S.fromList (processedList ++ cppList ++ blackList ++ knownFailures ++ parseFailList)
      tsts <- TestList <$> mapM (tests libdir done) ds
      _ <- runTests tsts
      return ()

runTests :: Test -> IO Counts
runTests t = do
  let n = testCaseCount t
  putStrLn $ "Running " ++ show n ++ " tests."
  putStrLn $ "Verbosity: " ++ show verb
  runTestTT t

tests :: LibDir -> S.Set String ->  FilePath -> IO Test
tests libdir done dir = do
  roundTripHackage libdir done dir

-- Selection:

-- Hackage dir
roundTripHackage :: LibDir -> S.Set String -> FilePath -> IO Test
roundTripHackage libdir done hackageDir = do
  packageDirs <- listDirectory hackageDir
  dirsOnly <- filterM (doesDirectoryExist . (hackageDir </>)) packageDirs
  when (verb <= Debug) (traceShowM hackageDir)
  when (verb <= Debug) (traceShowM dirsOnly)
  TestList <$> mapM (roundTripPackage libdir done) (zip [0..] (map (hackageDir </>) dirsOnly))


roundTripPackage :: LibDir -> S.Set String -> (Int, FilePath) -> IO Test
roundTripPackage libdir done (n, dir) = do
  putStrLn (show n)
  when (verb <= Status) (traceM dir)
  hsFiles <- filter (flip S.notMember done)  <$> findSrcFiles dir

  return (TestLabel (dropFileName dir) (TestList $ map (mkParserTest libdir) hsFiles))

mkParserTest :: LibDir -> FilePath -> Test
mkParserTest libdir fp =
    TestLabel fp $
    TestCase (do writeLog $ "starting:" ++ fp
                 r1 <- catchAny (roundTripTest libdir fp) $ \e -> do
                   writeError fp
                   throwIO e
                 case r1 of
                   Left (ParseFailure s) -> do
                     writeParseFail fp s
                     exitFailure
                   Right r -> do
                     writeProcessed fp
                     unless (status r == Success) (writeFailure fp (debugTxt r) >> writeFailed fp)
                     assertBool fp (status r == Success))

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch

