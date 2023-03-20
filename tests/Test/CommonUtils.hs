module Test.CommonUtils
  (
    findSrcFiles
  , readFileGhc

  -- * File paths and directories
  , hackageWorkDir
  , workDir
  , configDir
  , failuresDir
  , failuresHtmlDir
  , cppFile
  , parseFailFile
  , processed
  , processedFailFile
  , logFile
  , origFailuresFile
  , badpackagesFile
  , blackListed
  , knownFailuresFile
  , failuresHtmlFile
  ) where

import Control.Monad
import Control.Monad.Extra
import Data.List hiding (find)
import qualified GHC.Data.StringBuffer as GHC
import System.Directory
import System.FilePath

-- ---------------------------------------------------------------------

-- | Round trip working dir holding current hackage contents, can be deleted
hackageWorkDir :: FilePath
hackageWorkDir = "./hackage-roundtrip-work"

-- ---------------------------------------------------------------------

-- | Round trip working dir, can be deleted
workDir :: FilePath
workDir = "./roundtrip-work"

-- | Round trip configuration dir, keept under version control
configDir :: FilePath
configDir = "./roundtrip-config"

-- |Directory where results of failing tests are stored for later analysis
failuresDir :: FilePath
failuresDir = workDir </> "failures"

-- |Directory where results of failing tests are provided in html format
failuresHtmlDir :: FilePath
failuresHtmlDir = workDir </> "html"

-- |Generated:files known to fail due to CPP parse failures, caused by an Exception
cppFile :: FilePath
cppFile = workDir </> "cpp.txt"

-- |Generated:files returning ParseFail status
parseFailFile :: FilePath
parseFailFile = workDir </> "pfail.txt"

-- |Generated:files successfully processed
processed :: FilePath
processed = workDir </> "processed.txt"

-- |Generated:files which failed comparison
processedFailFile :: FilePath
processedFailFile = workDir </> "failed.txt"

-- |log of current file being processed, for knowing what to blacklist
logFile :: FilePath
logFile = workDir </> "roundtrip.log"

-- |list of original failures, when rerunning tests after static processing
origFailuresFile :: FilePath
origFailuresFile = workDir </> "origfailures.txt"

-- |name of index html page
failuresHtmlFile :: FilePath
failuresHtmlFile = "failures.html"

-- -- |location and name of index html page
-- failuresHtmlFile :: FilePath
-- failuresHtmlFile = failuresHtmlDir </> "failures.html"

-- ---------------------------------------------------------------------

-- |Hand edited list of files known to segfault
badpackagesFile :: FilePath
badpackagesFile = configDir </> "badpackages.txt"

-- |Hand edited list of files known to segfault
blackListed :: FilePath
blackListed = configDir </> "blacklist.txt"

-- |Hand edited list of files known to fail, no fix required/possible
knownFailuresFile :: FilePath
knownFailuresFile = configDir </> "knownfailures.txt"

-- ---------------------------------------------------------------------

-- Given base directory finds all haskell source files
findSrcFiles :: FilePath -> IO [FilePath]
findSrcFiles = traverseDir okDirectory accFile []
  where
    okDirectory :: FilePath -> Bool
    okDirectory path
      | "." `isPrefixOf` takeBaseName path = False
      | otherwise = True

    accFile :: [FilePath] -> FilePath -> IO [FilePath]
    accFile acc fileName = do
      return (if (takeExtension fileName == ".hs" && p fileName)
                then fileName:acc
                else acc)
      where
        p x
          | "refactored" `isInfixOf` x = False
          | "Setup.hs" `isInfixOf` x = False
          | "HLint.hs" `isInfixOf` x = False -- HLint config files
          | otherwise                 = True

-- ---------------------------------------------------------------------
-- Based on https://stackoverflow.com/questions/51712083/recursively-search-directories-for-all-files-matching-name-criteria-in-haskell
traverseDir :: (FilePath -> Bool) -> (b -> FilePath -> IO b) -> b -> FilePath -> IO b
traverseDir validDir transition =
    let go state dirPath =
            do names <- listDirectory dirPath
               let paths = map (dirPath </>) names
               (dirPaths, filePaths) <- partitionM doesDirectoryExist paths
               state' <- foldM transition state filePaths -- process current dir
               foldM go state' (filter validDir dirPaths) -- process subdirs
     in go

-- ---------------------------------------------------------------------
readFileGhc :: FilePath -> IO String
readFileGhc file = do
  buf@(GHC.StringBuffer _ len _) <- GHC.hGetStringBuffer file
  return (GHC.lexemeToString buf len)
