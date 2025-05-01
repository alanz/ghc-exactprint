{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
module Paths (
  findFirstInPath, findInPath,
  almsLibPath, findAlmsLib, findAlmsLibRel,
  shortenPath,
  version, versionString
) where

import Util

import Prelude ()
import Language.Haskell.TH
import System.FilePath
import System.Directory (doesFileExist, getCurrentDirectory)
import System.Environment (getEnv)
import System.IO.Error (catchIOError)
import Data.Version

#ifdef ALMS_CABAL_BUILD
import Paths_alms
#endif

builddir  :: FilePath
builddir   = $(runIO getCurrentDirectory >>= litE . stringL)

getBuildDir :: IO FilePath
getBuildDir  = catchIOError (getEnv "alms_builddir") (\_ -> return builddir)

#ifndef ALMS_CABAL_BUILD
version :: Version
version = Version {versionBranch = [0,0,0], versionTags = ["dev"]}

bindir, datadir :: FilePath

bindir     = builddir
datadir    = dropFileName builddir

getBinDir, getDataDir :: IO FilePath
getBinDir  = catchIOError (getEnv "alms_bindir") (\_ -> return bindir)
getDataDir = catchIOError (getEnv "alms_datadir") (\_ -> return datadir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir </> name)
#endif

findFirstInPath :: [FilePath] -> [FilePath] -> IO (Maybe FilePath)
findFirstInPath []     _  = return Nothing
findFirstInPath (f:fs) ds = do
  mpath <- findInPath f ds
  case mpath of
    Nothing -> findFirstInPath fs ds
    Just _  -> return mpath

findInPath :: FilePath -> [FilePath] -> IO (Maybe FilePath)
findInPath _    []     = return Nothing
findInPath name (d:ds) = do
  b <- doesFileExist (d </> name)
  if b
    then return (Just (normalise (d </> name)))
    else findInPath name ds

almsLibPath :: IO [FilePath]
almsLibPath = do
  user   <- liftM splitSearchPath (getEnv "ALMS_LIB_PATH")
             `catchIOError` \_ -> return []
  system <- liftM (</> "lib") getDataDir
  build  <- liftM (</> "lib") getBuildDir
  return $ user ++ [ system, build ]

-- | Find an Alms library with the given name
findAlmsLib :: FilePath -> IO (Maybe FilePath)
findAlmsLib name = do
  path <- almsLibPath
  findFirstInPath (nameAdjustments name) path

-- | Find an Alms library with the given name, first looking
--   relative to the given path
findAlmsLibRel :: FilePath -> FilePath -> IO (Maybe FilePath)
findAlmsLibRel name rel = do
  path <- almsLibPath
  let rel' = case rel of
               "."  -> "."
               "-"  -> "."
               _    -> dropFileName rel
  findFirstInPath (nameAdjustments name) (rel' : path)

-- | Produce a sequence of names to try to load based on a base name
-- nameAdjustments ∷ FilePath -> [FilePath]
nameAdjustments :: FilePath -> [FilePath]
nameAdjustments name =
  [ name , name <.> "alms" ]
  ++ if pathSeparator `elem` name
       then []
       else [ "lib" ++ name <.> "alms" ]

shortenPath :: FilePath -> IO FilePath
shortenPath fp = do
  cwd <- getCurrentDirectory
  let fp' = makeRelativeTo cwd fp
  return $ if length fp' < length fp then fp' else fp

makeRelativeTo :: FilePath -> FilePath -> FilePath
makeRelativeTo fp1 fp2 = loop (splitDirectories fp1) (splitDirectories fp2)
  where
    loop []     []     = "."
    loop []     ts     = joinPath ts
    loop fs     []     = joinPath [ ".." | _ <- fs ]
    loop (f:fs) (t:ts)
      | f == t         = loop fs ts
      | otherwise      = loop (f:fs) [] </> loop [] (t:ts)

versionString :: String
versionString  = "Alms, version " ++ showVersion version

