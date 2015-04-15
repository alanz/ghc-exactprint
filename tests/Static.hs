{-# LANGUAGE ViewPatterns #-}
module Main where

-- Static site generator for failing tests
import Data.Algorithm.Diff (getDiff)
import Data.Algorithm.DiffOutput (ppDiff)

import System.Directory
import System.FilePath

import Control.Monad

import Debug.Trace

import Data.List
import System.Environment
import Data.Maybe
import Text.Read

main :: IO ()
main = do
  n <- getArgs
  case readMaybe =<< listToMaybe n of
    Nothing -> site 100
    Just k  -> site k

site :: Int -> IO ()
site n = do
  putStrLn $ "Generating site for first: " ++ show n
  failPaths <- filterM doesFileExist =<< (map ("tests/roundtrip" </>)  . take n <$> getDirectoryContents "tests/roundtrip")
  traceShowM failPaths
  fails <- mapM parseFail failPaths
  writeFile "origfailures.txt" (intercalate "\n" (map getfname fails))
  writeFile "failures/failures.html" (makeIndex failPaths)
  let padded = "failures.html" : (map makeFailLink failPaths ++ ["failures.html"])
  let resolved = zipWith (\x (y,z) -> (x, y, z)) padded (zip (tail padded) (tail (tail padded)))
  mapM_ (uncurry page) (zip resolved fails)

makeFailLink :: FilePath -> String
makeFailLink fp = takeFileName fp  <.> "html"

makeIndex :: [FilePath] -> String
makeIndex files =
  intercalate "</br>" (map mkIndexLink files)
  where
    mkIndexLink f = mkLink (takeFileName f <.> "html") f



page :: (FilePath, FilePath, FilePath) -> Failure -> IO ()
page (prev, out, next) (Failure res fname) = do
--  traceM out
  original <- readFile fname
  let diff = getDiff (tokenize original) (tokenize res)
  let l = length (lines res)
  if (l > 10000)
    then putStrLn ("Skipping: " ++ fname) >> print l
    else
      writeFile ("failures" </> out) (mkPage (ppDiff diff) prev next original res)
  where
    tokenize :: String -> [[String]]
    tokenize s = map (:[]) . lines $ s

mkPage :: String -> String -> String -> String -> String -> String
mkPage diff prev next original printed  =
  intercalate "</br>"
  [mkLink prev "prev"
  , mkLink "failures.html" "home"
  , mkLink next "next"
  , ""
  , "<pre>" ++ diff ++ "</pre>"
  , "<h2>original</h2>"
  , "<pre>" ++ original ++ "</pre>"
  , "<h2>printed</h2>"
  , "<pre>" ++ printed ++ "</pre>"
  ]

mkLink :: String -> String -> String
mkLink s label =
  "<a href=\"" ++ s ++ "\">" ++ label ++ "</a>"

data Failure = Failure String FilePath

getfname (Failure _ fp) = fp

parseFail :: FilePath -> IO Failure
parseFail fp = do
  res <- lines <$> readFile fp
  let (finalres, head . tail -> fname) = break (=="==============") res
  return (Failure (unlines finalres) fname)

