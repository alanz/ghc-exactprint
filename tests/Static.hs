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

site = do
  failPaths <- filterM doesFileExist =<< (map ("tests/roundtrip" </>) <$> getDirectoryContents "tests/roundtrip")
  fails <- mapM parseFail failPaths
  writeFile "failures/failures.html" (makeIndex failPaths)
  let padded = "failures.html" : (map makeFailLink failPaths ++ ["failures.html"])
  let resolved = zipWith (\x (y,z) -> (x, y, z)) padded (zip (tail padded) (tail (tail padded)))
  mapM_ (uncurry page) (zip resolved fails)

makeFailLink :: FilePath -> String
makeFailLink fp = (takeFileName fp  <.> "html"  )

makeIndex :: [FilePath] -> String
makeIndex files =
  intercalate "</br>" (map mkIndexLink files)
  where
    mkIndexLink f = mkLink (takeFileName f <.> "html") f



page :: (FilePath, FilePath, FilePath) -> Failure -> IO ()
page (prev, out, next) (Failure res fname) = do
  original <- readFile fname
  let diff = getDiff (tokenize original) (tokenize res)
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

parseFail :: FilePath -> IO Failure
parseFail fp = do
  res <- lines <$> readFile fp
  let (finalres, head . tail -> fname) = break (=="==============") res
  return (Failure (unlines finalres) fname)

