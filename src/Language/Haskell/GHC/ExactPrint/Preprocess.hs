module Language.Haskell.GHC.ExactPrint.Preprocess
   (stripLinePragmas)

   where

import Data.List
import Language.Haskell.GHC.ExactPrint.Types
import Data.Maybe

stripLinePragmas :: String -> (String, [Comment])
stripLinePragmas = unlines' . unzip . findLines . lines
  where
    unlines' (a, b) = (unlines a, catMaybes b)

findLines :: [String] -> [(String, Maybe Comment)]
findLines = zipWith checkLine [1..]

checkLine :: Int -> String -> (String, Maybe Comment)
checkLine line s
  |  "{-# LINE" `isPrefixOf` s =
       let (pragma, res) = getPragma s
           size   = length pragma
       in (res, Just $ Comment ((line, 1), (line, size+1)) pragma)
  -- Deal with CPP directives too
  |  "#" `isPrefixOf` s = ("",Just $ Comment ((line, 1), (line, length s)) s)
  | otherwise = (s, Nothing)

getPragma :: String -> (String, String)
getPragma [] = error "Input must not be empty"
getPragma s@(x:xs)
  | "#-}" `isPrefixOf` s = ("#-}", "   " ++ drop 3 s)
  | otherwise =
      let (prag, remline) = getPragma xs
      in (x:prag, ' ':remline)

