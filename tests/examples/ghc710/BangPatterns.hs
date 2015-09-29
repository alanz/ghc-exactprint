{-# LANGUAGE BangPatterns #-}

-- From https://ocharles.org.uk/blog/posts/2014-12-05-bang-patterns.html

import Data.Function (fix)
import Data.List (foldl')

hello3 :: Bool -> String
hello3 !loud = "Hello."

mean :: [Double] -> Double
mean xs = s / fromIntegral l
  where
    (s, l) = foldl' step (0, 0) xs
    step (!s, !l) a = (s + a, l + 1)

