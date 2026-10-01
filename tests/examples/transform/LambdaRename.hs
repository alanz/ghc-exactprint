{-# LANGUAGE Arrows #-}
module LambdaRename where

import Control.Arrow

f :: IO Int
f = return 1 `longname` \y ->
  do
    return y

g :: Arrow k => k Int Int
g = proc x -> (returnA -< x) `longname` \y ->
  do
    returnA -< y
