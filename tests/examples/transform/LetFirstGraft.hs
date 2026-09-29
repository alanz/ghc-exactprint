{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecursiveDo #-}
module LetFirstGraft where

import Control.Arrow

f :: Int -> Int -> IO Int
f a b = do
  let z = a in return z
  return (graft)

g :: Int -> Int -> IO Int
g a b = do
  rec let z = a in return z
      x <- return (graft)
  return x

h :: Arrow k => k (Int, Int) Int
h = proc (a, b) -> do
  let c = a in returnA -< c
  returnA -< graft
