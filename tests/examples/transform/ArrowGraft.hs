{-# LANGUAGE Arrows #-}
{-# LANGUAGE LambdaCase #-}
module ArrowGraft where

import Control.Arrow

f :: Arrow k => k (Int, Int) Int
f = proc (a, b) -> do
  returnA -< graft

g :: ArrowChoice k => k (Bool, Int, Int) Int
g = proc (x, a, b) -> case x of
  True -> returnA -< graft

h :: ArrowChoice k => k (Bool, Int, Int) Int
h = proc (x, a, b) -> (\case
  True -> returnA -< graft) x
