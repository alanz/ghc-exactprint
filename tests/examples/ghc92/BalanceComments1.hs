module BalanceComments1 where

-- Captured in https://gitlab.haskell.org/ghc/ghc/-/issues/20297
-- The '-- do stuff' comment is attached to the wrong annotation

-- Chris done comment attachment problem
foo = x
  where -- do stuff
        doStuff = do stuff
x = 1
stuff = 4
