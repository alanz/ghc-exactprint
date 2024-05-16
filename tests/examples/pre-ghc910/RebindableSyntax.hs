{-# LANGUAGE RebindableSyntax, NoMonomorphismRestriction #-}

-- From https://ocharles.org.uk/blog/guest-posts/2014-12-06-rebindable-syntax.html

import Prelude hiding ((>>), (>>=), return)
import Data.Monoid
import Control.Monad ((<=<))
import Data.Map as M

addNumbers = do
  80
  60
  10
  where (>>) = (+)
        return = return

(>>) = mappend
return = mempty

-- We can perform the same computation as above using the Sum wrapper:

someSum :: Sum Int
someSum = do
    Sum 80
    Sum 60
    Sum 10
    return

someProduct :: Product Int
someProduct = do
    Product 10
    Product 30

-- Why not try something non-numeric?

tummyMuscle :: String
tummyMuscle = do
    "a"
    "b"


ff = let
  (>>)    = flip (.)
  return  = id

  arithmetic = do
      (+1)
      (*100)
      (/300)
      return

  -- Here, the input is numeric and all functions operate on a number.
  -- What if we want to take a list and output a string? No problem:

  check = do
      sum
      sqrt
      floor
      show
  in 4
