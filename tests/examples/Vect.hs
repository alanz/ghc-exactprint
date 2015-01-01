{-# LANGUAGE ParallelArrays #-}
{-# OPTIONS_GHC -fvectorise #-}
{-# LANGUAGE UnboxedTuples #-}

module Vect where

-- import Data.Array.Parallel


{-# VECTORISe isFive = blah #-}
{-# NoVECTORISE isEq #-}

{-# VECTORISE SCALAR type Int  #-}
{-# VECTORISE        type Char #-}
{-# VECTORISE        type ( ) #-}
{-# VECTORISE        type (# #) #-}

{-# VECTORISE SCALAR type Integer = Int #-}
{-# VECTORISE        type Bool    = String  #-}

{-# Vectorise class Eq #-}

blah = 5

data MyBool = MyTrue | MyFalse

class Eq a => Cmp a where
  cmp :: a -> a -> Bool

-- FIXME:
-- instance Cmp Int where
--   cmp = (==)
-- isFive :: (Eq a, Num a) => a -> Bool
isFive :: Int -> Bool
isFive x = x == 5

isEq :: Eq a => a -> Bool
isEq x = x == x


fiveEq :: Int -> Bool
fiveEq x = isFive x && isEq x

cmpArrs :: PArray Int -> PArray Int -> Bool
{-# NOINLINE cmpArrs #-}
cmpArrs v w = cmpArrs' (fromPArrayP v) (fromPArrayP w)

cmpArrs' :: [:Int:] -> [:Int:] -> Bool
cmpArrs' xs ys = andP [:x == y | x <- xs | y <- ys:]

isFives :: PArray Int -> Bool
{-# NOINLINE isFives #-}
isFives xs = isFives' (fromPArrayP xs)

isFives' :: [:Int:] -> Bool
isFives' xs = andP (mapP isFive xs)

isEqs :: PArray Int -> Bool
{-# NOINLINE isEqs #-}
isEqs xs = isEqs' (fromPArrayP xs)

isEqs' :: [:Int:] -> Bool
isEqs' xs = undefined -- andP (mapP isEq xs)

-- fudge for compiler

fromPArrayP = undefined
andP = undefined
mapP = undefined
data PArray a = PArray a
