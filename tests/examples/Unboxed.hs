{-# LANGUAGE UnboxedTuples #-}
module Layout.Unboxed where

a,b :: Int
a = 1
b = 2

c :: Maybe (a -> b)
c = Nothing

f :: (Num a1, Num a) => a -> a1 -> (# a, a1 #)
f x y = (# x+1, y-1 #)

f1 :: (Num a1, Num a) => a -> a1 -> (# , #) a a1
f1 x y = (# , #) x+1 y-1

g :: Num a => a -> a
g x = case f x x of { (# a, b #) -> a + b }


 -- Pattern bind
tup :: (Int, Int)
h :: Int
t :: Int
tup@(h,t) = head $ zip [1..10] [3..ff]
  where
    ff :: Int
    ff = 15


