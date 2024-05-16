{-# LANGUAGE UnboxedTuples #-}
module Layout.Unboxed where

f1 :: (Num a1, Num a) => a -> a1 -> (# , #) a a1
f1 x y = (# , #) (x+1) (y-1)

f2 x y z  = (# ,, #) x y z
