{-# LANGUAGE PartialTypeSignatures #-}
module ParensAroundContext where

f :: ((Eq a, _)) => a -> a -> Bool
f x y = x == y
