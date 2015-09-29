{-# LANGUAGE PartialTypeSignatures #-}

f :: (Eq a, _, _) => a -> a -> Bool
f x y = x == y
