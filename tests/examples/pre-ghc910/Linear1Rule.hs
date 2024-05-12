{-# LANGUAGE LinearTypes #-}
module Linear1Rule where

-- Test the 1 <= p rule
f :: a %1 -> b
f = f

-- f1 :: a %001 -> b
-- f1 = f1

g :: a %p -> b
g x = f x

