{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UnicodeSyntax #-}

module FromManual where

data T1 a = MkT1 a

construct :: a %1 -> MkT1 a
construct x = MkT1 x

deconstruct :: MkT1 a %1 -> a
deconstruct (MkT1 x) = x  -- must consume `x` exactly once

data T2 a b c where
    MkT2 :: a -> b %1 -> c %1 -> T2 a b  -- Note unrestricted arrow in the first argument

data T3 a b c where
    MkT3 :: a -> b ⊸ c ⊸ T2 a b  -- Note unrestricted arrow in the first argument

g :: A %1 -> (A, B)
h :: A %1 -> B %1 -> C

f :: A %1 -> C
f x = f' (g x)
  where
    f' :: (A, B) %1 -> C
    f' (y, z) = h y z
