{-# LANGUAGE Trustworthy,
    TypeOperators,
    PolyKinds, DataKinds,
    TypeFamilies,
    UndecidableInstances #-}


module Type.Compare where

import Data.Ord
import GHC.TypeLits

type family (a :: Ordering) $$ (b :: Ordering) :: Ordering where
  LT $$ b = LT
  GT $$ b = GT
  EQ $$ b = b
infixl 0 $$

-- | Compare two types of any (possibly different) kinds.
-- Since `Compare` itself is a closed type family, add instances to `CompareUser` if you want to compare other types.
type family Compare (a :: k) (b :: k') :: Ordering where

  Compare '() '() = EQ
