{-# LANGUAGE GADTs #-}
module GADTRecords2 (H1(..)) where

-- | h1
data H1 a b where
  C3 :: (Num a) => { field :: a -- ^ hello docs
                   } -> H1 Int Int
