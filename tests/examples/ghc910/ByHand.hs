module ByHand where

instance SDecide Nat where
  SZero %~ (SSucc _) = Disproved (\case)

