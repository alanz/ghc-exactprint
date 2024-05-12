module ClassParens where

class LiftingMonad  (trans :: MTrans) where
  proof :: Monad m :- Monad (trans m)

class LiftingMonad2  ((trans :: MTrans)) where
  proof :: Monad m :- Monad (trans m)

data Nat (t :: NatKind) where
    ZeroNat :: Nat Zero
    SuccNat :: Nat t -> Nat (Succ t)
