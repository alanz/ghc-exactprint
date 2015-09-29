{-# LANGUAGE GADTs              #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}

data StackItem a where
  Snum :: forall a. Fractional a => a -> StackItem a
  Sop  :: OpDesc -> StackItem a
deriving instance Show a => Show (StackItem a)

-- AZ added to test Trac #10399
data MaybeDefault v where
    SetTo :: forall v . ( Eq v, Show v ) => !v -> MaybeDefault v
    SetTo4 :: forall v a. (( Eq v, Show v ) => v -> MaybeDefault v -> a -> MaybeDefault [a])
