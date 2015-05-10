{-# LANGUAGE GADTs              #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}

data StackItem a where
  Snum :: forall a. Fractional a => a -> StackItem a
  Sop  :: OpDesc -> StackItem a
deriving instance Show a => Show (StackItem a)
