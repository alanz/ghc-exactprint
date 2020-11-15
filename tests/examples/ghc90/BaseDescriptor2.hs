{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module BaseDescriptors2 where

data ClassTag (tag :: Nat) where
  ExtDescrTag :: (forall (n :: Nat) . (0x6A <= n, n <= 0xFE) =>  ClassTag n)

data ClassTag2 (tag :: Nat) where
  ExtDescrTag2 :: forall (n :: Nat) . (0x6A <= n, n <= 0xFE) =>  ClassTag n
