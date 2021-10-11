{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
module ParensGADT where

-- | Base Descriptor Class Tags TODO rename to xxxTag
data ClassTag (tag :: Nat) where
  M4MuxChannelDescr                ::ClassTag 0x69
  ExtDescrTag :: (forall (n :: Nat) . (0x6A <= n, n <= 0xFE) =>  ClassTag n)
  OCIDescrTag :: (forall (n :: Nat) . (0x40 <= n, n <= 0x5F) =>  ClassTag n)

-- End of file comment
