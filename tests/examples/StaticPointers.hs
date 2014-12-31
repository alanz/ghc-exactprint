{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import GHC.StaticPtr
import GHC.Word
import GHC.Generics
import Data.Data
import Data.Binary
import Data.ByteString

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (n - 1)

main = do
  let sptr :: StaticPtr (Int -> Int)
      sptr = static fact
  print $ staticPtrInfo sptr
  print $ deRefStaticPtr sptr 10

-- ---------------------------------------------------------------------

type StaticKey1 = Fingerprint

-- Defined in GHC.Fingerprint.
data Fingerprint = Fingerprint {-# UNPACK #-} !Word64 {-# UNPACK #-} !Word64
  deriving (Generic, Typeable)

staticKey :: StaticPtr a -> StaticKey1
staticKey = undefined


