-- From ./hackage-roundtrip-work/hopenssl-2.2.5/src/OpenSSL/EVP/Digest/Initialization.hs
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface #-}

{- |
   Maintainer:  simons@cryp.to
   Stability:   provisional
   Portability: portable

   Low-level bindings to OpenSSL's EVP interface. Most users do not need this
   code. Check out "OpenSSL.Digest" for a more comfortable interface.
-}

module OpenSSL.EVP.Digest.Initialization ( initializeEVPDigests ) where

import Control.Concurrent.MVar
import Control.Monad
import System.IO.Unsafe as IO

#include "openssl/opensslv.h"

-- | Initialize the OpenSSL EVP engine and register all known digest types in
-- the internal data structures. This function must be called before any of the
-- message digest functions can succeed. This is generally handled
-- transparently by the Haskell implementation and users do not need to worry
-- about this.

initializeEVPDigests :: IO ()
initializeEVPDigests =
#if OPENSSL_VERSION_NUMBER >= 0x1010000f
  return ()
#else
  modifyMVar_ isDigestEngineInitialized $ \isInitialized ->
    unless isInitialized _addAllDigests >> return True

{-# NOINLINE isDigestEngineInitialized #-}
isDigestEngineInitialized :: MVar Bool
isDigestEngineInitialized = IO.unsafePerformIO $ newMVar False

foreign import ccall unsafe "openssl/evp.h OpenSSL_add_all_digests" _addAllDigests :: IO ()

#endif

