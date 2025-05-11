-- originally ./hackage-roundtrip-work/Scurry-0.0.3/src/Network/Util.hs
{-# OPTIONS -XForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}

module Network.Util (
    htonl,
    htons,
    ntohl,
    ntohs,
) where

import Data.Word

foreign import CALLCONV unsafe "htonl" htonl :: Word32 -> Word32
foreign import CALLCONV unsafe "htons" htons :: Word16 -> Word16
foreign import CALLCONV unsafe "ntohl" ntohl :: Word32 -> Word32
foreign import CALLCONV unsafe "ntohs" ntohs :: Word16 -> Word16
