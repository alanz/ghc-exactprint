-- From ./hackage-roundtrip-work/clr-host-0.2.1.0/src/Clr/Host.hs
{-# LANGUAGE CPP, RankNTypes, ScopedTypeVariables #-}

module Clr.Host (startClr, stopClr, withClr) where

import Control.Exception
import Clr.Marshal

import Clr.Host.Config
#ifdef HAVE_MONO
import Clr.Host.Mono
#endif
#ifdef HAVE_DOTNET
import Clr.Host.DotNet
#endif

import Clr.Host.BStr
import Clr.Host.DriverEntryPoints

import Data.Coerce
import Data.Word
import Foreign.Ptr


{-# WARNING startClr, stopClr [
        "This function was deprecated in version 0.2.0 in favor of withClr "
      , "which was never part of the public API."
      , ""
      , "withClr is there to provide a convenient syntax that ensures both "
      , "startClr and stopClr are called."
      , ""
      , "stopClr is no-op and so startClr must be called once per process. "
      , "See also: https://gitlab.com/tim-m89/clr-haskell/issues/33"
      ]
#-}
startClr :: IO ()
startClr = do
  ClrHostConfig hostType <- getClrHostConfig
  getPtrToMethod <- case hostType of
#ifdef HAVE_MONO
    ClrHostMono   -> startHostMono
#else
    ClrHostMono   -> error "not built with Mono support enabled"
#endif
#ifdef HAVE_DOTNET
    ClrHostDotNet -> startHostDotNet
#else
    ClrHostDotNet -> error "not built with .Net support enabled"
#endif
  if getPtrToMethod == nullFunPtr then
    error "Failed to boot the driver"
  else
    getPtrToMethod_set getPtrToMethod

stopClr :: IO ()
stopClr = putStrLn "stopClr"

withClr :: IO a -> IO a
withClr = bracket_ startClr stopClr

