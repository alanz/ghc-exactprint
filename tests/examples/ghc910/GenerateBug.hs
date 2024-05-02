{-# LANGUAGE TemplateHaskell #-}
module Generate where

onReturnCode = [||
  let _ = foo in
  \farInp ->
    $$({-trace "unGen.onReturnCode" $-} unGen)
  ||]

