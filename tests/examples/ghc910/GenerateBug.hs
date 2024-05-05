{-# LANGUAGE TemplateHaskell #-}
module Generate where

onReturnCode = [||
    $$(
       {- c1 -} unGen
      )
  ||]


foo =
      (
       {- c2 -} blah
      )
