{-# LANGUAGE TemplateHaskell #-}
module DecBracketGraft where

ds = [d|
  f a b = graft
  g = 1
  |]
