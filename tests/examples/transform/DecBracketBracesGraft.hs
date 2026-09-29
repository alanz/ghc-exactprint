{-# LANGUAGE TemplateHaskell #-}
module DecBracketBracesGraft where

ds = graft [d| { f = x
  where
    x = 1
  ; g = 2 } |]
