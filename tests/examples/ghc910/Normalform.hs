{-# LANGUAGE PatternGuards #-}
module Normalform where

nfCom = case expr of
      x :*: y  -- comment
         | x' <= y'  -> x' :*: y'
      _ -> blah

