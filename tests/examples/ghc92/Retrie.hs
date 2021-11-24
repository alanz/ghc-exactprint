module Retrie where

main = do
  return ({- comment -} x >= 1 || y >= 2)

  putStrLn $ show $ {- comment here -} foo || bar

{-# RULES "print" forall x. putStrLn $ show $ x = print $ x #-}
