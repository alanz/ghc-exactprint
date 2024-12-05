module Operator1 where

main :: IO ()
main = do
  putStrLn $ show {- c3 -} $ {- c4 -} foo {- c1 -} || {- c2 -} bar

{-# RULES "print" forall x. putStrLn $ show $ x = print $ x #-}
