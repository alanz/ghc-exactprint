module DependentStmt where

-- This test rewrites foo to baz, but only in scope of 'y'.

main :: IO ()
main = do
  x <- bar 7
  foo x
  y <- bar 54
  baz y
