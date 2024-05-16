module Data.Monoid.Factorial where

class FactorialMonoid m where
   -- comment A
   factors :: m
   -- comment B
   primePrefix :: m
