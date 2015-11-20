-- Let's trick you
{-# LANGUAGE ExplicitForAll #-}
module Test (foo) where

foo :: forall a. a -> a
foo x = x
