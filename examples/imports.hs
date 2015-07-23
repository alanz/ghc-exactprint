module Imports (foo, baz, bar, Foo(..)) where

class Foo a where
    foo :: a -> Int

bar :: Int
bar = 5

baz = 6

foo = 7
