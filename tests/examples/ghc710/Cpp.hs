{-# Language CPP #-}
#if __GLASGOW_HASKELL__ >= 718
{-# LANGUAGE RoleAnnotations #-}
#endif

module Cpp where

#if __GLASGOW_HASKELL__ > 704
foo :: Int
#else
foo :: Integer
#endif
foo = 3

bar :: (
#if __GLASGOW_HASKELL__ > 704
    Int)
#else
    Integer)
#endif
bar = 4
