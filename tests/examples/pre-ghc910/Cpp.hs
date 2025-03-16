{-# Language CPP #-}
#if __GLASGOW_HASKELL__ >= 718
{-# LANGUAGE RoleAnnotations #-}
#endif

module Cpp where

-- c1
#if __GLASGOW_HASKELL__ > 704
-- c2
foo :: Int
#else
-- c3
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
