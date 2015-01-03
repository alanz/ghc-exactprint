{-# Language CPP #-}

#if __GLASGOW_HASKELL__ > 704
foo :: Int
#else
foo :: Integer
#endif
foo = 3

