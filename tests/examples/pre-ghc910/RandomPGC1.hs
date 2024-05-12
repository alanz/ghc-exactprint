{-# LANGUAGE CPP #-}

module RandomPGC where

#ifdef __GLASGOW_HASKELL__
#endif

-- A comment
#ifdef __NHC__
#else
getTime = 2
#endif

