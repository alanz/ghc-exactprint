{-# LANGUAGE GHC_CPP #-}
-- {-# OPTIONS -ddump-ghc-cpp -dkeep-comments #-}
module Example15 where

#define MIN_VERSION_Cabal(a,b,c) 1

#ifdef MIN_VERSION_Cabal
#undef CH_MIN_VERSION_Cabal
#define CH_MIN_VERSION_Cabal MIN_VERSION_Cabal
#endif

