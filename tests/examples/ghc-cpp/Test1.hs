{-# LANGUAGE GHC_CPP #-}

module Test1 where

foo =
#if MIN_VERSION_llvm_hs_pure ( 5 , 0 , 0 )
    'a'
#else
    'b'
#endif

