{-# LANGUAGE
    BangPatterns
  , CPP
  , FlexibleContexts
  , FlexibleInstances
  , ScopedTypeVariables
  , UndecidableInstances
  , UnicodeSyntax
  #-}
module Data.Bitstream.Lazy where

unstreamChunks (Stream step s0 _) = go s0

{-# RULES
"Lazy Bitstream streamChunks/unstreamChunks fusion"
    ∀s. streamChunks (unId (unstreamChunks s)) = s
#if 1
"Lazy Bitstream unstreamChunks/streamChunks fusion"
    ∀v. unId (unstreamChunks (streamChunks v)) = v
#endif
  #-}

