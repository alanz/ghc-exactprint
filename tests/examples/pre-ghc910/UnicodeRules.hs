{-# LANGUAGE
    BangPatterns
  , FlexibleContexts
  , FlexibleInstances
  , ScopedTypeVariables
  , UnboxedTuples
  , UndecidableInstances
  , UnicodeSyntax
  #-}

strictHead ∷ G.Bitstream (Packet d) ⇒ Bitstream d → Bool
{-# RULES "head → strictHead" [1]
    ∀(v ∷ G.Bitstream (Packet d) ⇒ Bitstream d).
    head v = strictHead v #-}
{-# INLINE strictHead #-}
strictHead (Bitstream _ v) = head (SV.head v)
