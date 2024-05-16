{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}


-- | The style and color attributes can either be the terminal defaults. Or be equivalent to the
-- previously applied style. Or be a specific value.
data MaybeDefault v where
    Default :: MaybeDefault v
    KeepCurrent :: MaybeDefault v
    SetTo :: forall v . ( Eq v, Show v ) => !v -> MaybeDefault v
    SetTo2 :: (Eq a) => forall v . ( Eq v, Show v ) => !v -> a -> MaybeDefault v

bar :: forall v . (( Eq v, Show v ) => v -> MaybeDefault v -> a -> [a])
baz :: (Eq a) => forall v . ( Eq v, Show v ) => !v -> a -> MaybeDefault v

instance Dsp (S n) where
  data   (ASig (S n))   = S_A CVar
  data  ((KSig (S n)))  = S_K CVar
  data (((INum (S n)))) = S_I CVar
  getSr    = fst <$> ask
  getKsmps = snd <$> ask
