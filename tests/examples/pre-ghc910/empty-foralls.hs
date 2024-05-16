{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
-- Empty foralls are handled correctly in different situations.

data D = forall. D Int

data G where
  G :: forall. Int -> G
