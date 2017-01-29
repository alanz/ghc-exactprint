
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
-- {-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.ForFree where {

-- | Free monad from a functor
;
  data Free f x = Pure x | Free (f (Free f x));

  deriving instance (Eq (f (Free f a)), Eq a) => Eq (Free f a);
  deriving instance (Ord (f (Free f a)), Ord a) => Ord (Free f a);
  deriving instance (Read (f (Free f a)), Read a) => Read (Free f a);
  deriving instance (Show (f (Free f a)), Show a) => Show (Free f a);
}
