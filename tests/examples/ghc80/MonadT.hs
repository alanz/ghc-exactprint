{-# OPTIONS -XRank2Types #-}

module Control.Monatron.MonadT (
  MonadT(..), FMonadT(..), MMonadT(..), FComp(..), FunctorD(..), tmap, mtmap,
  module Control.Monad
) where

import Control.Monad


----------------------------------------------------------
-- Class of monad transformers with
-- a lifting of first-order operations
----------------------------------------------------------

class MonadT t where
    lift    :: Monad m  => m a -> t m a
    treturn :: Monad m => a -> t m a
    treturn =  lift. return
    tbind   :: Monad m => t m a -> (a -> t m b) -> t m b

newtype FunctorD f = FunctorD {fmapD :: forall a b . (a -> b) -> f a -> f b}

functor :: Functor f => FunctorD f
functor = FunctorD fmap

class MonadT t => FMonadT t where
    tmap' :: FunctorD m -> FunctorD n -> (a -> b) -> (forall x. m x -> n x) -> t m a -> t n b

tmap :: (FMonadT t, Functor m, Functor n) => (forall b. m b -> n b) -> t m a -> t n a
tmap = tmap' functor functor id

mtmap :: FMonadT t => FunctorD f -> (a -> b) -> t f a -> t f b
mtmap fd f = tmap' fd fd f id

class FMonadT t => MMonadT t where
    flift      :: Functor f => f a -> t f a --should coincide with lift!
    monoidalT  :: (Functor f, Functor g) => t f (t g a) -> t (FComp f g) a

----------------------------------------
-- Functor Composition
----------------------------------------

newtype (FComp f g) a = Comp {deComp :: (f (g a)) }

instance (Functor f, Functor g) => Functor (FComp f g) where
    fmap f (Comp fga) = Comp (fmap (fmap f) fga)

