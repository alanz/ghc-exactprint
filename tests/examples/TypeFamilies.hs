{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- From https://ocharles.org.uk/blog/posts/2014-12-12-type-families.html

import Control.Concurrent.STM
import Control.Concurrent.MVar
import Data.Foldable (forM_)
import Data.IORef

class IOStore store where
  newIO :: a -> IO (store a)
  getIO :: store a -> IO a
  putIO :: store a -> a -> IO ()

instance IOStore MVar where
  newIO = newMVar
  getIO = readMVar
  putIO mvar a = modifyMVar_ mvar (return . const a)

instance IOStore IORef where
  newIO = newIORef
  getIO = readIORef
  putIO ioref a = modifyIORef ioref (const a)

type Present = String
storePresentsIO :: IOStore store => [Present] -> IO (store [Present])
storePresentsIO xs = do
  store <- newIO []
  forM_ xs $ \x -> do
    old <- getIO store
    putIO store (x : old)
  return store

-- Type family version

class Store store where
  type StoreMonad store :: * -> *
  new :: a -> (StoreMonad store) (store a)
  get :: store a -> (StoreMonad store) a
  put :: store a -> a -> (StoreMonad store) ()

instance Store IORef where
  type StoreMonad IORef = IO
  new = newIORef
  get = readIORef
  put ioref a = modifyIORef ioref (const a)

instance Store TVar where
  type StoreMonad TVar = STM
  new = newTVar
  get = readTVar
  put ioref a = modifyTVar ioref (const a)

storePresents :: (Store store, Monad (StoreMonad store))
              => [Present] -> (StoreMonad store) (store [Present])
storePresents xs = do
  store <- new []
  forM_ xs $ \x -> do
    old <- get store
    put store (x : old)
  return store

type family (++) (a :: [k]) (b :: [k]) :: [k] where
    '[]       ++ b = b
    (a ': as) ++ b = a ': (as ++ b)

type family (f :: * -> *) |> (s :: * -> *) :: * -> *

type instance f |> Union s = Union (f :> s)

