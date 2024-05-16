{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- From https://ocharles.org.uk/blog/posts/2014-12-13-multi-param-type-classes.html

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Reader (ReaderT)
import Data.Foldable (forM_)
import Data.IORef

class Store store m where
 new :: a -> m (store a)
 get :: store a -> m a
 put :: store a -> a -> m ()

type Present = String
storePresents :: (Store store m, Monad m) => [Present] -> m (store [Present])
storePresents xs = do
  store <- new []
  forM_ xs $ \x -> do
    old <- get store
    put store (x : old)
  return store

instance Store IORef IO where
  new = newIORef
  get = readIORef
  put ioref a = modifyIORef ioref (const a)

-- ex ps = do
--   store <- storePresents ps
--   get (store :: IORef [Present])
