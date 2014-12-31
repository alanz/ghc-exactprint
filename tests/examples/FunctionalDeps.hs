{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

-- from https://ocharles.org.uk/blog/posts/2014-12-14-functional-dependencies.html

import Data.Foldable (forM_)
import Data.IORef

class Store store m | store -> m where
 new :: a -> m (store a)
 get :: store a -> m a
 put :: store a -> a -> m ()

instance Store IORef IO where
  new = newIORef
  get = readIORef
  put ioref a = modifyIORef ioref (const a)

