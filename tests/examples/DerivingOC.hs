{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- from https://ocharles.org.uk/blog/guest-posts/2014-12-15-deriving.html

import Data.Data
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.State

data MiniIoF a = Terminate
               | PrintLine String a
               | ReadLine (String -> a)
               deriving (Functor)


-- data List a = Nil | Cons a (List a)
--               deriving (Eq, Show, Functor, Foldable, Traversable)



data List a = Nil | Cons a (List a)
              deriving ( Eq, Show
                       , Functor, Foldable, Traversable
                       , Typeable, Data)

data Config = C String String
data AppState = S Int Bool

newtype App a = App { unApp :: ReaderT Config (StateT AppState IO) a }
                deriving (Monad, MonadReader Config,
                          MonadState AppState, MonadIO,
                          Applicative,Functor)
