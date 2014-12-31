{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

-- from https://ocharles.org.uk/blog/guest-posts/2014-12-18-rank-n-types.html

import System.Random
import Control.Monad.State
import Data.Char


id' :: forall a. a -> a
id' x = x

f = print (id' (3 :: Integer),
           id' "blah")

-- rank 2 polymorphism
type IdFunc = forall a. a -> a

id'' :: IdFunc
id'' x = x

someInt :: IdFunc -> Integer
someInt id' = id' 3

-- rank 3 polymorphism
type SomeInt = IdFunc -> Integer

someOtherInt :: SomeInt -> Integer
someOtherInt someInt' =
    someInt' id + someInt' id

-- random numbers

data Player =
    Player {
      playerName :: String,
      playerPos  :: (Double, Double)
    }
    deriving (Eq, Ord, Show)


{-
randomPlayer
    :: (MonadIO m, MonadState g m, RandomGen g)
    => m Player
-}

type GenAction m = forall a. (Random a) => m a

type GenActionR m = forall a. (Random a) => (a, a) -> m a

-- genRandom :: (RandomGen g) => GenAction (State g)
-- genRandom = state random

genRandomR :: (RandomGen g) => GenActionR (State g)
genRandomR range = state (randomR range)

genRandom :: (Random a, RandomGen g) => State g a
genRandom = state random


randomPlayer :: (MonadIO m) => GenActionR m -> m Player
randomPlayer genR = do
    liftIO (putStrLn "Generating random player...")

    len <- genR (8, 12)
    name <- replicateM len (genR ('a', 'z'))
    x <- genR (-100, 100)
    y <- genR (-100, 100)

    liftIO (putStrLn "Done.")
    return (Player name (x, y))

main :: IO ()
main = randomPlayer randomRIO >>= print

-- scott encoding

data List a
    = Cons a (List a)
    | Nil

uncons :: (a -> List a -> r) -> r -> List a -> r
uncons co ni (Cons x xs) = co x xs
uncons co ni Nil         = ni

listNull :: List a -> Bool
listNull = uncons (\_ _ -> False) True

listMap :: (a -> b) -> List a -> List b
listMap f =
    uncons (\x xs -> Cons (f x) (listMap f xs))
           Nil

newtype ListS a =
    ListS {
      unconsS :: forall r. (a -> ListS a -> r) -> r -> r
    }

nilS :: ListS a
nilS = ListS (\co ni -> ni)

consS :: a -> ListS a -> ListS a
consS x xs = ListS (\co ni -> co x xs)

unconsS' :: (a -> ListS a -> r) -> r -> ListS a -> r
unconsS' co ni (ListS f) = f co ni

instance Functor ListS where
    fmap f =
        unconsS' (\x xs -> consS (f x) (fmap f xs))
                 nilS

-- Church Encoding
newtype ListC a =
    ListC {
      foldC :: forall r. (a -> r -> r) -> r -> r
    }

foldC' :: (a -> r -> r) -> r -> ListC a -> r
foldC' co ni (ListC f) = f co ni

instance Functor ListC where
    fmap f = foldC' (\x xs -> consC (f x) xs) nilC

consC = undefined
nilC = undefined

-- GADTs and continuation passing style
data Some :: * -> * where
    SomeInt  :: Int -> Some Int
    SomeChar :: Char -> Some Char
    Anything :: a -> Some a


unSome :: Some a -> a
unSome (SomeInt x) = x + 3
unSome (SomeChar c) = toLower c
unSome (Anything x) = x


newtype SomeC a =
    SomeC {
      runSomeC ::
          forall r.
          ((a ~ Int) => Int -> r) ->
          ((a ~ Char) => Char -> r) ->
          (a -> r) ->
          r
      }

-- dependent types

idk :: forall (a :: *). a -> a
idk x = x

