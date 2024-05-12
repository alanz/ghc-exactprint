{-
    Copyright 2013-2015 Mario Blazevic

    License: BSD3 (see BSD3-LICENSE.txt file)
-}

-- | This module defines the 'FactorialMonoid' class and some of its instances.
--

{-# LANGUAGE Haskell2010, Trustworthy #-}

module Data.Monoid.Factorial (
   -- * Classes
   FactorialMonoid(..), StableFactorialMonoid,
   -- * Monad function equivalents
   mapM, mapM_
   )
where

import Prelude hiding (break, drop, dropWhile, foldl, foldMap, foldr, last, length, map, mapM, mapM_, max, min,
                       null, reverse, span, splitAt, take, takeWhile)

import Control.Arrow (first)
import qualified Control.Monad as Monad
import Data.Monoid (Monoid (..), Dual(..), Sum(..), Product(..), Endo(Endo, appEndo))
import qualified Data.Foldable as Foldable
import qualified Data.List as List
import qualified Data.ByteString as ByteString
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LazyText
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import qualified Data.Sequence as Sequence
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import Data.Int (Int64)
import Data.Numbers.Primes (primeFactors)

import Data.Monoid.Null (MonoidNull(null), PositiveMonoid)

-- | Class of monoids that can be split into irreducible (/i.e./, atomic or prime) 'factors' in a unique way. Factors of
-- a 'Product' are literally its prime factors:
--
-- prop> factors (Product 12) == [Product 2, Product 2, Product 3]
--
-- Factors of a list are /not/ its elements but all its single-item sublists:
--
-- prop> factors "abc" == ["a", "b", "c"]
--
-- The methods of this class satisfy the following laws:
--
-- > mconcat . factors == id
-- > null == List.null . factors
-- > List.all (\prime-> factors prime == [prime]) . factors
-- > factors == unfoldr splitPrimePrefix == List.reverse . unfoldr (fmap swap . splitPrimeSuffix)
-- > reverse == mconcat . List.reverse . factors
-- > primePrefix == maybe mempty fst . splitPrimePrefix
-- > primeSuffix == maybe mempty snd . splitPrimeSuffix
-- > inits == List.map mconcat . List.tails . factors
-- > tails == List.map mconcat . List.tails . factors
-- > foldl f a == List.foldl f a . factors
-- > foldl' f a == List.foldl' f a . factors
-- > foldr f a == List.foldr f a . factors
-- > span p m == (mconcat l, mconcat r) where (l, r) = List.span p (factors m)
-- > List.all (List.all (not . pred) . factors) . split pred
-- > mconcat . intersperse prime . split (== prime) == id
-- > splitAt i m == (mconcat l, mconcat r) where (l, r) = List.splitAt i (factors m)
-- > spanMaybe () (const $ bool Nothing (Maybe ()) . p) m == (takeWhile p m, dropWhile p m, ())
-- > spanMaybe s0 (\s m-> Just $ f s m) m0 == (m0, mempty, foldl f s0 m0)
-- > let (prefix, suffix, s') = spanMaybe s f m
-- >     foldMaybe = foldl g (Just s)
-- >     g s m = s >>= flip f m
-- > in all ((Nothing ==) . foldMaybe) (inits prefix)
-- >    && prefix == last (filter (isJust . foldMaybe) $ inits m)
-- >    && Just s' == foldMaybe prefix
-- >    && m == prefix <> suffix
--
-- A minimal instance definition must implement 'factors' or 'splitPrimePrefix'. Other methods are provided and should
-- be implemented only for performance reasons.
class MonoidNull m => FactorialMonoid m where
   -- | Returns a list of all prime factors; inverse of mconcat.
   factors :: m -> [m]
   -- | The prime prefix, 'mempty' if none.
   primePrefix :: m -> m
   -- | The prime suffix, 'mempty' if none.
   primeSuffix :: m -> m
   -- | Splits the argument into its prime prefix and the remaining suffix. Returns 'Nothing' for 'mempty'.
   splitPrimePrefix :: m -> Maybe (m, m)
   -- | Splits the argument into its prime suffix and the remaining prefix. Returns 'Nothing' for 'mempty'.
   splitPrimeSuffix :: m -> Maybe (m, m)
   -- | Returns the list of all prefixes of the argument, 'mempty' first.
   inits :: m -> [m]
   -- | Returns the list of all suffixes of the argument, 'mempty' last.
   tails :: m -> [m]
   -- | Like 'List.foldl' from "Data.List" on the list of 'primes'.
   foldl :: (a -> m -> a) -> a -> m -> a
   -- | Like 'List.foldl'' from "Data.List" on the list of 'primes'.
   foldl' :: (a -> m -> a) -> a -> m -> a
   -- | Like 'List.foldr' from "Data.List" on the list of 'primes'.
   foldr :: (m -> a -> a) -> a -> m -> a
   -- | The 'length' of the list of 'primes'.
   length :: m -> Int
   -- | Generalizes 'foldMap' from "Data.Foldable", except the function arguments are prime factors rather than the
   -- structure elements.
   foldMap :: Monoid n => (m -> n) -> m -> n
   -- | Like 'List.span' from "Data.List" on the list of 'primes'.
   span :: (m -> Bool) -> m -> (m, m)
   -- | Equivalent to 'List.break' from "Data.List".
   break :: (m -> Bool) -> m -> (m, m)
   -- | Splits the monoid into components delimited by prime separators satisfying the given predicate. The primes
   -- satisfying the predicate are not a part of the result.
   split :: (m -> Bool) -> m -> [m]
   -- | Equivalent to 'List.takeWhile' from "Data.List".
   takeWhile :: (m -> Bool) -> m -> m
   -- | Equivalent to 'List.dropWhile' from "Data.List".
   dropWhile :: (m -> Bool) -> m -> m
   -- | A stateful variant of 'span', threading the result of the test function as long as it returns 'Just'.
   spanMaybe :: s -> (s -> m -> Maybe s) -> m -> (m, m, s)
   -- | Strict version of 'spanMaybe'.
   spanMaybe' :: s -> (s -> m -> Maybe s) -> m -> (m, m, s)
   -- | Like 'List.splitAt' from "Data.List" on the list of 'primes'.
   splitAt :: Int -> m -> (m, m)
   -- | Equivalent to 'List.drop' from "Data.List".
   drop :: Int -> m -> m
   -- | Equivalent to 'List.take' from "Data.List".
   take :: Int -> m -> m
   -- | Equivalent to 'List.reverse' from "Data.List".
   reverse :: m -> m

   factors = List.unfoldr splitPrimePrefix
   primePrefix = maybe mempty fst . splitPrimePrefix
   primeSuffix = maybe mempty snd . splitPrimeSuffix
   splitPrimePrefix x = case factors x
                        of [] -> Nothing
                           prefix : rest -> Just (prefix, mconcat rest)
   splitPrimeSuffix x = case factors x
                        of [] -> Nothing
                           fs -> Just (mconcat (List.init fs), List.last fs)
   inits = foldr (\m l-> mempty : List.map (mappend m) l) [mempty]
   tails m = m : maybe [] (tails . snd) (splitPrimePrefix m)
   foldl f f0 = List.foldl f f0 . factors
   foldl' f f0 = List.foldl' f f0 . factors
   foldr f f0 = List.foldr f f0 . factors
   length = List.length . factors
   foldMap f = foldr (mappend . f) mempty
   span p m0 = spanAfter id m0
      where spanAfter f m = case splitPrimePrefix m
                            of Just (prime, rest) | p prime -> spanAfter (f . mappend prime) rest
                               _ -> (f mempty, m)
   break = span . (not .)
   spanMaybe s0 f m0 = spanAfter id s0 m0
      where spanAfter g s m = case splitPrimePrefix m
                              of Just (prime, rest) | Just s' <- f s prime -> spanAfter (g . mappend prime) s' rest
                                                    | otherwise -> (g mempty, m, s)
                                 Nothing -> (m0, m, s)
   spanMaybe' s0 f m0 = spanAfter id s0 m0
      where spanAfter g s m = seq s $
                              case splitPrimePrefix m
                              of Just (prime, rest) | Just s' <- f s prime -> spanAfter (g . mappend prime) s' rest
                                                    | otherwise -> (g mempty, m, s)
                                 Nothing -> (m0, m, s)
   split p m = prefix : splitRest
      where (prefix, rest) = break p m
            splitRest = case splitPrimePrefix rest
                        of Nothing -> []
                           Just (_, tl) -> split p tl
   takeWhile p = fst . span p
   dropWhile p = snd . span p
   splitAt n0 m0 | n0 <= 0 = (mempty, m0)
                 | otherwise = split' n0 id m0
      where split' 0 f m = (f mempty, m)
            split' n f m = case splitPrimePrefix m
                           of Nothing -> (f mempty, m)
                              Just (prime, rest) -> split' (pred n) (f . mappend prime) rest
   drop n p = snd (splitAt n p)
   take n p = fst (splitAt n p)
   reverse = mconcat . List.reverse . factors
   {-# MINIMAL factors | splitPrimePrefix #-}

-- | A subclass of 'FactorialMonoid' whose instances satisfy this additional law:
--
-- > factors (a <> b) == factors a <> factors b
class (FactorialMonoid m, PositiveMonoid m) => StableFactorialMonoid m

instance FactorialMonoid () where
   factors () = []
   primePrefix () = ()
   primeSuffix () = ()
   splitPrimePrefix () = Nothing
   splitPrimeSuffix () = Nothing
   length () = 0
   reverse = id

instance FactorialMonoid a => FactorialMonoid (Dual a) where
   factors (Dual a) = fmap Dual (reverse $ factors a)
   length (Dual a) = length a
   primePrefix (Dual a) = Dual (primeSuffix a)
   primeSuffix (Dual a) = Dual (primePrefix a)
   splitPrimePrefix (Dual a) = case splitPrimeSuffix a
                               of Nothing -> Nothing
                                  Just (p, s) -> Just (Dual s, Dual p)
   splitPrimeSuffix (Dual a) = case splitPrimePrefix a
                               of Nothing -> Nothing
                                  Just (p, s) -> Just (Dual s, Dual p)
   inits (Dual a) = fmap Dual (reverse $ tails a)
   tails (Dual a) = fmap Dual (reverse $ inits a)
   reverse (Dual a) = Dual (reverse a)

instance (Integral a, Eq a) => FactorialMonoid (Sum a) where
   primePrefix (Sum a) = Sum (signum a )
   primeSuffix = primePrefix
   splitPrimePrefix (Sum 0) = Nothing
   splitPrimePrefix (Sum a) = Just (Sum (signum a), Sum (a - signum a))
   splitPrimeSuffix (Sum 0) = Nothing
   splitPrimeSuffix (Sum a) = Just (Sum (a - signum a), Sum (signum a))
   length (Sum a) = abs (fromIntegral a)
   reverse = id

instance Integral a => FactorialMonoid (Product a) where
   factors (Product a) = List.map Product (primeFactors a)
   reverse = id

instance FactorialMonoid a => FactorialMonoid (Maybe a) where
   factors Nothing = []
   factors (Just a) | null a = [Just a]
                    | otherwise = List.map Just (factors a)
   length Nothing = 0
   length (Just a) | null a = 1
                   | otherwise = length a
   reverse = fmap reverse

instance (FactorialMonoid a, FactorialMonoid b) => FactorialMonoid (a, b) where
   factors (a, b) = List.map (\a1-> (a1, mempty)) (factors a) ++ List.map ((,) mempty) (factors b)
   primePrefix (a, b) | null a = (a, primePrefix b)
                      | otherwise = (primePrefix a, mempty)
   primeSuffix (a, b) | null b = (primeSuffix a, b)
                      | otherwise = (mempty, primeSuffix b)
   splitPrimePrefix (a, b) = case (splitPrimePrefix a, splitPrimePrefix b)
                             of (Just (ap, as), _) -> Just ((ap, mempty), (as, b))
                                (Nothing, Just (bp, bs)) -> Just ((a, bp), (a, bs))
                                (Nothing, Nothing) -> Nothing
   splitPrimeSuffix (a, b) = case (splitPrimeSuffix a, splitPrimeSuffix b)
                             of (_, Just (bp, bs)) -> Just ((a, bp), (mempty, bs))
                                (Just (ap, as), Nothing) -> Just ((ap, b), (as, b))
                                (Nothing, Nothing) -> Nothing
   inits (a, b) = List.map (flip (,) mempty) (inits a) ++ List.map ((,) a) (List.tail $ inits b)
   tails (a, b) = List.map (flip (,) b) (tails a) ++ List.map ((,) mempty) (List.tail $ tails b)
   foldl f a0 (x, y) = foldl f2 (foldl f1 a0 x) y
      where f1 a = f a . fromFst
            f2 a = f a . fromSnd
   foldl' f a0 (x, y) = a' `seq` foldl' f2 a' y
      where f1 a = f a . fromFst
            f2 a = f a . fromSnd
            a' = foldl' f1 a0 x
   foldr f a (x, y) = foldr (f . fromFst) (foldr (f . fromSnd) a y) x
   foldMap f (x, y) = foldMap (f . fromFst) x `mappend` foldMap (f . fromSnd) y
   length (a, b) = length a + length b
   span p (x, y) = ((xp, yp), (xs, ys))
      where (xp, xs) = span (p . fromFst) x
            (yp, ys) | null xs = span (p . fromSnd) y
                     | otherwise = (mempty, y)
   spanMaybe s0 f (x, y) | null xs = ((xp, yp), (xs, ys), s2)
                         | otherwise = ((xp, mempty), (xs, y), s1)
     where (xp, xs, s1) = spanMaybe s0 (\s-> f s . fromFst) x
           (yp, ys, s2) = spanMaybe s1 (\s-> f s . fromSnd) y
   spanMaybe' s0 f (x, y) | null xs = ((xp, yp), (xs, ys), s2)
                          | otherwise = ((xp, mempty), (xs, y), s1)
     where (xp, xs, s1) = spanMaybe' s0 (\s-> f s . fromFst) x
           (yp, ys, s2) = spanMaybe' s1 (\s-> f s . fromSnd) y
   split p (x0, y0) = fst $ List.foldr combine (ys, False) xs
      where xs = List.map fromFst $ split (p . fromFst) x0
            ys = List.map fromSnd $ split (p . fromSnd) y0
            combine x (~(y:rest), False) = (mappend x y : rest, True)
            combine x (rest, True) = (x:rest, True)
   splitAt n (x, y) = ((xp, yp), (xs, ys))
      where (xp, xs) = splitAt n x
            (yp, ys) | null xs = splitAt (n - length x) y
                     | otherwise = (mempty, y)
   reverse (a, b) = (reverse a, reverse b)

{-# INLINE fromFst #-}
fromFst :: Monoid b => a -> (a, b)
fromFst a = (a, mempty)

{-# INLINE fromSnd #-}
fromSnd :: Monoid a => b -> (a, b)
fromSnd b = (mempty, b)

instance FactorialMonoid [x] where
   factors xs = List.map (:[]) xs
   primePrefix [] = []
   primePrefix (x:_) = [x]
   primeSuffix [] = []
   primeSuffix xs = [List.last xs]
   splitPrimePrefix [] = Nothing
   splitPrimePrefix (x:xs) = Just ([x], xs)
   splitPrimeSuffix [] = Nothing
   splitPrimeSuffix xs = Just (splitLast id xs)
      where splitLast f last@[_] = (f [], last)
            splitLast f ~(x:rest) = splitLast (f . (x:)) rest
   inits = List.inits
   tails = List.tails
   foldl _ a [] = a
   foldl f a (x:xs) = foldl f (f a [x]) xs
   foldl' _ a [] = a
   foldl' f a (x:xs) = let a' = f a [x] in a' `seq` foldl' f a' xs
   foldr _ f0 [] = f0
   foldr f f0 (x:xs) = f [x] (foldr f f0 xs)
   length = List.length
   foldMap f = mconcat . List.map (f . (:[]))
   break f = List.break (f . (:[]))
   span f = List.span (f . (:[]))
   dropWhile f = List.dropWhile (f . (:[]))
   takeWhile f = List.takeWhile (f . (:[]))
   spanMaybe s0 f l = (prefix' [], suffix' [], s')
      where (prefix', suffix', s', _) = List.foldl' g (id, id, s0, True) l
            g (prefix, suffix, s1, live) x | live, Just s2 <- f s1 [x] = (prefix . (x:), id, s2, True)
                                           | otherwise = (prefix, suffix . (x:), s1, False)
   spanMaybe' s0 f l = (prefix' [], suffix' [], s')
      where (prefix', suffix', s', _) = List.foldl' g (id, id, s0, True) l
            g (prefix, suffix, s1, live) x | live, Just s2 <- f s1 [x] = seq s2 $ (prefix . (x:), id, s2, True)
                                           | otherwise = (prefix, suffix . (x:), s1, False)
   splitAt = List.splitAt
   drop = List.drop
   take = List.take
   reverse = List.reverse

instance FactorialMonoid ByteString.ByteString where
   factors x = factorize (ByteString.length x) x
      where factorize 0 _ = []
            factorize n xs = xs1 : factorize (pred n) xs'
              where (xs1, xs') = ByteString.splitAt 1 xs
   primePrefix = ByteString.take 1
   primeSuffix x = ByteString.drop (ByteString.length x - 1) x
   splitPrimePrefix x = if ByteString.null x then Nothing else Just (ByteString.splitAt 1 x)
   splitPrimeSuffix x = if ByteString.null x then Nothing else Just (ByteString.splitAt (ByteString.length x - 1) x)
   inits = ByteString.inits
   tails = ByteString.tails
   foldl f = ByteString.foldl f'
      where f' a byte = f a (ByteString.singleton byte)
   foldl' f = ByteString.foldl' f'
      where f' a byte = f a (ByteString.singleton byte)
   foldr f = ByteString.foldr (f . ByteString.singleton)
   break f = ByteString.break (f . ByteString.singleton)
   span f = ByteString.span (f . ByteString.singleton)
   spanMaybe s0 f b = case ByteString.foldr g id b (0, s0)
                      of (i, s') | (prefix, suffix) <- ByteString.splitAt i b -> (prefix, suffix, s')
      where g w cont (i, s) | Just s' <- f s (ByteString.singleton w) = let i' = succ i :: Int in seq i' $ cont (i', s')
                            | otherwise = (i, s)
   spanMaybe' s0 f b = case ByteString.foldr g id b (0, s0)
                       of (i, s') | (prefix, suffix) <- ByteString.splitAt i b -> (prefix, suffix, s')
      where g w cont (i, s) | Just s' <- f s (ByteString.singleton w) = let i' = succ i :: Int in seq i' $ seq s' $ cont (i', s')
                            | otherwise = (i, s)
   dropWhile f = ByteString.dropWhile (f . ByteString.singleton)
   takeWhile f = ByteString.takeWhile (f . ByteString.singleton)
   length = ByteString.length
   split f = ByteString.splitWith f'
      where f' = f . ByteString.singleton
   splitAt = ByteString.splitAt
   drop = ByteString.drop
   take = ByteString.take
   reverse = ByteString.reverse

instance FactorialMonoid LazyByteString.ByteString where
   factors x = factorize (LazyByteString.length x) x
      where factorize 0 _ = []
            factorize n xs = xs1 : factorize (pred n) xs'
               where (xs1, xs') = LazyByteString.splitAt 1 xs
   primePrefix = LazyByteString.take 1
   primeSuffix x = LazyByteString.drop (LazyByteString.length x - 1) x
   splitPrimePrefix x = if LazyByteString.null x then Nothing
                        else Just (LazyByteString.splitAt 1 x)
   splitPrimeSuffix x = if LazyByteString.null x then Nothing
                        else Just (LazyByteString.splitAt (LazyByteString.length x - 1) x)
   inits = LazyByteString.inits
   tails = LazyByteString.tails
   foldl f = LazyByteString.foldl f'
      where f' a byte = f a (LazyByteString.singleton byte)
   foldl' f = LazyByteString.foldl' f'
      where f' a byte = f a (LazyByteString.singleton byte)
   foldr f = LazyByteString.foldr f'
      where f' byte a = f (LazyByteString.singleton byte) a
   length = fromIntegral . LazyByteString.length
   break f = LazyByteString.break (f . LazyByteString.singleton)
   span f = LazyByteString.span (f . LazyByteString.singleton)
   spanMaybe s0 f b = case LazyByteString.foldr g id b (0, s0)
                      of (i, s') | (prefix, suffix) <- LazyByteString.splitAt i b -> (prefix, suffix, s')
      where g w cont (i, s) | Just s' <- f s (LazyByteString.singleton w) = let i' = succ i :: Int64 in seq i' $ cont (i', s')
                            | otherwise = (i, s)
   spanMaybe' s0 f b = case LazyByteString.foldr g id b (0, s0)
                       of (i, s') | (prefix, suffix) <- LazyByteString.splitAt i b -> (prefix, suffix, s')
      where g w cont (i, s)
              | Just s' <- f s (LazyByteString.singleton w) = let i' = succ i :: Int64 in seq i' $ seq s' $ cont (i', s')
              | otherwise = (i, s)
   dropWhile f = LazyByteString.dropWhile (f . LazyByteString.singleton)
   takeWhile f = LazyByteString.takeWhile (f . LazyByteString.singleton)
   split f = LazyByteString.splitWith f'
      where f' = f . LazyByteString.singleton
   splitAt = LazyByteString.splitAt . fromIntegral
   drop n = LazyByteString.drop (fromIntegral n)
   take n = LazyByteString.take (fromIntegral n)
   reverse = LazyByteString.reverse

instance FactorialMonoid Text.Text where
   factors = Text.chunksOf 1
   primePrefix = Text.take 1
   primeSuffix x = if Text.null x then Text.empty else Text.singleton (Text.last x)
   splitPrimePrefix = fmap (first Text.singleton) . Text.uncons
   splitPrimeSuffix x = if Text.null x then Nothing else Just (Text.init x, Text.singleton (Text.last x))
   inits = Text.inits
   tails = Text.tails
   foldl f = Text.foldl f'
      where f' a char = f a (Text.singleton char)
   foldl' f = Text.foldl' f'
      where f' a char = f a (Text.singleton char)
   foldr f = Text.foldr f'
      where f' char a = f (Text.singleton char) a
   length = Text.length
   span f = Text.span (f . Text.singleton)
   break f = Text.break (f . Text.singleton)
   dropWhile f = Text.dropWhile (f . Text.singleton)
   takeWhile f = Text.takeWhile (f . Text.singleton)
   spanMaybe s0 f t = case Text.foldr g id t (0, s0)
                      of (i, s') | (prefix, suffix) <- Text.splitAt i t -> (prefix, suffix, s')
      where g c cont (i, s) | Just s' <- f s (Text.singleton c) = let i' = succ i :: Int in seq i' $ cont (i', s')
                            | otherwise = (i, s)
   spanMaybe' s0 f t = case Text.foldr g id t (0, s0)
                       of (i, s') | (prefix, suffix) <- Text.splitAt i t -> (prefix, suffix, s')
      where g c cont (i, s) | Just s' <- f s (Text.singleton c) = let i' = succ i :: Int in seq i' $ seq s' $ cont (i', s')
                            | otherwise = (i, s)
   split f = Text.split f'
      where f' = f . Text.singleton
   splitAt = Text.splitAt
   drop = Text.drop
   take = Text.take
   reverse = Text.reverse

instance FactorialMonoid LazyText.Text where
   factors = LazyText.chunksOf 1
   primePrefix = LazyText.take 1
   primeSuffix x = if LazyText.null x then LazyText.empty else LazyText.singleton (LazyText.last x)
   splitPrimePrefix = fmap (first LazyText.singleton) . LazyText.uncons
   splitPrimeSuffix x = if LazyText.null x
                        then Nothing
                        else Just (LazyText.init x, LazyText.singleton (LazyText.last x))
   inits = LazyText.inits
   tails = LazyText.tails
   foldl f = LazyText.foldl f'
      where f' a char = f a (LazyText.singleton char)
   foldl' f = LazyText.foldl' f'
      where f' a char = f a (LazyText.singleton char)
   foldr f = LazyText.foldr f'
      where f' char a = f (LazyText.singleton char) a
   length = fromIntegral . LazyText.length
   span f = LazyText.span (f . LazyText.singleton)
   break f = LazyText.break (f . LazyText.singleton)
   dropWhile f = LazyText.dropWhile (f . LazyText.singleton)
   takeWhile f = LazyText.takeWhile (f . LazyText.singleton)
   spanMaybe s0 f t = case LazyText.foldr g id t (0, s0)
                      of (i, s') | (prefix, suffix) <- LazyText.splitAt i t -> (prefix, suffix, s')
      where g c cont (i, s) | Just s' <- f s (LazyText.singleton c) = let i' = succ i :: Int64 in seq i' $ cont (i', s')
                            | otherwise = (i, s)
   spanMaybe' s0 f t = case LazyText.foldr g id t (0, s0)
                       of (i, s') | (prefix, suffix) <- LazyText.splitAt i t -> (prefix, suffix, s')
      where g c cont (i, s) | Just s' <- f s (LazyText.singleton c) = let i' = succ i :: Int64 in seq i' $ seq s' $ cont (i', s')
                            | otherwise = (i, s)
   split f = LazyText.split f'
      where f' = f . LazyText.singleton
   splitAt = LazyText.splitAt . fromIntegral
   drop n = LazyText.drop (fromIntegral n)
   take n = LazyText.take (fromIntegral n)
   reverse = LazyText.reverse

instance Ord k => FactorialMonoid (Map.Map k v) where
   factors = List.map (uncurry Map.singleton) . Map.toAscList
   primePrefix map | Map.null map = map
                   | otherwise = uncurry Map.singleton $ Map.findMin map
   primeSuffix map | Map.null map = map
                   | otherwise = uncurry Map.singleton $ Map.findMax map
   splitPrimePrefix = fmap singularize . Map.minViewWithKey
      where singularize ((k, v), rest) = (Map.singleton k v, rest)
   splitPrimeSuffix = fmap singularize . Map.maxViewWithKey
      where singularize ((k, v), rest) = (rest, Map.singleton k v)
   foldl f = Map.foldlWithKey f'
      where f' a k v = f a (Map.singleton k v)
   foldl' f = Map.foldlWithKey' f'
      where f' a k v = f a (Map.singleton k v)
   foldr f = Map.foldrWithKey f'
      where f' k v a = f (Map.singleton k v) a
   length = Map.size
   reverse = id

instance FactorialMonoid (IntMap.IntMap a) where
   factors = List.map (uncurry IntMap.singleton) . IntMap.toAscList
   primePrefix map | IntMap.null map = map
                   | otherwise = uncurry IntMap.singleton $ IntMap.findMin map
   primeSuffix map | IntMap.null map = map
                   | otherwise = uncurry IntMap.singleton $ IntMap.findMax map
   splitPrimePrefix = fmap singularize . IntMap.minViewWithKey
      where singularize ((k, v), rest) = (IntMap.singleton k v, rest)
   splitPrimeSuffix = fmap singularize . IntMap.maxViewWithKey
      where singularize ((k, v), rest) = (rest, IntMap.singleton k v)
   foldl f = IntMap.foldlWithKey f'
      where f' a k v = f a (IntMap.singleton k v)
   foldl' f = IntMap.foldlWithKey' f'
      where f' a k v = f a (IntMap.singleton k v)
   foldr f = IntMap.foldrWithKey f'
      where f' k v a = f (IntMap.singleton k v) a
   length = IntMap.size
   reverse = id

instance FactorialMonoid IntSet.IntSet where
   factors = List.map IntSet.singleton . IntSet.toAscList
   primePrefix set | IntSet.null set = set
                   | otherwise = IntSet.singleton $ IntSet.findMin set
   primeSuffix set | IntSet.null set = set
                   | otherwise = IntSet.singleton $ IntSet.findMax set
   splitPrimePrefix = fmap singularize . IntSet.minView
      where singularize (min, rest) = (IntSet.singleton min, rest)
   splitPrimeSuffix = fmap singularize . IntSet.maxView
      where singularize (max, rest) = (rest, IntSet.singleton max)
   foldl f = IntSet.foldl f'
      where f' a b = f a (IntSet.singleton b)
   foldl' f = IntSet.foldl' f'
      where f' a b = f a (IntSet.singleton b)
   foldr f = IntSet.foldr f'
      where f' a b = f (IntSet.singleton a) b
   length = IntSet.size
   reverse = id

instance FactorialMonoid (Sequence.Seq a) where
   factors = List.map Sequence.singleton . Foldable.toList
   primePrefix = Sequence.take 1
   primeSuffix q = Sequence.drop (Sequence.length q - 1) q
   splitPrimePrefix q = case Sequence.viewl q
                        of Sequence.EmptyL -> Nothing
                           hd Sequence.:< rest -> Just (Sequence.singleton hd, rest)
   splitPrimeSuffix q = case Sequence.viewr q
                        of Sequence.EmptyR -> Nothing
                           rest Sequence.:> last -> Just (rest, Sequence.singleton last)
   inits = Foldable.toList . Sequence.inits
   tails = Foldable.toList . Sequence.tails
   foldl f = Foldable.foldl f'
      where f' a b = f a (Sequence.singleton b)
   foldl' f = Foldable.foldl' f'
      where f' a b = f a (Sequence.singleton b)
   foldr f = Foldable.foldr f'
      where f' a b = f (Sequence.singleton a) b
   span f = Sequence.spanl (f . Sequence.singleton)
   break f = Sequence.breakl (f . Sequence.singleton)
   dropWhile f = Sequence.dropWhileL (f . Sequence.singleton)
   takeWhile f = Sequence.takeWhileL (f . Sequence.singleton)
   spanMaybe s0 f b = case Foldable.foldr g id b (0, s0)
                      of (i, s') | (prefix, suffix) <- Sequence.splitAt i b -> (prefix, suffix, s')
      where g x cont (i, s) | Just s' <- f s (Sequence.singleton x) = let i' = succ i :: Int in seq i' $ cont (i', s')
                            | otherwise = (i, s)
   spanMaybe' s0 f b = case Foldable.foldr g id b (0, s0)
                       of (i, s') | (prefix, suffix) <- Sequence.splitAt i b -> (prefix, suffix, s')
      where g x cont (i, s) | Just s' <- f s (Sequence.singleton x) = let i' = succ i :: Int in seq i' $ seq s' $ cont (i', s')
                            | otherwise = (i, s)
   splitAt = Sequence.splitAt
   drop = Sequence.drop
   take = Sequence.take
   length = Sequence.length
   reverse = Sequence.reverse

instance Ord a => FactorialMonoid (Set.Set a) where
   factors = List.map Set.singleton . Set.toAscList
   primePrefix set | Set.null set = set
                   | otherwise = Set.singleton $ Set.findMin set
   primeSuffix set | Set.null set = set
                   | otherwise = Set.singleton $ Set.findMax set
   splitPrimePrefix = fmap singularize . Set.minView
      where singularize (min, rest) = (Set.singleton min, rest)
   splitPrimeSuffix = fmap singularize . Set.maxView
      where singularize (max, rest) = (rest, Set.singleton max)
   foldl f = Foldable.foldl f'
      where f' a b = f a (Set.singleton b)
   foldl' f = Foldable.foldl' f'
      where f' a b = f a (Set.singleton b)
   foldr f = Foldable.foldr f'
      where f' a b = f (Set.singleton a) b
   length = Set.size
   reverse = id

instance FactorialMonoid (Vector.Vector a) where
   factors x = factorize (Vector.length x) x
      where factorize 0 _ = []
            factorize n xs = xs1 : factorize (pred n) xs'
               where (xs1, xs') = Vector.splitAt 1 xs
   primePrefix = Vector.take 1
   primeSuffix x = Vector.drop (Vector.length x - 1) x
   splitPrimePrefix x = if Vector.null x then Nothing else Just (Vector.splitAt 1 x)
   splitPrimeSuffix x = if Vector.null x then Nothing else Just (Vector.splitAt (Vector.length x - 1) x)
   inits x0 = initsWith x0 []
      where initsWith x rest | Vector.null x = x:rest
                             | otherwise = initsWith (Vector.unsafeInit x) (x:rest)
   tails x = x : if Vector.null x then [] else tails (Vector.unsafeTail x)
   foldl f = Vector.foldl f'
      where f' a byte = f a (Vector.singleton byte)
   foldl' f = Vector.foldl' f'
      where f' a byte = f a (Vector.singleton byte)
   foldr f = Vector.foldr f'
      where f' byte a = f (Vector.singleton byte) a
   break f = Vector.break (f . Vector.singleton)
   span f = Vector.span (f . Vector.singleton)
   dropWhile f = Vector.dropWhile (f . Vector.singleton)
   takeWhile f = Vector.takeWhile (f . Vector.singleton)
   spanMaybe s0 f v = case Vector.ifoldr g Left v s0
                      of Left s' -> (v, Vector.empty, s')
                         Right (i, s') | (prefix, suffix) <- Vector.splitAt i v -> (prefix, suffix, s')
      where g i x cont s | Just s' <- f s (Vector.singleton x) = cont s'
                         | otherwise = Right (i, s)
   spanMaybe' s0 f v = case Vector.ifoldr' g Left v s0
                       of Left s' -> (v, Vector.empty, s')
                          Right (i, s') | (prefix, suffix) <- Vector.splitAt i v -> (prefix, suffix, s')
      where g i x cont s | Just s' <- f s (Vector.singleton x) = seq s' (cont s')
                         | otherwise = Right (i, s)
   splitAt = Vector.splitAt
   drop = Vector.drop
   take = Vector.take
   length = Vector.length
   reverse = Vector.reverse

instance StableFactorialMonoid ()
instance StableFactorialMonoid a => StableFactorialMonoid (Dual a)
instance StableFactorialMonoid [x]
instance StableFactorialMonoid ByteString.ByteString
instance StableFactorialMonoid LazyByteString.ByteString
instance StableFactorialMonoid Text.Text
instance StableFactorialMonoid LazyText.Text
instance StableFactorialMonoid (Sequence.Seq a)
instance StableFactorialMonoid (Vector.Vector a)

-- | A 'Monad.mapM' equivalent.
mapM :: (FactorialMonoid a, Monoid b, Monad m) => (a -> m b) -> a -> m b
mapM f = ($ return mempty) . appEndo . foldMap (Endo . Monad.liftM2 mappend . f)

-- | A 'Monad.mapM_' equivalent.
mapM_ :: (FactorialMonoid a, Monad m) => (a -> m b) -> a -> m ()
mapM_ f = foldr ((>>) . f) (return ())
