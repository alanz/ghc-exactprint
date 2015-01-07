{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Generics.Zipper
-- Copyright   :  (c) Michael D. Adams, 2010
-- License     :  BSD-style (see the LICENSE file)
--
-- ``Scrap Your Zippers: A Generic Zipper for Heterogeneous Types.
-- Michael D. Adams.  WGP '10: Proceedings of the 2010 ACM SIGPLAN
-- workshop on Generic programming, 2010.''
--
-- See <http://www.cs.indiana.edu/~adamsmd/papers/scrap_your_zippers/>
--
-----------------------------------------------------------------------------

{-# OPTIONS -Wall #-}
{-# LANGUAGE Rank2Types, GADTs #-}

module Zipper where

import Data.Generics
import Control.Monad ((<=<), MonadPlus, mzero, mplus, liftM)
import Data.Maybe (fromJust)

-- Core types

-- | A generic zipper with a root object of type @root@.
data Zipper root =
  forall hole. (Data hole) =>
    Zipper hole (Context hole root)

---- Internal types and functions
data Context hole root where
    CtxtNull :: Context a a
    CtxtCons ::
      forall hole root rights parent. (Data parent) =>
        Left (hole -> rights)
        -> Right rights parent
        -> Context parent root
        -> Context hole root

combine :: Left (hole -> rights)
         -> hole
         -> Right rights parent
         -> parent
combine lefts hole rights =
  fromRight ((fromLeft lefts) hole) rights

data Left expects
  = LeftUnit expects
  | forall b. (Data b) => LeftCons (Left (b -> expects)) b

toLeft :: (Data a) => a -> Left a
toLeft a = gfoldl LeftCons LeftUnit a

fromLeft :: Left r -> r
fromLeft (LeftUnit a)   = a
fromLeft (LeftCons f b) = fromLeft f b

data Right provides parent where
  RightNull :: Right parent parent
  RightCons ::
    (Data b) => b -> Right a t -> Right (b -> a) t

fromRight :: r -> Right r parent -> parent
fromRight f (RightNull)     = f
fromRight f (RightCons b r) = fromRight (f b) r

-- | Apply a generic monadic transformation to the hole
transM :: (Monad m) => GenericM m -> Zipper a -> m (Zipper a)
transM f (Zipper hole ctxt) = do
  hole' <- f hole
  return (Zipper hole' ctxt)

-- Generic zipper traversals
---- Traversal helpers
-- | A movement operation such as 'left', 'right', 'up', or 'down'.
type Move a = Zipper a -> Maybe (Zipper a)

-- | Apply a generic query using the specified movement operation.
moveQ :: Move a -- ^ Move operation
      -> b -- ^ Default if can't move
      -> (Zipper a -> b) -- ^ Query if can move
      -> Zipper a -- ^ Zipper
      -> b
moveQ move b f z = case move z of
                     Nothing -> b
                     Just z' -> f z'

-- | Repeatedly apply a monadic 'Maybe' generic transformation at the
-- top-most, left-most position that the transformation returns
-- 'Just'.  Behaves like iteratively applying 'zsomewhere' but is
-- more efficient because it re-evaluates the transformation
-- at only the parents of the last successful application.
zreduce :: GenericM Maybe -> Zipper a -> Zipper a
zreduce f z =
  case transM f z of
    Nothing ->
      downQ (g z) (zreduce f . leftmost) z where
        g z' = rightQ (upQ z' g z') (zreduce f) z'
    Just x  -> zreduce f (reduceAncestors1 f x x)

reduceAncestors1 ::
  GenericM Maybe -> Zipper a -> Zipper a -> Zipper a
reduceAncestors1 f z def = upQ def g z where
  g z' = reduceAncestors1 f z' def' where
    def' = case transM f z' of
             Nothing -> def
             Just x  -> reduceAncestors1 f x x

------ Query
-- | Apply a generic query to the left sibling if one exists.
leftQ :: b -- ^ Value to return of no left sibling exists.
      -> (Zipper a -> b) -> Zipper a -> b
leftQ b f z = moveQ left b f z

-- | Apply a generic query to the right sibling if one exists.
rightQ :: b -- ^ Value to return if no right sibling exists.
       -> (Zipper a -> b) -> Zipper a -> b
rightQ b f z = moveQ right b f z

-- | Apply a generic query to the parent if it exists.
downQ :: b -- ^ Value to return if no children exist.
      -> (Zipper a -> b) -> Zipper a -> b
downQ b f z = moveQ down b f z

-- | Apply a generic query to the rightmost child if one exists.
upQ :: b -- ^ Value to return if parent does not exist.
    -> (Zipper a -> b) -> Zipper a -> b
upQ b f z = moveQ up b f z

---- Basic movement

-- | Move left.  Returns 'Nothing' iff already at leftmost sibling.
left  :: Zipper a -> Maybe (Zipper a)
left (Zipper _ CtxtNull) = Nothing
left (Zipper _ (CtxtCons (LeftUnit _) _ _)) = Nothing
left (Zipper h (CtxtCons (LeftCons l h') r c)) =
  Just (Zipper h' (CtxtCons l (RightCons h r) c))

-- | Move right.  Returns 'Nothing' iff already at rightmost sibling.
right :: Zipper a -> Maybe (Zipper a)
right (Zipper _ CtxtNull) = Nothing
right (Zipper _ (CtxtCons _ RightNull _)) = Nothing
right (Zipper h (CtxtCons l (RightCons h' r) c)) =
  Just (Zipper h' (CtxtCons (LeftCons l h) r c))

-- | Move down.  Moves to rightmost immediate child.  Returns 'Nothing' iff at a leaf and thus no children exist.
down  :: Zipper a -> Maybe (Zipper a)
down (Zipper hole ctxt) =
  case toLeft hole of
    LeftUnit _ -> Nothing
    LeftCons l hole' ->
      Just (Zipper hole' (CtxtCons l RightNull ctxt))

-- | Move up.  Returns 'Nothing' iff already at root and thus no parent exists.
up    :: Zipper a -> Maybe (Zipper a)
up (Zipper _ CtxtNull) = Nothing
up (Zipper hole (CtxtCons l r ctxt)) =
  Just (Zipper (combine l hole r) ctxt)

------ Movement
-- | Move to the leftmost sibling.
leftmost :: Zipper a -> Zipper a
leftmost z = leftQ z leftmost z

