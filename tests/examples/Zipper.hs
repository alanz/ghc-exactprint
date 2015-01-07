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



---- Internal types and functions
data Context hole root where
    CtxtNull :: Context a a
    CtxtCons ::
      forall hole root rights parent. (Data parent) =>
        Left (hole -> rights)
        -> Right rights parent
        -> Context parent root
        -> Context hole root

data Left expects
  = LeftUnit expects
  | forall b. (Data b) => LeftCons (Left (b -> expects)) b

data Right provides parent where
  RightNull :: Right parent parent
  RightCons ::
    (Data b) => b -> Right a t -> Right (b -> a) t

