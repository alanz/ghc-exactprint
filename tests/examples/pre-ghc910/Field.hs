{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-|
    Module      :  AERN2.MP.Ball.Field
    Description :  Field operations on arbitrary precision dyadic balls
    Copyright   :  (c) Michal Konecny
    License     :  BSD3

    Maintainer  :  mikkonecny@gmail.com
    Stability   :  experimental
    Portability :  portable

    Field operations on arbitrary precision dyadic balls
-}
module AERN2.MP.Ball.Field
(mulBalls, mulByEndpoints)
where

import MixedTypesNumPrelude
-- import qualified Prelude as P

import qualified Numeric.CollectErrors as CN

import AERN2.Normalize


mulByEndpoints :: MPBall -> MPBall -> MPBall
mulByEndpoints b1 b2 =
  fromEndpoints l r
  where
  (l,r)
    | 0 <= l1 && 0 <= l2 = (l1*.l2, r1*^r2) -- 0 <= l1 <= r1, 0 <= l2 <= r2
    | r1 <= 0 && r2 <= 0 = (r1*.r2, l1*^l2) -- l1 <= r1 <= 0, l2 <= r2 <= 0
    | 0 <= l1 && r2 <= 0 = (r1*.l2, l1*^r2) -- l2 <= r2 <= 0 <= l1 <= r1
    | r1 <= 0 && 0 <= l2 = (l1*.r2, r1*^l2) -- l1 <= r1 <= 0 <= l2 <= r2
    | l1 < 0 && 0 < r1 && 0 <= l2 = (l1*.r2, r1*^r2) -- l1 < 0 < r1, 0 <= l2 <= r2
    | l1 < 0 && 0 < r1 && r2 <= 0 = (r1*.l2, l1*^l2) -- l1 < 0 < r1, l2 <= r2 <= 0
    | l2 < 0 && 0 < r2 && 0 <= l1 = (l2*.r1, r2*^r1) -- l2 < 0 < r2, 0 <= l1 <= r1
    | l2 < 0 && 0 < r2 && r1 <= 0 = (r2*.l1, l2*^l1) -- l2 < 0 < r2, l1 <= r1 <= 0
    | otherwise = -- l1 < 0 < r1, l2 < 0 < r2
      ((l1 *. r2) `min` (r1 *. l2)
      ,(l1 *^ l2) `max` (r1 *^ r2))
  (l1,r1) = endpoints b1
  (l2,r2) = endpoints b2


