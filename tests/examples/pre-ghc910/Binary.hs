{-# OPTIONS -fno-warn-orphans #-}

module Data.Curve.Binary
  ( module Data.Curve
  , Point(..)
  -- * Binary curves
  , BCurve(..)
  , BPoint
  -- ** Binary affine curves
  , BACurve(..)
  , BAPoint
  -- ** Binary projective curves
  , BPCurve(..)
  , BPPoint
  ) where

import Protolude

import Data.Field.Galois as F (GaloisField, PrimeField, frob, quad)
import GHC.Natural (Natural)
import Text.PrettyPrint.Leijen.Text (Pretty(..))

import Data.Curve

-------------------------------------------------------------------------------
-- Binary form
-------------------------------------------------------------------------------

-- | Binary points.
type BPoint = Point 'Binary

-- | Binary curves.
class (GaloisField q, PrimeField r, Curve 'Binary c e q r) => BCurve c e q r where
  {-# MINIMAL a_, b_, h_, p_, r_ #-}
  a_ :: BPoint c e q r -> q       -- ^ Coefficient @A@.
  b_ :: BPoint c e q r -> q       -- ^ Coefficient @B@.
  h_ :: BPoint c e q r -> Natural -- ^ Curve cofactor.
  p_ :: BPoint c e q r -> Natural -- ^ Curve polynomial.
  r_ :: BPoint c e q r -> Natural -- ^ Curve order.

-------------------------------------------------------------------------------
-- Affine coordinates
-------------------------------------------------------------------------------

-- | Binary affine points.
type BAPoint = BPoint 'Affine

-- | Binary affine curves @y^2 + xy = x^3 + Ax^2 + B@.
class BCurve 'Affine e q r => BACurve e q r where
  {-# MINIMAL gA_ #-}
  gA_ :: BAPoint e q r -- ^ Curve generator.

-- Binary affine curves are elliptic curves.
instance BACurve e q r => Curve 'Binary 'Affine e q r where

  data instance Point 'Binary 'Affine e q r = A q q -- ^ Affine point.
                                            | O     -- ^ Infinite point.
    deriving (Eq, Generic, NFData, Read, Show)

  add p O       = p
  add O q       = q
  add (A x1 y1) (A x2 y2)
    | xx == 0   = O
    | otherwise = A x3 y3
    where
      a  = a_ (witness :: BAPoint e q r)
      xx = x1 + x2
      yy = y1 + y2
      l  = yy / xx
      x3 = l * (l + 1) + xx + a
      y3 = l * (x1 + x3) + x3 + y1
  {-# INLINABLE add #-}

  char = const 2
  {-# INLINABLE char #-}

  cof = h_
  {-# INLINABLE cof #-}

  dbl O         = O
  dbl (A x y)
    | x == 0    = O
    | otherwise = A x' y'
    where
      a  = a_ (witness :: BAPoint e q r)
      l  = x + y / x
      l' = l + 1
      x' = l * l' + a
      y' = x * x + l' * x'
  {-# INLINABLE dbl #-}

  def O       = True
  def (A x y) = ((x + a) * x + y) * x + b + y * y == 0
    where
      a = a_ (witness :: BAPoint e q r)
      b = b_ (witness :: BAPoint e q r)
  {-# INLINABLE def #-}

  disc _ = b_ (witness :: BAPoint e q r)
  {-# INLINABLE disc #-}

  frob O       = O
  frob (A x y) = A (F.frob x) (F.frob y)
  {-# INLINABLE frob #-}

  fromA = identity
  {-# INLINABLE fromA #-}

  gen = gA_
  {-# INLINABLE gen #-}

  id = O
  {-# INLINABLE id #-}

  inv O       = O
  inv (A x y) = A x (x + y)
  {-# INLINABLE inv #-}

  order = r_
  {-# INLINABLE order #-}

  point x y = let p = A x y in if def p then Just p else Nothing
  {-# INLINABLE point #-}

  pointX x = A x <$> yX (witness :: BAPoint e q r) x
  {-# INLINABLE pointX #-}

  toA = identity
  {-# INLINABLE toA #-}

  yX _ x = quad 1 x ((x + a) * x * x + b)
    where
      a = a_ (witness :: BAPoint e q r)
      b = b_ (witness :: BAPoint e q r)
  {-# INLINABLE yX #-}

-- Binary affine points are pretty.
instance BACurve e q r => Pretty (BAPoint e q r) where

  pretty (A x y) = pretty (x, y)
  pretty O       = "O"

-------------------------------------------------------------------------------
-- Projective coordinates
-------------------------------------------------------------------------------

-- | Binary projective points.
type BPPoint = BPoint 'Projective

-- | Binary projective curves @y^2z + xyz = x^3 + Ax^2z + Bz@.
class BCurve 'Projective e q r => BPCurve e q r where
  {-# MINIMAL gP_ #-}
  gP_ :: BPPoint e q r -- ^ Curve generator.

-- Binary projective curves are elliptic curves.
instance BPCurve e q r => Curve 'Binary 'Projective e q r where

  data instance Point 'Binary 'Projective e q r = P q q q -- ^ Projective point.
    deriving (Generic, NFData, Read, Show)

  -- Addition formula add-2008-bl
  add  p           (P  _  _  0) = p
  add (P  _  _  0)  q           = q
  add (P x1 y1 z1) (P x2 y2 z2) = P x3 y3 z3
    where
      a'   = a_ (witness :: BPPoint e q r)
      y1z2 = y1 * z2
      x1z2 = x1 * z2
      a    = y1z2 + z1 * y2
      b    = x1z2 + z1 * x2
      ab   = a + b
      c    = b * b
      d    = z1 * z2
      e    = b * c
      f    = (a * ab + a' * c) * d + e
      x3   = b * f
      y3   = c * (a * x1z2 + b * y1z2) + ab * f
      z3   = e * d
  {-# INLINABLE add #-}

  char = const 2
  {-# INLINABLE char #-}

  cof = h_
  {-# INLINABLE cof #-}

  -- Doubling formula dbl-2008-bl
  dbl (P  _  _  0) = P  0  1  0
  dbl (P x1 y1 z1) = P x3 y3 z3
    where
      a' = a_ (witness :: BPPoint e q r)
      a  = x1 * x1
      b  = a + y1 * z1
      c  = x1 * z1
      bc = b + c
      d  = c * c
      e  = b * bc + a' * d
      x3 = c * e
      y3 = bc * e + a * a * c
      z3 = c * d
  {-# INLINABLE dbl #-}

  def (P x y z) = ((x + a * z) * x + yz) * x + y * yz + b * z * z * z == 0
    where
      a  = a_ (witness :: BPPoint e q r)
      b  = b_ (witness :: BPPoint e q r)
      yz = y * z
  {-# INLINABLE def #-}

  disc _ = b_ (witness :: BPPoint e q r)
  {-# INLINABLE disc #-}

  frob (P x y z) = P (F.frob x) (F.frob y) (F.frob z)
  {-# INLINABLE frob #-}

  fromA (A x y) = P x y 1
  fromA _       = P 0 1 0
  {-# INLINABLE fromA #-}

  gen = gP_
  {-# INLINABLE gen #-}

  id = P 0 1 0
  {-# INLINABLE id #-}

  inv (P x y z) = P x (x + y) z
  {-# INLINABLE inv #-}

  order = r_
  {-# INLINABLE order #-}

  point x y = let p = P x y 1 in if def p then Just p else Nothing
  {-# INLINABLE point #-}

  pointX x = flip (P x) 1 <$> yX (witness :: BPPoint e q r) x
  {-# INLINABLE pointX #-}

  toA (P _ _ 0) = O
  toA (P x y z) = A (x / z) (y / z)
  {-# INLINABLE toA #-}

  yX _ x = quad 1 x ((x + a) * x * x + b)
    where
      a = a_ (witness :: BPPoint e q r)
      b = b_ (witness :: BPPoint e q r)
  {-# INLINABLE yX #-}

-- Binary projective points are equatable.
instance BPCurve e q r => Eq (BPPoint e q r) where

  P x1 y1 z1 == P x2 y2 z2 = z1 == 0 && z2 == 0
    || x1 * z2 == x2 * z1 && y1 * z2 == y2 * z1
  {-# INLINABLE (==) #-}

-- Binary projective points are pretty.
instance BPCurve e q r => Pretty (BPPoint e q r) where

  pretty (P x y z) = pretty (x, y, z)

