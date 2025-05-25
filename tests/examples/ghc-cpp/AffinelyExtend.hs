-- from ./hackage-roundtrip-work/affinely-extended-0.1.0.0/src/Data/AffinelyExtend.hs
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}


{-# RULES
"isPosBounded/pack" forall x. isPosBounded (packBoth x) = isPos x
"isNegInfBounded/pack" forall x. isNegInfBounded (packBoth x) = isNegInf x
"isInfBounded/pack" forall x. isInfBounded (packBoth x) = isInf x
"isFiniteBounded/pack" forall x. isFiniteBounded (packBoth x) = isFinite x
#-}

instance (Ord a, Bounded a, Num a) => CanAffinelyExtend (AffinelyExtendBoundedBoth a) where
  type BaseType (AffinelyExtendBoundedBoth a) = a
  affinelyExtend_c = AffinelyExtendBoundedBoth
  unpackBoth_c (AffinelyExtendBoundedBoth x) = if
    | x == unwrappedPosInf -> PositiveInfinity
    | x == unwrappedNegInf -> NegativeInfinity
    | otherwise -> Finite x

  isPos = isPosBounded
  isNegInf = isNegInfBounded

  isInf = isInfBounded
  isFinite = isFiniteBounded

