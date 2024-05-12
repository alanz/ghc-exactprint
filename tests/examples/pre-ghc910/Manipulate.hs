{- |
Module      :  Control.Comonad.Sheet.Manipulate
Description :  Generic functions for manipulating multi-dimensional comonadic spreadsheets.
Copyright   :  Copyright (c) 2014 Kenneth Foner

Maintainer  :  kenneth.foner@gmail.com
Stability   :  experimental
Portability :  non-portable

This module defines the 'take', 'view', 'go', and 'insert' functions generically for any dimensionality of sheet. These
constitute the preferred way of manipulating sheets, providing an interface to: take finite slices ('take'), infinite
slices ('view'), move to locations ('go'), and insert finite or infinite structures ('insert').
-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Control.Comonad.Sheet.Manipulate where


-- | In the case of a @Nested@ structure, @asDimensionalAs@ defaults to @asNestedAs@.
instance (NestedAs x (Nested ts y), AsDimensionalAs x (Nested ts y) ~ AsNestedAs x (Nested ts y)) => DimensionalAs x (Nested ts y) where
   type x `AsDimensionalAs` (Nested ts a) = x `AsNestedAs` (Nested ts a)
   asDimensionalAs = asNestedAs

-- | @DimensionalAs@ also knows the dimensionality of an 'Indexed' sheet as well as regular @Nested@ structures.
instance (NestedAs x (Nested ts y)) => DimensionalAs x (Indexed ts y) where
   type x `AsDimensionalAs` (Indexed ts a) = x `AsNestedAs` (Nested ts a)
   x `asDimensionalAs` (Indexed i t)       = x `asNestedAs` t


instance DepIndex (a,b) TH_0 where
    type (a,b) `DepIndexResult` TH_0 = a
    (a,b) # TH_0 = a
