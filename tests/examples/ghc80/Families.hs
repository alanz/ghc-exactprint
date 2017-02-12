{-# LANGUAGE PolyKinds, TypeFamilies, DataKinds, KindSignatures,
    TypeOperators, UndecidableInstances #-}

{-# LANGUAGE RankNTypes, LiberalTypeSynonyms, EmptyDataDecls #-}

-- | A prelude for type-level programming with type families

module Prelude.Type.Families where

-- >>> T :: T ((I 4) `Minus` (I 7))
-- -3
type family (a :: k) `Minus` (b :: k) :: k
type instance a `Minus` b = a + Negate b

