{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, GADTs,
             ConstraintKinds, DataKinds, KindSignatures,
             FlexibleInstances #-}
{-# OPTIONS -fno-warn-redundant-constraints #-}

module T10195 where

import GHC.Exts

data Foo m zp r'q = Foo zp
data Dict :: Constraint -> * where
  Dict :: a => Dict a

type family BarFamily a b :: Bool
class Bar m m'
instance (BarFamily m m' ~ 'True) => Bar m m'

magic :: (Bar m m') => c m zp -> Foo m zp (c m' zq)
magic = undefined

getDict :: a -> Dict (Num a)
getDict _ = undefined
fromScalar :: r -> c m r
fromScalar = undefined

foo :: (Bar m m')
  => c m zp -> Foo m zp (c m' zq) -> Foo m zp (c m' zq)
foo b (Foo sc) =
  let scinv = fromScalar sc
  in case getDict scinv of
    Dict -> magic $ scinv * b
