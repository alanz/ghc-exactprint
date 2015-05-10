{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitNamespaces #-}

module Network.Routing.Dict
    ( -- * store
      Store
    , emptyStore
    , type ( Members ["foo" := Int, "bar" := Double] prms == (Member "foo" Int prms, Member "bar" Double prms)
--
type family   Members (kvs :: [KV *]) (prms :: [KV *]) :: Constraint
type instance Members '[] prms = ()
type instance Members (k := v ': kvs) prms = (Member k v prms, Members kvs prms)
