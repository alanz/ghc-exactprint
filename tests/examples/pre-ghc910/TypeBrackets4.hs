{-# LANGUAGE DataKinds, PolyKinds, TypeOperators, TypeFamilies #-}


type family ((a :: Bool) || (b :: Bool)) :: Bool
type instance 'True  || a = 'True
type instance a || 'True  = 'True
type instance 'False || a = a
type instance a || 'False = a
