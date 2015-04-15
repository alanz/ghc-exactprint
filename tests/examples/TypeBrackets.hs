{-# LANGUAGE ScopedTypeVariables #-}

foo (f :: (Maybe t -> Int)) =
     undefined

type (f `ObjectsFUnder` a) = ConstF f a :/\: f
type (f `ObjectsFOver`  a) = f :/\: ConstF f a

type (c `ObjectsUnder` a) = Id c `ObjectsFUnder` a
type (c `ObjectsOver`  a) = Id c `ObjectsFOver`  a

