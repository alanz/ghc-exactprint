{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

type family (++) (a :: [k]) (b :: [k]) :: [k] where
    '[]       ++ b = b
    (a ': as) ++ b = a ': (as ++ b)

type family F a :: * -> * -> *
type instance F Int = (->)
type instance F Char = ( ,  )
