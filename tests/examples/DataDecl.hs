{-# Language DatatypeContexts #-}
{-# Language ExistentialQuantification #-}

data Foo = A
         | B
         | C

--         | data_or_newtype capi_ctype tycl_hdr constrs deriving
data {-# Ctype "Foo" "bar" #-} F1 = F1
data {-# Ctype       "baz" #-} Eq a =>  F2 a = F2 a

data (Eq a,Ord a) => F3 a = F3 Int a

data F4 a = forall x y. (Eq x,Eq y) => F4 a x y
          | forall x y. (Eq x,Eq y) => F4b a x y


