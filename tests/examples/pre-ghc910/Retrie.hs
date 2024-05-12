{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedSums #-}
module Types4a where

import Data.Maybe hiding (f1,f2,n1,n2)

type Foo5 = forall r (a :: Type) (b :: TYPE r). (a -> b) -> a -> b

foo5 :: forall s (c :: Type) (d :: TYPE s). (c -> d) -> c -> d
foo5 = ($)
