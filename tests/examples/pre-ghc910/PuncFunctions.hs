-- | Compares two functions taking one container
(=*=) :: (Eq' a b) => (f -> a) -> (g -> b)
      -> SameAs f g r -> r -> Property
(f =*= g) sa i = f (toF sa i) =^= g (toG sa i)

-- | Compares two functions taking one scalar and one container
(=?*=) :: (Eq' a b) => (t -> f -> a) -> (t -> g -> b)
       -> SameAs f g r -> r -> t -> Property
(f =?*= g) sa i t = (f t =*= g t) sa i

-- | Compares functions taking two scalars and one container
(=??*=) :: (Eq' a b) => (t -> s -> f -> a) -> (t -> s -> g -> b)
        -> SameAs f g r -> r -> t -> s -> Property
(f =??*= g) sa i t s = (f t s =*= g t s) sa i

-- | Compares two functions taking two containers
(=**=) :: (Eq' a b) => (f -> f -> a) -> (g -> g -> b)
       -> SameAs f g r -> r -> r -> Property
(f =**= g) sa i = (f (toF sa i) =*= g (toG sa i)) sa

-- | Compares two functions taking one container with preprocessing
(=*==) :: (Eq' f g) => (z -> f) -> (z -> g) -> (p -> z)
       -> SameAs f g r -> p -> Property
(f =*== g) p _ i = f i' =^= g i'
  where i' = p i
