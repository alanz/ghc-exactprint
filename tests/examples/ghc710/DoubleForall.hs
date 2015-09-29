{-# LANGUAGE ScopedTypeVariables #-}


extremumNewton :: (Eq a, Fractional a) =>
                  (forall tag. forall tag1.
                          Tower tag1 (Tower tag a)
                              -> Tower tag1 (Tower tag a))
                      -> a -> [a]
extremumNewton f x0 = zeroNewton (diffUU f) x0
