{-# LANGUAGE PatternGuards #-}

match n
      | Just 5 <- Just n
      , Just 6 <- Nothing
      , Just 7 <- Just 9
      = Just 8
