{-# LANGUAGE PatternGuards #-}

match n
      | Just 5 <- Just n
      , Just 5 <- Nothing
      , Just 5 <- Just 5
      = Just 5
