{-# LANGUAGE LambdaCase #-}

foo = f >>= \case
        Just h -> loadTestDB (h ++ "/.testdb")
        Nothing -> fmap S.Right initTestDB
