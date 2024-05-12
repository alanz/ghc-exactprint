module Issue91 where

-- Based on https://github.com/alanz/ghc-exactprint/issues/91

foo = case hi of
        _ -> hi
        _ -> hi
        _ -> hi
