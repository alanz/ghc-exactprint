{-# LANGUAGE ScopedTypeVariables #-}

errors= do
  let ls :: [[String ]]= runR  readp $ pack "[" `append` (B.tail log) `append` pack "]"
  return ()

-- This can be seen as the definition of accumFilter
accumFilter2 :: (c -> a -> (c, Maybe b)) -> c -> SF (Event a) (Event b)
accumFilter2 f c_init =
    switch (never &&& attach c_init) afAux
    where
    afAux (c, a) =
            case f c a of
            (c', Nothing) -> switch (never &&& (notYet>>>attach c')) afAux
            (c', Just b)  -> switch (now b &&& (notYet>>>attach c')) afAux

    attach :: b -> SF (Event a) (Event (b, a))
        attach c = arr (fmap (\a -> (c, a)))
