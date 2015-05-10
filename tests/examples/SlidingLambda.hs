{-# LANGUAGE ImplicitParams #-}

foo = choice flips $ map (\p -> \b -> let ?pat = p in match s{ flips = b }) ps
