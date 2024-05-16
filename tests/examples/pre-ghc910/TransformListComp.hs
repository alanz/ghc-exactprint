{-# LANGUAGE TransformListComp #-}

oldest :: [Int] -> [String]
oldest tbl = [ "str"
             | n <- tbl
             , then id
             ]
