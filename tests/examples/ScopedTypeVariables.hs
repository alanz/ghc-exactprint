{-# LANGUAGE ScopedTypeVariables #-}
-- from https://ocharles.org.uk/blog/guest-posts/2014-12-20-scoped-type-variables.html

import qualified Data.Map as Map

insertMany ::  forall k v . Ord k => (v -> v -> v) -> [(k,v)] -> Map.Map k v -> Map.Map k v
insertMany f vs m = foldr f1 m vs
  where
    f1 :: (k, v) -> Map.Map k v -> Map.Map k v
    f1 (k,v) m = Map.insertWith f k v m
