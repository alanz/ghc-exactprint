{-# LANGUAGE Arrows #-}
module ArrowDoSemi where

import Control.Arrow

f :: Arrow k => k Int Int
f = proc x -> do { ; y <- returnA -< x ; returnA -< y }

g :: ArrowLoop k => k Int Int
g = proc x -> do { ; y <- returnA -< x
                 ; rec { ; z <- returnA -< y }
                 ; returnA -< z }
