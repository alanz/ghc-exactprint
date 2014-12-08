{-# LANGUAGE ParallelListComp,
             TransformListComp,
             RecordWildCards #-}
{-# LANGUAGE MonadComprehensions #-}

-- From https://ocharles.org.uk/blog/guest-posts/2014-12-07-list-comprehensions.html

import GHC.Exts
import qualified Data.Map as M
import Data.Ord (comparing)
import Data.List (sortBy)


-- Monad Comprehensions

sqrts :: M.Map Int Int
sqrts = M.fromList $ [ (x, sx)
                     | x  <- map (^2) [1..100]
                     | sx <- [1..100]
                     ]

sumIntSqrts :: Int -> Int -> Maybe Int
sumIntSqrts a b = [ x + y
                  | x <- M.lookup a sqrts
                  , y <- M.lookup b sqrts
                  ]

greet :: IO String
greet = [ name
        | name <- getLine
        , _ <- putStrLn $ unwords ["Hello, ", name, "!"]
        ]

