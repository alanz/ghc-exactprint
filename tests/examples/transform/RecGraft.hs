{-# LANGUAGE RecursiveDo #-}
module RecGraft where

g :: Int -> Int -> IO Int
g a b = do
  rec x <- return (graft)
      y <- return x
  return y
