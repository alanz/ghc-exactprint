{-# LANGUAGE PatternSynonyms #-}

module Sigs where

-- TypeSig
f :: Num a => a -> a
f = undefined

pattern Single :: () => (Show a) => a -> [a]
pattern Single x = [x]

g :: (Show a) => [a] -> a
g (Single x) = x
