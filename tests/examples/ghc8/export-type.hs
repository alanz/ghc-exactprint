{-# LANGUAGE PatternSynonyms #-}

module Export (A(..,MyB), B(MyA)) where

data A = A

data B = B

pattern MyB = B

pattern MyA = A
