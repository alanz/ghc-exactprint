module ConPat where

{-# RULES
"infix" forall a. let x1:x2:xs = flipFirst a in f x2 x1 = let x1:x2:xs = a in f x1 x2
  #-}
