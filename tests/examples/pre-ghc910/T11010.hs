{-# LANGUAGE PatternSynonyms, ExistentialQuantification, GADTSyntax #-}
{-# LANGUAGE TypeFamilies #-}

module T11010 where

data Expr a where
  Fun :: String -> (a -> b) -> (Expr a -> Expr b)

ggFun :: String -> (a -> b) -> (Expr a -> Expr b)
ggFun = undefined

pattern IntFun :: (a ~ Int) => String -> (a -> b) -> (Expr a -> Expr b)
pattern IntFun str f x = Fun str f x

-- Alternative syntax for pattern synonyms:
--   pattern
--     Suc :: () => (a ~ Int) => Expr a -> Expr Int
--     Suc n <- IntFun _     _     n where
--     Suc n =  IntFun "suc" (+ 1) n
pattern Suc :: (a ~ Int) => Expr a -> Expr Int
pattern Suc n <- IntFun _     _     n where
         Suc n =  IntFun "suc" (+ 1) n
