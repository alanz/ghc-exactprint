{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}

-- From https://ocharles.org.uk/blog/posts/2014-12-08-type-operators.html

import Data.String

data I a = I { unI :: a }
data Var a x = Var { unK :: a }

infixr 8 +
data ((f + g)) a = InL (f a) | InR (g a)
-- data (f + g) a = InL (f a) | InR (g a)

class sub :<: sup where
  inj :: sub a -> sup a

instance (sym :<: sym) where
  inj = id

instance (sym1 :<: (sym1 + sym2)) where inj = InL

instance (sym1 :<: sym3) => (sym1 :<: (sym2 + sym3)) where
  inj = InR . inj

instance (I :<: g, IsString s) => IsString ((f + g) s) where
  fromString = inj . I . fromString

var :: (Var a :<: f) => a -> f e
var = inj . Var

elim :: (I :<: f) => (a -> b) -> (Var a + f) b -> f b
elim eval f =
  case f of
    InL (Var xs) -> inj (I (eval xs))
    InR g        -> g

--------------------------------------------------------------------------------

data UserVar = UserName

data ChristmasVar = ChristmasPresent

email :: [(Var UserVar + Var ChristmasVar + I) String]
email = [ "Dear "
        , var UserName
        , ", thank you for your recent email to Santa & Santa Inc."
        , "You have asked for a: "
        , var ChristmasPresent
        ]

main :: IO ()
main =
  do name <- getLine
     present <- getLine
     putStrLn (concatMap (unI .
                          (elim (\ChristmasPresent -> present) .
                           elim (\UserName -> name)))
                         email)

{-

*Main> main
Ollie
Lambda Necklace
Dear Ollie, thank you for your recent email to Santa & Santa Inc.You have asked for a: Lambda Necklace

-}
