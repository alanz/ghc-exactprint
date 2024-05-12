{-# LANGUAGE TemplateHaskell, FlexibleInstances,
          MultiParamTypeClasses, TypeSynonymInstances #-}

module Splice where

import Language.Haskell.TH.Syntax
import Language.Haskell.TH

foo $( return $ VarP $ mkName "x" ) = x
bar $( [p| x |] ) = x

baz = [| \ $( return $ VarP $ mkName "x" ) -> $(dyn "x") |]


class Eq a => MyClass a
data Foo = Foo deriving Eq

instance MyClass Foo

data Bar = Bar
  deriving Eq

type Baz = Bar
instance MyClass Baz

data Quux a  = Quux a   deriving Eq
data Quux2 a = Quux2 a  deriving Eq
instance Eq a  => MyClass (Quux a)
instance Ord a => MyClass (Quux2 a)

class MyClass2 a b
instance MyClass2 Int Bool

makeLenses '' PostscriptFont

$(return [])

main = do
    putStrLn $(do { info <- reify ''MyClass; lift (pprint info) })
    print $(isInstance ''Eq [ConT ''Foo] >>= lift)
    print $(isInstance ''MyClass [ConT ''Foo] >>= lift)
    print $ not $(isInstance ''Show [ConT ''Foo] >>= lift)
    print $(isInstance ''MyClass [ConT ''Bar] >>= lift) -- this one
    print $(isInstance ''MyClass [ConT ''Baz] >>= lift)
    print $(isInstance ''MyClass [AppT (ConT ''Quux) (ConT ''Int)] >>= lift) --this one
    print $(isInstance ''MyClass [AppT (ConT ''Quux2) (ConT ''Int)] >>= lift) -- this one
    print $(isInstance ''MyClass2 [ConT ''Int, ConT ''Bool] >>= lift)
    print $(isInstance ''MyClass2 [ConT ''Bool, ConT ''Bool] >>= lift)

