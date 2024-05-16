{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators #-}
module ConstructorArgs where

class
    a  -- c1
    :+ -- c2
    b  -- c3

data Foo
  =  Int     -- c4
       :*    -- c5
     String  -- c6
