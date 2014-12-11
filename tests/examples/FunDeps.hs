{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}

-- FunDeps example
class Foo a b c | a b -> c where
  bar :: a -> b -> c

