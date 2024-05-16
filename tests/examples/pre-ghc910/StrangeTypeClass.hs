


instance
  (
  ) => Elms Z ix where
  data Elm Z ix = ElmZ !ix
  type Arg Z = Z
  getArg !(ElmZ _) = Z
  getIdx !(ElmZ ix) = ix
  {-# INLINE getArg #-}
  {-# INLINE getIdx #-}

foo :: (Eq a) => a-> Bool
foo = undefined

bar :: (   ) => a-> Bool
bar = undefined
