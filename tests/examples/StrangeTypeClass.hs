


instance
  (
  ) => Elms Z ix where
  data Elm Z ix = ElmZ !ix
  type Arg Z = Z
  getArg !(ElmZ _) = Z
  getIdx !(ElmZ ix) = ix
  {-# INLINE getArg #-}
  {-# INLINE getIdx #-}
