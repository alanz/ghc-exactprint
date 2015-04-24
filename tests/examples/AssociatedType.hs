{-# LANGUAGE TypeFamilies #-}
class Foldable t where
  type FoldableConstraint t x :: *
  type FoldableConstraint t x = ()

