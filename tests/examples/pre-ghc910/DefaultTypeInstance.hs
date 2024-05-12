{-# LANGUAGE TypeFamilies #-}


class Foldable t where
  type FoldableConstraint t x :: Constraint
  type FoldableConstraint t x = ()
