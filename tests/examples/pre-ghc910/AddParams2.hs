module AddParams2 where

collapse rightInner rightOuter = right
  where
    right           = (rightInner, rightOuter)
    righ2           = (rightInner, (rightOuter baz bar))

baz = undefined
bar = undefined
