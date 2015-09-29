module CloneDecl1 where

z = 3

foo a b =
  let
    x = a + b + z
    y = a * b - z
  in
    x + y
