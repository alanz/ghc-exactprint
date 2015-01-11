module EmptyMostlyNoSemis where

x=let{y=2}in y
class Foo a where {
  (--<>--) :: a -> a -> Int;
  infixl 5 --<>--;
  (--<>--) _ _ = 2 ; -- empty decl at the end.
 }
