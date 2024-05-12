{-# LANGUAGE ViewPatterns #-}

-- From https://ghc.haskell.org/trac/ghc/wiki/ViewPatterns
import Prelude hiding (length)

data JList a = Empty
             | Single a
             | Join (JList a) (JList a)

data JListView a = Nil
                 | Cons a (JList a)


view :: JList a -> JListView a
view Empty = Nil
view (Single a) = Cons a Empty
view (Join (view -> Cons xh xt) y) = Cons xh $ Join xt y
view (Join (view -> Nil) y) = view y

length :: JList a -> Integer
length (view -> Nil) = 0
length (view -> Cons x xs) = 1 + length xs

