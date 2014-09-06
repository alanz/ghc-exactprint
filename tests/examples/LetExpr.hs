-- | A simple let expression, to ensure the layout is detected
-- With some haddock in the top
{- And a normal
   mulitline comment too -}
  module {- blah -}  Layout.LetExpr    where

-- comment
foo = let x = 1
          y = 2
      in x + y

