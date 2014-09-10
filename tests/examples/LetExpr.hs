-- | A simple let expression, to ensure the layout is detected
-- With some haddock in the top
{- And a normal
   multiline comment too -}
  module {- blah -}  Layout.LetExpr ( foo -- foo does ..
                                    , bar -- bar does ..
                                    , Baz -- baz does ..
                                    )
    where

import Data.List
-- A comment in the middle
import qualified Data.Map as Map
import Control.Monad ()

-- comment
foo = let x = 1
          y = 2
      in x + y

bar = 3

data Baz = Baz1 | Baz2
