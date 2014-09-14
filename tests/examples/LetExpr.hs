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
import qualified Data.Map as {- blah -}  Foo.Map
import Control.Monad  (   )
import Data.Word (Word8)
import Data.Tree hiding  (  drawTree   )
import qualified Data.Text as T hiding    ( pack  , unpack  )

-- comment
foo = let x = 1
          y = 2
      in x + y

bar = 3
bbb x
 | x == 1 = ()
 | otherwise = ()


aaa [] _    = 0
aaa x  _unk = 1
aaa _  _    = 2

x `ccc` 1 = x + 1
x `ccc` y = x + y

x !@# y = x + y

data Baz = Baz1 | Baz2
