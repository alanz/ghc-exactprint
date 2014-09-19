-- A simple let statement, to ensure the layout is detected

module Layout.LetStmt where

foo = do
{- ffo -}let x = 1
             y = 2 -- baz
         x+y

