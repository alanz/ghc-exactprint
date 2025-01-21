module Operator where

main :: IO ()
main = do
  print $ 3 + 4
  -- lets make sure
  print $ foo ||
    -- that comments end up in the right place
    bar ||
    baz &&
    -- even this one
    quux

  -- lets also make sure
  print $ foo
    -- that these comments end up in the right place
    || bar
    || baz
    -- even when the operator is leading
    && quux

  return ({- comment -} x >= 1 || y >= 2)

  print $ foo || bar
  print $ foo || bar
  print $ foo || bar
  print $  {- comment here -} foo || bar
  print $ foo {- comment here -} || bar
  print $ foo || {- comment here -} bar
  print $ foo || bar {- comment here -}

  print $ foo || bar
  print $ foo || bar

{-# RULES "print" forall x. putStrLn $ show $ x = print $ x #-}

f :: Int -> Bool
f x = x == 2

g :: Int -> Bool
g y = (y == 2) /= False

roundtrip :: IO [a]
roundtrip = return $ mconcat
  [ timeToText $ time_enrolled - mod time_enrolled t
  , ":"
  ]

-- Ensure local fixity declarations are handled properly
(.@@@) :: (a -> b) -> a -> b
f .@@@ x = f x

infixr 0 .@@@
{-# RULES "printAt" forall x. putStrLn .@@@ show .@@@ x = print $ x #-}
