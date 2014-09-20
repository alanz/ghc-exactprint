module Layout.PatBind where

a,b :: Int
a = 1
b = 2

c :: Maybe (a -> b)
c = Nothing

 -- Pattern bind
tup :: (Int, Int)
h :: Int
t :: Int
tup@(h,t) = head $ zip [1..10] [3..ff]
  where
    ff :: Int
    ff = 15


