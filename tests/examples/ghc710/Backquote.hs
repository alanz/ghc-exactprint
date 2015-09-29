import Data.List

foo = do
  let genOut (f,st) = putStrLn (f ++ "\t"++go [e`div`4,e`div`2,3*e`div`4] (scanl1 (+) $ sort st))
  Just 5

f = undefined
go = undefined
e = undefined


