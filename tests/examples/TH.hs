{-# Language TemplateHaskell #-}

-- from https://ocharles.org.uk/blog/guest-posts/2014-12-22-template-haskell.html

import Language.Haskell.TH

e1 :: IO Exp
e1 = runQ [| 1 + 2 |]

e2 :: Integer
e2 = $( return (InfixE (Just (LitE (IntegerL 1)))
                       (VarE (mkName "+"))
                       (Just (LitE (IntegerL 2)))
               )
      )

fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibQ :: Int -> Q Exp
fibQ n = [| fibs !! n |]

-- e3 gives stage restriction, need to import this module to get it
-- e3 = $(fibQ 22)

e4 = $(runQ [| fibs !! $( [| 8 |]) |])

e5 :: IO Exp
e5 = runQ [| 1 + 2 |]

e6 :: IO [Dec]
e6 = runQ [d|x = 5|]

e7 :: IO Type
e7 = runQ [t|Int|]

e8 :: IO Pat
e8 = runQ [p|(x,y)|]

myExp :: Q Exp; myExp = runQ [| 1 + 2 |]

e9 = runQ(myExp) >>= putStrLn.pprint

-- ---------------------------------------------------------------------

isPrime :: (Integral a) => a -> Bool
isPrime k | k <=1 = False | otherwise = not $ elem 0 (map (mod k)[2..k-1])

nextPrime :: (Integral a) => a -> a
nextPrime n | isPrime n = n | otherwise = nextPrime (n+1)

-- returns a list of all primes between n and m, using the nextPrime function
doPrime :: (Integral a) => a -> a -> [a]
doPrime n m
        | curr > m = []
        | otherwise = curr:doPrime (curr+1) m
        where curr = nextPrime n

-- and our Q expression
primeQ :: Int -> Int -> Q Exp
primeQ n m = [| doPrime n m |]

-- stage restriction on e10
-- e10 = $(primeQ 0 67)

-- ---------------------------------------------------------------------

e11 = $(stringE . show =<< reify ''Bool)

-- stage restriction e12
-- e12 = $(stringE . show =<< reify 'primeQ)
