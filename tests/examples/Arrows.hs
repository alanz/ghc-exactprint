{-# LANGUAGE Arrows #-}
-- from https://ocharles.org.uk/blog/guest-posts/2014-12-21-arrows.html

import Control.Monad (guard)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Arrow (returnA, Kleisli(Kleisli), runKleisli)

f :: Int -> (Int, Int)
f = \x ->
  let y  = 2 * x
      z1 = y + 3
      z2 = y - 5
  in (z1, z2)

-- ghci> f 10
-- (23, 15)

fM :: Int -> Identity (Int, Int)
fM = \x -> do
  y  <- return (2 * x)
  z1 <- return (y + 3)
  z2 <- return (y - 5)
  return (z1, z2)

-- ghci> runIdentity (fM 10)
-- (23,15)


fA :: Int -> (Int, Int)
fA = proc x -> do
  y  <- (2 *) -< x
  z1 <- (+ 3) -< y
  z2 <- (subtract 5) -< y
  returnA -< (z1, z2)

-- ghci> fA 10
-- (23,15)

range :: Int -> [Int]
range r = [-r..r]

cM :: Int -> [(Int, Int)]
cM = \r -> do
  x <- range 5
  y <- range 5
  guard (x*x + y*y <= r*r)
  return (x, y)

-- ghci> take 10 (cM 5)
-- [(-5,0),(-4,-3),(-4,-2),(-4,-1),(-4,0),(-4,1),(-4,2),(-4,3),(-3,-4),(-3,-3)]


type K = Kleisli

k :: (a -> m b) -> Kleisli m a b
k = Kleisli

runK :: Kleisli m a b -> (a -> m b)
runK = runKleisli

cA :: Kleisli [] Int (Int, Int)
cA = proc r -> do
  x <- k range -< 5
  y <- k range -< 5
  k guard -< (x*x + y*y <= r*r)
  returnA -< (x, y)

-- ghci> take 10 (runK cA 5)
-- [(-5,0),(-4,-3),(-4,-2),(-4,-1),(-4,0),(-4,1),(-4,2),(-4,3),(-3,-4),(-3,-3)]

getLineM :: String -> IO String
getLineM prompt = do
  print prompt
  getLine

printM :: String -> IO ()
printM = print

writeFileM :: (FilePath, String) -> IO ()
writeFileM  (filePath, string) = writeFile filePath string

procedureM :: String -> IO ()
procedureM = \prompt -> do
  input <- getLineM prompt
  if input == "Hello"
    then printM "You said 'Hello'"
    else writeFileM ("/tmp/output", "The user said '" ++ input ++ "'")
