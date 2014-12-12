{-# LANGUAGE ImplicitParams #-}

-- From https://ocharles.org.uk/blog/posts/2014-12-11-implicit-params.html

import Data.Char


type LogFunction = String -> IO ()

type Present = String

queueNewChristmasPresents :: LogFunction -> [Present] -> IO ()
queueNewChristmasPresents logMessage presents = do
  mapM (logMessage . ("Queueing present for delivery: " ++)) presents
  return ()

queueNewChristmasPresents2 :: (?logMessage :: LogFunction) => [Present] -> IO ()
queueNewChristmasPresents2 presents = do
  mapM (?logMessage . ("Queueing present for delivery: " ++)) presents
  return ()


ex1 :: IO ()
ex1 =
  let ?logMessage = \t -> putStrLn ("[XMAS LOG]: " ++ t)
  in queueNewChristmasPresents2 ["Cuddly Lambda", "Gamma Christmas Pudding"]


ex2 :: IO ()
ex2 = do
  -- Specifies its own logger
  ex1

  -- We can locally define a new logging function
  let ?logMessage = \t -> putStrLn (zipWith (\i c -> if even i
                                                     then c
                                                     else toUpper c)
                                           [0..]
                                           t)
  queueNewChristmasPresents2 ["Category Theory Books"]

