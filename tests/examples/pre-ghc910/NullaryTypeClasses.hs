{-# LANGUAGE NullaryTypeClasses #-}

-- From https://ocharles.org.uk/blog/posts/2014-12-10-nullary-type-classes.html

class Logger where
  logMessage :: String -> IO ()

type Present = String
queueNewChristmasPresents :: Logger => [Present] -> IO ()
queueNewChristmasPresents presents = do
  mapM (logMessage . ("Queueing present for delivery: " ++)) presents
  return ()

instance Logger where
  logMessage t = putStrLn ("[XMAS LOG]: " ++ t)

