{-# LANGUAGE RecursiveDo #-}

bar :: IO ()
bar = do
  rec {}
  return ()

