{-# LANGUAGE TupleSections #-}

foo = do
  liftIO $ atomicModifyIORef ciTokens ((,()) . f)
  liftIO $ atomicModifyIORef ciTokens (((),) . f)
  liftIO $ atomicModifyIORef ciTokens ((,) . f)

