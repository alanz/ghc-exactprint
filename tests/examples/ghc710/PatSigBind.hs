{-# LANGUAGE ScopedTypeVariables #-}

runCoreRunIO
  :: EHCOpts        -- ^ options, e.g. for turning on tracing (if supported by runner)
     -> Mod         -- ^ the module to run
     -> IO (Either Err RVal)
runCoreRunIO opts mod = do
    catch
      (runCoreRun opts [] mod $ cmodRun opts mod)
      (\(e :: SomeException) -> hFlush stdout >> (return $ Left $ strMsg $ "runCoreRunIO: " ++ show e))


foo = do
  (a :: Int) <- baz
  return grue
