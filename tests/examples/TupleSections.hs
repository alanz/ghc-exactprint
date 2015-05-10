{-# LANGUAGE TupleSections #-}

foo = do
  liftIO $ atomicModifyIORef ciTokens ((,()) . f)
  liftIO $ atomicModifyIORef ciTokens (((),) . f)
  liftIO $ atomicModifyIORef ciTokens ((,) . f)

-- | Make bilateral dictionary from PoliMorf.
mkPoli :: [P.Entry] -> Poli
mkPoli = mkBila . map ((,,(),,()) <$> P.base <*> P.pos <*> P.form)

foo = baz
  where
    _1 = ((,Nothing,Nothing,Nothing,Nothing,Nothing) . Just <$>)
    _2 = ((Nothing,,Nothing,Nothing,Nothing,Nothing) . Just <$>)
    _3 = ((Nothing,Nothing,,Nothing,Nothing,Nothing) . Just <$>)
    _4 = ((Nothing,Nothing,Nothing,,Nothing,Nothing) . Just <$>)
    _5 = ((Nothing,Nothing,Nothing,Nothing,,Nothing) . Just <$>)
    _6 = ((Nothing,Nothing,Nothing,Nothing,Nothing,) . Just <$>)
