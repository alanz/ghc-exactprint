{-# LANGUAGE
  TemplateHaskell,
  MultiParamTypeClasses,
  FunctionalDependencies,
  UndecidableInstances
  #-}


genCodingInstance :: (Data c, Data h) => TypeQ -> Name -> [(c, h)] -> Q [Dec]
genCodingInstance ht ctn chs = do
  let n = const Nothing
  [d|
    instance Monad m => EncodeM m $(ht) $(conT ctn) where
      encodeM h = return $ $(
        caseE [| h |] [ match (dataToPatQ n h) (normalB (dataToExpQ n c)) [] | (c,h) <- chs ]
       )

    instance Monad m => DecodeM m $(ht) $(conT ctn) where
      decodeM c = return $ $(
        caseE [| c |] [ match (dataToPatQ n c) (normalB (dataToExpQ n h)) [] | (c,h) <- chs ]
       )
   |]
