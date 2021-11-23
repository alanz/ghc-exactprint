{-# LANGUAGE TypeFamilies, TypeApplications, PolyKinds #-}
{-# LANGUAGE TemplateHaskell #-}
module TH_reifyDecl1 where

test :: ()
test = $(let
      display :: Name -> Q ()
      display q = do { i <- reify q; runIO $ hPutStrLn stderr (pprint i) }
    in do { display ''T
          ; display ''DF3
          ; [| () |] })

