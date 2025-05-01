{-# LANGUAGE CPP                   #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}

module Data.Array.Accelerate.Internal.Orphans.Base ()
  where

$(runQ $ do
    let
        thFromIntegral1 big =
          [d|
              instance FromIntegral Int $(bigWordT big) where
                fromIntegral x =
#if   WORD_SIZE_IN_BITS == 32
                    fromIntegral (fromIntegral x :: Exp Int32)
#elif WORD_SIZE_IN_BITS == 64
                    fromIntegral (fromIntegral x :: Exp Int64)
#endif
            |]

 )

