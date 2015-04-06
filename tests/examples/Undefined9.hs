{-# LANGUAGE QuasiQuotes, TypeFamilies #-}

module Image.PNG (isPNG, pngSize) where

import Data.Maybe
import File.Binary.PNG
import File.Binary
import File.Binary.Instances
import File.Binary.Instances.BigEndian

isPNG :: String -> Bool
isPNG img = isJust (fromBinary () img :: Maybe (PNGHeader, String))

pngSize :: String -> Maybe (Double, Double)
pngSize src = case getChunks src of
    Right cs -> Just
        (fromIntegral $ width $ ihdr cs, fromIntegral $ height $ ihdr cs)
    _ -> Nothing

[binary|

PNGHeader deriving Show

1: 0x89
3: "PNG"
2: "\r\n"
1: "\SUB"
1: "\n"

|]
