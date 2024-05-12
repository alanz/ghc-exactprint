{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- From https://ocharles.org.uk/blog/posts/2014-12-03-pattern-synonyms.html

import Foreign.C

{-

data BlendMode = NoBlending -- | AlphaBlending | AdditiveBlending | ColourModulatedBlending

toBlendMode :: BlendMode -> CInt
toBlendMode NoBlending = 0 -- #{const SDL_BLENDMODE_NONE}
-- toBlendMode AlphaBlending = #{const SDL_BLENDMODE_BLEND}

fromBlendMode :: CInt -> Maybe BlendMode
fromBlendMode 0 = Just NoBlending

-}

{-

pattern AlphaBlending = (1) :: CInt -- #{const SDL_BLENDMODE_BLEND} :: CInt

setUpBlendMode :: CInt -> IO ()
setUpBlendMode AlphaBlending = do
  putStrLn "Enabling Alpha Blending"
  activateAlphaBlendingForAllTextures
  activateRenderAlphaBlending

-}

newtype BlendMode = MkBlendMode { unBlendMode :: CInt }

pattern NoBlending = MkBlendMode 0 -- #{const SDL_BLENDMODE_NONE}
pattern AlphaBlending = MkBlendMode 1 -- #{const SDL_BLENDMODE_BLEND}

setUpBlendMode :: BlendMode -> IO ()
setUpBlendMode AlphaBlending = do
  putStrLn "Enabling Alpha Blending"
  activateAlphaBlendingForAllTextures
  activateRenderAlphaBlending

data Renderer

setRenderAlphaBlending :: Renderer -> IO ()
setRenderAlphaBlending r =
  sdlSetRenderDrawBlendMode r (unBlendMode AlphaBlending)

activateAlphaBlendingForAllTextures = return ()
activateRenderAlphaBlending = return ()

sdlSetRenderDrawBlendMode _ _ = return ()

-- And from https://www.fpcomplete.com/user/icelandj/Pattern%20synonyms

data Date = Date { month :: Int, day :: Int } deriving Show

-- Months
pattern January  day = Date { month = 1,  day = day }
pattern February day = Date { month = 2,  day = day }
pattern March    day = Date { month = 3,  day = day }
-- elided
pattern December day = Date { month = 12, day = day }

-- Holidays
pattern Christmas    = Date { month = 12, day = 25  }

describe :: Date -> String
describe (January 1)  = "First day of year"
describe (February n) = show n ++ "th of February"
describe Christmas    = "Presents!"
describe _            = "meh"

pattern Christmas2 = December 25

pattern BeforeChristmas <- December (compare 25 -> GT)
pattern Christmas3      <- December (compare 25 -> EQ)
pattern AfterChristmas  <- December (compare 25 -> LT)

react :: Date -> String
react BeforeChristmas = "Waiting :("
react Christmas       = "Presents!"
react AfterChristmas  = "Have to wait a whole year :("
react _               = "It's not even December..."

isItNow :: Int -> (Ordering, Int)
isItNow day = (compare 25 day, day)

pattern BeforeChristmas4 day <- December (isItNow -> (GT, day))
pattern Christmas4           <- December (isItNow -> (EQ, _))
pattern AfterChristmas4  day <- December (isItNow -> (LT, day))

days'tilChristmas :: Date -> Int
days'tilChristmas (BeforeChristmas4 n) = 25 - n
days'tilChristmas Christmas4           = 0
days'tilChristmas (AfterChristmas4 n)  = 365 + 25 - n
