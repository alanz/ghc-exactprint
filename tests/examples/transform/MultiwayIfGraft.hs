{-# LANGUAGE MultiWayIf #-}
module MultiwayIfGraft where

checkNumber :: Int -> String
checkNumber x =
    if | x > 0     -> "Positive"
       | graft < 0  -> "Negative"
       | otherwise -> "Zero"
