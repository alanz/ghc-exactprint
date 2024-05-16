-- | Thompson's group F.
--
-- See eg. <https://en.wikipedia.org/wiki/Thompson_groups>
--
-- Based mainly on James Michael Belk's PhD thesis \"THOMPSON'S GROUP F\";
-- see <http://www.math.u-psud.fr/~breuilla/Belk.pdf>
--

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, BangPatterns, PatternSynonyms, DeriveFunctor #-}
module Math.Combinat.Groups.Thompson.F where

-- | Remove the carets with the given indices
-- (throws an error if there is no caret at the given index)
removeCarets :: [Int] -> T -> T
removeCarets idxs tree = if null rem then final else error ("removeCarets: some stuff remained: " ++ show rem) where

  (_,rem,final) =  go 0 idxs tree where

  go :: Int -> [Int] -> T -> (Int,[Int],T)
  go !x []         t  = (x + treeWidth t , [] , t)
