module QuickCheckTests where

import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Utils


import Data.List

import Test.QuickCheck
import Test.QuickCheck.Gen

-- import Debug.Trace

-- ---------------------------------------------------------------------

_main :: IO ()
_main = properties

-- ---------------------------------------------------------------------

properties = do
  quickCheck prop_stringBetween
  quickCheck prop_sortKeyAfter
  quickCheck prop_sortKeyBefore
  quickCheck prop_sortKeyBetween
  -- verboseCheck prop_sortKeyBetween

prop_stringBetween s1 s2 =
  length s1 > 0 && length s2 > 0 && s1 < s2
    ==> isBetween (stringBeween s1 s2)
  where
    isBetween s = s1 < s && s < s2

prop_sortKeyBefore sk = sortKeyBefore sk < sk

prop_sortKeyAfter  sk = sk < sortKeyAfter sk

prop_sortKeyBetween sk1 sk2 =
  sk1 < sk2
    ==> isBetween (sortKeyBetween sk1 sk2)
  where
    isBetween sk = sk1 < sk && sk < sk2

instance Arbitrary SortKey where
  arbitrary = do
    r <- choose (9,10)
    c <- choose (1,2)
    -- s <- choose ('a', 'z')
    s <- listOf1 (choose ('a','c'))
    return (SortKey (r,c,s))
