module QuickCheckTests where

import Language.Haskell.GHC.ExactPrint.Internal.Types
import Language.Haskell.GHC.ExactPrint.Utils


import Data.List
import Data.Ratio

import Test.QuickCheck
import Test.QuickCheck.Gen

-- import Debug.Trace

-- ---------------------------------------------------------------------

_main :: IO ()
_main = properties

-- ---------------------------------------------------------------------

properties = do
  quickCheck prop_sortKeyAfter
  quickCheck prop_sortKeyBefore
  quickCheck prop_sortKeyBetween
  -- verboseCheck prop_sortKeyBetween

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
    -- c <- choose (1,2)
    n <- choose (3,5)
    d <- choose (6,9)
    return (SortKey (r,1,n%d))
