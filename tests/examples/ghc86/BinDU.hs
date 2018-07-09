{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TypeFamilies #-}

{-
λ: let bin = binDU (V.fromList [0,1,2])

λ: bin
# BinDU cuts
0.0     1.0     2.0

λ: addCut bin 3
# BinDU cuts
0.0     1.0     2.0     3.0

λ: deleteCut bin 0
# BinDU cuts
1.0     2.0

-}

module Data.Histogram.Bin.BinDU (
    -- * Specialized to Double, Unboxed Vectors
    BinDU(..)
  , binDU
  , cuts
  , unsafeBinDU
  , AdaptableBin(..)
  ) where

import Control.DeepSeq (NFData(..))
import Data.Data       (Data,Typeable)
import Data.Vector.Unboxed  (Vector,(!))
import qualified Data.Vector.Unboxed as VU
import Data.Maybe

import Data.Histogram.Bin.Classes

-- | Double bins of unequal sizes.
--   Bins are defined by a vector of cuts marking bounadries between bins (The entire range is continuous.  There are n+1 cuts for n bins
--   Cuts are assumed to be in ascending order
--   Specialized on Data.Vector.Unboxed
--   TODO: Generic Vector type.
--   Type paramter:
--
--   [@v@] type of vector used to define bin cuts

data BinDU = BinDU !(Vector Double) -- vector of cuts
            deriving (Data,Typeable,Eq)

-- | Create bins unsafely
unsafeBinDU :: Vector Double -- ^ cuts
     -> BinDU
unsafeBinDU = BinDU

binDU :: Vector Double -- ^ cuts
     -> BinDU
binDU c
    | VU.length c < 2 = error "Data.Histogram.Bin.BinDU.binDU': nonpositive number of bins"
    | VU.any (uncurry (>)) (VU.zip (VU.init c) (VU.drop 1 c)) = error "Data.Histogram.Bin.BinDU.binDU': cuts not in ascending order"
    | otherwise = BinDU c

cuts :: BinDU -> Vector Double
cuts (BinDU c) = c

instance Bin BinDU where
  type BinValue BinDU = Double
  toIndex   (BinDU c) !x = case VU.findIndex (>x) c of
      Nothing -> error "Data.Histogram.Bin.BinDU.toIndex: above range"
      Just i  -> case i of
          0 -> error "Data.Histogram.Bin.BinDU.toIndex: below range"
          _ -> i-1

  fromIndex (BinDU c) !i
      | i >= VU.length c - 1 =
            error "Data.Histogram.Bin.BinDU.fromIndex: above range"
      | otherwise = ((c ! i) + (c ! (i+1)))/2

  nBins (BinDU c) = if VU.length c < 2 then 0 else VU.length c - 1
  {-# INLINE toIndex #-}

instance IntervalBin BinDU where
  binInterval (BinDU c) i = (c ! i, c ! (i+1))

instance Bin1D BinDU where
  lowerLimit (BinDU c) = VU.head c
  upperLimit (BinDU c) = VU.last c

instance SliceableBin BinDU where
  unsafeSliceBin i j (BinDU c) = BinDU (VU.drop i $ VU.take (j-i) c)

instance VariableBin BinDU where
  binSizeN (BinDU c) !i = c ! (i+1) - c ! i

-- | Equality is up to 3e-11 (2/3th of digits)
instance BinEq BinDU where
  binEq (BinDU c) (BinDU c')
    =  isNothing (VU.find (\(d,d') -> d - d' > eps * abs d) $ VU.zip c c')
    where
      eps = 3e-11

instance Show BinDU where
  show (BinDU c) = "# BinDU cuts\n" ++ concat (fmap showCut $ VU.toList c) ++ "\n\n"
    where
      showCut x = show x ++ "\t"

instance NFData BinDU

-- | Binning algorithms which support adaption.
class Bin b => AdaptableBin b where
  -- | delete a bin
  deleteCut :: b -> Int -> b
  -- | add a new bin
  addCut :: b -> Double -> b

instance AdaptableBin BinDU where
    deleteCut (BinDU c) !i
        | VU.length c <= 2 =
            error "Data.Histogram.Bin.BinDU.deletBin: deleting single bin"
        | otherwise = BinDU (VU.take i c VU.++ VU.drop (i+1) c)

    addCut (BinDU c) !x = BinDU (VU.concat [VU.take i c, VU.singleton x, VU.drop i c])
      where
        i = fromMaybe (VU.length c) (VU.findIndex (> x) c)

