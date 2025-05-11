-- From ./hackage-roundtrip-work/leb128-binary-0.1.3/lib/Data/Binary/SLEB128.hs
{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -ddump-simpl -ddump-to-file #-}

#include <MachDeps.h>

-- | __Signed LEB128 codec__. This codec encodes the two's complement
-- of a signed number
-- [as described here](https://en.wikipedia.org/wiki/LEB128#Signed_LEB128).
--
-- Any /getXXX/ decoder can decode bytes generated using any of the /putXXX/
-- encoders, provided the encoded number fits in the target type.
--
-- __WARNING__: This is not compatible with the /Unsigned LEB128/ codec at
-- "Data.Binary.ULEB128" nor with the /ZigZag LEB128/ codec at
-- "Data.Binary.ZLEB128".
module Data.Binary.SLEB128
 ( SLEB128(..)
   -- * Put
 , putInteger
 , putInt64
 , putInt32
 , putInt16
 , putInt8
 , putInt
 , putNatural
 , putWord64
 , putWord32
 , putWord16
 , putWord8
 , putWord
   -- * Get
 , getInteger
 , getInt64
 , getInt32
 , getInt16
 , getInt8
 , getInt
 , getNatural
 , getWord64
 , getWord32
 , getWord16
 , getWord8
 , getWord
   -- * Extras
   -- ** Scientific
 , putScientific
 , getScientific
   -- ** Rational
 , putRational
 , getRational
   -- ** Fixed
 , putFixed
 , getFixed
 ) where

import Data.Binary qualified as Bin
import Data.Binary.Get qualified as Bin
import Data.Binary.Put qualified as Bin
import Data.ByteString.Builder.Prim qualified as BB
import Data.ByteString.Builder.Prim.Internal qualified as BB
import Data.Scientific qualified as S
import Data.Bits
import Data.Coerce
import Data.Fixed
import Data.Proxy
import GHC.Real
import GHC.Exts
import GHC.Int
import GHC.Word
import GHC.Num.BigNat
import GHC.Num.Natural
import GHC.Num.Integer
import Foreign.Ptr
import Foreign.Storable
import Math.NumberTheory.Logarithms (integerLog10)

import Data.Binary.ULEB128 qualified as U

--------------------------------------------------------------------------------

-- | Newtype wrapper for 'Bin.Binary' encoding and decoding @x@ using the
-- /Signed LEB128/ codec. Useful in conjunction with @DerivingVia@.
newtype SLEB128 x = SLEB128 x

-- | Note: Maximum allowed number of input bytes is restricted to 1000.
-- Use 'putNatural' if you want a greater limit.
instance Bin.Binary (SLEB128 Integer) where
  put = coerce putInteger
  {-# INLINE put #-}
  get = coerce (getInteger 1000)
  {-# INLINE get #-}

-- | Note: Maximum allowed number of input bytes is restricted to 1000.
-- Use 'putNatural' if you want a greater limit.
instance Bin.Binary (SLEB128 Natural) where
  put = coerce putNatural
  {-# INLINE put #-}
  get = coerce (getNatural 1000)
  {-# INLINE get #-}

instance Bin.Binary (SLEB128 Int) where
  put = coerce putInt
  {-# INLINE put #-}
  get = coerce getInt
  {-# INLINE get #-}

instance Bin.Binary (SLEB128 Word) where
  put = coerce putWord
  {-# INLINE put #-}
  get = coerce getWord
  {-# INLINE get #-}

instance Bin.Binary (SLEB128 Int8) where
  put = coerce putInt8
  {-# INLINE put #-}
  get = coerce getInt8
  {-# INLINE get #-}

instance Bin.Binary (SLEB128 Word8) where
  put = coerce putWord8
  {-# INLINE put #-}
  get = coerce getWord8
  {-# INLINE get #-}

instance Bin.Binary (SLEB128 Int16) where
  put = coerce putInt16
  {-# INLINE put #-}
  get = coerce getInt16
  {-# INLINE get #-}

instance Bin.Binary (SLEB128 Word16) where
  put = coerce putWord16
  {-# INLINE put #-}
  get = coerce getWord16
  {-# INLINE get #-}

instance Bin.Binary (SLEB128 Int32) where
  put = coerce putInt32
  {-# INLINE put #-}
  get = coerce getInt32
  {-# INLINE get #-}

instance Bin.Binary (SLEB128 Word32) where
  put = coerce putWord32
  {-# INLINE put #-}
  get = coerce getWord32
  {-# INLINE get #-}

instance Bin.Binary (SLEB128 Int64) where
  put = coerce putInt64
  {-# INLINE put #-}
  get = coerce getInt64
  {-# INLINE get #-}

instance Bin.Binary (SLEB128 Word64) where
  put = coerce putWord64
  {-# INLINE put #-}
  get = coerce getWord64
  {-# INLINE get #-}

--------------------------------------------------------------------------------

{-# INLINE putInteger #-}
putInteger :: Integer -> Bin.Put
putInteger = \case
    IS x -> putInt (I# x)
    IP x -> putIP x $ fromIntegral (bigNatSizeInBase 2 x)
    IN x -> putIN x
  where
    {-# INLINE putIP #-}
    putIP :: BigNat# -> Int -> Bin.Put
    putIP !a !m = do
      Bin.putWord8 (W8# (wordToWord8# (or# (bigNatIndex# a 0#) 0x80##)))
      let b = bigNatShiftR# a 7## :: BigNat#
          n = m - 7
      if n > WORD_SIZE_IN_BITS - 1
         then putIP b n
         else putInt (I# (word2Int# (bigNatIndex# b 0#)))
    -- TODO: Faster 'putIN' implementation, similar to 'putIP'
    {-# INLINE putIN #-}
    putIN :: BigNat# -> Bin.Put
    putIN !a = do
      let b = unsafeShiftR (IN a) 7        :: Integer
          c = fromIntegral (IN a .&. 0x7f) :: Word8
          d = c .&. 0x40
      if d /= 0 && b == -1
         then Bin.putWord8 c
         else do Bin.putWord8 $! c .|. 0x80
                 putInteger b

putNatural :: Natural -> Bin.Put
putNatural = putInteger . fromIntegral
{-# INLINE putNatural #-}

putInt8 :: Int8 -> Bin.Put
putInt8 = Bin.putBuilder . BB.primBounded (BB.boundedPrim 2 unsafePoke)
{-# INLINE putInt8 #-}

putInt16 :: Int16 -> Bin.Put
putInt16 = Bin.putBuilder . BB.primBounded (BB.boundedPrim 3 unsafePoke)
{-# INLINE putInt16 #-}

putInt32 :: Int32 -> Bin.Put
putInt32 = Bin.putBuilder . BB.primBounded (BB.boundedPrim 5 unsafePoke)
{-# INLINE putInt32 #-}

putInt64 :: Int64 -> Bin.Put
putInt64 = Bin.putBuilder . BB.primBounded (BB.boundedPrim 10 unsafePoke)
{-# INLINE putInt64 #-}

putInt :: Int -> Bin.Put
putInt =
#if WORD_SIZE_IN_BITS == 64
  Bin.putBuilder . BB.primBounded (BB.boundedPrim 10 unsafePoke)
#elif WORD_SIZE_IN_BITS == 32
  Bin.putBuilder . BB.primBounded (BB.boundedPrim 5 unsafePoke)
#endif
{-# INLINE putInt #-}

putWord8 :: Word8 -> Bin.Put
putWord8 = Bin.putBuilder . BB.primBounded (BB.boundedPrim 2 unsafePoke)
{-# INLINE putWord8 #-}

putWord16 :: Word16 -> Bin.Put
putWord16 = Bin.putBuilder . BB.primBounded (BB.boundedPrim 3 unsafePoke)
{-# INLINE putWord16 #-}

putWord32 :: Word32 -> Bin.Put
putWord32 = Bin.putBuilder . BB.primBounded (BB.boundedPrim 5 unsafePoke)
{-# INLINE putWord32 #-}

putWord64 :: Word64 -> Bin.Put
putWord64 = Bin.putBuilder . BB.primBounded (BB.boundedPrim 10 unsafePoke)
{-# INLINE putWord64 #-}

putWord :: Word -> Bin.Put
putWord =
#if WORD_SIZE_IN_BITS == 64
  Bin.putBuilder . BB.primBounded (BB.boundedPrim 10 unsafePoke)
#elif WORD_SIZE_IN_BITS == 32
  Bin.putBuilder . BB.primBounded (BB.boundedPrim 5 unsafePoke)
#endif
{-# INLINE putWord #-}

--------------------------------------------------------------------------------

getInteger
  :: Int
  -- ^ /Maximum/ number of bytes to consume. If the 'Integer' number can be
  -- determined before consuming this number of bytes, it will be. If @0@,
  -- parsing fails.
  --
  -- Each SLEB128 byte encodes at most 7 bits of data. That is,
  -- \(length(encoded) == \lceil\frac{length(data)}{7}\rceil\).
  -> Bin.Get Integer
getInteger = unsafeGetSigned toInteger
{-# INLINE getInteger #-}

-- | Like 'getInteger', except it's offered here so that other parsers can use
-- this specilized to types other than 'Integer'. This is unsafe because it
-- only works for signed numbers whose SLEB128 representation is at most as
-- long as the specified 'Int', but none of that is checked by this parser.
{-# INLINE unsafeGetSigned #-}
unsafeGetSigned
  :: forall a. (Bits a, Num a) => (Word8 -> a) -> Int -> Bin.Get a
unsafeGetSigned fromWord8 = \m -> Bin.label "SLEB128" (go m 0 0)
  where
    {-# INLINE go #-}
    go :: Int -> Int -> a -> Bin.Get a
    go m i o | i < m = do
      w <- Bin.getWord8
      let !a = o .|. unsafeShiftL (fromWord8 (w .&. 0x7f)) (i * 7)
      if w >= 0x80 then go m (i + 1) a
      else pure $! a - bit ((i + 1) * 7)
                     * fromWord8 (unsafeShiftR (w .&. 0x40) 6)
    go _ _ _ = fail "input exceeds maximum allowed bytes"

getNatural
  :: Int
  -- ^ /Maximum/ number of bytes to consume. If the 'Natural' number can be
  -- determined before consuming this number of bytes, it will be. If @0@,
  -- parsing fails.
  --
  -- Each SLEB128 byte encodes at most 7 bits of data. That is,
  -- \(length(encoded) == \lceil\frac{length(data)}{7}\rceil\).
  -> Bin.Get Natural
getNatural = \m -> do
  i <- getInteger m
  Bin.label "SLEB128" (naturalFromInteger i)
{-# INLINE getNatural #-}

getBoundedIntegral
  :: forall a b
  .  (Bits a, Integral a, Bits b, Integral b)
  => Bin.Get a
  -> Bin.Get b
getBoundedIntegral = \ga -> do
  a <- ga
  maybe (fail "underflow or overflow") pure (toIntegralSized a)
{-# INLINE getBoundedIntegral #-}

getInt8 :: Bin.Get Int8
getInt8 = unsafeGetSigned fromIntegral 2
{-# INLINE getInt8 #-}

getInt16 :: Bin.Get Int16
getInt16 = unsafeGetSigned fromIntegral 3
{-# INLINE getInt16 #-}

getInt32 :: Bin.Get Int32
getInt32 = unsafeGetSigned fromIntegral 5
{-# INLINE getInt32 #-}

getInt64 :: Bin.Get Int64
getInt64 = unsafeGetSigned fromIntegral 10
{-# INLINE getInt64 #-}

getInt :: Bin.Get Int
getInt =
#if WORD_SIZE_IN_BITS == 64
  unsafeGetSigned fromIntegral 10
#elif WORD_SIZE_IN_BITS == 32
  unsafeGetSigned fromIntegral 5
#endif
{-# INLINE getInt #-}

getWord8 :: Bin.Get Word8
getWord8 = getBoundedIntegral (unsafeGetSigned @Int16 fromIntegral 2)
{-# INLINE getWord8 #-}

getWord16 :: Bin.Get Word16
getWord16 = getBoundedIntegral (unsafeGetSigned @Int32 fromIntegral 3)
{-# INLINE getWord16 #-}

getWord32 :: Bin.Get Word32
getWord32 = getBoundedIntegral (unsafeGetSigned @Int64 fromIntegral 5)
{-# INLINE getWord32 #-}

getWord64 :: Bin.Get Word64
getWord64 = getBoundedIntegral (getInteger 10)
{-# INLINE getWord64 #-}

getWord :: Bin.Get Word
getWord =
#if WORD_SIZE_IN_BITS == 64
  getBoundedIntegral (getInteger 10)
#elif WORD_SIZE_IN_BITS == 32
  getBoundedIntegral (unsafeGetSigned @Int64 fromIntegral 5)
#endif
{-# INLINE getWord #-}

--------------------------------------------------------------------------------

-- | SLEB128-encodes @a@ and writes it into 'Ptr'. Returns one past the last
-- written address. None of this is not checked.
{-# INLINE unsafePoke #-}
unsafePoke
  :: forall a. (Bits a, Integral a) => a -> Ptr Word8 -> IO (Ptr Word8)
unsafePoke = \a p ->
    -- We split neg and pos so that their internal 'if' checks for less things.
    if a < 0 then neg a p else pos a p
  where
    {-# INLINE neg #-}
    neg :: a -> Ptr Word8 -> IO (Ptr Word8)
    neg = \ !a !p -> do
      let b = unsafeShiftR a 7          :: a
          c = fromIntegral (a .&. 0x7f) :: Word8
          d = c .&. 0x40                :: Word8
      if d == 0 || b /= -1 then do
         poke p $! c .|. 0x80
         neg b $! plusPtr p 1
      else do
         poke p $! c .|. 0x40
         pure $! plusPtr p 1
    {-# INLINE pos #-}
    pos :: a -> Ptr Word8 -> IO (Ptr Word8)
    pos = \ !a !p -> do
      let b = unsafeShiftR a 7 :: a
          c = fromIntegral a   :: Word8
          d = c .&. 0x40       :: Word8
      if d /= 0 || b /= 0 then do
         poke p $! c .|. 0x80
         pos b $! plusPtr p 1
      else do
         poke p $! c
         pure $! plusPtr p 1

{-# INLINE naturalFromInteger #-}
naturalFromInteger :: MonadFail m => Integer -> m Natural
naturalFromInteger = \case
  IS x | isTrue# (0# <=# x) -> pure $ naturalFromWord# (int2Word# x)
  IP x -> pure $ naturalFromBigNat# x
  _ -> fail "underflow"

--------------------------------------------------------------------------------

-- | Compact 'S.Scientific' encoding. Internally, it uses both ULEB128 and
-- SLEB128. 0 is encoded as @\\x00@, other numbers take at least two bytes.
--
-- Compatible decoders are 'getFixed' and 'getScientific'.
putScientific :: S.Scientific -> Bin.Put
putScientific = \a -> case compare (S.coefficient a) 0 of
   LT -> do
      -- We store the coefficient sign alongside the base10Exponent so as to
      -- increase the chances that the coefficient fits in one less byte. The
      -- base10Exponent is usually much smaller than the coefficient in bit
      -- size, so this is likely to happen.
      let b = S.normalize a
      U.putNatural $ fromInteger $ negate $ S.coefficient b
      putInteger $ setBit (toInteger (S.base10Exponent b) `unsafeShiftL` 1) 0
   GT -> do
      let b = S.normalize a
      U.putNatural $ fromInteger $ S.coefficient b
      putInteger $ toInteger (S.base10Exponent b) `unsafeShiftL` 1
   EQ -> Bin.putWord8 0

-- | Decode a 'S.Scientific' encoded with 'putScientific' or 'putFixed'.
getScientific
  :: Int
  -- ^ /Maximum/ number of ULEB128 bytes to consume for the 'S.coefficient'
  -- part. See 'U.getNatural'.
  -> Bin.Get S.Scientific
getScientific n = Bin.label "getScientific" $ do
   c <- U.getInteger n
   if c /= 0
      then do
         e1 <- getInteger 10 -- Valid e1 can't be longer than 10.
         case toIntegralSized (e1 `unsafeShiftR` 1) of
            Just (e0 :: Int)
               | testBit e1 0 -> pure $ S.scientific (negate c) e0
               | otherwise -> pure $ S.scientific c e0
            Nothing -> fail "Exponent too large"
      else pure 0

--------------------------------------------------------------------------------

-- | Compact 'Rational' encoding. Internally, it uses both ULEB128 and SLEB128.
-- Decode with 'getRational'. 0 is encoded as @\\x00@, other numbers take at
-- least four bytes.
putRational :: Rational -> Bin.Put
putRational = \(unsafeReduceRational -> n :% d) ->
   if n /= 0
      then do
         -- ns: The coefficient is often larger than the non-negative exponent,
         -- so we store the coefficient sign with the exponent in order to
         -- improve the chances that the coefficient fit in one less byte.
         let ns = S.normalize (fromInteger n) :: S.Scientific
         U.putNatural $ fromInteger $ abs $ S.coefficient ns
         putInt $ if n < 0 then complement (S.base10Exponent ns)
                           else S.base10Exponent ns
         -- ds: The coefficient is more than 0, the exponent is at least 0.
         -- We decrease the coefficient by one to improve the chances it fits
         -- in one less byte.
         let ds = S.normalize (fromInteger d) :: S.Scientific
         U.putNatural $ fromInteger $ S.coefficient ds - 1
         U.putWord $ fromIntegral $ S.base10Exponent ds
      else Bin.putWord8 0
 where
   unsafeReduceRational :: Rational -> Rational
   unsafeReduceRational = \(n :% d) -> n % d
      -- Fails with 'error' if the denominator is 0. That's OK, that kind of
      -- 'Rational' is not supposed to exist, anyway.

-- | Decode a 'Rational' encoded with 'putRational'.
getRational
  :: Int
  -- ^ /Maximum/ number of bytes to consume for each of the numerator and
  -- denominator parts. See 'U.getNatural'.
  -> Bin.Get Rational
getRational m = Bin.label "getRational" $ do
   ncs0 <- U.getInteger m
   if ncs0 /= 0
      then do
         nes0 <- getInt
         let ns = if nes0 >= 0
                     then S.scientific ncs0 nes0
                     else S.scientific (negate ncs0) (complement nes0)
         case S.floatingOrInteger ns of
            Right n -> do
               dcs <- (1 +) <$> U.getInteger m
               des <- U.getInt
               case S.floatingOrInteger (S.scientific dcs des) of
                  Right d -> pure (n % d)
                  Left (_ :: Double) -> fail "Non-integer denominator"
            Left (_ :: Double) -> fail "Non-integer numerator"
      else pure 0

--------------------------------------------------------------------------------

-- | Same encoding as 'putScientific'.
--
-- Compatible decoders are 'getFixed' and 'getScientific'.
putFixed :: HasResolution r => Fixed r -> Bin.Put
putFixed = putScientific . fixedToScientific
{-# INLINE putFixed #-}

-- | Decode a 'Fixed' encoded with 'putFixed' or 'putScientific'.
getFixed
  :: (HasResolution r)
  => Int
  -- ^ /Maximum/ number of ULEB128 bytes to consume for the 'S.coefficient'
  -- part. See 'U.getNatural'.
  -> Bin.Get (Fixed r)
getFixed n = Bin.label "getFixed" $ do
  s <- getScientific n
  either fail pure $ fixedFromScientific s


fixedToScientific :: forall r. (HasResolution r) => Fixed r -> S.Scientific
fixedToScientific = \(MkFixed i) -> S.scientific i e
  where
   e :: Int
   e = negate $ integerLog10 $ resolution $ Proxy @r

fixedFromScientific
   :: forall r
    . (HasResolution r)
   => S.Scientific
   -> Either String (Fixed r)
fixedFromScientific = \s0 ->
   if | s0e <- S.base10Exponent s0, s0e >= e ->
         Right (MkFixed (S.coefficient s0 * 10 ^ (s0e - e)))
      | s1 <- S.normalize s0, s1e <- S.base10Exponent s1, s1e >= e ->
         Right (MkFixed (S.coefficient s1 * 10 ^ (s1e - e)))
      | otherwise -> Left "Too small"
  where
   e :: Int
   e = negate $ integerLog10 $ resolution $ Proxy @r

