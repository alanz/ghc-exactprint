{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Bindings to the C++ smallest enclosing ball library Miniball
-- (<http://www.inf.ethz.ch/personal/gaertner/miniball.html>).
--
-- Currently, the C++ library is set up for computation on each call
-- for the sake of referential transparency. If you need to repeatedly
-- compute the smallest enclosing ball of the same number of points in
-- the same dimension, or don't need referential transparency, you are
-- probably better off directly interfacing with Miniball yourself. I
-- may add a state-maintaining interface in the future, but for now
-- things are kept simple.
--
-- More interfaces will probably be added, and the current ones will
-- probably change. High on the list of priorities is taking the
-- points as a single packed matrix, and returning the ball's support
-- points.
module Numeric.Miniball(Ball(Ball, center, radius), miniball, miniball', miniballUnsafe) where

import Foreign.Ptr
import Foreign.C.Types
import Foreign.Storable
import Foreign.Marshal.Array
import Foreign.Marshal.Alloc
import Foreign.ForeignPtr.Safe
import Foreign.ForeignPtr.Unsafe
import System.IO.Unsafe
import qualified Data.Vector.Storable as V
import qualified Data.Vector as BV

-- | Metric Euclidean ball.
data Ball = Ball { center :: V.Vector Double, radius :: Double }
            deriving (Show)

pad :: [V.Vector Double] -> [V.Vector Double]
pad [] = []
pad xs = map (\y -> V.take n y V.++ V.replicate (n - V.length y) 0) xs
    where
      n = V.length (head xs)

-- | Compute the smallest enclosing ball of the points in the given
-- list. Each 'V.Vector' in the list /must/ have the same length, or
-- else behavior is undefined. The program may then even crash. If
-- this scares you, use 'miniball'' instead.
miniball :: [V.Vector Double] -> Ball
miniball [] = Ball V.empty 0
miniball xs = unsafeDupablePerformIO (helper c_miniball_wrapper xs)

-- | A version of 'miniball' that ensures that all the 'V.Vector's
-- have the same length, so as to avoid the undefined behavior
-- described above. Each one shorter than the first is padded with
-- zeros, and each one longer has its tail truncated. This may of
-- course not be what you want.
miniball' :: [V.Vector Double] -> Ball
miniball' [] = Ball V.empty 0
miniball' xs = unsafeDupablePerformIO (helper c_miniball_wrapper (pad xs))

-- | A version of 'miniball' that does its foreign calls in a more
-- direct way. This can drastically speed up /short/ computations, but
-- has the downside that the current (OS) thread is blocked during
-- execution. This function is only intended for when you need to
-- repeatedly compute the smallest enclosing ball of a /small number/
-- /of points/ in /low dimensions/ in a tight inner loop. Otherwise,
-- always use 'miniball'.
--
-- For more information, see
-- <http://blog.ezyang.com/2010/07/safety-first-ffi-and-threading/>.
miniballUnsafe :: [V.Vector Double] -> Ball
miniballUnsafe [] = Ball V.empty 0
miniballUnsafe xs = unsafeDupablePerformIO (helper c_miniball_wrapper_unsafe xs)

helper :: (CInt -> CInt -> Ptr (Ptr Double) -> Ptr Double -> Ptr Double -> IO ()) -> [V.Vector Double] -> IO Ball
helper action xs = mallocForeignPtr >>= \fpr ->
                   withForeignPtr fpr $ \pr ->
                   mallocForeignPtrArray d >>= \fpcs ->
                   withForeignPtr fpcs $ \pcs ->
                   withForeignPtr fppxs $ \ppxs ->
                   action n' d' ppxs pcs pr >>
                   withForeignPtr fpr peek >>= \r ->
                   BV.mapM_ (touchForeignPtr . fst . V.unsafeToForeignPtr0) xs' >> -- Touch the ForeignPtrs that were converted to (pxs :: Ptr).
                   return (Ball (V.unsafeFromForeignPtr0 fpcs d) r)
    where
      d = V.length (head xs)
      d' = fromIntegral d
      xs' = BV.fromList xs
      n = BV.length xs'
      n' = fromIntegral n
      pxs = V.generate n (\i -> (unsafeForeignPtrToPtr . fst . V.unsafeToForeignPtr0) (xs' BV.! i))   -- Remember to touch every ForeignPtr.
      fppxs = (fst . V.unsafeToForeignPtr0) pxs

foreign import ccall "miniball_wrapper.h miniball_wrapper"
        c_miniball_wrapper :: CInt -> CInt -> Ptr (Ptr Double) -> Ptr Double -> Ptr Double -> IO ()

foreign import ccall unsafe "miniball_wrapper.h miniball_wrapper"
        c_miniball_wrapper_unsafe :: CInt -> CInt -> Ptr (Ptr Double) -> Ptr Double -> Ptr Double -> IO ()
