{-# INLINE (|.) #-}; (|.)::Storable a=>Ptr a -> Int -> IO a         ; (|.) a i   = peekElemOff a i
