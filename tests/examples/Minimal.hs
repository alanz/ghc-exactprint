class AwsType a where
    toText :: a -> b


    {-# MINIMAL toText #-}

class Minimal a where
  toText :: a -> b
  {-# MINIMAL decimal, hexadecimal, realFloat, scientific #-}

class Minimal a where
  toText :: a -> b
  {-# MINIMAL shape, (maskedIndex | maskedLinearIndex) #-}

class Minimal a where
  toText :: a -> b
  {-# MINIMAL (toSample | toSamples) #-}

