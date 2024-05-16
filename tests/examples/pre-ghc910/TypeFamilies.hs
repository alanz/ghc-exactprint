{-# LANGUAGE TypeFamilies, DataKinds, PolyKinds, UndecidableInstances #-}

module TypeFamilies where

type family F a b  = r | r -> a b where
  F a      IO      = IO a   -- (1)
  F Char   b       = b Int  -- (2)
