{-# OPTIONS_GHC -fno-warn-redundant-constraints -haddock #-}
-- | Haddock comment,
-- coming before the module
module Haddock1 (

        -- | This is some inline documentation in the export list
        --
        -- > a code block using bird-tracks
        -- > each line must begin with > (which isn't significant unless it
        -- > is at the beginning of the line).
        f

        {-| nested-style doc comments -}
        , g

   ) where

-- | Haddock before imports
import Data.List

-- | Haddock before decl
f = undefined
g = undefined
