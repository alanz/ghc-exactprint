{-# LANGUAGE ImportQualifiedPost #-}
module Import where

import safe A
import qualified B as B
import B qualified as B
import C hiding (a,b)
import D (x,y)
import Data.List as L ( intersperse )
import "base" Prelude hiding (String)
