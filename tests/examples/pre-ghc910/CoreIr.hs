{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Llvm.Data.CoreIr
       ( module Llvm.Data.CoreIr
       , module Llvm.Data.Shared
       , module Llvm.Data.IrType
       , module Data.Word
       , Label
       ) where
import Llvm.Data.Shared
import Llvm.Data.IrType
import Compiler.Hoopl (Label)
import Data.Int
import Data.Word (Word8, Word16, Word32, Word64)
import Data.DoubleWord

data Conversion s v where {
  AddrSpaceCast :: T (Type s P) v -> Type s P -> Conversion s v;
  } deriving (Eq, Ord, Show)

