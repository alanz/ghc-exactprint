{-# LANGUAGE LambdaCase #-}
-- | @ghc-exactprint@ is a low level library to manage manipulating Haskell
-- source files. There three components.
module Language.Haskell.GHC.ExactPrint
        ( -- * Relativising
          relativiseApiAnns
        , relativiseApiAnnsWithComments
        , Anns
        , Comment
        , Annotation(..)
        , AnnKey(..)

        -- * Parsing
        , parseModule

        -- * Printing
        , exactPrintWithAnns
        , exactPrint

        ) where

import Language.Haskell.GHC.ExactPrint.Delta
import Language.Haskell.GHC.ExactPrint.Print
import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Parsers
