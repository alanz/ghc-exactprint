-- | @ghc-exactprint@ is a library to manage manipulating Haskell
-- source files. There are four components.
module Language.Haskell.GHC.ExactPrint
        ( -- * Relativising
          Anns
        , Comment
        , Annotation(..)
        , AnnKey(..)

        -- * Parsing
        , parseModule

        -- * Transformation
        , module Language.Haskell.GHC.ExactPrint.Transform

        -- * Printing
        , exactPrint

        ) where

import Language.Haskell.GHC.ExactPrint.ExactPrint
import Language.Haskell.GHC.ExactPrint.Transform
import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Parsers
