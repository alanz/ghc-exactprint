-- | @ghc-exactprint@ is a library to manage manipulating Haskell
-- source files. There are four components.
module Language.Haskell.GHC.ExactPrint
        ( -- * Types
          Comment

        -- * Parsing
        , parseModule

        -- * Transformation
        , module Language.Haskell.GHC.ExactPrint.Transform

        -- * Printing
        , ExactPrint(..)
        , exactPrint

        -- * Relativising
        , makeDeltaAst

        -- * Fixity
        , module Language.Haskell.GHC.ExactPrint.Fixity
        ) where

import Language.Haskell.GHC.ExactPrint.ExactPrint
import Language.Haskell.GHC.ExactPrint.Fixity
import Language.Haskell.GHC.ExactPrint.Transform
import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Parsers
