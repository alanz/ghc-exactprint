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


        -- * Dumping ASTs
        -- ** Temporary copy from GHC, shows AnchorOps embedded in SrcSpan
        , showAstData
        , BlankSrcSpan(..)
        , BlankEpAnnotations(..)
        ) where

import Language.Haskell.GHC.ExactPrint.Dump
import Language.Haskell.GHC.ExactPrint.ExactPrint
import Language.Haskell.GHC.ExactPrint.Transform
import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Parsers
