{-# LANGUAGE LambdaCase #-}
module Language.Haskell.GHC.ExactPrint
        ( -- * Relativising
          relativiseApiAnns
        , relativiseApiAnnsWithComments
        , Anns
        , Comment

        -- * Printing
        , exactPrintWithAnns
        , exactPrint

        -- * Utility
        , Parser
        , parseModule
        , parseExpr
        , parseImport
        , parseType
        , parseDecl
        , parsePattern
        , parseStmt
        , parseWith

        ) where

import Language.Haskell.GHC.ExactPrint.Delta
import Language.Haskell.GHC.ExactPrint.Preprocess
import Language.Haskell.GHC.ExactPrint.Print
import Language.Haskell.GHC.ExactPrint.Transform
import Language.Haskell.GHC.ExactPrint.Types

import GHC.Paths (libdir)

import qualified ApiAnnotation as GHC
import qualified DynFlags      as GHC
import qualified FastString    as GHC
import qualified GHC           as GHC hiding (parseModule)
import qualified HeaderInfo    as GHC
import qualified Lexer         as GHC
import qualified MonadUtils    as GHC
import qualified Outputable    as GHC
import qualified Parser        as GHC
import qualified SrcLoc        as GHC
import qualified StringBuffer  as GHC
import qualified RdrHsSyn as GHC ( checkPattern )
import qualified OrdList as OL

import qualified Data.Map as Map
import Control.Monad (void)


