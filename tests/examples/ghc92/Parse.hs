{-# language DataKinds #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language RankNTypes #-}
{-# language FunctionalDependencies, MultiParamTypeClasses #-}
{-# language TypeFamilies #-}

{-|
Module      : Language.Python.Internal.Parse
Copyright   : (C) CSIRO 2017-2019
License     : BSD3
Maintainer  : Isaac Elliott <isaace71295@gmail.com>
Stability   : experimental
Portability : non-portable
-}

module Language.Python.Internal.Parse where

import Control.Applicative (Alternative, (<|>), optional, many, some)
import Control.Lens.Cons (snoc)
import Control.Lens.Getter ((^.), view)
import Control.Lens.Prism (Prism')
import Control.Lens.Review ((#))
import Control.Monad (void)
import Data.Bifunctor (first, second)
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty, some1)
import Data.Proxy (Proxy(..))
import Data.Set (Set)
import Data.Void (Void)
import GHC.Stack (HasCallStack)
import Text.Megaparsec
  ( (<?>), MonadParsec, Parsec, Stream(..), SourcePos(..), eof, try, lookAhead
  , notFollowedBy
  )
import Text.Megaparsec.Char (satisfy)


import qualified Data.List.NonEmpty as NonEmpty
import qualified Text.Megaparsec as Megaparsec

import Language.Python.Internal.Lexer (SrcInfo(..), withSrcInfo)
import Language.Python.Internal.Syntax.IR
import Language.Python.Internal.Token
import Language.Python.Syntax.Ann
import Language.Python.Syntax.AugAssign
import Language.Python.Syntax.CommaSep
import Language.Python.Syntax.Comment
import Language.Python.Syntax.Ident
import Language.Python.Syntax.Import
import Language.Python.Syntax.ModuleNames
import Language.Python.Syntax.Operator.Binary
import Language.Python.Syntax.Operator.Unary
import Language.Python.Syntax.Punctuation
import Language.Python.Syntax.Strings
import Language.Python.Syntax.Whitespace


compoundStatement
  :: MonadParsec e PyTokens m
  => m (Indents SrcInfo)
  -> Indents SrcInfo
  -> m (CompoundStatement SrcInfo)
compoundStatement pIndent indentBefore =
  ifSt <|>
  whileSt <|>
  trySt <|>
  decorated <|>
  asyncSt <|>
  classSt indentBefore [] <|>
  fundef indentBefore Nothing [] <|>
  withSt Nothing <|>
  forSt Nothing
  where
    decorated = do
      ds <- decorators pIndent indentBefore
      i <- pIndent
      (do; a <- doAsync; fundef i (Just a) ds) <|>
        fundef i Nothing ds <|>
        classSt i ds

f = (0x01, 001)

