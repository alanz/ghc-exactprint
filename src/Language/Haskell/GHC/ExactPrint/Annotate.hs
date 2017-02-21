{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-} -- Needed for the DataId constraint on ResTyGADTHook
-- | 'annotate' is a function which given a GHC AST fragment, constructs
-- a syntax tree which indicates which annotations belong to each specific
-- part of the fragment.
--
-- "Delta" and "Print" provide two interpreters for this structure. You
-- should probably use those unless you know what you're doing!
--
-- The functor 'AnnotationF' has a number of constructors which correspond
-- to different sitations which annotations can arise. It is hoped that in
-- future versions of GHC these can be simplified by making suitable
-- modifications to the AST.

module Language.Haskell.GHC.ExactPrint.Annotate
       (
         annotate
       , AnnotationF(..)
       , Annotated
       , Annotate(..)
       , withSortKeyContextsHelper

       -- , 
       ) where

#if __GLASGOW_HASKELL__ <= 710
import Data.Ord ( comparing )
import Data.List ( sortBy )
#endif

import Language.Haskell.GHC.ExactPrint.Annotater
-- import Language.Haskell.GHC.ExactPrint.AnnotateTypes
-- import Language.Haskell.GHC.ExactPrint.Types
-- import Language.Haskell.GHC.ExactPrint.Utils

-- import qualified Bag            as GHC
-- import qualified BasicTypes     as GHC
-- import qualified BooleanFormula as GHC
-- import qualified Class          as GHC
-- import qualified CoAxiom        as GHC
-- import qualified FastString     as GHC
-- import qualified ForeignCall    as GHC
-- import qualified GHC            as GHC
#if __GLASGOW_HASKELL__ > 710
-- import qualified Lexeme         as GHC
#endif
-- import qualified Name           as GHC
-- import qualified RdrName        as GHC
-- import qualified Outputable     as GHC

-- import Control.Monad.Trans.Free
-- import Control.Monad.Free.TH (makeFreeCon)
-- import Control.Monad.Identity
-- import Data.Data
-- import Data.Maybe

-- import qualified Data.Set as Set

-- import Debug.Trace


{-# ANN module "HLint: ignore Eta reduce" #-}
{-# ANN module "HLint: ignore Redundant do" #-}
{-# ANN module "HLint: ignore Reduce duplication" #-}
-- ---------------------------------------------------------------------
