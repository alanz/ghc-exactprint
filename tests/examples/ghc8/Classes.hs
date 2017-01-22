{-# LANGUAGE CPP, Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude, MagicHash, StandaloneDeriving, BangPatterns,
             KindSignatures, DataKinds, ConstraintKinds,
              MultiParamTypeClasses, FunctionalDependencies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
  -- ip :: IP x a => a  is strictly speaking ambiguous, but IP is magic
{-# LANGUAGE UndecidableSuperClasses #-}
  -- Because of the type-variable superclasses for tuples

{-# OPTIONS_GHC -Wno-unused-imports #-}
-- -Wno-unused-imports needed for the GHC.Tuple import below. Sigh.

{-# OPTIONS_GHC -Wno-unused-top-binds #-}
-- -Wno-unused-top-binds is there (I hope) to stop Haddock complaining
-- about the constraint tuples being defined but not used

{-# OPTIONS_HADDOCK hide #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  GHC.Classes
-- Copyright   :  (c) The University of Glasgow, 1992-2002
-- License     :  see libraries/base/LICENSE
--
-- Maintainer  :  cvs-ghc@haskell.org
-- Stability   :  internal
-- Portability :  non-portable (GHC extensions)
--
-- Basic classes.
--
-----------------------------------------------------------------------------

module GHC.Classes where

-- GHC.Magic is used in some derived instances
import GHC.Magic ()
import GHC.IntWord64
import GHC.Prim
import GHC.Tuple
import GHC.Types

default ()              -- Double isn't available yet


{- *************************************************************
*                                                              *
*               Constraint tuples                              *
*                                                              *
************************************************************* -}

class ()
class (c1, c2)     => (c1, c2)
class (c1, c2, c3) => (c1, c2, c3)
