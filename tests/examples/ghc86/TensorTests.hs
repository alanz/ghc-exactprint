{-# LANGUAGE ConstraintKinds, FlexibleContexts, DataKinds, NoImplicitPrelude,
             RebindableSyntax, ScopedTypeVariables, TypeFamilies, TypeOperators,
             UndecidableInstances #-}

module TensorTests (tensorTests) where

import Apply.Cyc
import Tests
import Utils

import TestTypes

import Crypto.Lol
import Crypto.Lol.CRTrans
import Crypto.Lol.Cyclotomic.Tensor
import Crypto.Lol.Types

import Control.Applicative

import Data.Maybe

import Data.Singletons
import Data.Promotion.Prelude.Eq
import Data.Singletons.TypeRepStar ()

import qualified Test.Framework as TF

type TMRParams = ( '(,) <$> Tensors) <*> MRCombos
tmrParams :: Proxy TMRParams
tmrParams = Proxy

--type ExtParams = ( '(,) <$> Tensors) <*> MRExtCombos
type TrEmParams = ( '(,) <$> Tensors) <*> MM'RCombos
tremParams :: Proxy TrEmParams
tremParams = Proxy

type NormParams = ( '(,) <$> '[RT]) <*> (Filter Liftable MRCombos)

data Liftable :: TyFun (Factored, Type) Bool -> Type
type instance Apply Liftable '(m,zq) = Int64 :== (LiftOf zq)

