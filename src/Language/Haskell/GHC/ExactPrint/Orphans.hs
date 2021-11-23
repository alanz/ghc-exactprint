{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.Haskell.GHC.ExactPrint.Orphans where

import GHC hiding (EpaComment)

-- ---------------------------------------------------------------------
-- Orphan Monoid instances. See https://gitlab.haskell.org/ghc/ghc/-/issues/20372

instance Semigroup AnnPragma where
  (<>) = error "unimplemented"
instance Monoid AnnPragma where
  mempty = error "meaningless"

instance Semigroup EpAnnImportDecl where
  (<>) = error "unimplemented"
instance Monoid EpAnnImportDecl where
  mempty = error "meaningless"

instance Semigroup HsRuleAnn where
  (<>) = error "unimplemented"
instance Monoid HsRuleAnn where
  mempty = error "meaningless"

instance Semigroup AnnSig where
  (<>) = error "unimplemented"
instance Monoid AnnSig where
  mempty = error "meaningless"

instance Semigroup GrhsAnn where
  (<>) = error "unimplemented"
instance Monoid GrhsAnn where
  mempty = error "meaningless"

instance Semigroup EpAnnUnboundVar where
  (<>) = error "unimplemented"
instance Monoid EpAnnUnboundVar where
  mempty = error "meaningless"

instance Semigroup NoEpAnns where
  (<>) = error "unimplemented"
instance Monoid NoEpAnns where
  mempty = error "meaningless"

instance Semigroup AnnParen where
  (<>) = error "unimplemented"
instance Monoid AnnParen where
  mempty = error "meaningless"

instance Semigroup AnnExplicitSum where
  (<>) = error "unimplemented"
instance Monoid AnnExplicitSum where
  mempty = error "meaningless"

instance Semigroup EpAnnHsCase where
  (<>) = error "unimplemented"
instance Monoid EpAnnHsCase where
  mempty = error "meaningless"

instance Semigroup AnnsIf where
  (<>) = error "unimplemented"
instance Monoid AnnsIf where
  mempty = error "meaningless"

instance Semigroup AnnsLet where
  (<>) = error "unimplemented"
instance Monoid AnnsLet where
  mempty = error "meaningless"

instance Semigroup AnnProjection where
  (<>) = error "unimplemented"
instance Monoid AnnProjection where
  mempty = error "meaningless"

instance Semigroup AnnFieldLabel where
  (<>) = error "unimplemented"
instance Monoid AnnFieldLabel where
  mempty = error "meaningless"

instance Semigroup EpaLocation where
  (<>) = error "unimplemented"
instance Monoid EpaLocation where
  mempty = error "meaningless"

instance Semigroup AddEpAnn where
  (<>) = error "unimplemented"
instance Monoid AddEpAnn where
  mempty = error "meaningless"

instance Semigroup TrailingAnn where
  (<>) = error "unimplemented"
instance Monoid TrailingAnn where
  mempty = error "meaningless"

instance Semigroup AnnContext where
  (<>) = error "unimplemented"
instance Monoid AnnContext where
  mempty = AnnContext Nothing [] []

instance Semigroup EpAnnSumPat where
  (<>) = error "unimplemented"
instance Monoid EpAnnSumPat where
  mempty = error "meaningless"

instance Semigroup AnnsModule where
  (<>) = error "unimplemented"
instance Monoid AnnsModule where
  mempty = error "meaningless"
