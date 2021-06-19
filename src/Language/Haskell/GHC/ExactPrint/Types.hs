{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}
module Language.Haskell.GHC.ExactPrint.Types
  ( -- * Core Types
   Anns
  , emptyAnns
  , Annotation(..)
  , annNone

  , KeywordId(..)
  , Comment(..)
  -- * Positions
  , Pos
  , DeltaPos(..)
  , deltaRow, deltaColumn
  -- * AnnKey
  , AnnSpan
  , AnnKey(..)
  , mkAnnKey
  , AnnConName(..)
  , annGetConstr
#if __GLASGOW_HASKELL__ >= 808
  , badRealSrcSpan
#endif

  -- * Other

  , Rigidity(..)
  , AstContext(..),AstContextSet,defaultACS
  , ACS'(..)
  , ListContexts(..)

  -- * For managing compatibility
  , Constraints

  -- * GHC version compatibility
  , GhcPs
  , GhcRn
  , GhcTc

#if __GLASGOW_HASKELL__ > 804
  , noExt
#endif

  -- * Internal Types
  , LayoutStartCol(..)
  , declFun

  ) where

import Data.Data (Data, Typeable, toConstr,cast)

import qualified GHC
#if __GLASGOW_HASKELL__ >= 808
import GHC.Data.FastString     as GHC
import GHC.Driver.Session      as GHC
import GHC.Types.SrcLoc        as GHC
import GHC.Utils.Outputable    as GHC
#else
import qualified DynFlags      as GHC
import qualified Outputable    as GHC
#endif

import qualified Data.Map as Map
import qualified Data.Set as Set

-- ---------------------------------------------------------------------

type Constraints a = (Data a)

-- ---------------------------------------------------------------------

-- | A Haskell comment. The @AnnKeywordId@ is present if it has been converted
-- from an @AnnKeywordId@ because the annotation must be interleaved into the
-- stream and does not have a well-defined position
data Comment = Comment
    {
      commentContents   :: !String -- ^ The contents of the comment including separators

    -- AZ:TODO: commentIdentifier is a misnomer, should be commentSrcSpan, it is
    -- the thing we use to decide where in the output stream the comment should
    -- go.
    , commentIdentifier :: !AnnSpan -- ^ Needed to uniquely identify two comments with the same contents
    , commentOrigin     :: !(Maybe GHC.AnnKeywordId) -- ^ We sometimes turn syntax into comments in order to process them properly.
    }
  deriving (Eq,Typeable,Data,Ord)
instance Show Comment where
  show (Comment cs ss o) = "(Comment " ++ show cs ++ " " ++ showGhc ss ++ " " ++ show o ++ ")"

instance GHC.Outputable Comment where
  ppr x = GHC.text (show x)

type Pos = (Int,Int)

-- | A relative positions, row then column
newtype DeltaPos = DP (Int,Int) deriving (Show,Eq,Ord,Typeable,Data)

deltaRow, deltaColumn :: DeltaPos -> Int
deltaRow (DP (r, _)) = r
deltaColumn (DP (_, c)) = c


-- | Marks the start column of a layout block.
newtype LayoutStartCol = LayoutStartCol { getLayoutStartCol :: Int }
  deriving (Eq, Num)

instance Show LayoutStartCol where
  show (LayoutStartCol sc) = "(LayoutStartCol " ++ show sc ++ ")"


annNone :: Annotation
annNone = Ann (DP (0,0)) [] [] [] Nothing Nothing

data Annotation = Ann
  {
    -- The first three fields relate to interfacing up into the AST
    annEntryDelta      :: !DeltaPos
    -- ^ Offset used to get to the start of the SrcSpan, from whatever the prior
    -- output was, including all annPriorComments (field below).
  , annPriorComments   :: ![(Comment,  DeltaPos)]
    -- ^ Comments coming after the last non-comment output of the preceding
    -- element but before the SrcSpan being annotated by this Annotation. If
    -- these are changed then annEntryDelta (field above) must also change to
    -- match.
  , annFollowingComments   :: ![(Comment,  DeltaPos)]
    -- ^ Comments coming after the last output for the element subject to this
    -- Annotation. These will only be added by AST transformations, and care
    -- must be taken not to disturb layout of following elements.

  -- The next three fields relate to interacing down into the AST
  , annsDP             :: ![(KeywordId, DeltaPos)]
    -- ^ Annotations associated with this element.
#if __GLASGOW_HASKELL__ >= 808
  , annSortKey         :: !(Maybe [GHC.RealSrcSpan])
#else
  , annSortKey         :: !(Maybe [GHC.SrcSpan])
#endif
    -- ^ Captures the sort order of sub elements. This is needed when the
    -- sub-elements have been split (as in a HsLocalBind which holds separate
    -- binds and sigs) or for infix patterns where the order has been
    -- re-arranged. It is captured explicitly so that after the Delta phase a
    -- SrcSpan is used purely as an index into the annotations, allowing
    -- transformations of the AST including the introduction of new Located
    -- items or re-arranging existing ones.
  , annCapturedSpan    :: !(Maybe AnnKey)
    -- ^ Occasionally we must calculate a SrcSpan for an unlocated list of
    -- elements which we must remember for the Print phase. e.g. the statements
    -- in a HsLet or HsDo. These must be managed as a group because they all
    -- need eo be vertically aligned for the Haskell layout rules, and this
    -- guarantees this property in the presence of AST edits.

  } deriving (Typeable,Eq)

instance Show Annotation where
  show (Ann dp comments fcomments ans sk csp)
    = "(Ann (" ++ show dp ++ ") " ++ show comments ++ " "
        ++ show fcomments ++ " "
        ++ show ans ++ " " ++ showGhc sk ++ " "
        ++ showGhc csp ++ ")"


-- | This structure holds a complete set of annotations for an AST
type Anns = Map.Map AnnKey Annotation

emptyAnns :: Anns
emptyAnns = Map.empty

-- | For every @Located a@, use the @SrcSpan@ and constructor name of
-- a as the key, to store the standard annotation.
-- These are used to maintain context in the AP and EP monads
data AnnKey   = AnnKey AnnSpan AnnConName
                  deriving (Eq, Ord, Data)

-- | From GHC 9.0 the ParsedSource uses RealSrcSpan instead of SrcSpan.
--   Compatibility type
#if __GLASGOW_HASKELL__ >= 808
type AnnSpan = GHC.RealSrcSpan
#else
type AnnSpan = GHC.SrcSpan
#endif

-- More compact Show instance
instance Show AnnKey where
  show (AnnKey ss cn) = "AnnKey " ++ showGhc ss ++ " " ++ show cn


#if __GLASGOW_HASKELL__ >= 808
mkAnnKeyPrim :: (Data a) => GHC.Located a -> AnnKey
mkAnnKeyPrim (GHC.L (GHC.RealSrcSpan l _) a) = AnnKey l (annGetConstr a)
mkAnnKeyPrim (GHC.L _ a) = AnnKey badRealSrcSpan (annGetConstr a)
#else
mkAnnKeyPrim :: (Data a) => GHC.Located a -> AnnKey
mkAnnKeyPrim (GHC.L l a) = AnnKey l (annGetConstr a)
#endif


#if __GLASGOW_HASKELL__ >= 808
badRealSrcSpan :: GHC.RealSrcSpan
badRealSrcSpan = GHC.mkRealSrcSpan bad bad
  where
    bad = GHC.mkRealSrcLoc (GHC.fsLit "ghc-exactprint-nospan") 0 0
#endif

#if __GLASGOW_HASKELL__ <= 802
type GhcPs = GHC.RdrName
type GhcRn = GHC.Name
type GhcTc = GHC.Id
#else
type GhcPs = GHC.GhcPs
type GhcRn = GHC.GhcRn
type GhcTc = GHC.GhcTc
#endif


#if __GLASGOW_HASKELL__ >= 808
noExt :: GHC.NoExtField
noExt = GHC.NoExtField
#elif __GLASGOW_HASKELL__ > 804
noExt :: GHC.NoExt
noExt = GHC.noExt
#endif

-- |Make an unwrapped @AnnKey@ for the @LHsDecl@ case, a normal one otherwise.
mkAnnKey :: (Data a) => GHC.Located a -> AnnKey
mkAnnKey ld =
  case cast ld :: Maybe (GHC.LHsDecl GhcPs) of
    Just d -> declFun mkAnnKeyPrim d
    Nothing -> mkAnnKeyPrim ld

-- Holds the name of a constructor
data AnnConName = CN { unConName :: String }
                 deriving (Eq, Ord, Data)

-- More compact show instance
instance Show AnnConName where
  show (CN s) = "CN " ++ show s

annGetConstr :: (Data a) => a -> AnnConName
annGetConstr a = CN (show $ toConstr a)

-- | The different syntactic elements which are not represented in the
-- AST.
data KeywordId = G GHC.AnnKeywordId  -- ^ A normal keyword
               | AnnSemiSep          -- ^ A separating comma
#if __GLASGOW_HASKELL__ >= 808
               | AnnEofPos
#endif
#if __GLASGOW_HASKELL__ >= 800
               | AnnTypeApp          -- ^ Visible type application annotation
#endif
               | AnnComment Comment
               | AnnString String    -- ^ Used to pass information from
                                     -- Delta to Print when we have to work
                                     -- out details from the original
                                     -- SrcSpan.
#if __GLASGOW_HASKELL__ <= 710
               | AnnUnicode GHC.AnnKeywordId -- ^ Used to indicate that we should print using unicode syntax if possible.
#endif
               deriving (Eq, Ord, Data)

instance Show KeywordId where
  show (G gc)          = "(G " ++ show gc ++ ")"
  show AnnSemiSep      = "AnnSemiSep"
#if __GLASGOW_HASKELL__ >= 808
  show AnnEofPos       = "AnnEofPos"
#endif
#if __GLASGOW_HASKELL__ >= 800
  show AnnTypeApp      = "AnnTypeApp"
#endif
  show (AnnComment dc) = "(AnnComment " ++ show dc ++ ")"
  show (AnnString s)   = "(AnnString " ++ s ++ ")"
#if __GLASGOW_HASKELL__ <= 710
  show (AnnUnicode gc) = "(AnnUnicode " ++ show gc ++ ")"
#endif

-- ---------------------------------------------------------------------

instance GHC.Outputable KeywordId where
  ppr k     = GHC.text (show k)

instance GHC.Outputable AnnConName where
  ppr tr     = GHC.text (show tr)

instance GHC.Outputable Annotation where
  ppr a     = GHC.text (show a)

instance GHC.Outputable AnnKey where
  ppr a     = GHC.text (show a)

instance GHC.Outputable DeltaPos where
  ppr a     = GHC.text (show a)

-- ---------------------------------------------------------------------
--
-- Flag used to control whether we use rigid or normal layout rules.
-- NOTE: check is done via comparison of enumeration order, be careful with any changes
data Rigidity = NormalLayout | RigidLayout deriving (Eq, Ord, Show)
{-

Rigidity logic. The same type is used for two different things

1. As a flag in Annotate to the "SetLayoutFlag" operation, which specifies
   NormalLayout - Layout should be captured unconditionally

   RigidLayout - Layout should be captured or not depending on a parameter kept
                 in the interpreter Read state

2. As the controlling parameter for the optional (Rigid) layout

The nett effect is the following, where flag is the hard-coded flag value in
Annotate, and param is the interpreter param set when the interpreter is run

   flag         |  param       | result
   -------------+--------------+--------------------
   NormalLayout |  either      | layout captured
   RigidLayout  | NormalLayout | layout NOT captured
   RigidLayout  | RigidLayout  | layout captured

The flag is only used on HsIf and HsCase

So

   state                       | HsCase    | HsIf
   ----------------------------|-----------+------
   before rigidity flag (AZ)   | no layout | layout
   param NormalLayout          | no layout | no layout
   param RigidLayout           | layout    | layout
   ----------------------------+-----------+-------
   desired future HaRe         | no layout | layout
   desired future apply-refact | layout    | layout
-}

-- ---------------------------------------------------------------------

data ACS' a = ACS
  { acs :: !(Map.Map a Int) -- ^ how many levels each AstContext should
                            -- propagate down the AST. Removed when it hits zero
  } deriving (Show)

#if __GLASGOW_HASKELL__ >= 804
instance Semigroup (ACS' AstContext) where
  (<>) = mappend
#endif

instance Monoid (ACS' AstContext) where
  mempty = ACS mempty
  -- ACS a `mappend` ACS b = ACS (a `mappend` b)
  ACS a `mappend` ACS b = ACS (Map.unionWith max a b)
  -- For Data.Map, mappend == union, which is a left-biased replace for key collisions

type AstContextSet = ACS' AstContext
-- data AstContextSet = ACS
--   { acs :: !(Map.Map AstContext Int) -- ^ how many levels each AstContext should
--                                      -- propagate down the AST. Removed when it
--                                      -- hits zero
--   } deriving (Show)

defaultACS :: AstContextSet
defaultACS = ACS Map.empty

-- instance GHC.Outputable AstContextSet where
instance (Show a) => GHC.Outputable (ACS' a) where
  ppr x = GHC.text $ show x

data AstContext = LambdaExpr
                | CaseAlt
                | NoPrecedingSpace
                | HasHiding
                | AdvanceLine
                | NoAdvanceLine
                | Intercalate -- This item may have a list separator following
                | InIE -- possible 'type' or 'pattern'
                | PrefixOp
                | PrefixOpDollar
                | InfixOp -- RdrName may be used as an infix operator
                | ListStart -- Identifies first element of a list in layout, so its indentation can me managed differently
                | ListItem -- Identifies subsequent elements of a list in layout
                | TopLevel -- top level declaration
                | NoDarrow
                | AddVbar
                | Deriving
                | Parens -- TODO: Not currently used?
                | ExplicitNeverActive
                | InGadt
                | InRecCon
                | InClassDecl
                | InSpliceDecl
                | LeftMost -- Is this the leftmost operator in a chain of OpApps?
                | InTypeApp -- HsTyVar in a TYPEAPP context. Has AnnAt
                          -- TODO:AZ: do we actually need this?
                          -- TODO:AZ this is actually tight prefix

                -- Next four used to identify current list context
                | CtxOnly
                | CtxFirst
                | CtxMiddle
                | CtxLast
                | CtxPos Int -- 0 for first, increasing for subsequent

                -- Next are used in tellContext to push context up the tree
                | FollowingLine
                deriving (Eq, Ord, Show)


data ListContexts = LC { lcOnly,lcInitial,lcMiddle,lcLast :: !(Set.Set AstContext) }
  deriving (Eq,Show)

-- ---------------------------------------------------------------------

-- data LayoutContext = FollowingLine -- ^Indicates that an item such as a SigD
--                                    -- should not have blank lines after it
--                 deriving (Eq, Ord, Show)

-- ---------------------------------------------------------------------

declFun :: (forall a . Data a => GHC.Located a -> b) -> GHC.LHsDecl GhcPs -> b

#if __GLASGOW_HASKELL__ > 804
declFun f (GHC.L l de) =
  case de of
      GHC.TyClD _ d       -> f (GHC.L l d)
      GHC.InstD _ d       -> f (GHC.L l d)
      GHC.DerivD _ d      -> f (GHC.L l d)
      GHC.ValD _ d        -> f (GHC.L l d)
      GHC.SigD _ d        -> f (GHC.L l d)
#if __GLASGOW_HASKELL__ >= 808
      GHC.KindSigD _ d    -> f (GHC.L l d)
#endif
      GHC.DefD _ d        -> f (GHC.L l d)
      GHC.ForD _ d        -> f (GHC.L l d)
      GHC.WarningD _ d    -> f (GHC.L l d)
      GHC.AnnD _ d        -> f (GHC.L l d)
      GHC.RuleD _ d       -> f (GHC.L l d)
      GHC.SpliceD _ d     -> f (GHC.L l d)
      GHC.DocD _ d        -> f (GHC.L l d)
      GHC.RoleAnnotD _ d  -> f (GHC.L l d)
      GHC.XHsDecl _       -> error "declFun:XHsDecl"
#else
declFun f (GHC.L l de) =
  case de of
      GHC.TyClD d       -> f (GHC.L l d)
      GHC.InstD d       -> f (GHC.L l d)
      GHC.DerivD d      -> f (GHC.L l d)
      GHC.ValD d        -> f (GHC.L l d)
      GHC.SigD d        -> f (GHC.L l d)
      GHC.DefD d        -> f (GHC.L l d)
      GHC.ForD d        -> f (GHC.L l d)
      GHC.WarningD d    -> f (GHC.L l d)
      GHC.AnnD d        -> f (GHC.L l d)
      GHC.RuleD d       -> f (GHC.L l d)
      GHC.VectD d       -> f (GHC.L l d)
      GHC.SpliceD d     -> f (GHC.L l d)
      GHC.DocD d        -> f (GHC.L l d)
      GHC.RoleAnnotD d  -> f (GHC.L l d)
#if __GLASGOW_HASKELL__ < 711
      GHC.QuasiQuoteD d -> f (GHC.L l d)
#endif
#endif

-- ---------------------------------------------------------------------

-- Duplicated here so it can be used in show instances
showGhc :: (GHC.Outputable a) => a -> String
showGhc = GHC.showPpr GHC.unsafeGlobalDynFlags

-- ---------------------------------------------------------------------
