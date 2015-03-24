{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-} -- for GHC.DataId
module Language.Haskell.GHC.ExactPrint.Types
  (
    Comment(..)
  , DComment(..)
  , Pos
  , Span
  , PosToken
  , DeltaPos(..)
  , ColOffset(..) , ColDelta(..)
  , Annotation(..)
  , combineAnns
  , annNone
  , Anns,AnnKey(..)
  , KeywordId(..)
  , mkAnnKey
  , AnnConName(..)
  , annGetConstr

  , ResTyGADTHook(..)

  , getAnnotationEP
  , getAndRemoveAnnotationEP

  , LayoutFlag(..)

  ) where

import Data.Data (Data, Typeable, toConstr)

import qualified GHC           as GHC
import qualified Outputable    as GHC

import qualified Data.Map as Map

-- ---------------------------------------------------------------------

-- | A Haskell comment.
data Comment = Comment Span String
  deriving (Eq,Show,Typeable,Data)

instance GHC.Outputable Comment where
  ppr x = GHC.text (show x)

-- |Delta version of the comment.
data DComment = DComment (DeltaPos,DeltaPos) String
  deriving (Eq,Show,Typeable,Data,Ord)

instance Ord Comment where
  compare (Comment p1 _) (Comment p2 _) = compare p1 p2

type PosToken = (GHC.Located GHC.Token, String)

type Pos = (Int,Int)
type Span = (Pos,Pos)

newtype DeltaPos = DP (Int,Int) deriving (Show,Eq,Ord,Typeable,Data)

-- | Marks the start of a layout block.
newtype ColOffset = ColOffset { getColOffset :: Int }
  deriving (Eq, Show, Num)
-- | Marks the distance from the start of the layout block to the element.
newtype ColDelta  = ColDelta { getColDelta :: Int }
  deriving (Eq, Show, Num)

annNone :: Annotation
annNone = Ann (DP (0,0)) 0 []

combineAnns :: Annotation -> Annotation -> Annotation
combineAnns (Ann ed1 c1 dps1) (Ann _ed2  _c2  dps2)
  = Ann ed1 c1 (dps1 ++ dps2)

data Annotation = Ann
  {
    annEntryDelta      :: !DeltaPos -- ^ Offset used to get to the start
                                    --    of the SrcSpan.
  , annDelta           :: !ColDelta -- ^ Offset from the start of the current layout
                                   --  block. This is used when moving onto new
                                   --  lines when layout rules must be obeyed.
  , annsDP             :: [(KeywordId, DeltaPos)]  -- ^ Annotations associated with this element.

  } deriving (Typeable,Eq)

instance Show Annotation where
  show (Ann dp c ans) = "(Ann (" ++ show dp ++ ") " ++ show c ++ " " ++ " " ++ show ans ++ ")"

instance Monoid Annotation where
  mempty = annNone
  mappend = combineAnns

type Anns = Map.Map AnnKey Annotation

-- | For every @Located a@, use the @SrcSpan@ and constructor name of
-- a as the key, to store the standard annotation.
-- These are used to maintain context in the AP and EP monads
data AnnKey   = AnnKey GHC.SrcSpan AnnConName
                  deriving (Eq, Show, Ord)

mkAnnKey :: (Data a) => GHC.Located a -> AnnKey
mkAnnKey (GHC.L l a) = AnnKey l (annGetConstr a)

-- Holds the name of a constructor
data AnnConName = CN { unConName :: String }
                 deriving (Eq,Show,Ord)

annGetConstr :: (Data a) => a -> AnnConName
annGetConstr a = CN (show $ toConstr a)

-- |We need our own version of keywordid to distinguish between a
-- semi-colon appearing within an AST element and one separating AST
-- elements in a list.
data KeywordId = G GHC.AnnKeywordId
               | AnnSemiSep
               | AnnComment DComment
               | AnnList GHC.SrcSpan -- ^ In some circumstances we
                                     -- need to annotate a list of
                                     -- statements (e.g. HsDo) and
                                     -- must synthesise a SrcSpan to
                                     -- hang the annotations off. This
                                     -- needs to be preserved so that
                                     -- exactPC can find it, after
                                     -- potential AST edits.
               deriving (Eq,Show,Ord)

data LayoutFlag = LayoutRules | NoLayoutRules deriving (Show, Eq)

instance Monoid LayoutFlag where
  mempty = NoLayoutRules
  LayoutRules `mappend` _ = LayoutRules
  _ `mappend` LayoutRules = LayoutRules
  _ `mappend` _           = NoLayoutRules

-- ---------------------------------------------------------------------

instance GHC.Outputable KeywordId where
  ppr k     = GHC.text (show k)

instance GHC.Outputable (AnnConName) where
  ppr tr     = GHC.text (show tr)

instance GHC.Outputable Annotation where
  ppr a     = GHC.text (show a)

instance GHC.Outputable AnnKey where
  ppr a     = GHC.text (show a)

instance GHC.Outputable DeltaPos where
  ppr a     = GHC.text (show a)

-- ---------------------------------------------------------------------

-- ResTyGADT has a SrcSpan for the original sigtype, we need to create
-- a type for exactPC and annotatePC
data ResTyGADTHook name = ResTyGADTHook [GHC.LHsTyVarBndr name]
                   deriving (Typeable)
deriving instance (GHC.DataId name) => Data (ResTyGADTHook name)
deriving instance (Show (GHC.LHsTyVarBndr name)) => Show (ResTyGADTHook name)

instance (GHC.OutputableBndr name) => GHC.Outputable (ResTyGADTHook name) where
  ppr (ResTyGADTHook bs) = GHC.text "ResTyGADTHook" GHC.<+> GHC.ppr bs

-- ---------------------------------------------------------------------

getAnnotationEP :: (Data a) =>  GHC.Located a -> Anns -> Maybe Annotation
getAnnotationEP  (GHC.L ss a) annotations =
  Map.lookup (AnnKey ss (annGetConstr a)) annotations

getAndRemoveAnnotationEP :: (Data a)
                         => GHC.Located a -> Anns -> (Maybe Annotation,Anns)
getAndRemoveAnnotationEP (GHC.L ss a) annotations
 = let key = AnnKey ss (annGetConstr a) in
    case Map.lookup key annotations of
         Nothing  -> (Nothing, annotations)
         Just av -> (Just av, Map.delete key annotations)

-- ---------------------------------------------------------------------
