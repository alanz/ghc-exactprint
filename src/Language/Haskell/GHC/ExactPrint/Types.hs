{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-} -- for GHC.DataId
module Language.Haskell.GHC.ExactPrint.Types
  (
    Comment(..)
  , DComment(..)
  , Pos
  , Span
  , PosToken
  , DeltaPos(..)
  , ColOffset,ColDelta,Col
  , Annotation(..)
  , combineAnns
  , annNone
  , Anns,AnnKey(..)
  , KeywordId(..)
  , AnnConName(..)
  , annGetConstr
  , unConName

  , ResTyGADTHook(..)

  , getAnnotationEP
  , getAndRemoveAnnotationEP

  ) where

import Data.Data
import Data.Monoid

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

data DeltaPos = DP (Int,Int) deriving (Show,Eq,Ord,Typeable,Data)
type ColOffset = Int -- ^ indentation point for a new line
type ColDelta  = Int -- ^ difference between two cols
type Col       = Int

annNone :: Annotation
annNone = Ann (DP (0,0)) 0 []

combineAnns :: Annotation -> Annotation -> Annotation
combineAnns (Ann ed1 dp1 dps1) (Ann _ed2 _dp2 dps2)
  = Ann ed1 dp1 (dps1 ++ dps2)

data Annotation = Ann
  {
    ann_entry_delta  :: !DeltaPos -- ^ Offset used to get to the start
                                  -- of the SrcSpan, during the
                                  -- annotatePC phase
  , ann_delta        :: !ColOffset -- ^ Indentation level introduced
                                   -- by this SrcSpan, for other items
                                   -- at same layout level
  , anns             :: [(KeywordId, DeltaPos)]

  } deriving (Typeable)

instance Show Annotation where
  show (Ann dp d ans) = "(Ann (" ++ show dp ++ ") " ++ show d ++ " " ++ show ans ++ ")"

instance Monoid Annotation where
  mempty = annNone
  mappend = combineAnns


instance Show GHC.RdrName where
  show n = "(a RdrName)"

type Anns = Map.Map AnnKey Annotation

-- | For every @Located a@, use the @SrcSpan@ and constructor name of
-- a as the key, to store the standard annotation.
-- These are used to maintain context in the AP and EP monads
data AnnKey   = AnnKey GHC.SrcSpan AnnConName
                  deriving (Eq, Show, Ord)
--type AnnValue = (Annotation,[(KeywordId,DeltaPos)])
--type AnnKds   = [(KeywordId,DeltaPos)]

-- Holds the name of a constructor
data AnnConName = CN String
                 deriving (Eq,Show,Ord)

annGetConstr :: (Data a) => a -> AnnConName
annGetConstr a = CN (show $ toConstr a)

unConName :: AnnConName -> String
unConName (CN s) = s

-- |We need our own version of keywordid to distinguish between a
-- semi-colon appearing within an AST element and one separating AST
-- elements in a list.
data KeywordId = G GHC.AnnKeywordId
               | AnnSemiSep
               | AnnComment DComment
               deriving (Eq,Show,Ord)

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
getAnnotationEP  (GHC.L ss a) anns = Map.lookup (AnnKey ss (annGetConstr a)) anns

getAndRemoveAnnotationEP :: (Data a)
                         => GHC.Located a -> Anns -> (Maybe Annotation,Anns)
getAndRemoveAnnotationEP (GHC.L ss a) anns
 = let key = AnnKey ss (annGetConstr a) in
    case Map.lookup key anns of
         Nothing  -> (Nothing,anns)
         Just av -> (Just av,Map.delete key anns)

-- ---------------------------------------------------------------------
