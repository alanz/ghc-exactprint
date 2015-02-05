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
  , annNone
  , Anns,AnnKey,AnnValue,AnnKds
  , KeywordId(..)
  , AnnConName(..)
  , annGetConstr
  , unConName

  , ResTyGADTHook(..)

  , getAnnotationEP
  , getAndRemoveAnnotationEP

  ) where

import Data.Data

import qualified GHC           as GHC
import qualified Outputable    as GHC

import qualified Data.Map as Map

-- ---------------------------------------------------------------------

-- | A Haskell comment. The 'Bool' is 'True' if the comment is multi-line, i.e. @{- -}@.
data Comment = Comment Bool Span String
  deriving (Eq,Show,Typeable,Data)

-- |Delta version of the comment. The initial Int is the column offset
-- that was force when the DeltaPos values were calculated. If this is
-- different when it is output, they deltas must be updated.
data DComment = DComment Int Bool (DeltaPos,DeltaPos) String
  deriving (Eq,Show,Typeable,Data)

instance Ord Comment where
  compare (Comment _ p1 _) (Comment _ p2 _) = compare p1 p2

type PosToken = (GHC.Located GHC.Token, String)

type Pos = (Int,Int)
type Span = (Pos,Pos)

data DeltaPos = DP (Int,Int) deriving (Show,Eq,Ord,Typeable,Data)
type ColOffset = Int -- ^ indentation point for a new line
type ColDelta  = Int -- ^ difference between two cols
type Col       = Int

annNone :: Annotation
annNone = Ann [] (DP (0,0)) 0


data Annotation = Ann
  { ann_comments     :: ![DComment]
  , ann_entry_delta  :: !DeltaPos -- ^ Offset used to get to the start
                                  -- of the SrcSpan, during the
                                  -- annotatePC phase
  , ann_delta        :: !ColOffset

  } deriving (Show,Typeable)

instance Show GHC.RdrName where
  show n = "(a RdrName)"

type Anns = Map.Map AnnKey AnnValue
-- | For every @Located a@, use the @SrcSpan@ and constructor name of
-- a as the key, to store the standard annotation.
-- These are used to maintain context in the AP and EP monads
type AnnKey   = (GHC.SrcSpan,AnnConName)
type AnnValue = (Annotation,[(KeywordId,DeltaPos)])
type AnnKds   = [(KeywordId,DeltaPos)]

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
               deriving (Eq,Show,Ord)

-- ---------------------------------------------------------------------

instance GHC.Outputable KeywordId where
  ppr k     = GHC.text (show k)

instance GHC.Outputable (AnnConName) where
  ppr tr     = GHC.text (show tr)

instance GHC.Outputable Annotation where
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

getAnnotationEP :: (Data a) => Anns -> GHC.Located a -> Maybe Annotation
getAnnotationEP anns (GHC.L ss a)
  = case Map.lookup (ss, annGetConstr a) anns of
      Nothing -> Nothing
      Just (an,_kds) -> Just an

getAndRemoveAnnotationEP :: (Data a)
                         => Anns -> GHC.Located a -> (Maybe AnnValue,Anns)
getAndRemoveAnnotationEP anns (GHC.L ss a)
 = case Map.lookup (ss, annGetConstr a) anns of
     Nothing  -> (Nothing,anns)
     Just av -> (Just av,Map.delete (ss, annGetConstr a) anns)

-- ---------------------------------------------------------------------
