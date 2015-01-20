{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
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
  , Annotation(..)
  , annNone
  , Anns,anEP,anF
  , AnnsEP
  , AnnsFinal
  , KeywordId(..)
  , AnnConName
  , annGetConstr
  , unConName

  , ResTyGADTHook(..)

  , AnnKey
  , AnnKeyF
  , mkAnnKeyEP
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

data DComment = DComment Bool (DeltaPos,DeltaPos) String
  deriving (Eq,Show,Typeable,Data)

instance Ord Comment where
  compare (Comment _ p1 _) (Comment _ p2 _) = compare p1 p2

type PosToken = (GHC.Located GHC.Token, String)

type Pos = (Int,Int)
type Span = (Pos,Pos)

newtype DeltaPos = DP (Int,Int) deriving (Show,Eq,Ord,Typeable,Data)

annNone :: Annotation
annNone = Ann [] (DP (0,0))

data Annotation = Ann
  { ann_comments :: ![DComment]
  , ann_delta    :: !DeltaPos -- Do we need this? Yes indeed.
  } deriving (Show,Typeable)


instance Show GHC.RdrName where
  show n = "(a RdrName)"

-- first field carries the comments, second the offsets
type Anns = (AnnsEP,AnnsFinal)
anEP :: Anns -> AnnsEP
anEP (e,_) = e
anF :: Anns -> AnnsFinal
anF  (_,f) = f

-- Holds the name of a constructor
data AnnConName = CN String
                 deriving (Eq,Show,Ord)

annGetConstr :: (Data a) => a -> AnnConName
annGetConstr a = CN (show $ toConstr a)

unConName :: AnnConName -> String
unConName (CN s) = s


-- | For every @Located a@, use the @SrcSpan@ and constructor name of
-- a as the key, to store the standard annotation.
-- These are used to maintain context in the AP and EP monads
type AnnsEP = Map.Map (GHC.SrcSpan,AnnConName) Annotation
type AnnKey =         (GHC.SrcSpan,AnnConName)

-- | The offset values used for actually outputing the source. For a
-- given @'SrcSpan'@, in a context managed by the AP or EP monads,
-- store a list of offsets for a particular KeywordId. Mostly there
-- will only be one, but in certain circumstances they are multiple,
-- e.g. semi colons as separators, which can be repeated.
type AnnsFinal = Map.Map (GHC.SrcSpan,KeywordId) [DeltaPos]
type AnnKeyF   =         (GHC.SrcSpan,KeywordId)

-- |We need our own version of keywordid to distinguish between a
-- semi-colon appearing within an AST element and one separating AST
-- elements in a list.
data KeywordId = G GHC.AnnKeywordId
               | AnnSemiSep
               -- Used for managing indentation
               | AnnGroupOffset
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

mkAnnKeyEP :: (Data a) => GHC.Located a -> AnnKey
mkAnnKeyEP (GHC.L l a) = (l,annGetConstr a)

getAnnotationEP :: (Data a) => AnnsEP -> GHC.Located a -> Maybe Annotation
getAnnotationEP anns (GHC.L ss a) = Map.lookup (ss, annGetConstr a) anns

getAndRemoveAnnotationEP :: (Data a)
                         => AnnsEP -> GHC.Located a -> (Maybe Annotation,AnnsEP)
getAndRemoveAnnotationEP anns (GHC.L ss a)
 = case Map.lookup (ss, annGetConstr a) anns of
     Nothing  -> (Nothing,anns)
     Just ann -> (Just ann,Map.delete (ss, annGetConstr a) anns)
