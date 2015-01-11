{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
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
  , KeywordId(..)

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

-- | For every @Located a@, use the @SrcSpan@ and TypeRep of a as the
-- key, to store the standard annotation.
type AnnsEP = Map.Map (GHC.SrcSpan,TypeRep) Annotation

-- | For every SrcSpan, store an annotation as a value where the
-- TypeRep is of the item wrapped in the Value
-- type AnnsUser = Map.Map (GHC.SrcSpan,TypeRep) Value

type AnnsFinal = Map.Map (GHC.SrcSpan,KeywordId) [DeltaPos]

-- We need our own version of keywordid to distinguish between a
-- semi-colon appearing within an AST element and one separating AST
-- elements in a list.
data KeywordId = G GHC.AnnKeywordId
               | AnnSemiSep
               deriving (Eq,Show,Ord)

instance GHC.Outputable KeywordId where
  ppr k     = GHC.text (show k)

instance GHC.Outputable TypeRep where
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

-- ---------------------------------------------------------------------
{-

Rationale.

We need an offset for every SrcSpan, as well as the comments
associated with it.

We also need optional other annotations for keywords etc.

So perhaps one AnnKey based on the typeRep of a in (Located a), and
another for the user annotation.


-}

type AnnKey  = (GHC.SrcSpan, TypeRep)
type AnnKeyF = (GHC.SrcSpan, KeywordId)

mkAnnKeyEP :: (Typeable a) => GHC.Located a -> AnnKey
mkAnnKeyEP (GHC.L l a) = (l,typeOf a)

getAnnotationEP :: (Typeable a) => AnnsEP -> GHC.Located a -> Maybe Annotation
getAnnotationEP anns (GHC.L ss a) = Map.lookup (ss, (typeOf a)) anns

getAndRemoveAnnotationEP :: (Typeable a) => AnnsEP -> GHC.Located a -> (Maybe Annotation,AnnsEP)
getAndRemoveAnnotationEP anns (GHC.L ss a)
 = case Map.lookup (ss, (typeOf a)) anns of
     Nothing  -> (Nothing,anns)
     Just ann -> (Just ann,Map.delete (ss, (typeOf a)) anns)

