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
  , AnnsUser

  , ResTyGADTHook(..)

  , Value(..)
  , AnnKey
  , AnnKeyF
  , newValue
  , typeValue
  , fromValue
  , mkAnnKeyEP
  , mkAnnKeyV
  , getAnnotationEP
  , getAndRemoveAnnotationEP
  , getAnnotationValue
  , putAnnotationValue

  ) where

import Data.Data
import Data.Maybe

import qualified Bag           as GHC
import qualified DynFlags      as GHC
import qualified FastString    as GHC
import qualified ForeignCall   as GHC
import qualified GHC           as GHC
import qualified GHC.Paths     as GHC
import qualified Lexer         as GHC
import qualified Name          as GHC
import qualified NameSet       as GHC
import qualified Outputable    as GHC
import qualified RdrName       as GHC
import qualified SrcLoc        as GHC
import qualified StringBuffer  as GHC
import qualified UniqSet       as GHC
import qualified Unique        as GHC
import qualified Var           as GHC

import qualified Data.Map as Map

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
  -- , ann_specific :: !Value
  } deriving (Show,Typeable)


instance Show GHC.RdrName where
  show n = "(a RdrName)"


type Anns = (AnnsEP,AnnsFinal)
anEP (e,_) = e
anF  (_,f) = f

-- | For every @Located a@, use the @SrcSpan@ and TypeRep of a as the
-- key, to store the standard annotation.
type AnnsEP = Map.Map (GHC.SrcSpan,TypeRep) Annotation

-- | For every SrcSpan, store an annotation as a value where the
-- TypeRep is of the item wrapped in the Value
type AnnsUser = Map.Map (GHC.SrcSpan,TypeRep) Value

type AnnsFinal = Map.Map (GHC.SrcSpan,GHC.AnnKeywordId) [DeltaPos]

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
type AnnKeyF = (GHC.SrcSpan, GHC.AnnKeywordId)

mkAnnKeyEP :: (Typeable a) => GHC.Located a -> AnnKey
mkAnnKeyEP (GHC.L l a) = (l,typeOf a)

mkAnnKeyV :: GHC.SrcSpan -> Value -> AnnKey
-- mkAnnKeyV l a = (l,typeOf (Just (typeValue a)))
mkAnnKeyV l a = (l,typeValue a)

data Value = forall a . (Eq a, Show a, Typeable a) => Value a

instance Show (Value) where
  show _ = "Value (..)"

newValue :: (Eq a, Show a, Typeable a) => a -> Value
newValue = Value

typeValue :: Value -> TypeRep
typeValue (Value x) = typeOf x

fromValue :: Typeable a => Value -> a
fromValue (Value x) = fromMaybe (error errMsg) $ res
  where
    res = cast x
    errMsg = "fromValue, bad cast from " ++ show (typeOf x)
                ++ " to " ++ show (typeOf res)

getAnnotationEP :: (Typeable a) => AnnsEP -> GHC.Located a -> Maybe Annotation
getAnnotationEP anns (GHC.L span a) = Map.lookup (span, (typeOf a)) anns

getAndRemoveAnnotationEP :: (Typeable a) => AnnsEP -> GHC.Located a -> (Maybe Annotation,AnnsEP)
getAndRemoveAnnotationEP anns (GHC.L span a)
 = case Map.lookup (span, (typeOf a)) anns of
     Nothing  -> (Nothing,anns)
     Just ann -> (Just ann,Map.delete (span, (typeOf a)) anns)

-- | Retrieve an annotation based on the SrcSpan of the annotated AST
-- element, and the known type of the annotation.
getAnnotationValue :: (Typeable a) => AnnsUser -> GHC.SrcSpan -> Maybe a
getAnnotationValue anns span = res
  where res = case  Map.lookup (span, (typeOf res)) anns of
                Nothing -> Nothing
                Just d -> Just (fromValue d)

putAnnotationValue :: (Typeable a,Show a,Eq a) => AnnsUser -> GHC.SrcSpan -> a -> AnnsUser
putAnnotationValue anns span v
  = Map.insert (span,typeOf (Just v)) (newValue v) anns


