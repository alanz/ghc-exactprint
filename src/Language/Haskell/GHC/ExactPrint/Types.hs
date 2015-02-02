{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-} -- for GHC.DataId
module Language.Haskell.GHC.ExactPrint.Types
  (
    Comment(..)
  , DComment(..)
  , Pos
  , Span
  , PosToken
  , DeltaPos(..)
  , ColOffset,Col
  , Annotation(..)
  , annNone
  , Anns,anEP,anF
  , AnnsEP
  , AnnsFinal
  , KeywordId(..)
  , AnnConName(..)
  , annGetConstr
  , unConName

  , ResTyGADTHook(..)

  , AnnKey
  , AnnKeyF
  , mkAnnKeyEP
  , getAnnotationEP
  , getAndRemoveAnnotationEP

  , Value(..)
  , newValue
  , fromValue
  , emptyValue
  , isEmptyValue
  ) where

import Data.Data
import Data.Maybe

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
type ColOffset = Int
type Col       = Int

annNone :: Annotation
annNone = Ann [] 0 (DP (0,0)) 0

emptyValue :: Value
emptyValue = newValue (Just () :: Maybe ())

isEmptyValue :: Value -> Bool
isEmptyValue v = vv == Just ()
  where
    vv = fromValue v :: Maybe ()

data Annotation = Ann
  { ann_comments     :: ![DComment]
  , ann_end_col      :: !Col -- ^ prior end column at point annotation was captured
  , ann_nested_delta :: !DeltaPos
  , ann_delta        :: !ColOffset

  } deriving (Show,Typeable)

-- instance (Show a) => Show (GHC.Located a) where
--   show (GHC.L l a) = "L " ++ show l ++ " " ++ show a

instance Show GHC.RdrName where
  show n = "(a RdrName)"

-- TODO:AZ change Anns into
--   M.Map (SrcSpan, AnnConName) (Annotation,[(KeywordId, DeltaPos)])
-- with the list of (KeywordId, DeltaPos) ordered by original
-- occurence in the source

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

-- ---------------------------------------------------------------------
-- Based on
-- https://github.com/ndmitchell/shake/blob/master/Development/Shake/Value.hs


-- We deliberately avoid Typeable instances on Key/Value to stop them
-- accidentally being used inside themselves

newtype Key = Key Value
  -- deriving (Eq)

data Value = forall a . (Show a,Typeable a, GHC.Outputable a) => Value a

newValue :: (Show a, Typeable a, GHC.Outputable a) => a -> Value
newValue = Value

typeValue :: Value -> TypeRep
typeValue (Value x) = typeOf x

fromValue :: Typeable a => Value -> a
fromValue (Value x) = fromMaybe (error errMsg) $ res
  where
   res = cast x
   errMsg = "fromValue, bad cast from " ++ show (typeOf x)
          ++ " to " ++ show (typeOf res)

instance Show Key where
  show (Key a) = show a

instance Show Value where
  show (Value a) = show a

instance GHC.Outputable Value where
  ppr (Value a) = GHC.ppr a

{-
instance Eq Value where
  Value a == Value b = maybe False (a ==) $ cast b
  Value a /= Value b = maybe True (a /=) $ cast b
-}

-- ---------------------------------------------------------------------
