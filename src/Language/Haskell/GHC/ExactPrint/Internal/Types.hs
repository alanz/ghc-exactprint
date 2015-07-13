{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-} -- for GHC.DataId
module Language.Haskell.GHC.ExactPrint.Internal.Types
  (
    Comment
  , DComment
  , PComment(..)
  , Pos
  , Span
  , PosToken
  , DeltaPos(..)
  , deltaRow, deltaColumn
  , addDP
  , LayoutStartCol(..) , ColDelta(..)
  , Annotation(..)
  , annNone
  , Anns(..),AnnKey(..)
  , emptyAnns
  , getKeywordDeltas
  , modifyKeywordDeltas
  , KeywordId(..)
  , mkAnnKey
  , AnnConName(..)
  , annGetConstr

  , ResTyGADTHook(..)
  , WildCardAnon(..)

  , getAnnotationEP
  , getAndRemoveAnnotationEP

  -- , SortKey(..)

  , showGhc
  ) where

import Data.Data (Data, Typeable, toConstr)

import qualified DynFlags       as GHC
import qualified GHC
import qualified Outputable    as GHC

import qualified Data.Map as Map
import Data.Ord

-- ---------------------------------------------------------------------

-- | A Haskell comment. The @AnnKeywordId@ is present if it has been converted
-- from an @AnnKeywordId@ because the annotation must be interleaved into the
-- stream and does not have a well-defined position
data PComment a = Comment
    {
      commentPos        :: !a -- ^ Either `Span` for end or `DeltaPos` giving offset to end
    , commentContents   :: !String -- ^ The contents of the comment including separators
    , commentIdentifier :: !GHC.SrcSpan -- ^ Needed to uniquely identify two comments with the same contents
    , commentOrigin     :: !(Maybe GHC.AnnKeywordId) -- ^ We sometimes turn syntax into comments in order to process them properly.
    }
  deriving (Eq,Show,Typeable,Data, Functor)

type Comment  = PComment Span

type DComment = PComment DeltaPos

instance Show a => GHC.Outputable (PComment a) where
  ppr x = GHC.text (show x)

instance Ord a => Ord (PComment a) where
  compare = comparing commentPos

type PosToken = (GHC.Located GHC.Token, String)

type Pos = (Int,Int)
type Span = (Pos,Pos)

-- | A relative positions, row then column
newtype DeltaPos = DP (Int,Int) deriving (Show,Eq,Ord,Typeable,Data)

deltaRow, deltaColumn :: DeltaPos -> Int
deltaRow (DP (r, _)) = r
deltaColumn (DP (_, c)) = c

addDP :: DeltaPos -> DeltaPos -> DeltaPos
addDP (DP (a, b)) (DP (c, d)) =
  if c >= 1 then DP (a+c, d)
            else DP (a, b + d)


-- | Marks the start column of a layout block.
newtype LayoutStartCol = LayoutStartCol { getLayoutStartCol :: Int }
  deriving (Eq, Num)

instance Show LayoutStartCol where
  show (LayoutStartCol sc) = "(LayoutStartCol " ++ show sc ++ ")"


-- | Marks the distance from the start of the layout block to the element.
newtype ColDelta  = ColDelta { getColDelta :: Int }
  deriving (Eq, Num)

instance Show ColDelta where
  show (ColDelta v) = "(ColDelta " ++ show v ++ ")"

annNone :: Annotation
annNone = Ann (DP (0,0)) 0  (DP (0,0)) [] [] [] Nothing Nothing

data Annotation = Ann
  {
    annEntryDelta      :: !DeltaPos
    -- ^ Offset used to get to the start of the SrcSpan, from whatever the prior
    -- output was, including all annPriorComments (field below).
  , annDelta           :: !ColDelta
    -- ^ Offset from the start of the current layout block. This is used when
    -- moving onto new lines when layout rules must be obeyed.
  , annTrueEntryDelta  :: !DeltaPos
    -- ^ Offset from the previous SrcSpan, ignoring whitespace output such as
    -- comments. This is required for managing the annDelta for nested AST
    -- elements. If there are no intervening comments this will be the same as
    -- annEntryDelta.
  , annPriorComments   :: ![(DComment,  DeltaPos)]
    -- ^ Comments coming after the last non-comment output of the preceding
    -- element but before the SrcSpan being annotated by this Annotation. If
    -- these are changed then annEntryDelta (field above) must also change to
    -- match.
  , annFollowingComments   :: ![(DComment,  DeltaPos)]
    -- ^ Comments coming after the last output for the element subject to this
    -- Annotation. These will only be added by AST transformations, and care
    -- must be taken not to disturb layout of following elements.
  , annsDP             :: ![(KeywordId, DeltaPos)]
    -- ^ Annotations associated with this element.
  , annSortKey         :: !(Maybe [GHC.SrcSpan])
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
  show (Ann dp c comments fcomments toStart ans sk csp)
    = "(Ann (" ++ show dp ++ ") " ++ show c ++ " " ++ show comments ++ " "
        ++ show fcomments ++ " "
        ++ show toStart ++ " " ++ show ans ++ " " ++ showGhc sk ++ " "
        ++ showGhc csp ++ ")"

-----
-- Anns is kept abstract so that the sortKeys can't be modified
--

-- ++AZ++ TODO: this type can be simplified now. At least rename the record
-- accessor to simply 'anns'
data Anns = Anns
  { annsKeywordDeltas :: Map.Map AnnKey Annotation
  } deriving (Show, Typeable)

emptyAnns :: Anns
emptyAnns = Anns Map.empty

getKeywordDeltas :: Anns -> Map.Map AnnKey Annotation
getKeywordDeltas = annsKeywordDeltas

-- TODO: This should perhaps be called modifyAnns?
modifyKeywordDeltas :: (Map.Map AnnKey Annotation -> Map.Map AnnKey Annotation)
                    -> Anns -> Anns
modifyKeywordDeltas f as = as { annsKeywordDeltas = f (annsKeywordDeltas as)}

-- | For every @Located a@, use the @SrcSpan@ and constructor name of
-- a as the key, to store the standard annotation.
-- These are used to maintain context in the AP and EP monads
data AnnKey   = AnnKey GHC.SrcSpan AnnConName
                  deriving (Eq, Ord)

-- More compact Show instance
instance Show AnnKey where
  show (AnnKey ss cn) = "AnnKey " ++ showGhc ss ++ " " ++ show cn

mkAnnKey :: (Data a) => GHC.Located a -> AnnKey
mkAnnKey (GHC.L l a) = AnnKey l (annGetConstr a)



-- Holds the name of a constructor
data AnnConName = CN { unConName :: String }
                 deriving (Eq,Ord)

-- More compact show instance
instance Show AnnConName where
  show (CN s) = "CN " ++ show s

annGetConstr :: (Data a) => a -> AnnConName
annGetConstr a = CN (show $ toConstr a)

-- |We need our own version of keywordid to manage various special cases
data KeywordId = G GHC.AnnKeywordId
               | AnnSpanEntry -- ^ Marks the entry position of a SrcSpan, does
                              -- not generate specific output but does adjust
                              -- last output position, and cause comment
                              -- processing.
               | AnnSemiSep
               | AnnComment DComment
               | AnnString String    -- ^ Used to pass information from
                                     -- Delta to Print when we have to work
                                     -- out details from the original
                                     -- SrcSpan.
               | AnnUnicode GHC.AnnKeywordId -- ^ Used to indicate that we should print using unicode syntax if possible.
               deriving (Eq,Ord)

instance Show KeywordId where
  show (G gc)          = "(G " ++ show gc ++ ")"
  show AnnSpanEntry    = "AnnSpanEntry"
  show AnnSemiSep      = "AnnSemiSep"
  show (AnnComment dc) = "(AnnComment " ++ show dc ++ ")"
  show (AnnString s)   = "(AnnString " ++ s ++ ")"
  show (AnnUnicode gc) = "(AnnUnicode " ++ show gc ++ ")"

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

instance GHC.Outputable Anns where
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

data WildCardAnon = WildCardAnon deriving (Show,Data,Typeable)

-- ---------------------------------------------------------------------

getAnnotationEP :: (Data a) =>  GHC.Located a  -> Anns -> Maybe Annotation
getAnnotationEP  la as =
  Map.lookup (mkAnnKey la) (getKeywordDeltas as)

getAndRemoveAnnotationEP :: (Data a)
                         => GHC.Located a -> Anns -> (Maybe Annotation,Anns)
getAndRemoveAnnotationEP la as
 = let key = mkAnnKey la in
   (Map.lookup key (getKeywordDeltas as), modifyKeywordDeltas (Map.delete key) as)

-- ---------------------------------------------------------------------

-- |Show a GHC.Outputable structure
showGhc :: (GHC.Outputable a) => a -> String
showGhc = GHC.showPpr GHC.unsafeGlobalDynFlags

-- ---------------------------------------------------------------------

