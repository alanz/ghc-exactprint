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
  , mkAnnKeyWithD
  , AnnConName(..)
  , annGetConstr
  , Disambiguator(..)

  , ResTyGADTHook(..)
  , WildCardAnon(..)

  , getAnnotationEP
  , getAndRemoveAnnotationEP

  , LayoutFlag(..)

  , SortKey(..)

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
      commentPos :: a -- ^ Either `Span` or `DeltaPos`
    , commentContents :: String -- ^ The contents of the comment including separators
    , commentIdentifier :: GHC.SrcSpan -- ^ Needed to uniquely identify two comments with the same contents
    , commentOrigin :: (Maybe GHC.AnnKeywordId) -- ^ We sometimes turn syntax into comments in order to process them properly.
    }
  deriving (Eq,Show,Typeable,Data, Functor)

type Comment = PComment Span

type DComment = PComment DeltaPos

instance Show a => GHC.Outputable (PComment a) where
  ppr x = GHC.text (show x)

instance Ord a => Ord (PComment a) where
  compare = comparing commentPos

type PosToken = (GHC.Located GHC.Token, String)

type Pos = (Int,Int)
type Span = (Pos,Pos)

newtype DeltaPos = DP (Int,Int) deriving (Show,Eq,Ord,Typeable,Data)

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
annNone = Ann (DP (0,0)) 0  (DP (0,0)) [] [] Nothing Nothing

{-
-- TODO: This is wrong
combineAnns :: Annotation -> Annotation -> Annotation
combineAnns (Ann ed1 c1 comments1 toStart1 dps1) (Ann _ed2  _c2 _comments2 _toStart2 dps2)
  = Ann ed1 c1 comments1 toStart1 (dps1 ++ dps2)
  -- = Ann ed2 c2 comments2 toStart2 (dps1 ++ dps2)
-}

data Annotation = Ann
  {
    annEntryDelta      :: !DeltaPos -- ^ Offset used to get to the start
                                    --    of the SrcSpan.
  , annDelta           :: !ColDelta -- ^ Offset from the start of the current layout
                                   --  block. This is used when moving onto new
                                   --  lines when layout rules must be obeyed.
  , annTrueEntryDelta  :: !DeltaPos -- ^ Entry without comments
  , annPriorComments   :: ![(DComment, DeltaPos)]
  , annsDP             :: ![(KeywordId, DeltaPos)]  -- ^ Annotations associated with this element.
  , annSortKey         :: !(Maybe [GHC.SrcSpan])
    -- ^ Captures the sort order of sub elements. This is needed when the
    -- sub-elements have been split (as in a HsLocalBind which holds separate
    -- binds and sigs) or for infix patterns where the order has been
    -- re-arranged. It is captured explicitly so that after the Delta phase a
    -- SrcSpan is used purely as an index into the annotations, allowing
    -- transformations of the AST including the introduction of new Located
    -- items or re-arranging existing ones.
  , annCapturedSpan    :: !(Maybe (GHC.SrcSpan, Disambiguator))
    -- ^ Occasionally we must calculate a SrcSpan for an unlocated element
    -- which we must remember for the Print phase. TODO; Do we only have
    -- a captured span when we have a sort key?
  } deriving (Typeable,Eq)

instance Show Annotation where
  show (Ann dp c comments toStart ans sk csp) = "(Ann (" ++ show dp ++ ") " ++ show c ++ " " ++ show comments ++ " " ++ show toStart ++ " " ++ show ans ++ " " ++ showGhc sk ++ " " ++ showGhc csp ++ ")"

{-
instance Monoid Annotation where
  mempty = annNone
  mappend = combineAnns
  -}

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

modifyKeywordDeltas :: (Map.Map AnnKey Annotation -> Map.Map AnnKey Annotation)
                    -> Anns -> Anns
modifyKeywordDeltas f as = as { annsKeywordDeltas = f (annsKeywordDeltas as)}
{-
-- TODO: This should be replaced with higher level operations
modifySortKeys :: (Map.Map GHC.SrcSpan SortKey -> Map.Map GHC.SrcSpan SortKey)
               -> Anns -> Anns
modifySortKeys f as = as { annsSortKeys = f (annsSortKeys as)}
-}



-- | For every @Located a@, use the @SrcSpan@ and constructor name of
-- a as the key, to store the standard annotation.
-- These are used to maintain context in the AP and EP monads
data AnnKey   = AnnKey GHC.SrcSpan AnnConName Disambiguator
                  deriving (Eq, Ord)

-- More compact Show instance
instance Show AnnKey where
  show (AnnKey ss cn d) = "AnnKey " ++ showGhc ss ++ " " ++ show cn
                                    ++ " " ++ show d

mkAnnKey :: (Data a) => GHC.Located a -> AnnKey
mkAnnKey (GHC.L l a) = AnnKey l (annGetConstr a) NotNeeded

mkAnnKeyWithD :: (Data a) => GHC.Located a -> Disambiguator -> AnnKey
mkAnnKeyWithD (GHC.L l a) d = AnnKey l (annGetConstr a) d

-- Holds the name of a constructor
data AnnConName = CN { unConName :: String }
                 deriving (Eq,Ord)

-- More compact show instance
instance Show AnnConName where
  show (CN s) = "CN " ++ show s

annGetConstr :: (Data a) => a -> AnnConName
annGetConstr a = CN (show $ toConstr a)

-- |Under certain circumstances we can have an AST element with the same SrcSpan
-- and AnnConName. To disambiguate these we introduce this type.
data Disambiguator = NotNeeded | Ref String
                     deriving (Eq,Ord,Show)

-- |We need our own version of keywordid to manage various special cases
data KeywordId = G GHC.AnnKeywordId
               | AnnSpanEntry -- ^ Marks the entry position of a SrcSpan, does
                              -- not generate specific output but does adjust
                              -- last output position, and cause comment
                              -- processing.
               | AnnSemiSep
               | AnnComment DComment
               {-
               | AnnList GHC.SrcSpan Disambiguator -- ^ In some circumstances we
                                     -- need to annotate a list of
                                     -- statements (e.g. HsDo) and
                                     -- must synthesise a SrcSpan to
                                     -- hang the annotations off. This
                                     -- needs to be preserved so that
                                     -- exactPC can find it, after
                                     -- potential AST edits.
               -}
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
--  show (AnnList ss d)    = "(AnnList " ++ showGhc ss ++ " " ++ show d ++ ")"
  show (AnnString s)    = "(AnnString " ++ s ++ ")"
  show (AnnUnicode gc)    = "(AnnUnicode " ++ show gc ++ ")"

data LayoutFlag = LayoutRules | NoLayoutRules deriving (Show, Eq)

instance Monoid LayoutFlag where
  mempty = NoLayoutRules
  LayoutRules `mappend` _ = LayoutRules
  _ `mappend` LayoutRules = LayoutRules
  _ `mappend` _           = NoLayoutRules

-- ---------------------------------------------------------------------

-- |The SortKey is derived from the original SrcSpan.
-- It comprises the original start row and column, together with a third
-- component which is used to manage explicit sort order, to be able to always
-- insert a value between any two values.
data SortKey = SortKey (Int,Int,Rational)
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

instance GHC.Outputable SortKey where
  ppr a     = GHC.text (show a)

instance GHC.Outputable Anns where
  ppr a     = GHC.text (show a)

instance GHC.Outputable Disambiguator where
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

getAnnotationEP :: (Data a) =>  GHC.Located a -> Disambiguator -> Anns -> Maybe Annotation
getAnnotationEP  la d as =
  Map.lookup (mkAnnKeyWithD la d) (getKeywordDeltas as)

getAndRemoveAnnotationEP :: (Data a)
                         => GHC.Located a -> Disambiguator -> Anns -> (Maybe Annotation,Anns)
getAndRemoveAnnotationEP la d as
 = let key = mkAnnKeyWithD la d in
   (Map.lookup key (getKeywordDeltas as), modifyKeywordDeltas (Map.delete key) as)

-- ---------------------------------------------------------------------

-- |Show a GHC.Outputable structure
showGhc :: (GHC.Outputable a) => a -> String
showGhc = GHC.showPpr GHC.unsafeGlobalDynFlags

-- ---------------------------------------------------------------------

