{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.GHC.ExactPrint.Pretty
--
-- This module adds default annotations to an AST fragment that does not have
-- them, to be able to exactprint it in a way that preserves the orginal AST
-- when re-parsed.
--
-----------------------------------------------------------------------------

module Language.Haskell.GHC.ExactPrint.Pretty
        (
        addAnnotationsPretty
        ) where

import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Utils
import Language.Haskell.GHC.ExactPrint.Annotate
  (AnnotationF(..), Annotated, Annotate(..), annotate)
import Language.Haskell.GHC.ExactPrint.Lookup

import Control.Monad.Identity
import Control.Monad.RWS
import Control.Monad.Trans.Free
import Data.Data (Data)
import Data.List (sortBy, elemIndex)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)

import qualified GHC

import Control.Monad.RWS
import Control.Monad.Trans.Free

import Data.Data (Data)
import Data.List (sort, nub, partition, sortBy)

import Data.Ord

import Language.Haskell.GHC.ExactPrint.Utils
#if __GLASGOW_HASKELL__ <= 710
import Language.Haskell.GHC.ExactPrint.Lookup
#endif
import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Annotate (AnnotationF(..), Annotated
                                                , annotate, Annotate(..))

import qualified GHC
import qualified SrcLoc        as GHC

import qualified Data.Map as Map
#if __GLASGOW_HASKELL__ <= 710
import qualified Data.Set as Set
#endif

-- import Debug.Trace

{-# ANN module "HLint: ignore Eta reduce" #-}

-- ---------------------------------------------------------------------

-- |Add any missing annotations so that the full AST element will exactprint
-- properly when done.
addAnnotationsPretty :: (Data a) => [Comment] -> a -> Anns -> Anns
addAnnotationsPretty cs ast = mempty

-- ---------------------------------------------------------------------
--
-- | Type used in the Pretty Monad.
type Pretty a = RWS PrettyOptions PrettyWriter PrettyState a

runPrettyWithComments :: PrettyOptions -> [Comment] -> Annotated () -> GHC.ApiAnns -> Pos -> Anns
runPrettyWithComments opts cs action ga priorEnd =
  mkAnns . snd
  . (\next -> execRWS next opts (defaultPrettyState cs priorEnd ga))
  . prettyInterpret $ action
  where
    mkAnns :: PrettyWriter -> Anns
    mkAnns = f . dwAnns
    f :: Monoid a => Endo a -> a
    f = ($ mempty) . appEndo

-- ---------------------------------------------------------------------

data PrettyOptions = PrettyOptions
       {
         -- | Current `SrcSpan, part of current AnnKey`
         curSrcSpan  :: !GHC.SrcSpan

         -- | Constuctor of current AST element, part of current AnnKey
       , annConName       :: !AnnConName

        -- | Whether to use rigid or normal layout rules
       , drRigidity :: Rigidity

       }

data PrettyWriter = PrettyWriter
       { -- | Final list of annotations, and sort keys
         dwAnns :: Endo (Map.Map AnnKey Annotation)

         -- | Used locally to pass Keywords, delta pairs relevant to a specific
         -- subtree to the parent.
       , annKds         :: ![(KeywordId, DeltaPos)]
       , sortKeys       :: !(Maybe [GHC.SrcSpan])
       , dwCapturedSpan :: !(First AnnKey)
       }

data PrettyState = PrettyState
       { -- | Position reached when processing the last element
         priorEndPosition    :: !Pos

         -- | Ordered list of comments still to be allocated
       , apComments :: ![Comment]

         -- | The original GHC Pretty Annotations
       , apAnns :: !GHC.ApiAnns

       , apMarkLayout :: Bool
       , apLayoutStart :: LayoutStartCol

       }

instance Monoid PrettyWriter where
  mempty = PrettyWriter mempty mempty mempty mempty
  (PrettyWriter a b e g) `mappend` (PrettyWriter c d f h)
    = PrettyWriter (a <> c) (b <> d) (e <> f) (g <> h)

-- ---------------------------------------------------------------------

prettyOptions :: Rigidity -> PrettyOptions
prettyOptions ridigity =
  PrettyOptions
    { curSrcSpan = GHC.noSrcSpan
    , annConName = annGetConstr ()
    , drRigidity = ridigity
    }

normalLayout :: PrettyOptions
normalLayout = prettyOptions NormalLayout

defaultPrettyState :: [Comment] -> Pos -> GHC.ApiAnns -> PrettyState
defaultPrettyState injectedComments priorEnd ga =
    PrettyState
      { priorEndPosition    = priorEnd
      , apComments = cs ++ injectedComments
      , apAnns     = ga
      , apLayoutStart = 1
      , apMarkLayout = False
      }
  where
    cs :: [Comment]
    cs = flattenedComments ga

    flattenedComments :: GHC.ApiAnns -> [Comment]
    flattenedComments (_,cm) =
      map tokComment . GHC.sortLocated . concat $ Map.elems cm

-- ---------------------------------------------------------------------
-- Free Monad Interpretation code

prettyInterpret :: Annotated a -> Pretty a
prettyInterpret = iterTM go
  where
    go :: AnnotationF (Pretty a) -> Pretty a
    go (MarkPrim kwid _ next)           = addPrettyAnnotation kwid >> next
    go (MarkEOF next)                   = addEofAnnotation >> next
    go (MarkExternal ss akwid _ next)   = addPrettyAnnotationExt ss akwid >> next
    go (MarkOutside akwid kwid next)    = addPrettyAnnotationsOutside akwid kwid >> next
    go (MarkInside akwid next)          = addPrettyAnnotationsInside akwid >> next
    go (MarkMany akwid next)            = addPrettyAnnotations akwid >> next
    go (MarkOffsetPrim akwid n _ next)  = addPrettyAnnotationLs akwid n >> next
    go (WithAST lss prog next)          = withAST lss (prettyInterpret prog) >> next
    go (CountAnns kwid next)            = countAnnsPretty kwid >>= next
    go (WithSortKey kws next)           = withSortKey kws >> next
    go (SetLayoutFlag r action next)    = do
      rigidity <- asks drRigidity
      (if (r <= rigidity) then setLayoutFlag else id) (prettyInterpret action)
      next
    go (StoreOriginalSrcSpan key next)  = storeOriginalSrcSpanPretty key >>= next
    go (GetSrcSpanForKw kw next)        = getSrcSpanForKw kw >>= next
#if __GLASGOW_HASKELL__ <= 710
    go (StoreString s ss next)          = storeString s ss >> next
#endif
    go (AnnotationsToComments kws next) = annotationsToCommentsPretty kws >> next

-- ---------------------------------------------------------------------

addEofAnnotation :: Pretty ()
addEofAnnotation = return ()

-- ---------------------------------------------------------------------

addPrettyAnnotation :: GHC.AnnKeywordId -> Pretty ()
addPrettyAnnotation ann' = return ()

-- ---------------------------------------------------------------------

addPrettyAnnotationExt :: GHC.SrcSpan -> GHC.AnnKeywordId -> Pretty ()
addPrettyAnnotationExt s ann = return ()

-- ---------------------------------------------------------------------

addPrettyAnnotationsOutside :: GHC.AnnKeywordId -> KeywordId -> Pretty ()
addPrettyAnnotationsOutside akwid kwid = return ()

-- ---------------------------------------------------------------------

addPrettyAnnotationsInside :: GHC.AnnKeywordId -> Pretty ()
addPrettyAnnotationsInside ann = return ()

-- ---------------------------------------------------------------------

addPrettyAnnotations :: GHC.AnnKeywordId -> Pretty ()
addPrettyAnnotations ann = return ()

-- ---------------------------------------------------------------------

addPrettyAnnotationLs :: GHC.AnnKeywordId -> Int -> Pretty ()
addPrettyAnnotationLs ann off = return ()

-- ---------------------------------------------------------------------

-- | Enter a new AST element. Maintain SrcSpan stack
withAST :: Data a
        => GHC.Located a
        -> Pretty b -> Pretty b
withAST lss@(GHC.L ss _) action = undefined

-- ---------------------------------------------------------------------

countAnnsPretty :: GHC.AnnKeywordId -> Pretty Int
countAnnsPretty ann = undefined

-- ---------------------------------------------------------------------

withSortKey :: [(GHC.SrcSpan, Annotated b)] -> Pretty ()
withSortKey kws = return ()

-- ---------------------------------------------------------------------

storeOriginalSrcSpanPretty :: AnnKey -> Pretty AnnKey
storeOriginalSrcSpanPretty key = return key

-- ---------------------------------------------------------------------

getSrcSpanForKw :: GHC.AnnKeywordId -> Pretty GHC.SrcSpan
getSrcSpanForKw kw = undefined

-- ---------------------------------------------------------------------

#if __GLASGOW_HASKELL__ <= 710
storeString :: String -> GHC.SrcSpan -> Pretty ()
storeString s ss = return ()
#endif

-- ---------------------------------------------------------------------

setLayoutFlag :: Pretty () -> Pretty ()
setLayoutFlag action = return ()

-- ---------------------------------------------------------------------

annotationsToCommentsPretty :: [GHC.AnnKeywordId] -> Pretty ()
annotationsToCommentsPretty kws = return ()

-- ---------------------------------------------------------------------
