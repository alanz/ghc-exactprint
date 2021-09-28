{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns         #-}

module Language.Haskell.GHC.ExactPrint.Types
  where

import Data.Data hiding (Fixity)
import GHC hiding (EpaComment)
import GHC.Utils.Outputable hiding ( (<>) )

-- ---------------------------------------------------------------------

type Pos = (Int,Int)

-- ---------------------------------------------------------------------

data Rigidity = NormalLayout | RigidLayout deriving (Eq, Ord, Show)

-- ---------------------------------------------------------------------

-- | A Haskell comment. The @AnnKeywordId@ is present if it has been converted
-- from an @AnnKeywordId@ because the annotation must be interleaved into the
-- stream and does not have a well-defined position
data Comment = Comment
    {
      commentContents   :: !String -- ^ The contents of the comment including separators

    -- AZ:TODO: commentIdentifier is a misnomer, should be commentSrcSpan, it is
    -- the thing we use to decide where in the output stream the comment should
    -- go.
    , commentAnchor :: !Anchor
    , commentOrigin :: !(Maybe AnnKeywordId) -- ^ We sometimes turn syntax into comments in order to process them properly.
    }
  deriving (Data, Eq)

instance Show Comment where
  show (Comment cs ss o) = "(Comment " ++ show cs ++ " " ++ showPprUnsafe ss ++ " " ++ show o ++ ")"

instance Ord Comment where
  -- When we have CPP injected comments with a fake filename, or LINE
  -- pragma, the file name changes, so we need to compare the
  -- locations only, with out the filename.
  compare (Comment _ ss1 _) (Comment _ ss2 _) = compare (ss2pos $ anchor ss1) (ss2pos $ anchor ss2)
    where
      ss2pos ss = (srcSpanStartLine ss,srcSpanStartCol ss)

instance Outputable Comment where
  ppr x = text (show x)

-- | The different syntactic elements which are not represented in the
-- AST.
-- TODO:AZ: check which if these are still in use
data KeywordId = G AnnKeywordId  -- ^ A normal keyword
               | AnnSemiSep          -- ^ A separating comma
               | AnnTypeApp          -- ^ Visible type application annotation
               | AnnComment Comment
               | AnnString String    -- ^ Used to pass information from
                                     -- Delta to Print when we have to work
                                     -- out details from the original
                                     -- SrcSpan.
               deriving (Eq)

instance Show KeywordId where
  show (G gc)          = "(G " ++ show gc ++ ")"
  show AnnSemiSep      = "AnnSemiSep"
  show AnnTypeApp      = "AnnTypeApp"
  show (AnnComment dc) = "(AnnComment " ++ show dc ++ ")"
  show (AnnString s)   = "(AnnString " ++ s ++ ")"

-- | Marks the start column of a layout block.
newtype LayoutStartCol = LayoutStartCol { getLayoutStartCol :: Int }
  deriving (Eq, Num)

instance Show LayoutStartCol where
  show (LayoutStartCol sc) = "(LayoutStartCol " ++ show sc ++ ")"

-- ---------------------------------------------------------------------

-- Duplicated here so it can be used in show instances
showGhc :: (Outputable a) => a -> String
showGhc = showPprUnsafe
