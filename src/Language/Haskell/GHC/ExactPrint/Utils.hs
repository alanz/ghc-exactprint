{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Haskell.GHC.ExactPrint.Utils
  -- (
  --  -- * Manipulating Positons
  --   ss2pos
  -- , ss2posEnd
  -- , undelta
  -- , isPointSrcSpan
  -- , pos2delta
  -- , ss2delta
  -- , addDP
  -- , spanLength
  -- , isGoodDelta
  -- ) where
  where
import Control.Monad.State
import Data.Function
import Data.Ord (comparing)

import Data.Generics

import GHC.Hs.Dump
import Language.Haskell.GHC.ExactPrint.Lookup

import GHC hiding (EpaComment)
import qualified GHC
import GHC.Types.Name
import GHC.Types.Name.Reader
import GHC.Types.SrcLoc
import GHC.Data.FastString
import GHC.Utils.Outputable (showSDocUnsafe, showPprUnsafe)

import Control.Arrow

import Data.List (sortBy, elemIndex)

import Debug.Trace
import Language.Haskell.GHC.ExactPrint.Types

-- ---------------------------------------------------------------------

-- |Global switch to enable debug tracing in ghc-exactprint Delta / Print
debugEnabledFlag :: Bool
-- debugEnabledFlag = True
debugEnabledFlag = False

-- |Global switch to enable debug tracing in ghc-exactprint Pretty
debugPEnabledFlag :: Bool
debugPEnabledFlag = True
-- debugPEnabledFlag = False

-- |Provide a version of trace that comes at the end of the line, so it can
-- easily be commented out when debugging different things.
debug :: c -> String -> c
debug c s = if debugEnabledFlag
              then trace s c
              else c

-- |Provide a version of trace for the Pretty module, which can be enabled
-- separately from 'debug' and 'debugM'
debugP :: String -> c -> c
debugP s c = if debugPEnabledFlag
               then trace s c
               else c

debugM :: Monad m => String -> m ()
debugM s = when debugEnabledFlag $ traceM s


-- ---------------------------------------------------------------------

warn :: c -> String -> c
-- warn = flip trace
warn c _ = c

-- | A good delta has no negative values.
isGoodDelta :: DeltaPos -> Bool
isGoodDelta (SameLine co) = co >= 0
isGoodDelta (DifferentLine ro co) = ro > 0 && co >= 0
  -- Note: DifferentLine invariant is ro is nonzero and positive


-- | Create a delta from the current position to the start of the given
-- @SrcSpan@.
ss2delta :: Pos -> RealSrcSpan -> DeltaPos
ss2delta ref ss = pos2delta ref (ss2pos ss)

-- | create a delta from the end of a current span.  The +1 is because
-- the stored position ends up one past the span, this is prior to
-- that adjustment
ss2deltaEnd :: RealSrcSpan -> RealSrcSpan -> DeltaPos
ss2deltaEnd rrs ss = ss2delta ref ss
  where
    (r,c) = ss2posEnd rrs
    ref = if r == 0
             then (r,c+1)
             else (r,c)

-- | create a delta from the start of a current span.  The +1 is
-- because the stored position ends up one past the span, this is
-- prior to that adjustment
ss2deltaStart :: RealSrcSpan -> RealSrcSpan -> DeltaPos
ss2deltaStart rrs ss = ss2delta ref ss
  where
    (r,c) = ss2pos rrs
    ref = if r == 0
             then (r,c)
             else (r,c)

-- | Convert the start of the second @Pos@ to be an offset from the
-- first. The assumption is the reference starts before the second @Pos@
pos2delta :: Pos -> Pos -> DeltaPos
pos2delta (refl,refc) (l,c) = deltaPos lo co
  where
    lo = l - refl
    co = if lo == 0 then c - refc
                    else c

-- | Apply the delta to the current position, taking into account the
-- current column offset if advancing to a new line
undelta :: Pos -> DeltaPos -> LayoutStartCol -> Pos
undelta (l,c) (SameLine dc)         (LayoutStartCol _co) = (l, c + dc)
undelta (l,_) (DifferentLine dl dc) (LayoutStartCol co) = (fl,fc)
  where
    -- Note: invariant: dl > 0
    fl = l + dl
    fc = co + dc

undeltaSpan :: RealSrcSpan -> AnnKeywordId -> DeltaPos -> AddEpAnn
undeltaSpan anchor kw dp = AddEpAnn kw (EpaSpan sp)
  where
    (l,c) = undelta (ss2pos anchor) dp (LayoutStartCol 0)
    len = length (keywordToString (G kw))
    sp = range2rs ((l,c),(l,c+len))

-- ---------------------------------------------------------------------

adjustDeltaForOffset :: Int -> LayoutStartCol -> DeltaPos -> DeltaPos
adjustDeltaForOffset _ _colOffset                      dp@(SameLine _) = dp
adjustDeltaForOffset d (LayoutStartCol colOffset) (DifferentLine l c)
  = DifferentLine l (c - colOffset - d)

-- ---------------------------------------------------------------------

ss2pos :: RealSrcSpan -> Pos
ss2pos ss = (srcSpanStartLine ss,srcSpanStartCol ss)

ss2posEnd :: RealSrcSpan -> Pos
ss2posEnd ss = (srcSpanEndLine ss,srcSpanEndCol ss)

ss2range :: SrcSpan -> (Pos,Pos)
ss2range ss = (ss2pos $ rs ss, ss2posEnd $ rs ss)

rs2range :: RealSrcSpan -> (Pos,Pos)
rs2range ss = (ss2pos ss, ss2posEnd ss)

rs :: SrcSpan -> RealSrcSpan
rs (RealSrcSpan s _) = s
rs _ = badRealSrcSpan

range2rs :: (Pos,Pos) -> RealSrcSpan
range2rs (s,e) = mkRealSrcSpan (mkLoc s) (mkLoc e)
  where
    mkLoc (l,c) = mkRealSrcLoc (fsLit "ghc-exactprint") l c

badRealSrcSpan :: RealSrcSpan
badRealSrcSpan = mkRealSrcSpan bad bad
  where
    bad = mkRealSrcLoc (fsLit "ghc-exactprint-nospan") 0 0

spanLength :: RealSrcSpan -> Int
spanLength = (-) <$> srcSpanEndCol <*> srcSpanStartCol

-- ---------------------------------------------------------------------
-- | Checks whether a SrcSpan has zero length.
isPointSrcSpan :: RealSrcSpan -> Bool
isPointSrcSpan ss = spanLength ss == 0
                  && srcSpanStartLine ss == srcSpanEndLine ss

-- ---------------------------------------------------------------------

-- |Given a list of items and a list of keys, returns a list of items
-- ordered by their position in the list of keys.
orderByKey :: [(RealSrcSpan,a)] -> [RealSrcSpan] -> [(RealSrcSpan,a)]
orderByKey keys order
    -- AZ:TODO: if performance becomes a problem, consider a Map of the order
    -- SrcSpan to an index, and do a lookup instead of elemIndex.

    -- Items not in the ordering are placed to the start
 = sortBy (comparing (flip elemIndex order . fst)) keys

-- ---------------------------------------------------------------------

isGadt :: [LConDecl (GhcPass p)] -> Bool
isGadt [] = True
isGadt ((L _ (ConDeclGADT{})):_) = True
isGadt _ = False

-- ---------------------------------------------------------------------

insertCppComments ::  ParsedSource -> [LEpaComment] -> ParsedSource
insertCppComments (L l p) cs = L l p'
  where
    ncs = EpaComments cs
    an' = case GHC.hsmodAnn p of
      (EpAnn a an ocs) -> EpAnn a an (ocs <> ncs)
      unused -> unused
    p' = p { GHC.hsmodAnn = an' }

-- ---------------------------------------------------------------------

ghcCommentText :: LEpaComment -> String
ghcCommentText (L _ (GHC.EpaComment (EpaDocCommentNext s) _))  = s
ghcCommentText (L _ (GHC.EpaComment (EpaDocCommentPrev s) _))  = s
ghcCommentText (L _ (GHC.EpaComment (EpaDocCommentNamed s) _)) = s
ghcCommentText (L _ (GHC.EpaComment (EpaDocSection _ s) _))    = s
ghcCommentText (L _ (GHC.EpaComment (EpaDocOptions s) _))      = s
ghcCommentText (L _ (GHC.EpaComment (EpaLineComment s) _))     = s
ghcCommentText (L _ (GHC.EpaComment (EpaBlockComment s) _))    = s
ghcCommentText (L _ (GHC.EpaComment (EpaEofComment) _))        = ""

tokComment :: LEpaComment -> Comment
tokComment t@(L lt _) = mkComment (normaliseCommentText $ ghcCommentText t) lt

mkLEpaComment :: String -> Anchor -> LEpaComment
-- Note: fudging the ac_prior_tok value, hope it does not cause a problem
mkLEpaComment s anc = (L anc (GHC.EpaComment (EpaLineComment s) (anchor anc)))

mkComment :: String -> Anchor -> Comment
mkComment c anc = Comment c anc Nothing

-- Windows comments include \r in them from the lexer.
normaliseCommentText :: String -> String
normaliseCommentText [] = []
normaliseCommentText ('\r':xs) = normaliseCommentText xs
normaliseCommentText (x:xs) = x:normaliseCommentText xs

-- | Makes a comment which originates from a specific keyword.
mkKWComment :: AnnKeywordId -> EpaLocation -> Comment
mkKWComment kw (EpaSpan ss)
  = Comment (keywordToString $ G kw) (Anchor ss UnchangedAnchor) (Just kw)
mkKWComment kw (EpaDelta dp)
  = Comment (keywordToString $ G kw) (Anchor placeholderRealSpan (MovedAnchor dp)) (Just kw)

comment2dp :: (Comment,  DeltaPos) -> (KeywordId, DeltaPos)
comment2dp = first AnnComment

sortAnchorLocated :: [GenLocated Anchor a] -> [GenLocated Anchor a]
sortAnchorLocated = sortBy (compare `on` (anchor . getLoc))

-- | Calculates the distance from the start of a string to the end of
-- a string.
dpFromString ::  String -> DeltaPos
dpFromString xs = dpFromString' xs 0 0
  where
    dpFromString' "" line col =
      if line == 0
        then SameLine col
        else DifferentLine line col
    dpFromString' ('\n': cs) line _   = dpFromString' cs (line + 1) 0
    dpFromString' (_:cs)     line col = dpFromString' cs line       (col + 1)

-- ---------------------------------------------------------------------

rdrName2String :: RdrName -> String
rdrName2String r =
  case isExact_maybe r of
    Just n  -> name2String n
    Nothing ->
      case r of
        Unqual occ       -> occNameString occ
        Qual modname occ -> moduleNameString modname ++ "."
                                ++ occNameString occ
        Orig _ occ       -> occNameString occ
        Exact n          -> getOccString n

name2String :: Name -> String
name2String = showPprUnsafe

-- ---------------------------------------------------------------------

locatedAnAnchor :: LocatedAn a t -> RealSrcSpan
locatedAnAnchor (L (SrcSpanAnn EpAnnNotUsed l) _) = realSrcSpan l
locatedAnAnchor (L (SrcSpanAnn (EpAnn a _ _) _) _) = anchor a

 -- ---------------------------------------------------------------------

showAst :: (Data a) => a -> String
showAst ast
  = showSDocUnsafe
    $ showAstData NoBlankSrcSpan NoBlankEpAnnotations ast
