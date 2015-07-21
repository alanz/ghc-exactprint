{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
module Language.Haskell.GHC.ExactPrint.Utils
  ( -- * GHC Functions
    srcSpanStartLine
  , srcSpanEndLine
  , srcSpanStartColumn
  , srcSpanEndColumn
  , rdrName2String
  , isSymbolRdrName
  , ghcCommentText
  , isListComp

   -- * Manipulating Positons
  , ss2pos
  , ss2posEnd
  , undelta
  , isPointSrcSpan
  , pos2delta
  , ss2delta
  , spanLength
  , isGoodDelta

  -- * Manipulating Comments
  , mkComment
  , mkKWComment
  , dpFromString
  , comment2dp


  -- * Manipulating Annotations
  , getAnnotationEP
  , annTrueEntryDelta

  -- * General Utility
  , orderByKey



  -- * For tests
  , debug
  , debugM
  , warn
  , showGhc
  , showAnnData

  -- AZ's baggage
  , ghead,glast,gtail,gfromJust
  ) where


import Control.Monad.State
import Data.Data (Data, toConstr, showConstr, cast)
import Data.Generics (extQ, ext1Q, ext2Q, gmapQ)
import Data.List (intercalate, sortBy, elemIndex)
import Data.Ord (comparing)

import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Lookup


import qualified GHC
import qualified Bag            as GHC
import qualified DynFlags       as GHC
import qualified FastString     as GHC
import qualified Name           as GHC
import qualified NameSet        as GHC
import qualified Outputable     as GHC
import qualified RdrName        as GHC
import qualified Var            as GHC

import qualified OccName(occNameString)

import Control.Arrow

--import qualified Data.Generics as SYB

import qualified Data.Map as Map

import Debug.Trace

-- ---------------------------------------------------------------------

-- |Global switch to enable debug tracing in ghc-exactprint
debugEnabledFlag :: Bool
-- debugEnabledFlag = True
debugEnabledFlag = False

-- |Provide a version of trace the comes at the end of the line, so it can
-- easily be commented out when debugging different things.
debug :: c -> String -> c
debug c s = if debugEnabledFlag
              then trace s c
              else c

debugM :: Monad m => String -> m ()
debugM s = when debugEnabledFlag $ traceM s

-- ---------------------------------------------------------------------

warn :: c -> String -> c
-- warn = flip trace
warn c _ = c

isGoodDelta :: DeltaPos -> Bool
isGoodDelta (DP (ro,co)) = ro >= 0 && co >= 0


-- | Create a delta from the current position to the start of the given
-- @SrcSpan@.
ss2delta :: Pos -> GHC.SrcSpan -> DeltaPos
ss2delta ref ss = pos2delta ref (ss2pos ss)

-- | Convert the start of the second @Pos@ to be an offset from the
-- first. The assumption is the reference starts before the second @Pos@
pos2delta :: Pos -> Pos -> DeltaPos
pos2delta (refl,refc) (l,c) = DP (lo,co)
  where
    lo = l - refl
    co = if lo == 0 then c - refc
                    else c

-- | Apply the delta to the current position, taking into account the
-- current column offset if advancing to a new line
undelta :: Pos -> DeltaPos -> LayoutStartCol -> Pos
undelta (l,c) (DP (dl,dc)) (LayoutStartCol co) = (fl,fc)
  where
    fl = l + dl
    fc = if dl == 0 then c  + dc
                    else co + dc

-- ---------------------------------------------------------------------

ss2pos :: GHC.SrcSpan -> Pos
ss2pos ss = (srcSpanStartLine ss,srcSpanStartColumn ss)

ss2posEnd :: GHC.SrcSpan -> Pos
ss2posEnd ss = (srcSpanEndLine ss,srcSpanEndColumn ss)

srcSpanEndColumn :: GHC.SrcSpan -> Int
srcSpanEndColumn (GHC.RealSrcSpan s) = GHC.srcSpanEndCol s
srcSpanEndColumn _ = 0

srcSpanStartColumn :: GHC.SrcSpan -> Int
srcSpanStartColumn (GHC.RealSrcSpan s) = GHC.srcSpanStartCol s
srcSpanStartColumn _ = 0

srcSpanEndLine :: GHC.SrcSpan -> Int
srcSpanEndLine (GHC.RealSrcSpan s) = GHC.srcSpanEndLine s
srcSpanEndLine _ = 0

srcSpanStartLine :: GHC.SrcSpan -> Int
srcSpanStartLine (GHC.RealSrcSpan s) = GHC.srcSpanStartLine s
srcSpanStartLine _ = 0

spanLength :: GHC.SrcSpan -> Int
spanLength = (-) <$> srcSpanEndColumn <*> srcSpanStartColumn

-- ---------------------------------------------------------------------

{-span2ss :: Span -> GHC.SrcSpan
span2ss ((sr,sc),(er,ec)) = l
  where
   filename = GHC.mkFastString "f"
   l = GHC.mkSrcSpan (GHC.mkSrcLoc filename sr sc) (GHC.mkSrcLoc filename er ec)
   -}

-- ---------------------------------------------------------------------

isPointSrcSpan :: GHC.SrcSpan -> Bool
isPointSrcSpan ss = s == e where (s,e) = (ss2pos ss, ss2posEnd ss)

-- ---------------------------------------------------------------------

-- |Given a list of items and their SrcSpan, return a list of items arranged as
-- per the sortkey.
orderByKey :: [(GHC.SrcSpan,a)] -> [GHC.SrcSpan] -> [a]
orderByKey keys order
    -- AZ:TODO: if performance becomes a problem, consider a Map of the order
    -- SrcSpan to an index, and do a lookup instead of elemIndex.

    -- Items not in the ordering are placed to the start
 = map snd (sortBy (comparing (flip elemIndex order . fst)) keys)

-- ---------------------------------------------------------------------

isListComp :: GHC.HsStmtContext name -> Bool
isListComp cts = case cts of
          GHC.ListComp  -> True
          GHC.MonadComp -> True
          GHC.PArrComp  -> True

          GHC.DoExpr       -> False
          GHC.MDoExpr      -> False
          GHC.ArrowExpr    -> False
          GHC.GhciStmtCtxt -> False

          GHC.PatGuard {}      -> False
          GHC.ParStmtCtxt {}   -> False
          GHC.TransStmtCtxt {} -> False

-- ---------------------------------------------------------------------

ghcCommentText :: GHC.Located GHC.AnnotationComment -> String
ghcCommentText (GHC.L _ (GHC.AnnDocCommentNext s))  = s
ghcCommentText (GHC.L _ (GHC.AnnDocCommentPrev s))  = s
ghcCommentText (GHC.L _ (GHC.AnnDocCommentNamed s)) = s
ghcCommentText (GHC.L _ (GHC.AnnDocSection _ s))    = s
ghcCommentText (GHC.L _ (GHC.AnnDocOptions s))      = s
ghcCommentText (GHC.L _ (GHC.AnnDocOptionsOld s))   = s
ghcCommentText (GHC.L _ (GHC.AnnLineComment s))     = s
ghcCommentText (GHC.L _ (GHC.AnnBlockComment s))    = s

mkComment :: String -> GHC.SrcSpan -> Comment
mkComment c ss = Comment c ss Nothing

mkKWComment :: GHC.AnnKeywordId -> GHC.SrcSpan -> Comment
mkKWComment kw ss = Comment (keywordToString $ G kw) ss (Just kw)

comment2dp :: (Comment,  DeltaPos) -> (KeywordId, DeltaPos)
comment2dp = first AnnComment

getAnnotationEP :: (Data a) =>  GHC.Located a  -> Anns -> Maybe Annotation
getAnnotationEP  la as =
  Map.lookup (mkAnnKey la) as

annTrueEntryDelta :: Annotation -> DeltaPos
annTrueEntryDelta Ann{annEntryDelta, annPriorComments} =
  foldr addDP (DP (0,0)) (map (\(a, b) -> addDP b (dpFromString $ commentContents a)) annPriorComments )
    `addDP` annEntryDelta

dpFromString ::  String -> DeltaPos
dpFromString xs = dpFromString' xs 0 0
  where
    dpFromString' "" line col = DP (line, col)
    dpFromString' ('\n': cs) line _ = dpFromString' cs (line + 1) 0
    dpFromString' (_:cs)     line col = dpFromString' cs line (col + 1)

-- ---------------------------------------------------------------------

isSymbolRdrName :: GHC.RdrName -> Bool
isSymbolRdrName n = GHC.isSymOcc $ GHC.rdrNameOcc n

rdrName2String :: GHC.RdrName -> String
rdrName2String r =
  case GHC.isExact_maybe r of
    Just n  -> name2String n
    Nothing ->
      case r of
        GHC.Unqual occ       -> GHC.occNameString occ
        GHC.Qual modname occ -> GHC.moduleNameString modname ++ "."
                            ++ GHC.occNameString occ
        GHC.Orig _ occ          -> GHC.occNameString occ
        GHC.Exact _           -> error $ "GHC.Exact introduced after renaming" ++ showGhc r

name2String :: GHC.Name -> String
name2String = showGhc

-- ---------------------------------------------------------------------

-- Based on ghc-syb-utils version, but adding the annotation
-- information to each SrcLoc.
showAnnData :: Data a => Anns -> Int -> a -> String
showAnnData anns n =
  generic -- `ext1Q` located
          `ext1Q` list
          `extQ` string `extQ` fastString `extQ` srcSpan
          `extQ` name `extQ` occName `extQ` moduleName `extQ` var `extQ` dataCon
          `extQ` overLit
          `extQ` bagName `extQ` bagRdrName `extQ` bagVar `extQ` nameSet
          `extQ` fixity
          `ext2Q` located
  where generic :: Data a => a -> String
        generic t = indent n ++ "(" ++ showConstr (toConstr t)
                 ++ space (unwords (gmapQ (showAnnData anns (n+1)) t)) ++ ")"
        space "" = ""
        space s  = ' ':s
        indent i = "\n" ++ replicate i ' '
        string     = show :: String -> String
        fastString = ("{FastString: "++) . (++"}") . show :: GHC.FastString -> String
        list l     = indent n ++ "["
                              ++ intercalate "," (map (showAnnData anns (n+1)) l) ++ "]"

        name       = ("{Name: "++) . (++"}") . showSDoc_ . GHC.ppr :: GHC.Name -> String
        occName    = ("{OccName: "++) . (++"}") .  OccName.occNameString
        moduleName = ("{ModuleName: "++) . (++"}") . showSDoc_ . GHC.ppr :: GHC.ModuleName -> String

        -- srcSpan    = ("{"++) . (++"}") . showSDoc_ . GHC.ppr :: GHC.SrcSpan -> String
        srcSpan :: GHC.SrcSpan -> String
        srcSpan ss = "{ "++ showSDoc_ (GHC.hang (GHC.ppr ss) (n+2)
                                                 -- (GHC.ppr (Map.lookup ss anns)
                                                 (GHC.text "")
                                                 )
                      ++"}"

        var        = ("{Var: "++) . (++"}") . showSDoc_ . GHC.ppr :: GHC.Var -> String
        dataCon    = ("{DataCon: "++) . (++"}") . showSDoc_ . GHC.ppr :: GHC.DataCon -> String

        overLit :: GHC.HsOverLit GHC.RdrName -> String
        overLit    = ("{HsOverLit:"++) . (++"}") . showSDoc_ . GHC.ppr

        bagRdrName:: GHC.Bag (GHC.Located (GHC.HsBind GHC.RdrName)) -> String
        bagRdrName = ("{Bag(Located (HsBind RdrName)): "++) . (++"}") . list . GHC.bagToList
        bagName   :: GHC.Bag (GHC.Located (GHC.HsBind GHC.Name)) -> String
        bagName    = ("{Bag(Located (HsBind Name)): "++) . (++"}") . list . GHC.bagToList
        bagVar    :: GHC.Bag (GHC.Located (GHC.HsBind GHC.Var)) -> String
        bagVar     = ("{Bag(Located (HsBind Var)): "++) . (++"}") . list . GHC.bagToList

        nameSet = ("{NameSet: "++) . (++"}") . list . GHC.nameSetElems

        fixity = ("{Fixity: "++) . (++"}") . showSDoc_ . GHC.ppr :: GHC.Fixity -> String

        located :: (Data b,Data loc) => GHC.GenLocated loc b -> String
        -- located la = show (getAnnotationEP la anns)
        located (GHC.L ss a) =
          indent n ++ "("
            ++ case cast ss of
                    Just (s :: GHC.SrcSpan) ->
                      srcSpan s
                      ++ indent (n + 1) ++
                      show (getAnnotationEP (GHC.L s a) anns)
                    Nothing -> "nnnnnnnn"
                  ++ showAnnData anns (n+1) a
                  ++ ")"

-- ---------------------------------------------------------------------


 -- ---------------------------------------------------------------------

showSDoc_ :: GHC.SDoc -> String
showSDoc_ = GHC.showSDoc GHC.unsafeGlobalDynFlags


-- ---------------------------------------------------------------------
-- Putting these here for the time being, to avoid import loops

ghead :: String -> [a] -> a
ghead  info []    = error $ "ghead "++info++" []"
ghead _info (h:_) = h

glast :: String -> [a] -> a
glast  info []    = error $ "glast " ++ info ++ " []"
glast _info h     = last h

gtail :: String -> [a] -> [a]
gtail  info []   = error $ "gtail " ++ info ++ " []"
gtail _info h    = tail h

gfromJust :: String -> Maybe a -> a
gfromJust _info (Just h) = h
gfromJust  info Nothing = error $ "gfromJust " ++ info ++ " Nothing"
