{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Language.Haskell.GHC.ExactPrint.Utils
  (
   -- * Manipulating Positons
    ss2pos
  , ss2posEnd
  , undelta
  , isPointSrcSpan
  , pos2delta
  , ss2delta
  , addDP
  , spanLength
  , isGoodDelta

  -- * Manipulating Comments
  , mkComment
  , mkKWComment
  , dpFromString
  , comment2dp

    -- * GHC Functions
  , srcSpanStartLine
  , srcSpanEndLine
  , srcSpanStartColumn
  , srcSpanEndColumn
  , rdrName2String
  , isSymbolRdrName
  , tokComment
  , isListComp


  -- * Manipulating Annotations
  , getAnnotationEP
  , annTrueEntryDelta
  , annCommentEntryDelta
  , annLeadingCommentEntryDelta

  -- * General Utility
  , orderByKey


  -- * AST Context management
  , setAcs, setAcsWithLevel
  , inAcs
  , pushAcs

  -- * For tests
  , debug
  , debugM
  , warn
  , showGhc
  , showAnnData

  , showSDoc_,  showSDocDebug_
  -- AZ's baggage
  , ghead,glast,gtail,gfromJust
  ) where


import Control.Monad.State
import qualified Data.ByteString as B
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
import qualified Data.Set as Set
import Data.List

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

-- | Show a GHC.Outputable structure
showGhc :: (GHC.Outputable a) => a -> String
showGhc = GHC.showPpr GHC.unsafeGlobalDynFlags

-- ---------------------------------------------------------------------

warn :: c -> String -> c
-- warn = flip trace
warn c _ = c

-- | A good delta has no negative values.
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

-- | Add together two @DeltaPos@ taking into account newlines
--
-- > DP (0, 1) `addDP` DP (0, 2) == DP (0, 3)
-- > DP (0, 9) `addDP` DP (1, 5) == DP (1, 5)
-- > DP (1, 4) `addDP` DP (1, 3) == DP (2, 3)
addDP :: DeltaPos -> DeltaPos -> DeltaPos
addDP (DP (a, b)) (DP (c, d)) =
  if c >= 1 then DP (a+c, d)
            else DP (a,   b+d)

-- | "Subtract" two @DeltaPos@ from each other, in the sense of calculating the
-- remaining delta for the second after the first has been applied.
-- invariant : if c = a `addDP` b
--             then a `stepDP` c == b
--
-- Cases where first DP is <= than second
-- > DP (0, 1) `addDP` DP (0, 2) == DP (0, 1)
-- > DP (1, 1) `addDP` DP (2, 0) == DP (1, 0)
-- > DP (1, 3) `addDP` DP (1, 4) == DP (0, 1)
-- > DP (1, 4) `addDP` DP (1, 4) == DP (1, 4)
--
-- Cases where first DP is > than second
-- > DP (0,  3) `addDP` DP (0, 2) == DP (0,1)  -- advance one at least
-- > DP (3,  3) `addDP` DP (2, 4) == DP (1, 4) -- go one line forward and to expected col
-- > DP (3,  3) `addDP` DP (0, 4) == DP (0, 1) -- maintain col delta at least
-- > DP (1, 21) `addDP` DP (1, 4) == DP (1, 4) -- go one line forward and to expected col
stepDP :: DeltaPos -> DeltaPos -> DeltaPos
stepDP (DP (a,b)) (DP (c,d))
  | (a,b) == (c,d) = DP (a,b)
  | a == c = if b < d then DP (0,d - b)
                      else if d == 0
                             then DP (1,0)
                             -- else DP (0,1)
                             else DP (c,d)
  | a < c = DP (c - a,d)
  | otherwise = DP (1,d)

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
-- | Checks whether a SrcSpan has zero length.
isPointSrcSpan :: GHC.SrcSpan -> Bool
isPointSrcSpan ss = spanLength ss == 0
                  && srcSpanStartLine ss == srcSpanEndLine ss

-- ---------------------------------------------------------------------

-- |Given a list of items and a list of keys, returns a list of items
-- ordered by their position in the list of keys.
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

tokComment :: GHC.Located GHC.AnnotationComment -> Comment
tokComment t@(GHC.L lt _) = mkComment (ghcCommentText t) lt

mkComment :: String -> GHC.SrcSpan -> Comment
mkComment c ss = Comment c ss Nothing

-- | Makes a comment which originates from a specific keyword.
mkKWComment :: GHC.AnnKeywordId -> GHC.SrcSpan -> Comment
mkKWComment kw ss = Comment (keywordToString $ G kw) ss (Just kw)

comment2dp :: (Comment,  DeltaPos) -> (KeywordId, DeltaPos)
comment2dp = first AnnComment

getAnnotationEP :: (Data a) =>  GHC.Located a  -> Anns -> Maybe Annotation
getAnnotationEP  la as =
  Map.lookup (mkAnnKey la) as

-- | The "true entry" is the distance from the last concrete element to the
-- start of the current element.
annTrueEntryDelta :: Annotation -> DeltaPos
annTrueEntryDelta Ann{annEntryDelta, annPriorComments} =
  foldr addDP (DP (0,0)) (map (\(a, b) -> addDP b (dpFromString $ commentContents a)) annPriorComments )
    `addDP` annEntryDelta

-- | Take an annotation and a required "true entry" and calculate an equivalent
-- one relative to the last comment in the annPriorComments.
annCommentEntryDelta :: Annotation -> DeltaPos -> DeltaPos
annCommentEntryDelta Ann{annPriorComments} trueDP = dp
  where
    commentDP =
      foldr addDP (DP (0,0)) (map (\(a, b) -> addDP b (dpFromString $ commentContents a)) annPriorComments )
    dp = stepDP commentDP trueDP

-- | Return the DP of the first item that generates output, either a comment or the entry DP
annLeadingCommentEntryDelta :: Annotation -> DeltaPos
annLeadingCommentEntryDelta Ann{annPriorComments,annEntryDelta} = dp
  where
    dp = case annPriorComments of
      [] -> annEntryDelta
      ((_,ed):_) -> ed

-- | Calculates the distance from the start of a string to the end of
-- a string.
dpFromString ::  String -> DeltaPos
dpFromString xs = dpFromString' xs 0 0
  where
    dpFromString' "" line col = DP (line, col)
    dpFromString' ('\n': cs) line _   = dpFromString' cs (line + 1) 0
    dpFromString' (_:cs)     line col = dpFromString' cs line       (col + 1)

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
        GHC.Orig _ occ       -> GHC.occNameString occ
        GHC.Exact n          -> GHC.getOccString n

name2String :: GHC.Name -> String
name2String = showGhc

-- ---------------------------------------------------------------------

-- | Put the provided context elements into the existing set with fresh level
-- counts
setAcs :: Set.Set AstContext -> AstContextSet -> AstContextSet
setAcs ctxt acs = setAcsWithLevel ctxt 3 acs

-- | Put the provided context elements into the existing set with given level
-- counts
setAcsWithLevel :: Set.Set AstContext -> Int -> AstContextSet -> AstContextSet
setAcsWithLevel ctxt level (ACS a) = ACS a'
  where
    upd s (k,v) = Map.insert k v s
    a' = foldl' upd a $ zip (Set.toList ctxt) (repeat level)

-- ---------------------------------------------------------------------

-- | Are any of the contexts currently active?
inAcs :: Set.Set AstContext -> AstContextSet -> Bool
inAcs ctxt (ACS a) = not $ Set.null $ Set.intersection ctxt (Set.fromList $ Map.keys a)

-- | propagate the ACS down a level, dropping all values which hit zero
pushAcs :: AstContextSet -> AstContextSet
pushAcs (ACS a) = ACS $ Map.mapMaybe f a
  where
    f n
      | n <= 1    = Nothing
      | otherwise = Just (n - 1)

-- ---------------------------------------------------------------------

-- | Show a GHC AST with interleaved Annotation information.
showAnnData :: Data a => Anns -> Int -> a -> String
showAnnData anns n =
  generic -- `ext1Q` located
          `ext1Q` list
          `extQ` string `extQ` fastString `extQ` srcSpan
          `extQ` bytestring
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
        bytestring = show :: B.ByteString -> String
        list l     = indent n ++ "["
                              ++ intercalate "," (map (showAnnData anns (n+1)) l) ++ "]"

        name       = ("{Name: "++) . (++"}") . showSDocDebug_ . GHC.ppr :: GHC.Name -> String
        occName    = ("{OccName: "++) . (++"}") .  OccName.occNameString
        moduleName = ("{ModuleName: "++) . (++"}") . showSDoc_ . GHC.ppr :: GHC.ModuleName -> String

        -- srcSpan    = ("{"++) . (++"}") . showSDoc_ . GHC.ppr :: GHC.SrcSpan -> String
        srcSpan :: GHC.SrcSpan -> String
        srcSpan ss = "{ "++ showSDoc_ (GHC.hang (GHC.ppr ss) (n+2)
                                                 -- (GHC.ppr (Map.lookup ss anns)
                                                 (GHC.text "")
                                                 )
                      ++"}"

        var        = ("{Var: "++) . (++"}") . showSDocDebug_ . GHC.ppr :: GHC.Var -> String
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
                      -- ++ case showWrappedDeclAnns (GHC.L s a) of
                      --   Nothing -> ""
                      --   Just annStr  -> indent (n + 1) ++ annStr
                    Nothing -> "nnnnnnnn"
                  ++ showAnnData anns (n+1) a
                  ++ ")"

{-
        showWrappedDeclAnns :: (Data a) => GHC.Located a -> Maybe String
        showWrappedDeclAnns t = everything mappend (Nothing `mkQ` showDecl) t
          where
            showDecl :: GHC.LHsDecl GHC.RdrName -> Maybe String
            showDecl d = Just $ declFun doShowAnn  d

        doShowAnn :: (Data a) => GHC.Located a -> String
        doShowAnn a =  show (getAnnotationEP a anns)
-}
 -- ---------------------------------------------------------------------

showSDoc_ :: GHC.SDoc -> String
showSDoc_ = GHC.showSDoc GHC.unsafeGlobalDynFlags

showSDocDebug_ :: GHC.SDoc -> String
#if __GLASGOW_HASKELL__ <= 710
showSDocDebug_ = GHC.showSDoc GHC.unsafeGlobalDynFlags
#else
showSDocDebug_ = GHC.showSDocDebug GHC.unsafeGlobalDynFlags
#endif

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
