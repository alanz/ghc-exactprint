{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
module Language.Haskell.GHC.ExactPrint.Utils
  (

    srcSpanStartLine
  , srcSpanEndLine
  , srcSpanStartColumn
  , srcSpanEndColumn

  , ss2span
  , ss2pos
  , ss2posEnd
  , span2ss
  , undelta
  , rdrName2String
  , isSymbolRdrName
  , ghcCommentText
  , mkComment
  , mkKWComment
  , mkDComment
  , isPointSrcSpan
  , pos2delta
  , ss2delta
  , spanLength
  , isGoodDelta

  , isListComp

  , showGhc
  , showAnnData

  -- * Managing SortKeys
  -- , ss2SortKey
  -- , sortKeyBefore
  -- , sortKeyAfter
  -- , sortKeyBetween
  -- , stringBeween

  -- * For tests
  , debug
  , debugM
  , warn

  ) where


import Control.Monad.State
import Data.Data (Data, toConstr, showConstr, cast)
import Data.Generics (extQ, ext1Q, ext2Q, gmapQ)
import Data.List (intercalate)
import Data.Functor (($>))

import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Lookup
import Language.Haskell.GHC.ExactPrint.Internal.Types


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

--import qualified Data.Generics as SYB

--import qualified Data.Map as Map

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

ss2span :: GHC.SrcSpan -> Span
ss2span ss = (ss2pos ss,ss2posEnd ss)

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

span2ss :: Span -> GHC.SrcSpan
span2ss ((sr,sc),(er,ec)) = l
  where
   filename = GHC.mkFastString "f"
   l = GHC.mkSrcSpan (GHC.mkSrcLoc filename sr sc) (GHC.mkSrcLoc filename er ec)

-- ---------------------------------------------------------------------

isPointSrcSpan :: GHC.SrcSpan -> Bool
isPointSrcSpan ss = s == e where (s,e) = ss2span ss

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
mkComment c ss = Comment (ss2span ss) c ss Nothing

mkKWComment :: GHC.AnnKeywordId -> GHC.SrcSpan -> Comment
mkKWComment kw ss = Comment (ss2span ss) (keywordToString $ G kw) ss (Just kw)

mkDComment :: Comment -> DeltaPos -> DComment
mkDComment = ($>)

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
                      show (getAnnotationEP (GHC.L s a) NotNeeded anns)
                    Nothing -> "nnnnnnnn"
                  ++ showAnnData anns (n+1) a
                  ++ ")"

-- ---------------------------------------------------------------------
{-
-- Start with a 0.5 position, can always split toward zero or toward one, never
-- actually at them
ss2SortKey :: GHC.SrcSpan -> SortKey
ss2SortKey ss = SortKey (r,c,1%2)
  where
    (r,c) = ss2pos ss

-- See http://www.virtualnerd.com/tutorials/?id=Alg1_2z
rationalBetween :: Rational -> Rational -> Rational
rationalBetween lower upper = (lower + upper) / 2

-- |Construct a SortKey that will be ordered before the given one
sortKeyBefore :: SortKey -> SortKey
sortKeyBefore (SortKey (r,c,s)) = SortKey (r,c,rationalBetween 0 s)

-- |Construct a SortKey that will be ordered after the given one
sortKeyAfter :: SortKey -> SortKey
sortKeyAfter (SortKey (r,c,s)) = SortKey (r,c,rationalBetween s 1)

-- |Construct a SortKey that will be between the given ones
-- Precondition: sk1 < sk2 already
sortKeyBetween :: SortKey -> SortKey -> SortKey
sortKeyBetween sk1@(SortKey (r1,c1,s1)) sk2@(SortKey (r2,c2,s2))
  | sk1 >= sk2 = error $ "sortKeyBetween:first key not LT second:" ++  show (sk1,sk2)
  | r1 == r2 && c1 == c2 = SortKey (r1,c1,rationalBetween s1 s2)
  | otherwise = sortKeyAfter sk1

-- |Create a String that sorts lexically between the given two.
-- precondition: s1 < s2
stringBeween :: String -> String -> String
stringBeween str1 str2 = go str1 str2
  where
    go [] [] = "a"
    go s1 [] = s1 ++ "a"
    go (s1:s1s) (s2:s2s) = if s1 == s2
                              then s1 : go s1s s2s
                              else (s1:s1s) ++ "a"
    go s1 _s2 = s1 ++ "a"
-}
 -- ---------------------------------------------------------------------

showSDoc_ :: GHC.SDoc -> String
showSDoc_ = GHC.showSDoc GHC.unsafeGlobalDynFlags

-- ---------------------------------------------------------------------
{-
-- | In GHC 7.10.1 the HsPar statement has an incorrect SrcSpan.
-- See https://ghc.haskell.org/trac/ghc/ticket/10207
-- This provides a workaround for it
fixBuggySrcSpan :: (SYB.Data a) => Maybe GHC.SrcSpan -> GHC.Located a -> GHC.Located a
fixBuggySrcSpan mbegin orig@(GHC.L _l a) = r -- `debug` ("fixBuggySrcSpan for " ++ show (mkAnnKey orig,SYB.typeOf a))
  where
    -- Need fully expanded type for the match
    hasStmt :: (Data t) => t -> Maybe (GHC.StmtLR GHC.RdrName GHC.RdrName (GHC.GenLocated GHC.SrcSpan (GHC.HsExpr GHC.RdrName)))
    hasStmt = SYB.gfindtype

    parStmtBlocSpan :: GHC.ParStmtBlock GHC.RdrName GHC.RdrName -> GHC.SrcSpan
    parStmtBlocSpan (GHC.ParStmtBlock []    _ _) = GHC.noSrcSpan -- Should never happen
    parStmtBlocSpan (GHC.ParStmtBlock stmts _ _) = GHC.combineLocs (head stmts) (last stmts)

    r1@(GHC.L l1 a1) = case hasStmt orig of
      Nothing -> orig `debug` ("fixBuggySrcSpan: got Nothing")
      Just (GHC.ParStmt [] _ _) -> orig `debug` ("fixBuggySrcSpan:found empty ParStmt")
      Just (GHC.ParStmt pbs _ _) -> GHC.L ss' a `debug` ("fixBuggySrcSpan:found ParStmt:returning:" ++ showGhc ss')
           where ss' = GHC.combineSrcSpans (parStmtBlocSpan $ head pbs) (parStmtBlocSpan $ last pbs)
      _ -> orig

    r = case mbegin of
      Nothing -> r1
      Just begin -> GHC.L (GHC.combineSrcSpans begin l1) a1

-- ---------------------------------------------------------------------

-- |There are a number of bugs in GHC 7.10.1 which result in incorrect
-- SrcSpans being allocated to AST elements, such that they do not
-- include all the annotated items in the SrcSpan.
-- See https://ghc.haskell.org/trac/ghc/ticket/10207
-- and https://ghc.haskell.org/trac/ghc/ticket/10209
-- and https://ghc.haskell.org/trac/ghc/ticket/10214
type FB a = State GHC.ApiAnns a
 -- runState  :: State s a -> s -> (a, s)
fixBugsInAst :: (SYB.Data t) => GHC.ApiAnns -> t -> (GHC.ApiAnns,t)
fixBugsInAst anns t = (anns',t')
  where
    (t',anns') = runState f anns

    -- Note: bottom up
    f = SYB.everywhereM (-- SYB.mkM parStmtBlock
                        -- `SYB.extM` parStmt
                        SYB.mkM parStmt
                        `SYB.extM` hsKind) t

    -- ---------------------------------

    changeAnnSpan :: GHC.SrcSpan -> GHC.SrcSpan -> FB ()
    changeAnnSpan old new = do
      (anKW,anComments) <- get
      let
        changeSrcSpan ss
          | ss == old = new
          | otherwise = ss

        changeAnnKey :: GHC.ApiAnnKey -> GHC.ApiAnnKey
        changeAnnKey (ss,kw) = (changeSrcSpan ss,kw)

        changeAnnKeys :: (SYB.Data t) => t -> t
        changeAnnKeys = SYB.everywhere (SYB.mkT changeAnnKey)

        changeSpans :: (SYB.Data t) => t -> t
        changeSpans = SYB.everywhere (SYB.mkT changeSrcSpan)
        anKW'       = changeAnnKeys anKW
        anComments' = changeSpans anComments
      put (anKW',anComments')

    addAnnotation :: GHC.SrcSpan -> GHC.SrcSpan -> GHC.AnnKeywordId -> FB ()
    addAnnotation parent loc kw = do
      (anKW,anComments) <- get
      let anKW' = Map.insertWith (++) (parent,kw) [loc] anKW
      put (anKW',anComments)

    -- ---------------------------------

    parStmtBlockSpan :: GHC.ParStmtBlock GHC.RdrName GHC.RdrName -> GHC.SrcSpan
    parStmtBlockSpan (GHC.ParStmtBlock []    _ _) = GHC.noSrcSpan -- Should never happen
    parStmtBlockSpan (GHC.ParStmtBlock stmts _ _) = GHC.combineLocs (head stmts) (last stmts)

    parStmt :: GHC.Located (GHC.StmtLR GHC.RdrName GHC.RdrName (GHC.GenLocated GHC.SrcSpan (GHC.HsExpr GHC.RdrName)))
        -> FB (GHC.Located (GHC.StmtLR GHC.RdrName GHC.RdrName (GHC.GenLocated GHC.SrcSpan (GHC.HsExpr GHC.RdrName))))
    parStmt ps@(GHC.L _(GHC.ParStmt [] _ _)) = return ps
    {-
    parStmt (GHC.L _ ps@(GHC.ParStmt pbs _ _)) = do -- #10207
      let ss = GHC.combineSrcSpans (parStmtBlockSpan $ head pbs) (parStmtBlockSpan $ last pbs)
      return (GHC.L ss ps)

    parStmt (GHC.L ss ast@GHC.TransStmt{..}) = -- #10214
      let ss' = GHC.combineLocs (head trS_stmts) trS_using in
        changeAnnSpan ss ss' >> return (GHC.L ss' ast)
    -}
    parStmt x = return x

    -- ---------------------------------

    -- #10207
    parStmtBlock :: GHC.GenLocated GHC.SrcSpan (GHC.ParStmtBlock GHC.RdrName GHC.RdrName)
                 -> FB (GHC.GenLocated GHC.SrcSpan (GHC.ParStmtBlock GHC.RdrName GHC.RdrName))
    parStmtBlock psb@(GHC.L _  (GHC.ParStmtBlock []    _  _ )) = return psb
    parStmtBlock     (GHC.L ss (GHC.ParStmtBlock stmts ns se)) = do
      changeAnnSpan ss ss'
      return (GHC.L ss' (GHC.ParStmtBlock stmts ns se))
      where
        ss' = GHC.combineLocs (head stmts) (last stmts)

    -- ---------------------------------

    hsKind :: (GHC.GenLocated GHC.SrcSpan (GHC.HsType GHC.RdrName))
           -> FB (GHC.GenLocated GHC.SrcSpan (GHC.HsType GHC.RdrName))
    hsKind (GHC.L ss k) = do
      changeAnnSpan ss ss'
      addAnnotation ss' ss GHC.AnnEofPos
      return (GHC.L ss' k)
      where
        ss' = case GHC.getAnnotation anns ss GHC.AnnDcolon of
          []     -> ss
          (ld:_) -> GHC.combineSrcSpans ld ss

-- ---------------------------------------------------------------------

-}
