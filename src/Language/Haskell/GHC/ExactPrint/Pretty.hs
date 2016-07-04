{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-} -- ++AZ++ TODO: get rid of this abomination

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
        addAnnotationsForPretty
        ) where

import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Utils
import Language.Haskell.GHC.ExactPrint.Annotate

import Control.Exception
import Control.Monad.Identity
import Control.Monad.RWS
import Control.Monad.Trans.Free
import Data.Generics
import Data.List (sortBy, elemIndex)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)

#if __GLASGOW_HASKELL__ <= 710
-- import Language.Haskell.GHC.ExactPrint.Lookup
#endif

import qualified GHC
import qualified SrcLoc        as GHC

import qualified Data.Map as Map
import qualified Data.Set as Set

import Debug.Trace

{-# ANN module "HLint: ignore Eta reduce" #-}

-- ---------------------------------------------------------------------

-- |Add any missing annotations so that the full AST element will exactprint
-- properly when done.
addAnnotationsForPretty :: (Annotate a) => [Comment] -> GHC.Located a -> Anns -> Anns
addAnnotationsForPretty cs ast ans
  = runPrettyWithComments opts cs (annotate ast) ans (0,0)
  where
    opts = prettyOptions NormalLayout

-- ---------------------------------------------------------------------
--
-- | Type used in the Pretty Monad.
type Pretty a = RWS PrettyOptions PrettyWriter PrettyState a

runPrettyWithComments :: PrettyOptions -> [Comment] -> Annotated () -> Anns -> Pos -> Anns
runPrettyWithComments opts cs action ans priorEnd =
  mkAnns . snd
  . (\next -> execRWS next opts (defaultPrettyState cs priorEnd ans))
  . prettyInterpret $ action
  where
    mkAnns :: PrettyWriter -> Anns
    mkAnns = f . dwAnns
    f :: Monoid a => Endo a -> a
    f = ($ mempty) . appEndo

-- ---------------------------------------------------------------------

-- TODO: rename this, it is the R part of the RWS
data PrettyOptions = PrettyOptions
       {
         -- | Current `SrcSpan, part of current AnnKey`
         curSrcSpan  :: !GHC.SrcSpan

         -- | Constuctor of current AST element, part of current AnnKey
       , annConName       :: !AnnConName

        -- | Whether to use rigid or normal layout rules
       , drRigidity :: !Rigidity

       -- | Current higher level context. e.g. whether a Match is part of a
       -- LambdaExpr or a FunBind
       , prContext :: !AstContextSet
       } deriving Show

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
    , prContext  = defaultACS
    }

normalLayout :: PrettyOptions
normalLayout = prettyOptions NormalLayout

defaultPrettyState :: [Comment] -> Pos -> Anns -> PrettyState
defaultPrettyState injectedComments priorEnd ans =
    PrettyState
      { priorEndPosition    = priorEnd
      , apComments = cs ++ injectedComments
      -- , apAnns     = ga
      , apAnns     = mempty
      , apLayoutStart = 1
      , apMarkLayout = False
      }
  where
    cs :: [Comment]
    -- cs = flattenedComments ga
    cs = []

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
    go (MarkPPOptional _kwid _ next)    = next
    go (MarkEOF next)                   = addEofAnnotation >> next
    go (MarkExternal ss akwid _ next)   = addPrettyAnnotation akwid >> next
    go (MarkOutside akwid kwid next)    = addPrettyAnnotationsOutside akwid kwid >> next
    go (MarkInside akwid next)          = addPrettyAnnotationsInside akwid >> next
    go (MarkMany akwid next)            = addPrettyAnnotations akwid >> next
    go (MarkOffsetPrim akwid n _ next)  = addPrettyAnnotationLs akwid n >> next
    go (WithAST lss prog next)          = withAST lss (prettyInterpret prog) >> next
    go (CountAnns kwid next)            = countAnnsPretty kwid >>= next
    go (WithSortKey kws next)           = withSortKey kws >> next
    go (SetLayoutFlag r action next)    = do
      rigidity <- asks drRigidity
      (if r <= rigidity then setLayoutFlag else id) (prettyInterpret action)
      next
    go (StoreOriginalSrcSpan key next)  = storeOriginalSrcSpanPretty key >>= next
    go (GetSrcSpanForKw kw next)        = getSrcSpanForKw kw >>= next
#if __GLASGOW_HASKELL__ <= 710
    go (StoreString s ss next)          = storeString s ss >> next
#endif
    go (AnnotationsToComments kws next) = annotationsToCommentsPretty kws >> next

    go (SetContextLevel ctxt lvl action next)  = setContextPretty ctxt lvl (prettyInterpret action) >> next
    go (IfInContext  ctxt ifAction elseAction next) = ifInContextPretty ctxt ifAction elseAction >> next
    go (NotInContext ctxt action next)  = notInContextPretty ctxt action >> next

-- ---------------------------------------------------------------------

addEofAnnotation :: Pretty ()
addEofAnnotation = tellKd (G GHC.AnnEofPos, DP (1,0))

-- ---------------------------------------------------------------------

addPrettyAnnotation :: GHC.AnnKeywordId -> Pretty ()
addPrettyAnnotation ann = do
  cur <- asks prContext
  case ann of
    GHC.AnnVal    -> if inAcs (Set.fromList [NoPrecedingSpace]) cur
                       then tellKd (G ann,DP (0,0))
                       else tellKd (G ann,DP (0,1))
    GHC.AnnCloseC -> tellKd (G ann,DP (0,0))
    GHC.AnnDcolon -> tellKd (G ann,DP (0,1))
    GHC.AnnEqual  -> tellKd (G ann,DP (0,1))
    GHC.AnnIn     -> tellKd (G ann,DP (1,0))
    GHC.AnnOf     -> tellKd (G ann,DP (0,1))
    GHC.AnnOpenC  -> tellKd (G ann,DP (0,0))
    GHC.AnnRarrow -> tellKd (G ann,DP (0,1))
    GHC.AnnWhere  -> tellKd (G ann,DP (0,1))
    _ ->             tellKd (G ann,DP (0,0))

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
addPrettyAnnotationLs ann _off = addPrettyAnnotation ann

-- ---------------------------------------------------------------------

withSrcSpanPretty :: Data a => GHC.Located a -> Pretty b -> Pretty b
withSrcSpanPretty (GHC.L l a) =
  local (\s -> s { curSrcSpan = l
                 , annConName = annGetConstr a
                 , prContext = pushAcs (prContext s)
                 })

-- ---------------------------------------------------------------------

-- | Enter a new AST element. Maintain SrcSpan stack
withAST :: Data a
        => GHC.Located a
        -> Pretty b -> Pretty b
withAST lss@(GHC.L ss t) action = do
  -- Calculate offset required to get to the start of the SrcSPan
  off <- gets apLayoutStart
  withSrcSpanPretty lss $ do
    return () `debug` ("Pretty.withAST:enter:(ss)=" ++ showGhc (ss,showConstr (toConstr t)))

    let maskWriter s = s { annKds = []
                         , sortKeys = Nothing
                         , dwCapturedSpan = mempty
                         }
    ctx <- asks prContext
    let spanStart = ss2pos ss
        -- edp = DP (0,0)
    edp <- entryDpFor ctx t

    let cs = []
    (res, w) <- censor maskWriter (listen action)

    let kds = annKds w
        an = Ann
               { annEntryDelta = edp
               , annPriorComments = cs
               , annFollowingComments = [] -- only used in Transform and Print
               , annsDP     = kds
               , annSortKey = sortKeys w
               , annCapturedSpan = getFirst $ dwCapturedSpan w }

    addAnnotationsPretty an
     `debug` ("Pretty.withAST:(annkey,an)=" ++ show (mkAnnKey lss,an))
    return res

-- ---------------------------------------------------------------------

entryDpFor :: Typeable a => AstContextSet -> a -> Pretty DeltaPos
entryDpFor ctx a = do
  (def
    `extQ` funBind
    `extQ` match
    ) a
  where
    lineDefault = if inAcs (Set.singleton AdvanceLine) ctx
                    then 1 else 0

    def :: a -> Pretty DeltaPos
    def _ = return $ DP (lineDefault,0)

    inCase = inAcs (Set.singleton CaseAlt) ctx
    -- listStart = inAcs (Set.singleton ListStart) ctx
    listStart = trace ("listStart:ctx=" ++ show ctx) $ inAcs (Set.singleton ListStart) ctx

    funBind :: GHC.HsBind GHC.RdrName -> Pretty DeltaPos
    funBind GHC.FunBind{} =
      if listStart
        then return $ DP (1,2)
        else return $ DP (2,0)
    funBind GHC.PatBind{} = return $ DP (2,0)
    funBind _ = return $ DP (lineDefault,0)

    match :: GHC.Match GHC.RdrName (GHC.LHsExpr GHC.RdrName) -> Pretty DeltaPos
    match _ = do
      -- if inCase then DP (1,2) else DP (0,0)
      if listStart
        then return (DP (0,0))
        else fromLayout (DP (1,0)) (DP (1,12))

-- ---------------------------------------------------------------------

-- |Like fromMaybe, in that if no layout flag is set return the first value,
-- else return the second and reset the layout flag.
fromLayout :: a -> a -> Pretty a
fromLayout def lay = do
  PrettyState{apMarkLayout} <- get
  if apMarkLayout
    then do
      -- error "foo"
      modify (\s -> s { apMarkLayout = False })
      return lay
    else return def


-- ---------------------------------------------------------------------

-- |Add some annotation to the currently active SrcSpan
addAnnotationsPretty :: Annotation -> Pretty ()
addAnnotationsPretty ann = do
    l <- ask
    return () `debug` ("addAnnotationsPretty:=" ++ showGhc (curSrcSpan l,prContext l))
    tellFinalAnn (getAnnKey l,ann)

getAnnKey :: PrettyOptions -> AnnKey
getAnnKey PrettyOptions {curSrcSpan, annConName}
  = AnnKey curSrcSpan annConName

-- ---------------------------------------------------------------------

countAnnsPretty :: GHC.AnnKeywordId -> Pretty Int
countAnnsPretty ann = return 0

-- ---------------------------------------------------------------------

withSortKey :: [(GHC.SrcSpan, Annotated b)] -> Pretty ()
withSortKey kws =
  let order = sortBy (comparing fst) kws
  in do
    tellSortKey (map fst order)
    mapM_ (prettyInterpret . snd) order

-- ---------------------------------------------------------------------

storeOriginalSrcSpanPretty :: AnnKey -> Pretty AnnKey
storeOriginalSrcSpanPretty key = return key

-- ---------------------------------------------------------------------

getSrcSpanForKw :: GHC.AnnKeywordId -> Pretty GHC.SrcSpan
getSrcSpanForKw kw = assert False undefined

-- ---------------------------------------------------------------------

#if __GLASGOW_HASKELL__ <= 710
storeString :: String -> GHC.SrcSpan -> Pretty ()
storeString s ss = return ()
#endif

-- ---------------------------------------------------------------------

setLayoutFlag :: Pretty () -> Pretty ()
setLayoutFlag action = do
  oldLay <- gets apLayoutStart
  modify (\s -> s { apMarkLayout = True } )
  let reset = do
                modify (\s -> s { apMarkLayout = False
                                , apLayoutStart = oldLay })
  action <* reset

-- ---------------------------------------------------------------------

setContextPretty :: Set.Set AstContext -> Int -> Pretty () -> Pretty ()
setContextPretty ctxt lvl =
  local (\s -> s { prContext = setAcsWithLevel ctxt lvl (prContext s) } )

ifInContextPretty :: Set.Set AstContext -> Annotated () -> Annotated () -> Pretty ()
ifInContextPretty ctxt ifAction elseAction = do
  cur <- asks prContext
  let inContext = inAcs ctxt cur
  if inContext
    then prettyInterpret ifAction
    else prettyInterpret elseAction

notInContextPretty :: Set.Set AstContext -> Annotated () -> Pretty ()
notInContextPretty ctxt action = do
  cur <- asks prContext
  let notInContext = not $ inAcs ctxt cur
  when notInContext (prettyInterpret action)

-- ---------------------------------------------------------------------

annotationsToCommentsPretty :: [GHC.AnnKeywordId] -> Pretty ()
annotationsToCommentsPretty kws = return ()

-- ---------------------------------------------------------------------

-- Writer helpers

tellFinalAnn :: (AnnKey, Annotation) -> Pretty ()
tellFinalAnn (k, v) =
  tell (mempty { dwAnns = Endo (Map.insert k v) })

tellKd :: (KeywordId, DeltaPos) -> Pretty ()
tellKd kd = tell (mempty { annKds = [kd] })

tellSortKey :: [GHC.SrcSpan] -> Pretty ()
tellSortKey xs = tell (mempty { sortKeys = Just xs } )
