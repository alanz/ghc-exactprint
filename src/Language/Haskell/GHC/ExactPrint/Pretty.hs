{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

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

import Control.Monad.RWS
import Control.Monad.Trans.Free
import Data.Generics
import Data.List
import Data.Ord (comparing)


#if __GLASGOW_HASKELL__ <= 710
import qualified BooleanFormula as GHC
import qualified Outputable     as GHC
#endif
import qualified GHC

import qualified Data.Map as Map
import qualified Data.Set as Set

{-# ANN module "HLint: ignore Eta reduce" #-}
{-# ANN module "HLint: ignore Redundant do" #-}
{-# ANN module "HLint: ignore Reduce duplication" #-}

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
       , annKds          :: ![(KeywordId, DeltaPos)]
       , sortKeys        :: !(Maybe [AnnSpan])
       , dwCapturedSpan  :: !(First AnnKey)
       , prLayoutContext :: !(ACS' AstContext)
       }

data PrettyState = PrettyState
       { -- | Position reached when processing the last element
         priorEndPosition    :: !Pos

         -- | Ordered list of comments still to be allocated
       , apComments :: ![Comment]

       , apMarkLayout  :: Bool
       , apLayoutStart :: LayoutStartCol

       , apNoPrecedingSpace :: Bool

       }

#if __GLASGOW_HASKELL__ >= 804
instance Semigroup PrettyWriter where
  (<>) = mappend
#endif

instance Monoid PrettyWriter where
  mempty = PrettyWriter mempty mempty mempty mempty mempty
  (PrettyWriter a b e g i) `mappend` (PrettyWriter c d f h j)
    = PrettyWriter (a <> c) (b <> d) (e <> f) (g <> h) (i <> j)

-- ---------------------------------------------------------------------

prettyOptions :: Rigidity -> PrettyOptions
prettyOptions ridigity =
  PrettyOptions
    { curSrcSpan = GHC.noSrcSpan
    , annConName = annGetConstr ()
    , drRigidity = ridigity
    , prContext  = defaultACS
    }

defaultPrettyState :: [Comment] -> Pos -> Anns -> PrettyState
defaultPrettyState injectedComments priorEnd _ans =
    PrettyState
      { priorEndPosition    = priorEnd
      , apComments = cs ++ injectedComments
      , apLayoutStart = 1
      , apMarkLayout = False
      , apNoPrecedingSpace = False
      }
  where
    cs :: [Comment]
    cs = []

-- ---------------------------------------------------------------------
-- Free Monad Interpretation code

prettyInterpret :: Annotated a -> Pretty a
prettyInterpret = iterTM go
  where
    go :: AnnotationF (Pretty a) -> Pretty a
    go (MarkPrim kwid _ next)           = addPrettyAnnotation (G kwid) >> next
    go (MarkPPOptional _kwid _ next)    = next
    go (MarkEOF next)                   = addEofAnnotation >> next
    go (MarkExternal _ss akwid _ next)  = addPrettyAnnotation (G akwid) >> next
#if __GLASGOW_HASKELL__ >= 800
    go (MarkInstead akwid kwid next)    = addPrettyAnnotationsInstead akwid kwid >> next
#endif
    go (MarkOutside akwid kwid next)    = addPrettyAnnotationsOutside akwid kwid >> next
    -- go (MarkOutside akwid kwid next)    = addPrettyAnnotation kwid >> next
    go (MarkInside akwid next)          = addPrettyAnnotationsInside akwid >> next
    go (MarkMany akwid next)            = addPrettyAnnotation (G akwid) >> next
    go (MarkManyOptional _akwid next)   = next
    go (MarkOffsetPrim akwid n _ next)  = addPrettyAnnotationLs akwid n >> next
    go (MarkOffsetPrimOptional _akwid _n _ next)  = next
    go (WithAST lss prog next)          = withAST lss (prettyInterpret prog) >> next
    go (CountAnns kwid next)            = countAnnsPretty kwid >>= next
    go (WithSortKey             kws next) = withSortKey             kws >> next
    go (WithSortKeyContexts ctx kws next) = withSortKeyContexts ctx kws >> next
    go (SetLayoutFlag r action next)    = do
      rigidity <- asks drRigidity
      (if r <= rigidity then setLayoutFlag else id) (prettyInterpret action)
      next
    go (StoreOriginalSrcSpan l key next) = storeOriginalSrcSpanPretty l key >>= next
    go (MarkAnnBeforeAnn _ann1 _ann2 next) = next
    go (GetSrcSpanForKw ss kw next)      = getSrcSpanForKw ss kw >>= next
#if __GLASGOW_HASKELL__ <= 710
    go (StoreString s ss next)           = storeString s ss >> next
#endif
    go (AnnotationsToComments kws next)       = annotationsToCommentsPretty kws >> next
#if __GLASGOW_HASKELL__ <= 710
    go (AnnotationsToCommentsBF bf kws next)  = annotationsToCommentsBFPretty bf kws >> next
    go (FinalizeBF l next)                    = finalizeBFPretty l >> next
#endif

    go (SetContextLevel ctxt lvl action next)  = setContextPretty ctxt lvl (prettyInterpret action) >> next
    go (UnsetContext    ctxt     action next)  = unsetContextPretty ctxt (prettyInterpret action) >> next
    go (IfInContext ctxt ia ea next)           = ifInContextPretty ctxt ia ea >> next
    go (TellContext c next)                    = tellContext c >> next

-- ---------------------------------------------------------------------

addEofAnnotation :: Pretty ()
addEofAnnotation = do
#if __GLASGOW_HASKELL__ >= 808
  tellKd (AnnEofPos, DP (1,0))
#else
  tellKd (G GHC.AnnEofPos, DP (1,0))
#endif

-- ---------------------------------------------------------------------

addPrettyAnnotation :: KeywordId -> Pretty ()
addPrettyAnnotation ann = do
  noPrec <- gets apNoPrecedingSpace
  ctx <- asks prContext
  _ <- debugP ("Pretty.addPrettyAnnotation:=" ++ showGhc (ann,noPrec,ctx)) $ asks prContext
  let
    dp = case ann of
           (G GHC.AnnAs)           -> tellKd (ann,DP (0,1))
           (G GHC.AnnAt)           -> tellKd (ann,DP (0,0))
#if __GLASGOW_HASKELL__ >= 806
           (G GHC.AnnAnyclass)     -> tellKd (ann,DP (0,1))
#endif
           (G GHC.AnnBackquote)    -> tellKd (ann,DP (0,1))
           (G GHC.AnnBang)         -> tellKd (ann,DP (0,1))
           (G GHC.AnnBy)           -> tellKd (ann,DP (0,1))
           (G GHC.AnnCase )        -> tellKd (ann,DP (0,1))
           (G GHC.AnnClass)        -> tellKd (ann,DP (0,1))
           (G GHC.AnnClose)        -> tellKd (ann,DP (0,1))
           (G GHC.AnnCloseC)       -> tellKd (ann,DP (0,0))
#if __GLASGOW_HASKELL__ >= 802
           (G GHC.AnnCloseQ)       -> tellKd (ann,DP (0,1))
#endif
           (G GHC.AnnDcolon)       -> tellKd (ann,DP (0,1))
           (G GHC.AnnDeriving)     -> tellKd (ann,DP (0,1))
           (G GHC.AnnDo)           -> tellKd (ann,DP (0,1))
#if __GLASGOW_HASKELL__ >= 808
           (G GHC.AnnDollar)       -> tellKd (ann,DP (0,1))
           (G GHC.AnnDollarDollar) -> tellKd (ann,DP (0,1))
#endif
           (G GHC.AnnDotdot)       -> tellKd (ann,DP (0,1))
           (G GHC.AnnElse)         -> tellKd (ann,DP (1,2))
           (G GHC.AnnEqual)        -> tellKd (ann,DP (0,1))
           (G GHC.AnnExport)       -> tellKd (ann,DP (0,1))
           (G GHC.AnnFamily)       -> tellKd (ann,DP (0,1))
           (G GHC.AnnForall)       -> tellKd (ann,DP (0,1))
           (G GHC.AnnGroup)        -> tellKd (ann,DP (0,1))
           (G GHC.AnnHiding)       -> tellKd (ann,DP (0,1))
           (G GHC.AnnIf)           -> tellKd (ann,DP (0,1))
           (G GHC.AnnImport)       -> tellKd (ann,DP (0,1))
           (G GHC.AnnIn)           -> tellKd (ann,DP (1,0))
           (G GHC.AnnInstance)     -> tellKd (ann,DP (0,1))
           (G GHC.AnnLam)          -> tellKd (ann,DP (0,1))
           (G GHC.AnnLet)          -> tellKd (ann,DP (0,1))
#if __GLASGOW_HASKELL__ >= 808
           -- (G GHC.AnnLolly)        -> tellKd (ann,DP (0,1))
           (G GHC.AnnLollyU)       -> tellKd (ann,DP (0,1))
           (G GHC.AnnPercentOne)   -> tellKd (ann,DP (0,1))
           (G GHC.AnnPercent)      -> tellKd (ann,DP (0,1))
#endif
           (G GHC.AnnMinus)        -> tellKd (ann,DP (0,1)) -- need to separate from preceding operator
           (G GHC.AnnModule)       -> tellKd (ann,DP (0,1))
           (G GHC.AnnNewtype)      -> tellKd (ann,DP (0,1))
           (G GHC.AnnOf)           -> tellKd (ann,DP (0,1))
           (G GHC.AnnOpenC)        -> tellKd (ann,DP (0,0))
           (G GHC.AnnOpenP)        -> tellKd (ann,DP (0,1))
           (G GHC.AnnOpenS)        -> tellKd (ann,DP (0,1))
#if __GLASGOW_HASKELL__ < 808
           (G GHC.AnnOpenPE)       -> tellKd (ann,DP (0,1))
           (G GHC.AnnOpenPTE)      -> tellKd (ann,DP (0,1))
#endif
           (G GHC.AnnQualified)    -> tellKd (ann,DP (0,1))
           (G GHC.AnnRarrow)       -> tellKd (ann,DP (0,1))
#if __GLASGOW_HASKELL__ > 710
           (G GHC.AnnRarrowU)      -> tellKd (ann,DP (0,1))
#endif
           (G GHC.AnnRole)         -> tellKd (ann,DP (0,1))
           (G GHC.AnnSafe)         -> tellKd (ann,DP (0,1))
#if __GLASGOW_HASKELL__ >= 806
           (G GHC.AnnStock)        -> tellKd (ann,DP (0,1))
#endif
           (G GHC.AnnSimpleQuote)  -> tellKd (ann,DP (0,1))
#if __GLASGOW_HASKELL__ < 808
           (G GHC.AnnThIdSplice)   -> tellKd (ann,DP (0,1))
           (G GHC.AnnThIdTySplice) -> tellKd (ann,DP (0,1))
#endif
           (G GHC.AnnThTyQuote)    -> tellKd (ann,DP (0,1))
           (G GHC.AnnThen)         -> tellKd (ann,DP (1,2))
           (G GHC.AnnTilde)        -> tellKd (ann,DP (0,1))
           (G GHC.AnnType)         -> tellKd (ann,DP (0,1))
           (G GHC.AnnUsing)        -> tellKd (ann,DP (0,1))
           (G GHC.AnnVal)          -> tellKd (ann,DP (0,1))
           (G GHC.AnnValStr)       -> tellKd (ann,DP (0,1))
           (G GHC.AnnVbar)         -> tellKd (ann,DP (0,1))
#if __GLASGOW_HASKELL__ >= 806
           (G GHC.AnnVia)          -> tellKd (ann,DP (0,1))
#endif
           (G GHC.AnnWhere)        -> tellKd (ann,DP (1,2))
#if __GLASGOW_HASKELL__ >= 800
           AnnTypeApp              -> tellKd (ann,DP (0,1))
#endif
           _ ->                tellKd (ann,DP (0,0))
  fromNoPrecedingSpace (tellKd (ann,DP (0,0))) dp

-- ---------------------------------------------------------------------

#if __GLASGOW_HASKELL__ >= 800
addPrettyAnnotationsInstead :: GHC.AnnKeywordId -> KeywordId -> Pretty ()
addPrettyAnnotationsInstead _akwid AnnSemiSep = return ()
addPrettyAnnotationsInstead _akwid kwid = addPrettyAnnotation kwid
#endif

-- ---------------------------------------------------------------------

addPrettyAnnotationsOutside :: GHC.AnnKeywordId -> KeywordId -> Pretty ()
addPrettyAnnotationsOutside _akwid AnnSemiSep = return ()
addPrettyAnnotationsOutside _akwid kwid = addPrettyAnnotation kwid

-- ---------------------------------------------------------------------

addPrettyAnnotationsInside :: GHC.AnnKeywordId -> Pretty ()
addPrettyAnnotationsInside _ann = return ()

-- ---------------------------------------------------------------------

addPrettyAnnotationLs :: GHC.AnnKeywordId -> Int -> Pretty ()
addPrettyAnnotationLs ann _off = addPrettyAnnotation (G ann)

-- ---------------------------------------------------------------------

#if __GLASGOW_HASKELL__ <= 710
getUnallocatedComments :: Pretty [Comment]
getUnallocatedComments = gets apComments

putUnallocatedComments :: [Comment] -> Pretty ()
putUnallocatedComments cs = modify (\s -> s { apComments = cs } )
#endif

-- ---------------------------------------------------------------------

withSrcSpanPretty :: Data a => GHC.Located a -> Pretty b -> Pretty b
withSrcSpanPretty (GHC.L l a) action = do
  -- peek into the current state of the output, to extract the layout context
  -- flags passed up from subelements of the AST.
  (_,w) <- listen (return () :: Pretty ())

  _ <- debugP ("withSrcSpanPretty: prLayoutContext w=" ++ show (prLayoutContext w) ) (return ())

  local (\s -> s { curSrcSpan = l
                 , annConName = annGetConstr a
                 -- , prContext  = pushAcs (prContext s)
                 , prContext  = (pushAcs (prContext s)) <> (prLayoutContext w)
                 })
        action

-- ---------------------------------------------------------------------

-- | Enter a new AST element. Maintain SrcSpan stack
withAST :: Data a
        => GHC.Located a
        -> Pretty b -> Pretty b
withAST lss@(GHC.L ss t) action = do
  return () `debug` ("Pretty.withAST:enter 1:(ss)=" ++ showGhc (ss,showConstr (toConstr t)))
  -- Calculate offset required to get to the start of the SrcSPan
  -- off <- gets apLayoutStart
  withSrcSpanPretty lss $ do
    return () `debug` ("Pretty.withAST:enter:(ss)=" ++ showGhc (ss,showConstr (toConstr t)))

    let maskWriter s = s { annKds          = []
                         , sortKeys        = Nothing
                         , dwCapturedSpan  = mempty
                         -- , prLayoutContext = pushAcs (prLayoutContext s)
                         }

#if __GLASGOW_HASKELL__ <= 710
    let spanStart = ss2pos ss
    cs <- do
      if GHC.isGoodSrcSpan ss
        then
          commentAllocation (priorComment spanStart) return
        else
          return []
#else
    let cs = []
#endif

    -- ctx <- debugP ("Pretty.withAST:cs:(ss,cs,uncs)=" ++ showGhc (ss,cs,uncs)) $ asks prContext
    ctx <- asks prContext

    noPrec <- gets apNoPrecedingSpace
    edp <- debugP ("Pretty.withAST:enter:(ss,constr,noPrec,ctx)=" ++ showGhc (ss,showConstr (toConstr t),noPrec,ctx)) $ entryDpFor ctx t
    -- edp <- entryDpFor ctx t

    let ctx1 = debugP ("Pretty.withAST:edp:(ss,constr,edp)=" ++ showGhc (ss,showConstr (toConstr t),edp)) ctx
    -- (res, w) <- if inAcs (Set.fromList [ListItem,TopLevel]) ctx1
    (res, w) <- if inAcs (Set.fromList [ListItem,TopLevel,InTypeApp]) ctx1
      then
           -- debugP ("Pretty.withAST:setNoPrecedingSpace") $
             censor maskWriter (listen (setNoPrecedingSpace action))
      else
           -- debugP ("Pretty.withAST:setNoPrecedingSpace") $
            censor maskWriter (listen action)

    let kds = annKds w
        an = Ann
               { annEntryDelta        = edp
               , annPriorComments     = cs
               , annFollowingComments = [] -- only used in Transform and Print
               , annsDP               = kds
               , annSortKey           = sortKeys w
               , annCapturedSpan      = getFirst $ dwCapturedSpan w
               }

    addAnnotationsPretty an
     `debug` ("Pretty.withAST:(annkey,an)=" ++ show (mkAnnKey lss,an))
    return res

-- ---------------------------------------------------------------------

entryDpFor :: Typeable a => AstContextSet -> a -> Pretty DeltaPos
entryDpFor ctx a = (def `extQ` grhs) a
  where
    lineDefault = if inAcs (Set.singleton AdvanceLine) ctx
                    then 1 else 0
    noAdvanceLine = inAcs (Set.singleton NoAdvanceLine) ctx &&
                    inAcs (Set.singleton ListStart) ctx

    def :: a -> Pretty DeltaPos
    def _ =
      debugP ("entryDpFor:(topLevel,listStart,inList,noAdvanceLine,ctx)=" ++ show (topLevel,listStart,inList,noAdvanceLine,ctx)) $
        if noAdvanceLine
          then (if inTypeApp then return (DP (0,0)) else return (DP (0,1)))
          -- then (if inTypeApp then error "inTypeAp" else return (DP (0,1)))
          else
            if listStart
              then return (DP (1,2))
              else if inList
                then if topLevel then return (DP (2,0)) else return (DP (1,0))
                else if topLevel then return (DP (2,0)) else return (DP (lineDefault,0))

    topLevel = inAcs (Set.singleton TopLevel) ctx
    listStart = inAcs (Set.singleton ListStart) ctx
              && not (inAcs (Set.singleton TopLevel) ctx)
    inList = inAcs (Set.singleton ListItem) ctx
    inLambda = inAcs (Set.singleton LambdaExpr) ctx
    inTypeApp = inAcs (Set.singleton InTypeApp) ctx

    grhs :: GHC.GRHS GHC.RdrName (GHC.LHsExpr GHC.RdrName) -> Pretty DeltaPos
    grhs _ = do
      if inLambda
        then return (DP (0,1))
        else return (DP (1,2))

-- ---------------------------------------------------------------------

fromNoPrecedingSpace :: Pretty a -> Pretty a -> Pretty a
fromNoPrecedingSpace def lay = do
  PrettyState{apNoPrecedingSpace} <- get
  -- ctx <- asks prContext
  if apNoPrecedingSpace
    then do
      modify (\s -> s { apNoPrecedingSpace = False
                      })
      debugP ("fromNoPrecedingSpace:def") def
      -- def
    else
      -- lay
      debugP ("fromNoPrecedingSpace:lay") lay


-- ---------------------------------------------------------------------

-- |Add some annotation to the currently active SrcSpan
addAnnotationsPretty :: Annotation -> Pretty ()
addAnnotationsPretty ann = do
    l <- ask
    return () `debug` ("addAnnotationsPretty:=" ++ showGhc (curSrcSpan l,prContext l))
    tellFinalAnn (getAnnKey l,ann)

getAnnKey :: PrettyOptions -> AnnKey
getAnnKey PrettyOptions {curSrcSpan, annConName}
  = AnnKey (rs curSrcSpan) annConName

-- ---------------------------------------------------------------------

countAnnsPretty :: GHC.AnnKeywordId -> Pretty Int
countAnnsPretty _ann = return 0

-- ---------------------------------------------------------------------

withSortKey :: [(AnnSpan, Annotated b)] -> Pretty ()
withSortKey kws =
  let order = sortBy (comparing fst) kws
  in do
    tellSortKey (map fst order)
    mapM_ (prettyInterpret . snd) order

withSortKeyContexts :: ListContexts -> [(AnnSpan, Annotated ())] -> Pretty ()
withSortKeyContexts ctxts kws =
  let order = sortBy (comparing fst) kws
  in do
    tellSortKey (map fst order)
    withSortKeyContextsHelper prettyInterpret ctxts order

-- ---------------------------------------------------------------------

storeOriginalSrcSpanPretty :: GHC.SrcSpan -> AnnKey -> Pretty AnnKey
storeOriginalSrcSpanPretty _s key = do
  tellCapturedSpan key
  return key

-- ---------------------------------------------------------------------

getSrcSpanForKw :: GHC.SrcSpan -> GHC.AnnKeywordId -> Pretty GHC.SrcSpan
getSrcSpanForKw ss _kw = return ss

-- ---------------------------------------------------------------------

#if __GLASGOW_HASKELL__ <= 710
storeString :: String -> GHC.SrcSpan -> Pretty ()
storeString s _ss = addPrettyAnnotation (AnnString s)
#endif

-- ---------------------------------------------------------------------

setLayoutFlag :: Pretty () -> Pretty ()
setLayoutFlag action = do
  oldLay <- gets apLayoutStart
  modify (\s -> s { apMarkLayout = True } )
  let reset = modify (\s -> s { apMarkLayout = False
                              , apLayoutStart = oldLay })
  action <* reset

-- ---------------------------------------------------------------------

setNoPrecedingSpace :: Pretty a -> Pretty a
setNoPrecedingSpace action = do
  oldVal <- gets apNoPrecedingSpace
  modify (\s -> s { apNoPrecedingSpace = True } )
  let reset = modify (\s -> s { apNoPrecedingSpace = oldVal })
  action <* reset

-- ---------------------------------------------------------------------

setContextPretty :: Set.Set AstContext -> Int -> Pretty () -> Pretty ()
setContextPretty ctxt lvl =
  local (\s -> s { prContext = setAcsWithLevel ctxt lvl (prContext s) } )

unsetContextPretty :: AstContext -> Pretty () -> Pretty ()
unsetContextPretty ctxt =
  local (\s -> s { prContext = unsetAcs ctxt (prContext s) } )


ifInContextPretty :: Set.Set AstContext -> Annotated () -> Annotated () -> Pretty ()
ifInContextPretty ctxt ifAction elseAction = do
  cur <- asks prContext
  let inContext = inAcs ctxt cur
  if inContext
    then prettyInterpret ifAction
    else prettyInterpret elseAction

-- ---------------------------------------------------------------------

annotationsToCommentsPretty :: [GHC.AnnKeywordId] -> Pretty ()
annotationsToCommentsPretty _kws = return ()

-- ---------------------------------------------------------------------

#if __GLASGOW_HASKELL__ <= 710
annotationsToCommentsBFPretty :: (GHC.Outputable a) => GHC.BooleanFormula (GHC.Located a) -> [GHC.AnnKeywordId] -> Pretty ()
annotationsToCommentsBFPretty bf _kws = do
  -- cs <- gets apComments
  cs <- debugP ("annotationsToCommentsBFPretty:" ++ showGhc (bf,makeBooleanFormulaAnns bf)) $ gets apComments
  -- return$ debugP ("annotationsToCommentsBFPretty:" ++ showGhc (bf,makeBooleanFormulaAnns bf)) ()
  -- error ("annotationsToCommentsBFPretty:" ++ showGhc (bf,makeBooleanFormulaAnns bf))
  let
    kws = makeBooleanFormulaAnns bf
    newComments = map (uncurry mkKWComment ) kws
  putUnallocatedComments (cs ++ newComments)


finalizeBFPretty :: GHC.SrcSpan -> Pretty ()
finalizeBFPretty _ss = do
  commentAllocation (const True) (mapM_ (uncurry addPrettyComment))
  return ()
#endif

-- ---------------------------------------------------------------------
#if __GLASGOW_HASKELL__ <= 710
-- |Split the ordered list of comments into ones that occur prior to
-- the give SrcSpan and the rest
priorComment :: Pos -> Comment -> Bool
priorComment start c = (ss2pos . commentIdentifier $ c) < start

-- TODO:AZ: We scan the entire comment list here. It may be better to impose an
-- invariant that the comments are sorted, and consume them as the pos
-- advances. It then becomes a process of using `takeWhile p` rather than a full
-- partition.
allocateComments :: (Comment -> Bool) -> [Comment] -> ([Comment], [Comment])
allocateComments = partition
#endif

-- ---------------------------------------------------------------------

#if __GLASGOW_HASKELL__ <= 710
commentAllocation :: (Comment -> Bool)
                  -> ([(Comment, DeltaPos)] -> Pretty a)
                  -> Pretty a
commentAllocation p k = do
  cs <- getUnallocatedComments
  let (allocated,cs') = allocateComments p cs
  putUnallocatedComments cs'
  k =<< mapM makeDeltaComment (sortBy (comparing commentIdentifier) allocated)

makeDeltaComment :: Comment -> Pretty (Comment, DeltaPos)
makeDeltaComment c = do
  return (c, DP (0,1))

addPrettyComment :: Comment -> DeltaPos -> Pretty ()
addPrettyComment d p = do
  tellKd (AnnComment d, p)
#endif

-- ---------------------------------------------------------------------

-- Writer helpers

tellFinalAnn :: (AnnKey, Annotation) -> Pretty ()
tellFinalAnn (k, v) =
  tell (mempty { dwAnns = Endo (Map.insert k v) })

tellCapturedSpan :: AnnKey -> Pretty ()
tellCapturedSpan key = tell ( mempty { dwCapturedSpan = First $ Just key })

tellKd :: (KeywordId, DeltaPos) -> Pretty ()
tellKd kd = tell (mempty { annKds = [kd] })

tellSortKey :: [AnnSpan] -> Pretty ()
tellSortKey xs = tell (mempty { sortKeys = Just xs } )

tellContext :: Set.Set AstContext -> Pretty ()
tellContext lc = tell (mempty { prLayoutContext = setAcsWithLevel lc 2 mempty} )
