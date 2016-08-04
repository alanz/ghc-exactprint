{-# LANGUAGE CPP #-}
{-# LANGUAGE NamedFieldPuns #-}
-- |  This module converts 'GHC.ApiAnns' into 'Anns' by traversing a
-- structure created by the "Annotate" modue.
--
-- == Structure of an Annotation
--
-- As a rule of thumb, every located element in the GHC AST will have
-- a corresponding entry in 'Anns'. An 'Annotation' contains 6 fields which
-- can be modifed to change how the AST is printed.
--
-- == Layout Calculation
--
-- Certain expressions such as do blocks and let bindings obey
-- <https://en.wikibooks.org/wiki/Haskell/Indentation layout rules>. We
-- calculate the 'annEntryDelta' slightly differently when such rules
-- apply.
--
-- 1. The first element which the layout rule applies to is given
-- a 'annEntryDelta' as normal.
-- 2. Further elements which must obey the rules are then given
-- 'annEntryDelta's relative to the LHS of the first element.
--
-- For example, in the following expression the statement corresponding to
-- `baz` will be given a 'annEntryDelta' of @DP (1, 2)@ as it appears
-- 1 line and 2 columns after the @do@ keyword. On the other hand, @bar@
-- will be given a 'annEntryDelta' of @DP (1,0)@ as it appears 1 line
-- further than @baz@ but in the same column as the start of the layout
-- block.
--
-- @
-- foo = do
--   baz
--   bar
-- @
--
-- A useful way to think of these rules is that the 'DeltaPos' is relative
-- to the further left an expression could have been placed. In the
-- previous example, we could have placed @baz@ anywhere on the line as its
-- position determines where the other statements must be. @bar@ could have
-- not been placed any further left without resulting in a syntax error
-- which is why the relative column is 0.
--
-- === annTrueEntryDelta
-- A very useful function is 'annTrueEntryDelta' which calculates the
-- offset from the last synctactic element (ignoring comments). This is
-- different to 'annEntryDelta' which does not ignore comments.
--
--
--
module Language.Haskell.GHC.ExactPrint.Delta
  ( relativiseApiAnns
  , relativiseApiAnnsWithComments
  , relativiseApiAnnsWithOptions

  -- * Configuration
  , DeltaOptions(drRigidity)
  , deltaOptions
  , normalLayout
  ) where

-- import Control.Exception
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
import Language.Haskell.GHC.ExactPrint.Annotate

import qualified GHC

import qualified Data.Map as Map
import qualified Data.Set as Set

-- import Debug.Trace

{-# ANN module "HLint: ignore Eta reduce" #-}

-- ---------------------------------------------------------------------
-- | Transform concrete annotations into relative annotations which are
-- more useful when transforming an AST.
relativiseApiAnns :: Annotate ast
                  => GHC.Located ast
                  -> GHC.ApiAnns
                  -> Anns
relativiseApiAnns = relativiseApiAnnsWithComments []

-- | Exactly the same as 'relativiseApiAnns' but with the possibilty to
-- inject comments. This is typically used if the source has been preprocessed
-- by e.g. CPP, and the parts stripped out of the original source are re-added
-- as comments so they are not lost for round tripping.
relativiseApiAnnsWithComments ::
                     Annotate ast
                  => [Comment]
                  -> GHC.Located ast
                  -> GHC.ApiAnns
                  -> Anns
relativiseApiAnnsWithComments =
    relativiseApiAnnsWithOptions normalLayout

relativiseApiAnnsWithOptions ::
                     Annotate ast
                  => DeltaOptions
                  -> [Comment]
                  -> GHC.Located ast
                  -> GHC.ApiAnns
                  -> Anns
relativiseApiAnnsWithOptions opts cs modu ghcAnns
   = runDeltaWithComments
      opts cs (annotate modu) ghcAnns
      (ss2pos $ GHC.getLoc modu)

-- ---------------------------------------------------------------------
--
-- | Type used in the Delta Monad.
type Delta a = RWS DeltaOptions DeltaWriter DeltaState a

runDeltaWithComments :: DeltaOptions -> [Comment] -> Annotated () -> GHC.ApiAnns -> Pos -> Anns
runDeltaWithComments opts cs action ga priorEnd =
  mkAnns . snd
  . (\next -> execRWS next opts (defaultDeltaState cs priorEnd ga))
  . deltaInterpret $ action
  where
    mkAnns :: DeltaWriter -> Anns
    mkAnns = f . dwAnns
    f :: Monoid a => Endo a -> a
    f = ($ mempty) . appEndo

-- ---------------------------------------------------------------------

-- TODO: rename this, it is the R part of the RWS
data DeltaOptions = DeltaOptions
       {
         -- | Current `SrcSpan, part of current AnnKey`
         curSrcSpan :: !GHC.SrcSpan

         -- | Constuctor of current AST element, part of current AnnKey
       , annConName :: !AnnConName

        -- | Whether to use rigid or normal layout rules
       , drRigidity :: !Rigidity

       -- | Current higher level context. e.g. whether a Match is part of a
       -- LambdaExpr or a FunBind
       , drContext :: !AstContextSet
       }

data DeltaWriter = DeltaWriter
       { -- | Final list of annotations, and sort keys
         dwAnns :: Endo (Map.Map AnnKey Annotation)

         -- | Used locally to pass Keywords, delta pairs relevant to a specific
         -- subtree to the parent.
       , annKds         :: ![(KeywordId, DeltaPos)]
       , sortKeys       :: !(Maybe [GHC.SrcSpan])
       , dwCapturedSpan :: !(First AnnKey)
       }

data DeltaState = DeltaState
       { -- | Position reached when processing the last element
         priorEndPosition    :: !Pos

         -- | Ordered list of comments still to be allocated
       , apComments :: ![Comment]

         -- | The original GHC Delta Annotations
       , apAnns :: !GHC.ApiAnns

       , apMarkLayout :: Bool
       , apLayoutStart :: LayoutStartCol

       }

-- ---------------------------------------------------------------------

deltaOptions :: Rigidity -> DeltaOptions
deltaOptions ridigity =
  DeltaOptions
    { curSrcSpan = GHC.noSrcSpan
    , annConName = annGetConstr ()
    , drRigidity = ridigity
    , drContext  = defaultACS
    }

normalLayout :: DeltaOptions
normalLayout = deltaOptions NormalLayout

defaultDeltaState :: [Comment] -> Pos -> GHC.ApiAnns -> DeltaState
defaultDeltaState injectedComments priorEnd ga =
    DeltaState
      { priorEndPosition    = priorEnd
      , apComments = cs ++ injectedComments
      , apAnns     = ga
      , apLayoutStart = 1
      , apMarkLayout = False
      }
  where
    cs :: [Comment]
    cs = extractComments ga


-- Writer helpers

tellFinalAnn :: (AnnKey, Annotation) -> Delta ()
tellFinalAnn (k, v) =
  -- tell (mempty { dwAnns = Endo (Map.insertWith (<>) k v) })
  tell (mempty { dwAnns = Endo (Map.insert k v) })

tellSortKey :: [GHC.SrcSpan] -> Delta ()
tellSortKey xs = tell (mempty { sortKeys = Just xs } )

tellCapturedSpan :: AnnKey -> Delta ()
tellCapturedSpan key = tell ( mempty { dwCapturedSpan = First $ Just key })

tellKd :: (KeywordId, DeltaPos) -> Delta ()
tellKd kd = tell (mempty { annKds = [kd] })

instance Monoid DeltaWriter where
  mempty = DeltaWriter mempty mempty mempty mempty
  (DeltaWriter a b e g) `mappend` (DeltaWriter c d f h)
    = DeltaWriter (a <> c) (b <> d) (e <> f) (g <> h)

-----------------------------------
-- Free Monad Interpretation code

deltaInterpret :: Annotated a -> Delta a
deltaInterpret = iterTM go
  where
    go :: AnnotationF (Delta a) -> Delta a
    go (MarkEOF next)                   = addEofAnnotation >> next
    go (MarkPrim kwid _ next)           = addDeltaAnnotation kwid >> next
    go (MarkPPOptional kwid _ next)     = addDeltaAnnotation kwid >> next
    go (MarkOutside akwid kwid next)    = addDeltaAnnotationsOutside akwid kwid >> next
    go (MarkInside akwid next)          = addDeltaAnnotationsInside akwid >> next
    go (MarkMany akwid next)            = addDeltaAnnotations akwid >> next
    go (MarkManyOptional akwid next)    = addDeltaAnnotations akwid >> next
    go (MarkOffsetPrim akwid n _ next)  = addDeltaAnnotationLs akwid n >> next
    go (MarkOffsetPrimOptional akwid n _ next) = addDeltaAnnotationLs akwid n >> next
    go (WithAST lss prog next)          = withAST lss (deltaInterpret prog) >> next
    go (CountAnns kwid next)            = countAnnsDelta kwid >>= next
    go (SetLayoutFlag r action next)    = do
      rigidity <- asks drRigidity
      (if r <= rigidity then setLayoutFlag else id) (deltaInterpret action)
      next
    go (MarkExternal ss akwid _ next)    = addDeltaAnnotationExt ss akwid >> next
    go (StoreOriginalSrcSpan _ key next) = storeOriginalSrcSpanDelta key >>= next
    go (GetSrcSpanForKw ss kw next)      = getSrcSpanForKw ss kw >>= next
#if __GLASGOW_HASKELL__ <= 710
    go (StoreString s ss next)           = storeString s ss >> next
#endif
    go (AnnotationsToComments     kws next) = annotationsToCommentsDelta kws >> next
#if __GLASGOW_HASKELL__ <= 710
    go (AnnotationsToCommentsBF _ kws next) = annotationsToCommentsDelta kws >> next
    go (FinalizeBF _ next)                  = next
#endif
    go (WithSortKey             kws next) = withSortKey kws >> next
    go (WithSortKeyContexts ctx kws next) = withSortKeyContexts ctx kws >> next

    go (SetContextLevel ctxt lvl action next) = setContextDelta ctxt lvl (deltaInterpret action) >> next
    go (UnsetContext   _ctxt action next) = deltaInterpret action >> next
    go (IfInContext    ctxt ifAction elseAction next) = ifInContextDelta ctxt ifAction elseAction >> next
    go (NotInContext ctxt action next)        = notInContextDelta ctxt action >> next
    go (BumpContext action next)              = bumpContextDelta (deltaInterpret action) >> next

withSortKey :: [(GHC.SrcSpan, Annotated b)] -> Delta ()
withSortKey kws =
  let order = sortBy (comparing fst) kws
  in do
    tellSortKey (map fst order)
    mapM_ (deltaInterpret . snd) order


withSortKeyContexts :: ListContexts -> [(GHC.SrcSpan, Annotated ())] -> Delta ()
withSortKeyContexts ctxts kws = do
  tellSortKey (map fst order)
  -- mapM_ (deltaInterpret . snd) order
  withSortKeyContextsHelper deltaInterpret ctxts order
  where
    order = sortBy (comparing fst) kws


setLayoutFlag :: Delta () -> Delta ()
setLayoutFlag action = do
  oldLay <- gets apLayoutStart
  modify (\s -> s { apMarkLayout = True } )
  let reset = do
                modify (\s -> s { apMarkLayout = False
                                , apLayoutStart = oldLay })
  action <* reset

-- ---------------------------------------------------------------------

setContextDelta :: Set.Set AstContext -> Int -> Delta () -> Delta ()
setContextDelta ctxt lvl =
  local (\s -> s { drContext = setAcsWithLevel ctxt lvl (drContext s) } )

ifInContextDelta :: Set.Set AstContext -> Annotated () -> Annotated () -> Delta ()
ifInContextDelta ctxt ifAction elseAction = do
  cur <- asks drContext
  let inContext = inAcs ctxt cur
  if inContext
    then deltaInterpret ifAction
    else deltaInterpret elseAction

notInContextDelta :: Set.Set AstContext -> Annotated () -> Delta ()
notInContextDelta ctxt action = do
  cur <- asks drContext
  let notInContext = not $ inAcs ctxt cur
  when notInContext (deltaInterpret action)

bumpContextDelta :: Delta () -> Delta ()
bumpContextDelta =
  local (\s -> s { drContext = bumpAcs (drContext s) } )

-- ---------------------------------------------------------------------

storeOriginalSrcSpanDelta :: AnnKey -> Delta AnnKey
storeOriginalSrcSpanDelta key = do
  tellCapturedSpan key
  return key

#if __GLASGOW_HASKELL__ <= 710
storeString :: String -> GHC.SrcSpan -> Delta ()
storeString s ss = addAnnotationWorker (AnnString s) ss
#endif

-- ---------------------------------------------------------------------

-- |In order to interleave annotations into the stream, we turn them into
-- comments.
annotationsToCommentsDelta :: [GHC.AnnKeywordId] -> Delta ()
annotationsToCommentsDelta kws = do
  ga <- gets apAnns
  ss <- getSrcSpan
  cs <- gets apComments
  let
    doOne :: GHC.AnnKeywordId -> [Comment]
    doOne kw = comments
      where
        spans = GHC.getAnnotation ga ss kw
        comments = map (mkKWComment kw) spans
    -- TODO:AZ make sure these are sorted/merged properly when the invariant for
    -- allocateComments is re-established.
    newComments = concatMap doOne kws
  putUnallocatedComments (cs ++ newComments)

-- ---------------------------------------------------------------------

-- | This function exists to overcome a shortcoming in the GHC AST for 7.10.1
getSrcSpanForKw :: GHC.SrcSpan -> GHC.AnnKeywordId -> Delta GHC.SrcSpan
getSrcSpanForKw _ kw = do
    ga <- gets apAnns
    ss <- getSrcSpan
    case GHC.getAnnotation ga ss kw of
      []     -> return GHC.noSrcSpan
      (sp:_) -> return sp

-- ---------------------------------------------------------------------

getSrcSpan :: Delta GHC.SrcSpan
getSrcSpan = asks curSrcSpan

withSrcSpanDelta :: Data a => GHC.Located a -> Delta b -> Delta b
withSrcSpanDelta (GHC.L l a) =
  local (\s -> s { curSrcSpan = l
                 , annConName = annGetConstr a
                 , drContext = pushAcs (drContext s)
                 })


getUnallocatedComments :: Delta [Comment]
getUnallocatedComments = gets apComments

putUnallocatedComments :: [Comment] -> Delta ()
putUnallocatedComments cs = modify (\s -> s { apComments = cs } )

-- ---------------------------------------------------------------------

adjustDeltaForOffsetM :: DeltaPos -> Delta DeltaPos
adjustDeltaForOffsetM dp = do
  colOffset <- gets apLayoutStart
  return (adjustDeltaForOffset colOffset dp)

adjustDeltaForOffset :: LayoutStartCol -> DeltaPos -> DeltaPos
adjustDeltaForOffset _colOffset              dp@(DP (0,_)) = dp -- same line
adjustDeltaForOffset (LayoutStartCol colOffset) (DP (l,c)) = DP (l,c - colOffset)

-- ---------------------------------------------------------------------

getPriorEnd :: Delta Pos
getPriorEnd = gets priorEndPosition

setPriorEnd :: Pos -> Delta ()
setPriorEnd pe =
  modify (\s -> s { priorEndPosition = pe })

setPriorEndAST :: GHC.SrcSpan -> Delta ()
setPriorEndAST pe = do
  setLayoutStart (snd (ss2pos pe))
  modify (\s -> s { priorEndPosition    = ss2posEnd pe } )

setLayoutStart :: Int -> Delta ()
setLayoutStart p = do
  DeltaState{apMarkLayout} <- get
  when apMarkLayout (
                      modify (\s -> s { apMarkLayout = False
                                     , apLayoutStart = LayoutStartCol p}))


-- -------------------------------------

peekAnnotationDelta :: GHC.AnnKeywordId -> Delta [GHC.SrcSpan]
peekAnnotationDelta an = do
    ga <- gets apAnns
    ss <- getSrcSpan
#if __GLASGOW_HASKELL__ <= 710
    return $ GHC.getAnnotation ga ss an
#else
    let unicodeAnns = case unicodeEquivalent an of
          [] -> []
          [kw] -> GHC.getAnnotation ga ss kw
    return $ unicodeAnns ++ GHC.getAnnotation ga ss an
#endif

getAnnotationDelta :: GHC.AnnKeywordId -> Delta ([GHC.SrcSpan],GHC.AnnKeywordId)
getAnnotationDelta an = do
    ss <- getSrcSpan
    getAndRemoveAnnotationDelta ss an

getAndRemoveAnnotationDelta :: GHC.SrcSpan -> GHC.AnnKeywordId -> Delta ([GHC.SrcSpan],GHC.AnnKeywordId)
getAndRemoveAnnotationDelta sp an = do
    ga <- gets apAnns
#if __GLASGOW_HASKELL__ <= 710
    let (r,ga') = GHC.getAndRemoveAnnotation ga sp an
        kw = an
#else
    let (r,ga',kw) = case GHC.getAndRemoveAnnotation ga sp an of
                    ([],_) -> (ss,g,k)
                      where
                        k = GHC.unicodeAnn an
                        (ss,g) = GHC.getAndRemoveAnnotation ga sp k
                    (ss,g)  -> (ss,g,an)
#endif
    modify (\s -> s { apAnns = ga' })
    return (r,kw)

getOneAnnotationDelta :: GHC.AnnKeywordId -> Delta ([GHC.SrcSpan],GHC.AnnKeywordId)
getOneAnnotationDelta an = do
    ss <- getSrcSpan
    getAndRemoveOneAnnotationDelta ss an

getAndRemoveOneAnnotationDelta :: GHC.SrcSpan -> GHC.AnnKeywordId -> Delta ([GHC.SrcSpan],GHC.AnnKeywordId)
getAndRemoveOneAnnotationDelta sp an = do
    (anns,cs) <- gets apAnns
#if __GLASGOW_HASKELL__ <= 710
    let (r,ga',kw) = case Map.lookup (sp,an) anns of
                    Nothing -> ([],(anns,cs),an)
                    Just []     -> ([], (Map.delete (sp,an)    anns,cs),an)
                    Just (s:ss) -> ([s],(Map.insert (sp,an) ss anns,cs),an)
#else
    let getKw kw =
          case Map.lookup (sp,kw) anns of
            Nothing -> ([],(anns,cs),kw)
            Just []     -> ([], (Map.delete (sp,kw)    anns,cs),kw)
            Just (s:ss) -> ([s],(Map.insert (sp,kw) ss anns,cs),kw)

    let (r,ga',kw) =
          case getKw an of
            ([],_,_) -> getKw (GHC.unicodeAnn an)
            v        -> v
#endif
    modify (\s -> s { apAnns = ga' })
    return (r,kw)

-- ---------------------------------------------------------------------

-- |Add some annotation to the currently active SrcSpan
addAnnotationsDelta :: Annotation -> Delta ()
addAnnotationsDelta ann = do
    l <- ask
    tellFinalAnn (getAnnKey l,ann)

getAnnKey :: DeltaOptions -> AnnKey
getAnnKey DeltaOptions {curSrcSpan, annConName}
  = AnnKey curSrcSpan annConName

-- -------------------------------------

addAnnDeltaPos :: KeywordId -> DeltaPos -> Delta ()
addAnnDeltaPos kw dp = tellKd (kw, dp)

-- -------------------------------------

-- | Enter a new AST element. Maintain SrcSpan stack
withAST :: Data a
        => GHC.Located a
        -> Delta b -> Delta b
withAST lss@(GHC.L ss _) action = do
  -- Calculate offset required to get to the start of the SrcSPan
  off <- gets apLayoutStart
  (resetAnns .  withSrcSpanDelta lss) (do

    let maskWriter s = s { annKds = []
                         , sortKeys = Nothing
                         , dwCapturedSpan = mempty }

    -- make sure all kds are relative to the start of the SrcSpan
    let spanStart = ss2pos ss

    cs <- do
      priorEndBeforeComments <- getPriorEnd
      if GHC.isGoodSrcSpan ss && priorEndBeforeComments < ss2pos ss
        then
          commentAllocation (priorComment spanStart) return
        else
          return []
    priorEndAfterComments <- getPriorEnd
    let edp = adjustDeltaForOffset
                -- Use the propagated offset if one is set
                -- Note that we need to use the new offset if it has
                -- changed.
                off (ss2delta priorEndAfterComments ss)
    -- Preparation complete, perform the action
    when (GHC.isGoodSrcSpan ss && priorEndAfterComments < ss2pos ss) (do
      modify (\s -> s { priorEndPosition    = (ss2pos ss) } ))
    (res, w) <- censor maskWriter (listen action)

    let kds = annKds w
        an = Ann
               { annEntryDelta = edp
               , annPriorComments = cs
               , annFollowingComments = [] -- only used in Transform and Print
               , annsDP     = kds
               , annSortKey = sortKeys w
               , annCapturedSpan = getFirst $ dwCapturedSpan w }

    addAnnotationsDelta an
     `debug` ("leaveAST:(annkey,an)=" ++ show (mkAnnKey lss,an))
    return res)

resetAnns :: Delta a -> Delta a
resetAnns action = do
  ans <- gets apAnns
  action <* modify (\s -> s { apAnns = ans })


-- ---------------------------------------------------------------------
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

-- ---------------------------------------------------------------------

addAnnotationWorker :: KeywordId -> GHC.SrcSpan -> Delta ()
addAnnotationWorker ann pa =
  -- Zero-width source spans are injected by the GHC Lexer when it puts virtual
  -- '{', ';' and '}' tokens in for layout
  unless (isPointSrcSpan pa) $
    do
      pe <- getPriorEnd
      ss <- getSrcSpan
      let p = ss2delta pe pa
      case (ann,isGoodDelta p) of
        (G GHC.AnnComma,False) -> return ()
        (G GHC.AnnSemi, False) -> return ()
        (G GHC.AnnOpen, False) -> return ()
        (G GHC.AnnClose,False) -> return ()
        _ -> do
          p' <- adjustDeltaForOffsetM p
          commentAllocation (priorComment (ss2pos pa)) (mapM_ (uncurry addDeltaComment))
#if __GLASGOW_HASKELL__ <= 710
          addAnnDeltaPos (checkUnicode ann pa) p'
#else
          addAnnDeltaPos ann p'
#endif
          setPriorEndAST pa
              `debug` ("addAnnotationWorker:(ss,ss,pe,pa,p,p',ann)=" ++ show (showGhc ss,showGhc ss,pe,showGhc pa,p,p',ann))

#if __GLASGOW_HASKELL__ <= 710
checkUnicode :: KeywordId -> GHC.SrcSpan -> KeywordId
checkUnicode gkw@(G kw) ss =
  if kw `Set.member` unicodeSyntax
    then
      let s = keywordToString gkw in
      if (length s /= spanLength ss)
        then AnnUnicode kw
        else gkw
  else
    gkw
  where
    unicodeSyntax = Set.fromList
      [ GHC.AnnDcolon
      , GHC.AnnDarrow
      , GHC.AnnForall
      , GHC.AnnRarrow
      , GHC.AnnLarrow
      , GHC.Annlarrowtail
      , GHC.Annrarrowtail
      , GHC.AnnLarrowtail
      , GHC.AnnRarrowtail]
checkUnicode kwid _ = kwid
#else

unicodeEquivalent :: GHC.AnnKeywordId -> [GHC.AnnKeywordId]
unicodeEquivalent kw =
  case Map.lookup kw unicodeSyntax of
    Nothing -> []
    Just kwu -> [kwu]
  where
    unicodeSyntax = Map.fromList
      [ (GHC.AnnDcolon,     GHC.AnnDcolonU)
      , (GHC.AnnDarrow,     GHC.AnnDarrowU)
      , (GHC.AnnForall,     GHC.AnnForallU)
      , (GHC.AnnRarrow,     GHC.AnnRarrowU)
      , (GHC.AnnLarrow,     GHC.AnnLarrowU)
      , (GHC.Annlarrowtail, GHC.AnnlarrowtailU)
      , (GHC.Annrarrowtail, GHC.AnnrarrowtailU)
      , (GHC.AnnLarrowtail, GHC.AnnLarrowtailU)
      , (GHC.AnnRarrowtail, GHC.AnnRarrowtailU)]
#endif


-- ---------------------------------------------------------------------

commentAllocation :: (Comment -> Bool)
                  -> ([(Comment, DeltaPos)] -> Delta a)
                  -> Delta a
commentAllocation p k = do
  cs <- getUnallocatedComments
  let (allocated,cs') = allocateComments p cs
  putUnallocatedComments cs'
  k =<< mapM makeDeltaComment (sortBy (comparing commentIdentifier) allocated)


makeDeltaComment :: Comment -> Delta (Comment, DeltaPos)
makeDeltaComment c = do
  let pa = commentIdentifier c
  pe <- getPriorEnd
  let p = ss2delta pe pa
  p' <- adjustDeltaForOffsetM p
  setPriorEnd (ss2posEnd pa)
  return (c, p')

addDeltaComment :: Comment -> DeltaPos -> Delta ()
addDeltaComment d p = do
  addAnnDeltaPos (AnnComment d) p

-- ---------------------------------------------------------------------

-- | Look up and add a Delta annotation at the current position, and
-- advance the position to the end of the annotation
addDeltaAnnotation :: GHC.AnnKeywordId -> Delta ()
addDeltaAnnotation ann' = do
  ss <- getSrcSpan
  (ma,ann) <- getOneAnnotationDelta ann'
  case nub ma of -- ++AZ++ TODO: get rid of duplicates earlier
    []     -> return () `debug` ("addDeltaAnnotation empty ma for:" ++ show (ss,ann))
    [pa]   -> addAnnotationWorker (G ann) pa
    (pa:_) -> addAnnotationWorker (G ann) pa `warn` ("addDeltaAnnotation:(ss,ann,ma)=" ++ showGhc (ss,ann,ma))

-- | Look up and add a Delta annotation at the current position, and
-- advance the position to the end of the annotation
addDeltaAnnotationLs :: GHC.AnnKeywordId -> Int -> Delta ()
addDeltaAnnotationLs ann off = do
  ss <- getSrcSpan
  ma <- peekAnnotationDelta ann
  let ma' = filter (\s -> (GHC.isSubspanOf s ss)) ma
  case drop off ma' of
    [] -> return ()
        `debug` ("addDeltaAnnotationLs:missed:(off,ann,ma)=" ++ showGhc (off,ss,ann))
    (pa:_) -> addAnnotationWorker (G ann) pa

-- | Look up and add possibly multiple Delta annotation at the current
-- position, and advance the position to the end of the annotations
addDeltaAnnotations :: GHC.AnnKeywordId -> Delta ()
addDeltaAnnotations ann = do
  (ma,kw) <- getAnnotationDelta ann
  let do_one ap' = addAnnotationWorker (G kw) ap'
                    `debug` ("addDeltaAnnotations:do_one:(ap',ann)=" ++ showGhc (ap',ann))
  mapM_ do_one (sort ma)

-- | Look up and add possibly multiple Delta annotations enclosed by
-- the current SrcSpan at the current position, and advance the
-- position to the end of the annotations
addDeltaAnnotationsInside :: GHC.AnnKeywordId -> Delta ()
addDeltaAnnotationsInside ann = do
  ss <- getSrcSpan
  ma <- peekAnnotationDelta ann
  let do_one ap' = addAnnotationWorker (G ann) ap'
                    -- `debug` ("addDeltaAnnotations:do_one:(ap',ann)=" ++ showGhc (ap',ann))
  let filtered = sort $ filter (\s -> GHC.isSubspanOf s ss) ma
  mapM_ do_one filtered

-- | Look up and add possibly multiple Delta annotations not enclosed by
-- the current SrcSpan at the current position, and advance the
-- position to the end of the annotations
-- The first argument (gann) is the one to look up in the GHC annotations, the
-- second is the one to apply in the ghc-exactprint ones. These are different
-- for GHC.AnnSemi mapping to AnnSemiSep, to ensure that it reflects the ';'
-- outside the current span.
addDeltaAnnotationsOutside :: GHC.AnnKeywordId -> KeywordId -> Delta ()
addDeltaAnnotationsOutside gann ann = do
  ss <- getSrcSpan
  (ma,kw) <- getAndRemoveAnnotationDelta ss gann
  let do_one ap' = if ann == AnnSemiSep
                     then addAnnotationWorker ann    ap'
                     else addAnnotationWorker (G kw) ap'
  mapM_ do_one (sort $ filter (\s -> not (GHC.isSubspanOf s ss)) ma)

-- | Add a Delta annotation at the current position, and advance the
-- position to the end of the annotation
addDeltaAnnotationExt :: GHC.SrcSpan -> GHC.AnnKeywordId -> Delta ()
addDeltaAnnotationExt s ann = addAnnotationWorker (G ann) s

addEofAnnotation :: Delta ()
addEofAnnotation = do
  pe <- getPriorEnd
  (ma,_kw) <- withSrcSpanDelta (GHC.noLoc ()) (getAnnotationDelta GHC.AnnEofPos)
  case ma of
    [] -> return ()
    (pa:pss) -> do
      commentAllocation (const True) (mapM_ (uncurry addDeltaComment))
      let DP (r,c) = ss2delta pe pa
      addAnnDeltaPos (G GHC.AnnEofPos) (DP (r, c - 1))
      setPriorEndAST pa `warn` ("Trailing annotations after Eof: " ++ showGhc pss)

-- ---------------------------------------------------------------------

countAnnsDelta :: GHC.AnnKeywordId -> Delta Int
countAnnsDelta ann = do
  ma <- peekAnnotationDelta ann
  return (length ma)

