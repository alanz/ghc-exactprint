{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}

-- |  This module converts 'GHC.ApiAnns' into 'Anns' by traversing a
-- structure created by the "Annotate" module.
--
-- == Structure of an Annotation
--
-- As a rule of thumb, every located element in the GHC AST will have
-- a corresponding entry in 'Anns'. An 'Annotation' contains 6 fields which
-- can be modifed to change how the AST is printed.
--
-- == Layout Calculation
--
-- In order to properly place syntax nodes and comments properly after
-- refactoring them (in such a way that the indentation level changes), their
-- position (encoded in the 'addEntryDelta' field) is not expressed as absolute
-- but relative to their context. As further motivation, consider the simple
-- let-into-where-block refactoring, from:
--
-- @
-- foo = do
--   let bar = do
--         x
--         -- comment
--         y
--   bar
-- @
--
-- to
--
-- @
-- foo = do
--   bar
--  where
--   bar = do
--     x
--     -- comment
--     y
-- @
--
-- Notice how the column of @x@, @y@ and the comment change due to this
-- refactoring but certain relative positions (e.g. the comment starting at the
-- same column as @x@) remain unchanged.
--
-- Now, what does "context" mean exactly? Here we reference the
-- "indentation level" as used in the haskell report (see chapter 2.7:
-- <https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-210002.7>):
-- 'addEntryDelta' is mostly relative to the current (inner-most) indentation
-- level. But in order to get better results, for the purpose of defining
-- relative positions a the offside-rule is modified slightly: Normally it
-- fires (only) at the first elements after where/let/do/of, introducing a new
-- indentation level. In addition, the rule here fires also at the "@let@"
-- keyword (when it is part of a "@let-in@" construct) and at the "@if@" keyword.
--
-- The effect of this additional applications of the offside-rule is that any
-- elements (more or less directly) following the "@let@" ("@if@"")
-- keyword have a position relative to the "@let@" ("@if@")
-- keyword position, even when the regular offside-rule does apply not yet/not
-- anymore. This affects two concrete things: Comments directly following
-- "@let@"/"@if@", and the respective follow-up keywords: "@in@" or
-- "@then@"/"@else@".
--
-- Due to this additional indentation level, it is possible to observe/obtain
-- negative delta-positions; consider:
--
-- @
-- foo = let x = 1
--   in x
-- @
--
-- Here, the @in@ keyword has an 'annEntryDelta' of @DP (1, -4)@ as it appears
-- one line below the previous elements and 4 columns /left/ relative to the
-- start of the @let@ keyword.
--
-- In general, the element that defines such an indentation level (i.e. the
-- first element after a where/let/do/of) will have an 'annEntryDelta' relative
-- to the previous inner-most indentation level; in other words: a new
-- indentation level becomes relevant only after the construct introducing the
-- element received its 'annEntryDelta' position. (Otherwise these elements
-- always would have a zero horizontal position - relative to itself.)
--
-- (This affects comments, too: A comment preceding the first element of a
-- layout block will have a position relative to the outer block, not of the
-- newly introduced layout block.)
--
-- For example, in the following expression the statement corresponding to
-- @baz@ will be given a 'annEntryDelta' of @DP (1, 2)@ as it appears
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
-- offset from the last syntactic element (ignoring comments). This is
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

import Control.Monad.RWS
import Control.Monad.Trans.Free

import Data.Data (Data)
import Data.List (sort, nub, partition, sortBy, sortOn)

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

{-# ANN module "HLint: ignore Eta reduce" #-}
{-# ANN module "HLint: ignore Redundant do" #-}
{-# ANN module "HLint: ignore Reduce duplication" #-}

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
       , sortKeys       :: !(Maybe [AnnSpan])
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

tellSortKey :: [AnnSpan] -> Delta ()
tellSortKey xs = tell (mempty { sortKeys = Just xs } )

tellCapturedSpan :: AnnKey -> Delta ()
tellCapturedSpan key = tell ( mempty { dwCapturedSpan = First $ Just key })

tellKd :: (KeywordId, DeltaPos) -> Delta ()
tellKd kd = tell (mempty { annKds = [kd] })

#if __GLASGOW_HASKELL__ >= 804
instance Semigroup DeltaWriter where
  (<>) = mappend
#endif

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
#if __GLASGOW_HASKELL__ >= 800
    go (MarkInstead akwid kwid next)    = addDeltaAnnotationInstead akwid kwid >> next
#endif
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
    go (MarkAnnBeforeAnn ann1 ann2 next) = deltaMarkAnnBeforeAnn ann1 ann2 >> next
    go (MarkExternal ss akwid _ next)    = addDeltaAnnotationExt (rs ss) akwid >> next
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
    go (TellContext _ next)                  = next

withSortKey :: [(AnnSpan, Annotated b)] -> Delta ()
withSortKey kws =
  let order = sortBy (comparing fst) kws
  in do
    tellSortKey (map fst order)
    mapM_ (deltaInterpret . snd) order


withSortKeyContexts :: ListContexts -> [(AnnSpan, Annotated ())] -> Delta ()
withSortKeyContexts ctxts kws = do
  tellSortKey (map fst order)
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
  ss <- getSrcSpan
  cs <- gets apComments
  let
    doOne :: GHC.AnnKeywordId -> Delta [Comment]
    doOne kw = do
      (spans,_) <- getAndRemoveAnnotationDelta ss kw
      return $ map (mkKWComment kw . sr) spans
    -- TODO:AZ make sure these are sorted/merged properly when the invariant for
    -- allocateComments is re-established.
  newComments <- mapM doOne kws
  putUnallocatedComments (cs ++ concat newComments)

-- ---------------------------------------------------------------------

-- | This function exists to overcome a shortcoming in the GHC AST for 7.10.1
getSrcSpanForKw :: GHC.SrcSpan -> GHC.AnnKeywordId -> Delta GHC.SrcSpan
getSrcSpanForKw _ kw = do
    ga <- gets apAnns
    ss <- getSrcSpan
    case GHC.getAnnotation ga (rs ss) kw of
      []     -> return GHC.noSrcSpan
#if __GLASGOW_HASKELL__ >= 808
      (sp:_) -> return (GHC.RealSrcSpan sp Nothing)
#else
      (sp:_) -> return sp
#endif

-- ---------------------------------------------------------------------

getSrcSpan :: Delta GHC.SrcSpan
getSrcSpan = asks curSrcSpan

withSrcSpanDelta :: Data a => GHC.Located a -> Delta b -> Delta b
withSrcSpanDelta (GHC.L l a) =
  local (\s -> s { curSrcSpan = l
                 , annConName = annGetConstr a
                 , drContext = pushAcs (drContext s)
                    `debug` ("withSrcSpanDelta: (l,annConName,drContext)=" ++ showGhc (l,annGetConstr a, pushAcs (drContext s)))
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

setPriorEndAST :: AnnSpan -> Delta ()
setPriorEndAST pe = do
  setLayoutStart (snd (ss2pos (sr pe)))
  modify (\s -> s { priorEndPosition    = ss2posEnd (sr pe) } )

setLayoutStart :: Int -> Delta ()
setLayoutStart p = do
  DeltaState{apMarkLayout} <- get
  when apMarkLayout (
                      modify (\s -> s { apMarkLayout = False
                                     , apLayoutStart = LayoutStartCol p}))


-- -------------------------------------

peekAnnotationDelta :: GHC.AnnKeywordId -> Delta [AnnSpan]
peekAnnotationDelta an = do
    ga <- gets apAnns
    ss <- getSrcSpan
#if __GLASGOW_HASKELL__ <= 710
    return $ GHC.getAnnotation ga ss an
#else
    let unicodeAnns = case unicodeEquivalent an of
          [] -> []
          [kw] -> GHC.getAnnotation ga (rs ss) kw
          (kw:_) -> GHC.getAnnotation ga (rs ss) kw -- Keep exhaustiveness checker happy
    return $ unicodeAnns ++ GHC.getAnnotation ga (rs ss) an
#endif

getAnnotationDelta :: GHC.AnnKeywordId -> Delta ([AnnSpan],GHC.AnnKeywordId)
getAnnotationDelta an = do
    ss <- getSrcSpan
    getAndRemoveAnnotationDelta ss an

getAndRemoveAnnotationDelta :: GHC.SrcSpan -> GHC.AnnKeywordId -> Delta ([AnnSpan],GHC.AnnKeywordId)
getAndRemoveAnnotationDelta sp an = do
    ga <- gets apAnns
#if __GLASGOW_HASKELL__ <= 710
    let (r,ga') = GHC.getAndRemoveAnnotation ga sp an
        kw = an
#else
    let (r,ga',kw) = case GHC.getAndRemoveAnnotation ga (rs sp) an of
                    ([],_) -> (ss,g,k)
                      where
                        k = GHC.unicodeAnn an
                        (ss,g) = GHC.getAndRemoveAnnotation ga (rs sp) k
                    (ss,g)  -> (ss,g,an)
#endif
    modify (\s -> s { apAnns = ga' })
    return (r,kw)

getOneAnnotationDelta :: GHC.AnnKeywordId -> Delta ([AnnSpan],GHC.AnnKeywordId)
getOneAnnotationDelta an = do
    ss <- getSrcSpan
    getAndRemoveOneAnnotationDelta ss an

getAndRemoveOneAnnotationDelta :: GHC.SrcSpan -> GHC.AnnKeywordId -> Delta ([AnnSpan],GHC.AnnKeywordId)
getAndRemoveOneAnnotationDelta sp an = do
#if __GLASGOW_HASKELL__ >= 808
    ann <- gets apAnns
    let anns = GHC.apiAnnItems ann
    let cs = GHC.apiAnnComments ann -- TODO:AZ: what about apiAnnRogueComments?
#else
    (anns,cs) <- gets apAnns
#endif

#if __GLASGOW_HASKELL__ <= 710
    let (r,ga',kw) = case Map.lookup (sp,an) anns of
                    Nothing -> ([],(anns,cs),an)
                    Just []     -> ([], (Map.delete (sp,an)    anns,cs),an)
                    Just (s:ss) -> ([s],(Map.insert (sp,an) ss anns,cs),an)
#else
    let getKw kw =
          case Map.lookup (rs sp,kw) anns of
            Nothing -> ([],(anns,cs),kw)
            Just []     -> ([], (Map.delete (rs sp,kw)    anns,cs),kw)
            Just (s:ss) -> ([s],(Map.insert (rs sp,kw) ss anns,cs),kw)

    let (r,(a',c'),kw) =
          case getKw an of
            ([],_,_) -> getKw (GHC.unicodeAnn an)
            v        -> v
#if __GLASGOW_HASKELL__ >= 808
    let ga' = ann {  GHC.apiAnnItems = a', GHC.apiAnnComments = c' }
#else
    let ga' = (a',c')
#endif
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
  = AnnKey (rs curSrcSpan) annConName

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
      modify (\s -> s { priorEndPosition    = ss2pos ss } ))
    (res, w) <- censor maskWriter (listen action)

    let kds = annKds w
        an = Ann
               { annEntryDelta        = edp
               , annPriorComments     = cs
               , annFollowingComments = [] -- only used in Transform and Print
               , annsDP               = kds
               , annSortKey           = sortKeys w
               , annCapturedSpan      = getFirst $ dwCapturedSpan w }

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
priorComment start c = (ss2pos . sr . commentIdentifier $ c) < start

-- TODO:AZ: We scan the entire comment list here. It may be better to impose an
-- invariant that the comments are sorted, and consume them as the pos
-- advances. It then becomes a process of using `takeWhile p` rather than a full
-- partition.
allocateComments :: (Comment -> Bool) -> [Comment] -> ([Comment], [Comment])
allocateComments = partition

-- ---------------------------------------------------------------------

addAnnotationWorker :: KeywordId -> AnnSpan -> Delta ()
addAnnotationWorker ann pa =
  -- Zero-width source spans are injected by the GHC Lexer when it puts virtual
  -- '{', ';' and '}' tokens in for layout
  unless (isPointSrcSpan pa) $
    do
      pe <- getPriorEnd
      ss <- getSrcSpan
      let p = ss2delta pe (sr pa)
      case (ann,isGoodDelta p) of
        (G GHC.AnnComma,False) -> return ()
        (G GHC.AnnSemi, False) -> return ()
        (G GHC.AnnOpen, False) -> return ()
        (G GHC.AnnClose,False) -> return ()
        _ -> do
          p' <- adjustDeltaForOffsetM p
          commentAllocation (priorComment (ss2pos (sr pa))) (mapM_ (uncurry addDeltaComment))
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
      if length s /= spanLength ss
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
      , (GHC.AnnRarrowtail, GHC.AnnRarrowtailU)
#if __GLASGOW_HASKELL__ > 801
      , (GHC.AnnCloseB,     GHC.AnnCloseBU)
      , (GHC.AnnCloseQ,     GHC.AnnCloseQU)
      , (GHC.AnnOpenB,      GHC.AnnOpenBU)
      , (GHC.AnnOpenEQ,     GHC.AnnOpenEQU)
#endif
      ]
#endif


-- ---------------------------------------------------------------------

commentAllocation :: (Comment -> Bool)
                  -> ([(Comment, DeltaPos)] -> Delta a)
                  -> Delta a
commentAllocation p k = do
  cs <- getUnallocatedComments
  let (allocated,cs') = allocateComments p cs
  putUnallocatedComments cs'
  k =<< mapM makeDeltaComment (sortOn (unpack . commentIdentifier) allocated)
  where
    -- unpack a RealSrcSpan into ((start line, start col), (end line, end col)).
    -- The file name is ignored.
#if __GLASGOW_HASKELL__ >= 808
    unpack :: GHC.RealSrcSpan -> Maybe ((Int, Int), (Int, Int))
    unpack x =
#else
    unpack :: GHC.SrcSpan -> Maybe ((Int, Int), (Int, Int))
    unpack (GHC.RealSrcSpan x) =
#endif
       Just ( (GHC.srcSpanStartLine x, GHC.srcSpanStartCol x)
            , (GHC.srcSpanEndLine x, GHC.srcSpanEndCol x) )
    unpack _ = Nothing

makeDeltaComment :: Comment -> Delta (Comment, DeltaPos)
makeDeltaComment c = do
  let pa = commentIdentifier c
  pe <- getPriorEnd
  let p = ss2delta pe (sr pa)
  p' <- adjustDeltaForOffsetM p
  setPriorEnd (ss2posEnd (sr pa))
  return (c, p')

addDeltaComment :: Comment -> DeltaPos -> Delta ()
addDeltaComment d p = do
  addAnnDeltaPos (AnnComment d) p

-- ---------------------------------------------------------------------

-- |If the first annotation has a smaller SrcSpan than the second, then mark it.
deltaMarkAnnBeforeAnn :: GHC.AnnKeywordId -> GHC.AnnKeywordId -> Delta ()
deltaMarkAnnBeforeAnn annBefore annAfter = do
  ss <- getSrcSpan
  mb <- peekAnnotationDelta annBefore
  ma <- peekAnnotationDelta annAfter
  let
    before = sort $ filter (\s -> GHC.isSubspanOf (sr s) ss) mb
    after  = sort $ filter (\s -> GHC.isSubspanOf (sr s) ss) ma
  case (before,after) of
    (b:_, a:_) -> when (b < a) $ addDeltaAnnotation annBefore
    _ -> return ()

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
  let ma' = filter (\s -> GHC.isSubspanOf (sr s) ss) ma
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
  let filtered = sort $ filter (\s -> GHC.isSubspanOf (sr s) ss) ma
  mapM_ do_one filtered

-- ---------------------------------------------------------------------
#if __GLASGOW_HASKELL__ >= 800
addDeltaAnnotationInstead :: GHC.AnnKeywordId  -> KeywordId -> Delta ()
addDeltaAnnotationInstead ann' kw = do
  ss <- getSrcSpan
  (ma,ann) <- getOneAnnotationDelta ann'
  case nub ma of -- ++AZ++ TODO: get rid of duplicates earlier
    []     -> return () `debug` ("addDeltaAnnotation empty ma for:" ++ show (ss,ann))
    [pa]   -> addAnnotationWorker kw pa
    (pa:_) -> addAnnotationWorker kw pa `warn` ("addDeltaAnnotationInstead:(ss,ann,kw,ma)=" ++ showGhc (ss,ann,kw,ma))
#endif
-- ---------------------------------------------------------------------

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
  mapM_ do_one (sort $ filter (\s -> not (GHC.isSubspanOf (sr s) ss)) ma)

-- | Add a Delta annotation at the current position, and advance the
-- position to the end of the annotation
addDeltaAnnotationExt :: AnnSpan -> GHC.AnnKeywordId -> Delta ()
addDeltaAnnotationExt s ann = addAnnotationWorker (G ann) s

addEofAnnotation :: Delta ()
#if __GLASGOW_HASKELL__ >= 808
addEofAnnotation = do
  pe <- getPriorEnd
  ga <- gets apAnns
  let Just pa = GHC.apiAnnEofPos ga
  commentAllocation (const True) (mapM_ (uncurry addDeltaComment))
  let DP (r,c) = ss2delta pe (sr pa)
  addAnnDeltaPos AnnEofPos (DP (r, c - 1))
  setPriorEndAST pa
#else
addEofAnnotation = do
  pe <- getPriorEnd
  (ma,_kw) <- withSrcSpanDelta (GHC.noLoc () :: GHC.GenLocated GHC.SrcSpan ())
                               (getAnnotationDelta GHC.AnnEofPos)
  case ma of
    [] -> return ()
    (pa:pss) -> do
      commentAllocation (const True) (mapM_ (uncurry addDeltaComment))
      let DP (r,c) = ss2delta pe pa
      addAnnDeltaPos (G GHC.AnnEofPos) (DP (r, c - 1))
      setPriorEndAST pa `warn` ("Trailing annotations after Eof: " ++ showGhc pss)
#endif

-- ---------------------------------------------------------------------

countAnnsDelta :: GHC.AnnKeywordId -> Delta Int
countAnnsDelta ann = do
  ma <- peekAnnotationDelta ann
  return (length ma)
