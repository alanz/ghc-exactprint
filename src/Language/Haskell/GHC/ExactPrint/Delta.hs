{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE BangPatterns #-}
module Language.Haskell.GHC.ExactPrint.Delta
  ( relativiseApiAnns
  , relativiseApiAnnsWithComments

  , tokComment
  ) where

import Control.Monad.RWS
import Control.Monad.Trans.Free

import Data.Data (Data)
import Data.List (sort, nub, partition, sortBy)

import Data.Ord

import Language.Haskell.GHC.ExactPrint.Utils
import Language.Haskell.GHC.ExactPrint.Lookup
import Language.Haskell.GHC.ExactPrint.Internal.Types
import Language.Haskell.GHC.ExactPrint.Annotate (AnnotationF(..), Annotated
                                                , annotate,  Annotate(..))

import qualified GHC
import qualified SrcLoc         as GHC

import qualified Data.Map as Map


-- ---------------------------------------------------------------------
-- | Transform concrete annotations into relative annotations which are
-- more useful when transforming an AST.
relativiseApiAnns :: Annotate ast
                  => GHC.Located ast
                  -> GHC.ApiAnns
                  -> Anns
relativiseApiAnns = relativiseApiAnnsWithComments []

relativiseApiAnnsWithComments ::
                     Annotate ast
                  => [Comment]
                  -> GHC.Located ast
                  -> GHC.ApiAnns
                  -> Anns
relativiseApiAnnsWithComments cs modu ghcAnns
   = runDeltaWithComments cs (annotate modu) ghcAnns (ss2pos $ GHC.getLoc modu)

-- ---------------------------------------------------------------------
--
-- | Type used in the Delta Monad.
type Delta a = RWS DeltaReader DeltaWriter DeltaState a

runDeltaWithComments :: [Comment] -> Annotated () -> GHC.ApiAnns -> Pos -> Anns
runDeltaWithComments cs action ga priorEnd =
  mkAnns . snd
  . (\next -> execRWS next initialDeltaReader (defaultDeltaState cs priorEnd ga))
  . deltaInterpret $ action
  where
    mkAnns :: DeltaWriter -> Anns
    mkAnns = Anns <$> (f . dwAnns)
    f :: Monoid a => Endo a -> a
    f = ($ mempty) . appEndo

-- ---------------------------------------------------------------------

data DeltaReader = DeltaReader
       {
         -- AZ:TODO: Should we replace the next three fields witn an explicit
         -- AnnKey value?

         -- | Current `SrcSpan, part of current AnnKey`
         curSrcSpan  :: !GHC.SrcSpan

         -- | Constuctor of current AST element, part of current AnnKey
       , annConName       :: !AnnConName

         -- | Start column of the current layout block
       , layoutStart :: !LayoutStartCol
       }

data DeltaWriter = DeltaWriter
       { -- | Final list of annotations, and sort keys
         dwAnns :: Endo (Map.Map AnnKey Annotation)

         -- | Used locally to pass Keywords, delta pairs relevant to a specific
         -- subtree to the parent.
       , annKds         :: ![(KeywordId, DeltaPos)]
       , sortKeys       :: !(Maybe [GHC.SrcSpan])
       , dwCapturedSpan :: !(First AnnKey)
       , dwLayoutStart  :: ![DeltaPos]
       }

data DeltaState = DeltaState
       { -- | Position reached when processing the last element
         priorEndPosition    :: !Pos

         -- | Position reached when processing last AST element
         --   this is necessary to enforce layout rules.
       , priorEndASTPosition :: !Pos

         -- | Ordered list of comments still to be allocated
       , apComments :: ![Comment]

         -- | The original GHC Delta Annotations
       , apAnns :: !GHC.ApiAnns

       }

-- ---------------------------------------------------------------------

initialDeltaReader :: DeltaReader
initialDeltaReader =
  DeltaReader
    { curSrcSpan = GHC.noSrcSpan
    , annConName = annGetConstr ()
    , layoutStart = 1
    }

defaultDeltaState :: [Comment] -> Pos -> GHC.ApiAnns -> DeltaState
defaultDeltaState injectedComments priorEnd ga =
    DeltaState
      { priorEndPosition    = priorEnd
      , priorEndASTPosition = priorEnd
      , apComments = cs ++ injectedComments
      , apAnns     = ga
      }
  where
    cs :: [Comment]
    cs = flattenedComments ga

    flattenedComments :: GHC.ApiAnns -> [Comment]
    flattenedComments (_,cm) =
      map tokComment . GHC.sortLocated . concat $ Map.elems cm

tokComment :: GHC.Located GHC.AnnotationComment -> Comment
tokComment t@(GHC.L lt _) = mkComment (ghcCommentText t) lt

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

tellLayoutStart :: DeltaPos -> Delta ()
tellLayoutStart c = tell ( mempty { dwLayoutStart = [c] } )

instance Monoid DeltaWriter where
  mempty = DeltaWriter mempty mempty mempty mempty mempty
  (DeltaWriter a b e g j) `mappend` (DeltaWriter c d f h i)
    = DeltaWriter (a <> c) (b <> d) (e <> f) (g <> h) (j <> i)

-----------------------------------
-- Free Monad Interpretation code

deltaInterpret :: Annotated a -> Delta a
deltaInterpret = iterTM go
  where
    go :: AnnotationF (Delta a) -> Delta a
    go (MarkEOF next)                   = addEofAnnotation >> next
    go (MarkPrim kwid _ next)           = addDeltaAnnotation kwid >> next
    go (MarkOutside akwid kwid next)    = addDeltaAnnotationsOutside akwid kwid >> next
    go (MarkInside akwid next)          = addDeltaAnnotationsInside akwid >> next
    go (MarkMany akwid next)            = addDeltaAnnotations akwid >> next
    go (MarkOffsetPrim akwid n _ next)  = addDeltaAnnotationLs akwid n >> next
    go (MarkAfter akwid next)           = addDeltaAnnotationAfter akwid >> next
    go (WithAST lss layoutflag prog next) =
      withAST lss layoutflag (deltaInterpret prog) >> next
    go (CountAnns kwid next)             = countAnnsDelta kwid >>= next
    go (SetLayoutFlag ss action next)    = setLayoutFlag ss (deltaInterpret action)  >> next
    go (MarkExternal ss akwid _ next)    = addDeltaAnnotationExt ss akwid >> next
    go (StoreOriginalSrcSpan key next)   = storeOriginalSrcSpanDelta key >>= next
    go (GetSrcSpanForKw kw next)         = getSrcSpanForKw kw >>= next
    go (StoreString s ss next)           = storeString s ss >> next
    go (AnnotationsToComments kws next)  = annotationsToCommentsDelta kws >> next
    go (WithSortKey kws next)  = withSortKey kws >> next

withSortKey :: [(GHC.SrcSpan, Annotated b)] -> Delta ()
withSortKey kws =
  let order = sortBy (comparing fst) kws
  in do
    tellSortKey (map fst order)
    mapM_ (deltaInterpret . snd) order


setLayoutFlag :: GHC.SrcSpan -> Delta () -> Delta ()
setLayoutFlag ss action = do
  p <- gets priorEndPosition
  tellLayoutStart =<< adjustDeltaForOffsetM (ss2delta p ss)
  local (\s -> s { layoutStart = LayoutStartCol (srcSpanStartColumn ss) }) action

-- ---------------------------------------------------------------------

storeOriginalSrcSpanDelta :: AnnKey -> Delta AnnKey
storeOriginalSrcSpanDelta key = do
  tellCapturedSpan key
  return key

storeString :: String -> GHC.SrcSpan -> Delta ()
storeString s ss = addAnnotationWorker (AnnString s) ss

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
getSrcSpanForKw :: GHC.AnnKeywordId -> Delta GHC.SrcSpan
getSrcSpanForKw kw = do
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
                 })


getUnallocatedComments :: Delta [Comment]
getUnallocatedComments = gets apComments

putUnallocatedComments :: [Comment] -> Delta ()
putUnallocatedComments cs = modify (\s -> s { apComments = cs } )

-- ---------------------------------------------------------------------

adjustDeltaForOffsetM :: DeltaPos -> Delta DeltaPos
adjustDeltaForOffsetM dp = do
  colOffset <- asks layoutStart
  return (adjustDeltaForOffset colOffset dp)

adjustDeltaForOffset :: LayoutStartCol -> DeltaPos -> DeltaPos
adjustDeltaForOffset _colOffset              dp@(DP (0,_)) = dp -- same line
adjustDeltaForOffset (LayoutStartCol colOffset) (DP (l,c)) = DP (l,c - colOffset)

-- ---------------------------------------------------------------------

getPriorEnd :: Delta Pos
getPriorEnd = gets priorEndPosition

getPriorEndAST :: Delta Pos
getPriorEndAST = gets priorEndASTPosition

setPriorEnd :: Pos -> Delta ()
setPriorEnd pe =
  modify (\s -> s { priorEndPosition = pe })

setPriorEndAST :: Pos -> Delta ()
setPriorEndAST pe =
  modify (\s -> s { priorEndPosition    = pe
                  , priorEndASTPosition = pe })


setLayoutOffset :: LayoutStartCol -> Delta a -> Delta a
setLayoutOffset lhs =
  local (\s -> s { layoutStart = lhs })

-- -------------------------------------

peekAnnotationDelta :: GHC.AnnKeywordId -> Delta [GHC.SrcSpan]
peekAnnotationDelta an = do
    ga <- gets apAnns
    ss <- getSrcSpan
    return $ GHC.getAnnotation ga ss an

getAnnotationDelta :: GHC.AnnKeywordId -> Delta [GHC.SrcSpan]
getAnnotationDelta an = do
    ss <- getSrcSpan
    getAndRemoveAnnotationDelta ss an

getAndRemoveAnnotationDelta :: GHC.SrcSpan -> GHC.AnnKeywordId -> Delta [GHC.SrcSpan]
getAndRemoveAnnotationDelta sp an = do
    ga <- gets apAnns
    let (r,ga') = GHC.getAndRemoveAnnotation ga sp an
    r <$ modify (\s -> s { apAnns = ga' })

getOneAnnotationDelta :: GHC.AnnKeywordId -> Delta [GHC.SrcSpan]
getOneAnnotationDelta an = do
    ss <- getSrcSpan
    getAndRemoveOneAnnotationDelta ss an

getAndRemoveOneAnnotationDelta :: GHC.SrcSpan -> GHC.AnnKeywordId -> Delta [GHC.SrcSpan]
getAndRemoveOneAnnotationDelta sp an = do
    (anns,cs) <- gets apAnns
    let (r,ga') = case Map.lookup (sp,an) anns of
                    Nothing -> ([],(anns,cs))
                    Just []     -> ([], (Map.delete (sp,an)    anns,cs))
                    Just (s:ss) -> ([s],(Map.insert (sp,an) ss anns,cs))
    modify (\s -> s { apAnns = ga' })
    return r

-- ---------------------------------------------------------------------

-- |Add some annotation to the currently active SrcSpan
addAnnotationsDelta :: Annotation -> Delta ()
addAnnotationsDelta ann = do
    l <- ask
    tellFinalAnn (getAnnKey l,ann)

getAnnKey :: DeltaReader -> AnnKey
getAnnKey DeltaReader {curSrcSpan, annConName}
  = AnnKey curSrcSpan annConName

-- -------------------------------------

addAnnDeltaPos :: KeywordId -> DeltaPos -> Delta ()
addAnnDeltaPos kw dp = tellKd (kw, dp)

-- -------------------------------------

-- | Enter a new AST element. Maintain SrcSpan stack
withAST :: Data a
        => GHC.Located a
        -> LayoutFlag
        -> Delta b -> Delta b
withAST lss@(GHC.L ss _) layout action = do
  return () `debug` ("enterAST:(annkey,d,layout)=" ++ show (mkAnnKey lss,layout))
  -- Calculate offset required to get to the start of the SrcSPan
  off <- asks layoutStart
  let newOff =
        case layout of
          LayoutRules   -> (LayoutStartCol (srcSpanStartColumn ss))
          NoLayoutRules -> off

  (resetAnns . setLayoutOffset newOff .  withSrcSpanDelta lss) (do

    let maskWriter s = s { annKds = []
                         , sortKeys = Nothing
                         , dwCapturedSpan = mempty
                         , dwLayoutStart  = mempty }

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
                newOff (ss2delta priorEndAfterComments ss)
    peAST <- getPriorEndAST
    let edpAST = adjustDeltaForOffset
                  newOff (ss2delta peAST ss)
    -- Preparation complete, perform the action
    when (GHC.isGoodSrcSpan ss && priorEndAfterComments < ss2pos ss)
            (setPriorEndAST (ss2pos ss))
    (res, w) <- censor maskWriter (listen action)

    let kds = annKds w
        an = Ann
               { annEntryDelta = edp
               , annDelta   = ColDelta (srcSpanStartColumn ss
                                         - getLayoutStartCol off)
               , annTrueEntryDelta  = edpAST
               , annPriorComments = cs
               , annFollowingComments = [] -- only used in Transform and Print
               , annsDP     = kds
               , annSortKey = sortKeys w
               , annCapturedSpan = getFirst $ dwCapturedSpan w
               , annLayoutStart  = dwLayoutStart w }

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
priorComment start c = (fst . commentPos $ c) < start

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
          addAnnDeltaPos (checkUnicode ann pa) p'
          setPriorEndAST (ss2posEnd pa)
              `debug` ("addAnnotationWorker:(ss,ss,pe,pa,p,p',ann)=" ++ show (showGhc ss,ss2span ss,pe,ss2span pa,p,p',ann))

checkUnicode :: KeywordId -> GHC.SrcSpan -> KeywordId
checkUnicode gkw@(G kw) ss =
  if kw `elem` unicodeSyntax
    then
      let s = keywordToString gkw in
      if (length s /= spanLength ss)
        then AnnUnicode kw
        else gkw
  else
    gkw
  where
    unicodeSyntax =
      [ GHC.AnnDcolon
      , GHC.AnnDarrow
      , GHC.AnnForall
      , GHC.AnnRarrow
      , GHC.AnnLarrow
      , GHC.Annlarrowtail
      , GHC.Annrarrowtail
      , GHC.AnnLarrowtail
      , GHC.AnnLarrowtail]
checkUnicode kwid _ = kwid

-- ---------------------------------------------------------------------

commentAllocation :: (Comment -> Bool)
                  -> ([(DComment, DeltaPos)] -> Delta a)
                  -> Delta a
commentAllocation p k = do
  cs <- getUnallocatedComments
  let (allocated,cs') = allocateComments p cs
  putUnallocatedComments cs'
  k =<< mapM makeDeltaComment (sort allocated)


makeDeltaComment :: Comment -> Delta (DComment, DeltaPos)
makeDeltaComment c = do
  let paspan = commentPos c
      pa = span2ss paspan
  pe <- getPriorEnd
  let p = ss2delta pe pa
  p' <- adjustDeltaForOffsetM p
  setPriorEnd (ss2posEnd pa)
  let e = pos2delta pe (snd paspan)
  e' <- adjustDeltaForOffsetM e
  return $ (mkDComment c e', p')

addDeltaComment :: DComment -> DeltaPos -> Delta ()
addDeltaComment d p = do
  addAnnDeltaPos (AnnComment d) p

-- ---------------------------------------------------------------------

-- | Look up and add a Delta annotation at the current position, and
-- advance the position to the end of the annotation
addDeltaAnnotation :: GHC.AnnKeywordId -> Delta ()
addDeltaAnnotation ann = do
  ss <- getSrcSpan
  -- ma <- getAnnotationDelta ann
  ma <- getOneAnnotationDelta ann
  case nub ma of -- ++AZ++ TODO: get rid of duplicates earlier
    []     -> return () `debug` ("addDeltaAnnotation empty ma for:" ++ show (ss,ann))
    [pa]   -> addAnnotationWorker (G ann) pa
    (pa:_) -> addAnnotationWorker (G ann) pa `warn` ("addDeltaAnnotation:(ss,ann,ma)=" ++ showGhc (ss,ann,ma))

-- | Look up and add a Delta annotation appearing beyond the current
-- SrcSpan at the current position, and advance the position to the
-- end of the annotation
addDeltaAnnotationAfter :: GHC.AnnKeywordId -> Delta ()
addDeltaAnnotationAfter ann = do
  ss <- getSrcSpan
  ma <- getAnnotationDelta ann
  let ma' = filter (\s -> not (GHC.isSubspanOf s ss)) ma
  case ma' of
    []     -> return () `debug` ("addDeltaAnnotationAfter empty ma for(ss,ann,ma)=" ++ showGhc (ss,ann,ma))
    [pa]   -> addAnnotationWorker (G ann) pa
    (pa:_) -> addAnnotationWorker (G ann) pa `warn` ("addDeltaAnnotationAfter:(ss,ann,ma)=" ++ showGhc (ss,ann,ma))

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
  ma <- getAnnotationDelta ann
  let do_one ap' = addAnnotationWorker (G ann) ap'
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
addDeltaAnnotationsOutside :: GHC.AnnKeywordId -> KeywordId -> Delta ()
addDeltaAnnotationsOutside gann ann = do
  ss <- getSrcSpan
  unless (ss2span ss == ((1,1),(1,1))) $
    do
      ma <- getAndRemoveAnnotationDelta ss gann
      let do_one ap' = addAnnotationWorker ann ap'
      mapM_ do_one (sort $ filter (\s -> not (GHC.isSubspanOf s ss)) ma)

-- | Add a Delta annotation at the current position, and advance the
-- position to the end of the annotation
addDeltaAnnotationExt :: GHC.SrcSpan -> GHC.AnnKeywordId -> Delta ()
addDeltaAnnotationExt s ann = addAnnotationWorker (G ann) s

addEofAnnotation :: Delta ()
addEofAnnotation = do
  pe <- getPriorEnd
  ma <- withSrcSpanDelta (GHC.noLoc ()) (getAnnotationDelta GHC.AnnEofPos)
  case ma of
    [] -> return ()
    (pa:pss) -> do
      commentAllocation (const True) (mapM_ (uncurry addDeltaComment))
      let DP (r,c) = ss2delta pe pa
      addAnnDeltaPos (G GHC.AnnEofPos) (DP (r, c - 1))
      setPriorEndAST (ss2posEnd pa) `warn` ("Trailing annotations after Eof: " ++ showGhc pss)


countAnnsDelta :: GHC.AnnKeywordId -> Delta Int
countAnnsDelta ann = do
  ma <- peekAnnotationDelta ann
  return (length ma)

