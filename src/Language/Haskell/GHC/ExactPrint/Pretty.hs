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
import Data.List
-- import Data.Maybe
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

       , apMarkLayout :: Bool
       , apLayoutStart :: LayoutStartCol

       , apNoPrecedingSpace :: Bool

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
      , apLayoutStart = 1
      , apMarkLayout = False
      , apNoPrecedingSpace = False
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
    go (MarkMany akwid next)            = addPrettyAnnotation akwid >> next
    go (MarkManyOptional _akwid next)   = next
    go (MarkOffsetPrim akwid n _ next)  = addPrettyAnnotationLs akwid n >> next
    go (WithAST lss prog next)          = withAST lss (prettyInterpret prog) >> next
    go (CountAnns kwid next)            = countAnnsPretty kwid >>= next
    go (WithSortKey             kws next) = withSortKey             kws >> next
    go (WithSortKeyContexts ctx kws next) = withSortKeyContexts ctx kws >> next
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
    go (UnsetContext    ctxt     action next)  = unsetContextPretty ctxt (prettyInterpret action) >> next
    go (IfInContext  ctxt ifAction elseAction next) = ifInContextPretty ctxt ifAction elseAction >> next
    go (NotInContext ctxt action next)  = notInContextPretty ctxt action >> next

-- ---------------------------------------------------------------------

addEofAnnotation :: Pretty ()
addEofAnnotation = tellKd (G GHC.AnnEofPos, DP (1,0))

-- ---------------------------------------------------------------------

addPrettyAnnotation :: GHC.AnnKeywordId -> Pretty ()
addPrettyAnnotation ann = do
  noPrec <- gets apNoPrecedingSpace
  ctx <- asks prContext
  cur <- trace ("Pretty.addPrettyAnnotation:=" ++ showGhc (ann,noPrec,ctx)) $ asks prContext
  -- cur <- asks prContext
  let
    dp = case ann of
           GHC.AnnAs       -> tellKd (G ann,DP (0,1))
           GHC.AnnCloseC   -> tellKd (G ann,DP (0,0))
           GHC.AnnDcolon   -> tellKd (G ann,DP (0,1))
           GHC.AnnDeriving -> tellKd (G ann,DP (0,1))
           GHC.AnnEqual    -> tellKd (G ann,DP (0,1))
           GHC.AnnHiding   -> tellKd (G ann,DP (0,1))
           GHC.AnnIn       -> tellKd (G ann,DP (1,0))
           GHC.AnnOf       -> tellKd (G ann,DP (0,1))
           GHC.AnnOpenC    -> tellKd (G ann,DP (0,0))
           GHC.AnnQualified -> tellKd (G ann,DP (0,1))
           GHC.AnnRarrow   -> tellKd (G ann,DP (0,1))
           GHC.AnnVal      -> tellKd (G ann,DP (0,1))
           GHC.AnnWhere    -> tellKd (G ann,DP (0,1))
           _ ->               tellKd (G ann,DP (0,0))
  fromNoPrecedingSpace (tellKd (G ann,DP (0,0))) dp

-- ---------------------------------------------------------------------

addPrettyAnnotationsOutside :: GHC.AnnKeywordId -> KeywordId -> Pretty ()
addPrettyAnnotationsOutside _akwid _kwid = return ()

-- ---------------------------------------------------------------------

addPrettyAnnotationsInside :: GHC.AnnKeywordId -> Pretty ()
addPrettyAnnotationsInside _ann = return ()

-- ---------------------------------------------------------------------

addPrettyAnnotations :: GHC.AnnKeywordId -> Pretty ()
addPrettyAnnotations _ann = return ()

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
  -- off <- gets apLayoutStart
  withSrcSpanPretty lss $ do
    return () `debug` ("Pretty.withAST:enter:(ss)=" ++ showGhc (ss,showConstr (toConstr t)))

    let maskWriter s = s { annKds         = []
                         , sortKeys       = Nothing
                         , dwCapturedSpan = mempty
                         }
    ctx <- asks prContext
    -- let spanStart = ss2pos ss
        -- edp = DP (0,0)
    noPrec <- gets apNoPrecedingSpace
    -- edp <- trace ("Pretty.withAST:enter:(ss,constr,noPrec,ctx)=" ++ showGhc (ss,showConstr (toConstr t),noPrec,ctx)) $ entryDpFor ctx t
    edp <- entryDpFor ctx t

    -- let cs = []
    let cs = trace ("Pretty.withAST:enter:(ss,constr,noPrec,ctx,edp)=" ++ showGhc (ss,showConstr (toConstr t),noPrec,ctx,edp)) []
    -- (res, w) <- if inAcs (Set.singleton ListItem) ctx
    (res, w) <- if inAcs (Set.fromList [ListItem,TopLevel]) ctx
      then
           trace ("Pretty.withAST:setNoPrecedingSpace") $
             censor maskWriter (listen (setNoPrecedingSpace action))
      else
           -- trace ("Pretty.withAST:setNoPrecedingSpace") $
            censor maskWriter (listen action)

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
        -- `extQ` funBind
        -- `extQ` match
        `extQ` grhs
        ) a
  where
    lineDefault = if inAcs (Set.singleton AdvanceLine) ctx
                    then 1 else 0

    def :: a -> Pretty DeltaPos
    -- def _ = return $ DP (lineDefault,0)
    def _ =
      trace ("entryDpFor:(topLevel,listStart,inList)=" ++ show (topLevel,listStart,inList)) $
        if listStart
          then return (DP (1,2))
          else if inList
            then if topLevel then return (DP (2,0)) else return (DP (1,0))
            else if topLevel then return (DP (2,0)) else return (DP (lineDefault,0))
            -- else return (DP (lineDefault,0))
            -- else fromLayout (DP (2,0)) (DP (lineDefault,0))

    topLevel = inAcs (Set.singleton TopLevel) ctx
    inCase = inAcs (Set.singleton CaseAlt) ctx
    -- listStart = trace ("listStart:ctx=" ++ show ctx) $ inAcs (Set.singleton ListStart) ctx
    --                                                  && not (inAcs (Set.singleton TopLevel) ctx)
    listStart = inAcs (Set.singleton ListStart) ctx
              && not (inAcs (Set.singleton TopLevel) ctx)
    inList = inAcs (Set.singleton ListItem) ctx
            -- && not (inAcs (Set.singleton TopLevel) ctx)
    inLambda = inAcs (Set.singleton LambdaExpr) ctx

    funBind :: GHC.HsBind GHC.RdrName -> Pretty DeltaPos
    funBind GHC.FunBind{} =
      if listStart
        then return $ DP (1,2)
        else fromLayout (DP (2,0)) (DP (1,2))
    funBind GHC.PatBind{} = return $ DP (2,0)
    funBind _ = return $ DP (lineDefault,0)

    match :: GHC.Match GHC.RdrName (GHC.LHsExpr GHC.RdrName) -> Pretty DeltaPos
    match _ = do
      let
        defVal = if inLambda then DP (0,1) else DP (1,0)
      if listStart
        then return (DP (0,0))
        else fromLayout defVal (DP (1,2))

    grhs :: GHC.GRHS GHC.RdrName (GHC.LHsExpr GHC.RdrName) -> Pretty DeltaPos
    grhs _ = do
      fromLayout (DP (1,2)) (DP (1,2))
      -- if listStart
      --   then return (DP (0,0))
      --   else fromLayout (DP (1,0)) (DP (1,12))

-- ---------------------------------------------------------------------

-- |Like fromMaybe, in that if no layout flag is set return the first value,
-- else return the second and reset the layout flag.
fromLayout :: a -> a -> Pretty a
fromLayout def lay = do
  PrettyState{apMarkLayout} <- get
  if apMarkLayout
    then do
      modify (\s -> s { apMarkLayout = False
                      })
      return lay
    else return def

fromNoPrecedingSpace :: Pretty a -> Pretty a -> Pretty a
fromNoPrecedingSpace def lay = do
  PrettyState{apNoPrecedingSpace} <- get
  ctx <- asks prContext
  if apNoPrecedingSpace
    then do
      modify (\s -> s { apNoPrecedingSpace = False
                      })
      trace ("fromNoPrecedingSpace:def") def
      -- def
    else
      -- if (inAcs (Set.singleton TopLevel) ctx)
      --   then trace ("fromNoPrecedingSpace:tl:def") def
      --   else trace ("fromNoPrecedingSpace:lay") lay
      trace ("fromNoPrecedingSpace:lay") lay


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

withSortKeyContexts :: ListContexts -> [(GHC.SrcSpan, Annotated ())] -> Pretty ()
withSortKeyContexts ctxts kws =
  let order = sortBy (comparing fst) kws
  in do
    tellSortKey (map fst order)
    -- mapM_ (prettyInterpret . snd) order
    withSortKeyContextsHelper prettyInterpret ctxts order

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

withNoPrecedingSpace :: Pretty () -> Pretty ()
withNoPrecedingSpace action = do
  oldVal <- gets apNoPrecedingSpace
  inLayout <- gets apMarkLayout
  if inLayout
    then do
      modify (\s -> s { apNoPrecedingSpace = True } )
      let reset = modify (\s -> s { apNoPrecedingSpace = oldVal
                                  , apMarkLayout = False
                                  })
      action <* reset
    else action

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
