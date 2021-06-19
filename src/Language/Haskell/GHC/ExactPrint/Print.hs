{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.GHC.ExactPrint.Print
--
-- This module inverts the process performed by "Delta". Given 'Anns' and
-- a corresponding AST we produce a source file based on this information.
--
-----------------------------------------------------------------------------
module Language.Haskell.GHC.ExactPrint.Print
        (
        exactPrint
        , exactPrintWithOptions

        -- * Configuration
        , PrintOptions(epRigidity, epAstPrint, epTokenPrint, epWhitespacePrint)
        , stringOptions
        , printOptions

        ) where

import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Utils
import Language.Haskell.GHC.ExactPrint.Annotate
import Language.Haskell.GHC.ExactPrint.Lookup

import Control.Monad.Identity
import Control.Monad.RWS
import Control.Monad.Trans.Free
import Data.Data (Data)
import Data.List (sortBy, elemIndex)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)

import qualified Data.Set as Set

import qualified GHC

{-# ANN module "HLint: ignore Eta reduce" #-}
{-# ANN module "HLint: ignore Redundant do" #-}
{-# ANN module "HLint: ignore Reduce duplication" #-}
-- ---------------------------------------------------------------------
-- Printing of source elements

-- | Print an AST with a map of potential modified `Anns`. The usual way to
-- generate such a map is by using one of the parsers in
-- "Language.Haskell.GHC.ExactPrint.Parsers".
exactPrint :: Annotate ast
                     => GHC.Located ast
                     -> Anns
                     -> String
exactPrint ast as = runIdentity (exactPrintWithOptions stringOptions ast as)

-- | The additional option to specify the rigidity and printing
-- configuration.
exactPrintWithOptions :: (Annotate ast, Monoid b, Monad m)
                      => PrintOptions m b
                      -> GHC.Located ast
                      -> Anns
                      -> m b
exactPrintWithOptions r ast as =
    runEP r (annotate ast) as

------------------------------------------------------
-- The EP monad and basic combinators

data PrintOptions m a = PrintOptions
            {
              epAnn :: !Annotation
            , epAstPrint :: forall ast . Data ast => GHC.Located ast -> a -> m a
            , epTokenPrint :: String -> m a
            , epWhitespacePrint :: String -> m a
            , epRigidity :: Rigidity
            , epContext :: !AstContextSet
            }

-- | Helper to create a 'PrintOptions'
printOptions ::
      (forall ast . Data ast => GHC.Located ast -> a -> m a)
      -> (String -> m a)
      -> (String -> m a)
      -> Rigidity
      -> PrintOptions m a
printOptions astPrint tokenPrint wsPrint rigidity = PrintOptions
             {
               epAnn = annNone
             , epAstPrint = astPrint
             , epWhitespacePrint = wsPrint
             , epTokenPrint = tokenPrint
             , epRigidity = rigidity
             , epContext = defaultACS
             }

-- | Options which can be used to print as a normal String.
stringOptions :: PrintOptions Identity String
stringOptions = printOptions (\_ b -> return b) return return NormalLayout

data EPWriter a = EPWriter
              { output :: !a }

#if __GLASGOW_HASKELL__ >= 804
instance Monoid w => Semigroup (EPWriter w) where
  (<>) = mappend
#endif

instance Monoid w => Monoid (EPWriter w) where
  mempty = EPWriter mempty
  (EPWriter a) `mappend` (EPWriter b) = EPWriter (a <> b)

data EPState = EPState
             { epPos    :: !Pos -- ^ Current output position
             , epAnns   :: !Anns
             , epAnnKds :: ![[(KeywordId, DeltaPos)]] -- MP: Could this be moved to the local statE w mith suitable refactoring?
             , epMarkLayout :: Bool
             , epLHS :: LayoutStartCol
             }

---------------------------------------------------------

type EP w m a = RWST (PrintOptions m w) (EPWriter w) EPState m a



runEP :: (Monad m, Monoid a)
      => PrintOptions m a
      -> Annotated () -> Anns -> m a
runEP epReader action ans =
  fmap (output . snd) .
    (\next -> execRWST next epReader
    (defaultEPState ans))
  . printInterpret $ action

-- ---------------------------------------------------------------------

defaultEPState :: Anns -> EPState
defaultEPState as = EPState
             { epPos    = (1,1)
             , epAnns   = as
             , epAnnKds = []
             , epLHS    = 1
             , epMarkLayout = False
             }


-- ---------------------------------------------------------------------

printInterpret :: forall w m a . (Monad m, Monoid w)
               => Annotated a -> EP w m a
printInterpret m = iterTM go (hoistFreeT (return . runIdentity) m)
  where
    go :: AnnotationF (EP w m a) -> EP w m a
    go (MarkEOF next) =
#if __GLASGOW_HASKELL__ >= 808
      printStringAtMaybeAnn        AnnEofPos  (Just "") >> next
#else
      printStringAtMaybeAnn (G GHC.AnnEofPos) (Just "") >> next
#endif
    go (MarkPrim kwid mstr next) =
      markPrim (G kwid) mstr >> next
    go (MarkPPOptional kwid mstr next) =
      markPrim (G kwid) mstr >> next
#if __GLASGOW_HASKELL__ >= 800
    go (MarkInstead _ kwid next) =
      printStringAtMaybeAnnAll kwid Nothing  >> next
#endif
    go (MarkOutside _ kwid next) =
      printStringAtMaybeAnnAll kwid Nothing  >> next
    go (MarkInside akwid next) =
      allAnns akwid >> next
    go (MarkMany akwid next) =
      allAnns akwid >> next
    go (MarkManyOptional akwid next) =
      allAnns akwid >> next
    go (MarkOffsetPrim kwid _ mstr next) =
      printStringAtMaybeAnn (G kwid) mstr >> next
    go (MarkOffsetPrimOptional kwid _ mstr next) =
      printStringAtMaybeAnn (G kwid) mstr >> next
    go (WithAST lss action next) =
      exactPC lss (printInterpret action) >> next
    go (CountAnns kwid next) =
      countAnnsEP (G kwid) >>= next
    go (SetLayoutFlag r action next) = do
      rigidity <- asks epRigidity
      (if r <= rigidity then setLayout else id) (printInterpret action)
      next

    go (MarkAnnBeforeAnn ann1 ann2 next) = printMarkAnnBeforeAnn (G ann1) (G ann2) >> next
    go (MarkExternal _ akwid s next) =
      printStringAtMaybeAnn (G akwid) (Just s) >> next
    go (StoreOriginalSrcSpan _ _ next) = storeOriginalSrcSpanPrint >>= next
    go (GetSrcSpanForKw _ _ next) = return GHC.noSrcSpan >>= next
#if __GLASGOW_HASKELL__ <= 710
    go (StoreString _ _ next) =
      printStoredString >> next
#endif
    go (AnnotationsToComments     _ next) = next
#if __GLASGOW_HASKELL__ <= 710
    go (AnnotationsToCommentsBF _ _ next) = next
    go (FinalizeBF _ next)                = next
#endif
    go (WithSortKey             ks next) = withSortKey             ks >> next
    go (WithSortKeyContexts ctx ks next) = withSortKeyContexts ctx ks >> next

    go (SetContextLevel ctxt lvl       action next) = setContextPrint ctxt lvl (printInterpret action) >> next
    go (UnsetContext   _ctxt           action next) = printInterpret action >> next
    go (IfInContext  ctxt ifAction elseAction next) = ifInContextPrint ctxt ifAction elseAction >> next
    go (TellContext _ next)                  = next

-------------------------------------------------------------------------

storeOriginalSrcSpanPrint :: (Monad m, Monoid w) => EP w m AnnKey
storeOriginalSrcSpanPrint = do
  Ann{..} <- asks epAnn
  case annCapturedSpan of
    Nothing -> error "Missing captured SrcSpan"
    Just v  -> return v

#if __GLASGOW_HASKELL__ <= 710
printStoredString :: (Monad m, Monoid w) => EP w m ()
printStoredString = do
  kd <- gets epAnnKds

  let
    isAnnString (AnnString _,_) = True
    isAnnString _             = False

  case filter isAnnString (ghead "printStoredString" kd) of
    ((AnnString ss,_):_) -> printStringAtMaybeAnn (AnnString ss) (Just ss)
    _                    -> return ()
#endif

withSortKey :: (Monad m, Monoid w) => [(AnnSpan, Annotated ())] -> EP w m ()
withSortKey xs = do
  Ann{..} <- asks epAnn
  let ordered = case annSortKey of
                  Nothing   -> xs
                  Just keys -> orderByKey xs keys
                                `debug` ("withSortKey:" ++
                                         showGhc (map fst (sortBy (comparing (flip elemIndex keys . fst)) xs),
                                                 map fst xs,
                                                 keys)
                                         )
  mapM_ (printInterpret . snd) ordered

withSortKeyContexts :: (Monad m, Monoid w) => ListContexts -> [(AnnSpan, Annotated ())] -> EP w m ()
withSortKeyContexts ctxts xs = do
  Ann{..} <- asks epAnn
  let ordered = case annSortKey of
                  Nothing   -> xs
                  Just keys -> orderByKey xs keys
                                `debug` ("withSortKey:" ++
                                         showGhc (map fst (sortBy (comparing (flip elemIndex keys . fst)) xs),
                                                 map fst xs,
                                                 keys)
                                         )
  -- mapM_ printInterpret ordered
  withSortKeyContextsHelper printInterpret ctxts ordered

-- ---------------------------------------------------------------------

setContextPrint :: (Monad m, Monoid w) => Set.Set AstContext -> Int -> EP w m () -> EP w m ()
setContextPrint ctxt lvl =
  local (\s -> s { epContext = setAcsWithLevel ctxt lvl (epContext s) } )

ifInContextPrint :: (Monad m, Monoid w) => Set.Set AstContext -> Annotated () -> Annotated () -> EP w m ()
ifInContextPrint ctxt ifAction elseAction = do
  cur <- asks epContext
  let inContext = inAcs ctxt cur
  if inContext
    then printInterpret ifAction
    else printInterpret elseAction

-- ---------------------------------------------------------------------

allAnns :: (Monad m, Monoid w) => GHC.AnnKeywordId -> EP w m ()
allAnns kwid = printStringAtMaybeAnnAll (G kwid) Nothing

-------------------------------------------------------------------------
-- |First move to the given location, then call exactP
-- exactPC :: (Data ast, Monad m, Monoid w) => GHC.Located ast -> EP w m a -> EP w m a
-- exactPC :: (Data ast, Data (GHC.SrcSpanLess ast), GHC.HasSrcSpan ast, Monad m, Monoid w)
exactPC :: (Data ast, Monad m, Monoid w) => GHC.Located ast -> EP w m a -> EP w m a
exactPC ast action =
    do
      return () `debug` ("exactPC entered for:" ++ show (mkAnnKey ast))
      ma <- getAndRemoveAnnotation ast
      let an@Ann{ annEntryDelta=edp
                , annPriorComments=comments
                , annFollowingComments=fcomments
                , annsDP=kds
                } = fromMaybe annNone ma
      PrintOptions{epAstPrint} <- ask
      r <- withContext kds an
       (mapM_ (uncurry printQueuedComment) comments
       >> advance edp
       >> censorM (epAstPrint ast) action
       <* mapM_ (uncurry printQueuedComment) fcomments)
      return r `debug` ("leaving exactPCfor:" ++ show (mkAnnKey ast))

censorM :: (Monoid w, Monad m) => (w -> m w) -> EP w m a -> EP w m a
censorM f m = passM (liftM (\x -> (x,f)) m)

passM :: (Monad m) => EP w m (a, w -> m w) -> EP w m a
passM m = RWST $ \r s -> do
      ~((a, f),s', EPWriter w) <- runRWST m r s
      w' <- f w
      return (a, s', EPWriter w')

advance :: (Monad m, Monoid w) => DeltaPos -> EP w m ()
advance cl = do
  p <- getPos
  colOffset <- getLayoutOffset
  printWhitespace (undelta p cl colOffset)

getAndRemoveAnnotation :: (Monad m, Monoid w, Data a) => GHC.Located a -> EP w m (Maybe Annotation)
getAndRemoveAnnotation a = gets (getAnnotationEP a . epAnns)

markPrim :: (Monad m, Monoid w) => KeywordId -> Maybe String -> EP w m ()
markPrim kwid mstr =
  printStringAtMaybeAnn kwid mstr

withContext :: (Monad m, Monoid w)
            => [(KeywordId, DeltaPos)]
            -> Annotation
            -> EP w m a -> EP w m a
withContext kds an x = withKds kds (withOffset an x)

-- ---------------------------------------------------------------------
--
-- | Given an annotation associated with a specific SrcSpan, determines a new offset relative to the previous
-- offset
--
withOffset :: (Monad m, Monoid w) => Annotation -> (EP w m a -> EP w m a)
withOffset a =
  local (\s -> s { epAnn = a, epContext = pushAcs (epContext s) })


-- ---------------------------------------------------------------------
--
-- Necessary as there are destructive gets of Kds across scopes
withKds :: (Monad m, Monoid w) => [(KeywordId, DeltaPos)] -> EP w m a -> EP w m a
withKds kd action = do
  modify (\s -> s { epAnnKds = kd : epAnnKds s })
  r <- action
  modify (\s -> s { epAnnKds = tail (epAnnKds s) })
  return r

------------------------------------------------------------------------

setLayout :: (Monad m, Monoid w) => EP w m () -> EP w m ()
setLayout k = do
  oldLHS <- gets epLHS
  modify (\a -> a { epMarkLayout = True } )
  let reset = modify (\a -> a { epMarkLayout = False
                              , epLHS = oldLHS } )
  k <* reset

getPos :: (Monad m, Monoid w) => EP w m Pos
getPos = gets epPos

setPos :: (Monad m, Monoid w) => Pos -> EP w m ()
setPos l = modify (\s -> s {epPos = l})

-- |Get the current column offset
getLayoutOffset :: (Monad m, Monoid w) => EP w m LayoutStartCol
getLayoutOffset = gets epLHS

-- ---------------------------------------------------------------------

-- |If the first annotation has a smaller SrcSpan than the second, then mark it.
-- In the printer this means the first appearing before the second in the list
-- of annotations remaining
printMarkAnnBeforeAnn :: (Monad m, Monoid w) => KeywordId -> KeywordId -> EP w m ()
printMarkAnnBeforeAnn annBefore annAfter = do
  kd <- gets epAnnKds
  case kd of
    []    -> return () -- Should never be triggered
    (k:_kds) -> do
      -- find the first ann, then the second. If found in that order, annotate.
      let find a = (\(kw,_) -> kw == a)
      case break (find annBefore) k of
        (_,[]) -> return () -- annBefore not present
        (_,rest) -> if null (snd $ break (find annAfter) rest)
                      then return ()
                      else markPrim annBefore (Nothing)

-- ---------------------------------------------------------------------

printStringAtMaybeAnn :: (Monad m, Monoid w) => KeywordId -> Maybe String -> EP w m ()
printStringAtMaybeAnn an mstr = printStringAtMaybeAnnThen an mstr (return ())

printStringAtMaybeAnnAll :: (Monad m, Monoid w) => KeywordId -> Maybe String -> EP w m ()
printStringAtMaybeAnnAll an mstr = go
  where
    go = printStringAtMaybeAnnThen an mstr go

printStringAtMaybeAnnThen :: (Monad m, Monoid w) => KeywordId -> Maybe String -> EP w m () -> EP w m ()
printStringAtMaybeAnnThen an mstr next = do
  let str = fromMaybe (keywordToString an) mstr
  annFinal <- getAnnFinal an
  case (annFinal, an) of
#if __GLASGOW_HASKELL__ <= 710
    -- Could be unicode syntax
    -- TODO: This is a bit fishy, refactor
    (Nothing, G kw) -> do
      res <- getAnnFinal (AnnUnicode kw)
      return () `debug` ("printStringAtMaybeAnn:missed:Unicode:(an,res)" ++ show (an,res))
      unless (null res) $ do
        forM_
          res
          (\(comments, ma) -> printStringAtLsDelta comments ma (unicodeString (G kw)))
        next
#else
    -- Could be unicode syntax
    -- TODO: This is a bit fishy, refactor
    (Nothing, G kw') -> do
      let kw = GHC.unicodeAnn kw'
      let str' = fromMaybe (keywordToString (G kw)) mstr
      res <- getAnnFinal (G kw)
      return () `debug` ("printStringAtMaybeAnn:missed:Unicode:(an,res)" ++ show (an,res))
      unless (null res) $ do
        forM_
          res
          (\(comments, ma) -> printStringAtLsDelta comments ma str')
        next
#endif
    (Just (comments, ma),_) -> printStringAtLsDelta comments ma str >> next
    (Nothing, _) -> return () `debug` ("printStringAtMaybeAnn:missed:(an)" ++ show an)
                    -- Note: do not call next, nothing to chain
    -- ++AZ++: Enabling the following line causes a very weird error associated with AnnPackageName. I suspect it is because it is forcing the evaluation of a non-existent an or str
    -- `debug` ("printStringAtMaybeAnn:(an,ma,str)=" ++ show (an,ma,str))

-- ---------------------------------------------------------------------

-- |destructive get, hence use an annotation once only
getAnnFinal :: (Monad m, Monoid w) => KeywordId -> EP w m (Maybe ([(Comment, DeltaPos)], DeltaPos))
getAnnFinal kw = do
  kd <- gets epAnnKds
  case kd of
    []    -> return Nothing -- Should never be triggered
    (k:kds) -> do
      let (res, kd') = destructiveGetFirst kw ([],k)
      modify (\s -> s { epAnnKds = kd' : kds })
      return res

-- | Get and remove the first item in the (k,v) list for which the k matches.
-- Return the value, together with any comments skipped over to get there.
destructiveGetFirst :: KeywordId
                    -> ([(KeywordId, v)],[(KeywordId,v)])
                    -> (Maybe ([(Comment, v)], v),[(KeywordId,v)])
destructiveGetFirst _key (acc,[]) = (Nothing, acc)
destructiveGetFirst  key (acc, (k,v):kvs )
  | k == key = (Just (skippedComments, v), others ++ kvs)
  | otherwise = destructiveGetFirst key (acc ++ [(k,v)], kvs)
  where
    (skippedComments, others) = foldr comments ([], []) acc
    comments (AnnComment comment , dp ) (cs, kws) = ((comment, dp) : cs, kws)
    comments kw (cs, kws)                        = (cs, kw : kws)


-- ---------------------------------------------------------------------

-- |This should be the final point where things are mode concrete,
-- before output. Hence the point where comments can be inserted
printStringAtLsDelta :: (Monad m, Monoid w) => [(Comment, DeltaPos)] -> DeltaPos -> String -> EP w m ()
printStringAtLsDelta cs cl s = do
  p <- getPos
  colOffset <- getLayoutOffset
  if isGoodDeltaWithOffset cl colOffset
    then do
      mapM_ (uncurry printQueuedComment) cs
      printStringAt (undelta p cl colOffset) s
        `debug` ("printStringAtLsDelta:(pos,s):" ++ show (undelta p cl colOffset,s))
    else return () `debug` ("printStringAtLsDelta:bad delta for (mc,s):" ++ show (cl,s))


isGoodDeltaWithOffset :: DeltaPos -> LayoutStartCol -> Bool
isGoodDeltaWithOffset dp colOffset = isGoodDelta (DP (undelta (0,0) dp colOffset))

printQueuedComment :: (Monad m, Monoid w) => Comment -> DeltaPos -> EP w m ()
printQueuedComment Comment{commentContents} dp = do
  p <- getPos
  colOffset <- getLayoutOffset
  let (dr,dc) = undelta (0,0) dp colOffset
  -- do not lose comments against the left margin
  when (isGoodDelta (DP (dr,max 0 dc))) $
    printCommentAt (undelta p dp colOffset) commentContents

-- ---------------------------------------------------------------------

-- |non-destructive get
peekAnnFinal :: (Monad m, Monoid w) => KeywordId -> EP w m (Maybe DeltaPos)
peekAnnFinal kw = do
  (r, _) <- (\kd -> destructiveGetFirst kw ([], kd)) <$> gets (ghead "peekAnnFinal" . epAnnKds)
  return (snd <$> r)

countAnnsEP :: (Monad m, Monoid w) => KeywordId -> EP w m Int
countAnnsEP an = length <$> peekAnnFinal an

-- ---------------------------------------------------------------------


-- ---------------------------------------------------------------------
-- Printing functions

printString :: (Monad m, Monoid w) => Bool -> String -> EP w m ()
printString layout str = do
  EPState{epPos = (_,c), epMarkLayout} <- get
  PrintOptions{epTokenPrint, epWhitespacePrint} <- ask
  when (epMarkLayout && layout) $
    modify (\s -> s { epLHS = LayoutStartCol c, epMarkLayout = False } )

  -- Advance position, taking care of any newlines in the string
  let strDP@(DP (cr,_cc)) = dpFromString str
  p <- getPos
  colOffset <- getLayoutOffset
  if cr == 0
    then setPos (undelta p strDP colOffset)
    else setPos (undelta p strDP 1)

  --
  if not layout && c == 0
    then lift (epWhitespacePrint str) >>= \s -> tell EPWriter { output = s}
    else lift (epTokenPrint      str) >>= \s -> tell EPWriter { output = s}


newLine :: (Monad m, Monoid w) => EP w m ()
newLine = do
    (l,_) <- getPos
    printString False "\n"
    setPos (l+1,1)

padUntil :: (Monad m, Monoid w) => Pos -> EP w m ()
padUntil (l,c) = do
    (l1,c1) <- getPos
    if | l1 == l && c1 <= c -> printString False $ replicate (c - c1) ' '
       | l1 < l             -> newLine >> padUntil (l,c)
       | otherwise          -> return ()

printWhitespace :: (Monad m, Monoid w) => Pos -> EP w m ()
printWhitespace = padUntil

printCommentAt :: (Monad m, Monoid w) => Pos -> String -> EP w m ()
printCommentAt p str = printWhitespace p >> printString False str

printStringAt :: (Monad m, Monoid w) => Pos -> String -> EP w m ()
printStringAt p str = printWhitespace p >> printString True str
