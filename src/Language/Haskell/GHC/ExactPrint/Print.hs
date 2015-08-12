{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

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
        , semanticPrint

        ) where

import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Utils
import Language.Haskell.GHC.ExactPrint.Annotate
  (AnnotationF(..), Annotated, Annotate(..), annotate)
import Language.Haskell.GHC.ExactPrint.Lookup (keywordToString, unicodeString)

import Control.Monad.RWS
import Data.Data (Data)
import Data.List (sortBy, elemIndex)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe)

import Control.Monad.Trans.Free
import Control.Monad.Identity
import Control.Monad.Writer

import qualified GHC

------------------------------------------------------------------------------
-- Printing of source elements

-- | Print an AST with a map of potential modified `Anns`. The usual way to
-- generate such a map is by using one of the parsers in
-- "Language.Haskell.GHC.ExactPrint.Parsers".
exactPrint :: Annotate ast
                     => GHC.Located ast
                     -> Anns
                     -> String
exactPrint = semanticPrint (\_ b -> b) id id


-- | A more general version of 'exactPrint' which allows the customisation
-- of the output whilst retaining the original source formatting. This is
-- useful for smarter syntax highlighting.
semanticPrintM :: (Annotate ast, Monoid b, Monad m) =>
              (forall a . Data a => GHC.Located a -> b -> m b) -- ^ How to surround an AST fragment
              -> (String -> m b) -- ^ How to output a token
              -> (String -> m b) -- ^ How to output whitespace
              -> GHC.Located ast
              -> Anns
              -> m b
semanticPrintM astOut tokenOut whiteOut ast as =  runEP astOut tokenOut whiteOut (annotate ast) as


semanticPrint :: (Annotate ast, Monoid b) =>
              (forall a . Data a => GHC.Located a -> b -> b) -- ^ How to surround an AST fragment
              -> (String -> b) -- ^ How to output a token
              -> (String -> b) -- ^ How to output whitespace
              -> GHC.Located ast
              -> Anns
              -> b
semanticPrint a b c d e = runIdentity (semanticPrintM (\ast s -> Identity (a ast s)) (return . b) (return . c) d e)


------------------------------------------------------
-- The EP monad and basic combinators

data EPReader m a = EPReader
            {
              epAnn :: !Annotation
            , epAstPrint :: forall ast . Data ast => GHC.Located ast -> a -> m a
            , epTokenPrint :: String -> m a
            , epWhitespacePrint :: String -> m a
            }

data EPWriter a = EPWriter
              { output :: !a }

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

type EP w m a = RWST (EPReader m w) (EPWriter w) EPState m a



runEP :: (Monad m, Monoid a) =>
      (forall ast . Data ast => GHC.Located ast -> a -> m a)
      -> (String -> m a)
      -> (String -> m a)
      -> Annotated () -> Anns -> m a
runEP astPrint wsPrint tokenPrint action ans =
  fmap (output . snd) .
    (\next -> execRWST next (initialEPReader astPrint tokenPrint wsPrint) (defaultEPState ans))
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

initialEPReader ::
      (forall ast . Data ast => GHC.Located ast -> a -> m a)
      -> (String -> m a)
      -> (String -> m a)
      -> EPReader m a
initialEPReader astPrint tokenPrint wsPrint  = EPReader
             {
               epAnn = annNone
             , epAstPrint = astPrint
             , epWhitespacePrint = wsPrint
             , epTokenPrint = tokenPrint
             }

-- ---------------------------------------------------------------------

printInterpret :: forall w m a . (Monad m, Monoid w) => Annotated a -> EP w m a
printInterpret m = iterTM go (hoistFreeT (return . runIdentity) m)
  where
    go :: (Monad m, Monoid w) => AnnotationF (EP w m a) -> EP w m a
    go (MarkEOF next) =
      printStringAtMaybeAnn (G GHC.AnnEofPos) "" >> next
    go (MarkPrim kwid mstr next) =
      markPrim (G kwid) mstr >> next
      -- let annString = fromMaybe (keywordToString kwid) mstr in
      --   printStringAtMaybeAnn (G kwid) annString >> next
    go (MarkOutside _ kwid next) =
      -- markPrim kwid Nothing >> next
      let annString = keywordToString kwid in
      printStringAtMaybeAnnAll kwid annString  >> next
      -- printStringAtMaybeAnnAll kwid ";"  >> next
    go (MarkInside akwid next) =
      allAnns akwid >> next
    go (MarkMany akwid next) =
      allAnns akwid >> next
    go (MarkOffsetPrim kwid _ mstr next) =
      let annString = fromMaybe (keywordToString (G kwid)) mstr in
        printStringAtMaybeAnn (G kwid) annString >> next
    go (WithAST lss action next) =
      exactPC lss (printInterpret action) >> next
    go (CountAnns kwid next) =
      countAnnsEP (G kwid) >>= next
    go (SetLayoutFlag  action next) =
      setLayout (printInterpret action) >> next
    go (MarkExternal _ akwid s next) =
      printStringAtMaybeAnn (G akwid) s >> next
    go (StoreOriginalSrcSpan _ next) = storeOriginalSrcSpanPrint >>= next
    go (GetSrcSpanForKw _ next) = return GHC.noSrcSpan >>= next
    go (StoreString _ _ next) =
      printStoredString >> next
    go (AnnotationsToComments _ next) = next
    go (WithSortKey ks next) = withSortKey ks >> next

-------------------------------------------------------------------------

storeOriginalSrcSpanPrint :: (Monad m, Monoid w) => EP w m AnnKey
storeOriginalSrcSpanPrint = do
  Ann{..} <- asks epAnn
  case annCapturedSpan of
    Nothing -> error "Missing captured SrcSpan"
    Just v  -> return v

printStoredString :: (Monad m, Monoid w) => EP w m ()
printStoredString = do
  kd <- gets epAnnKds

  let
    isAnnString (AnnString _,_) = True
    isAnnString _             = False

  case filter isAnnString (ghead "printStoredString" kd) of
    ((AnnString ss,_):_) -> printStringAtMaybeAnn (AnnString ss) ss
    _                    -> return ()

withSortKey :: (Monad m, Monoid w) => [(GHC.SrcSpan, Annotated ())] -> EP w m ()
withSortKey xs = do
  Ann{..} <- asks epAnn
  let ordered = case annSortKey of
                  Nothing   -> map snd xs
                  Just keys -> orderByKey xs keys
                                `debug` ("withSortKey:" ++
                                         showGhc (map fst (sortBy (comparing (flip elemIndex keys . fst)) xs),
                                                 map fst xs,
                                                 keys)
                                         )
  mapM_ printInterpret ordered

-------------------------------------------------------------------------

allAnns :: (Monad m, Monoid w) => GHC.AnnKeywordId -> EP w m ()
allAnns kwid = printStringAtMaybeAnnAll (G kwid) (keywordToString (G kwid))

-------------------------------------------------------------------------
-- |First move to the given location, then call exactP
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
      EPReader{epAstPrint} <- ask
      r <- withContext kds an
       (mapM_ (uncurry printQueuedComment) comments
       >> advance edp
       >> censorM (epAstPrint ast) action
       <* mapM_ (uncurry printQueuedComment) fcomments)
      return r `debug` ("leaving exactPCfor:" ++ show (mkAnnKey ast))

censorM f m = passM (liftM (\x -> (x,f)) m)

passM :: (Monoid w, Monad m) => EP w m (a, w -> m w) -> EP w m a
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
getAndRemoveAnnotation a = gets ((getAnnotationEP a) . epAnns)

markPrim :: (Monad m, Monoid w) => KeywordId -> Maybe String -> EP w m ()
markPrim kwid mstr =
  let annString = fromMaybe (keywordToString kwid) mstr
  in printStringAtMaybeAnn kwid annString

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
  local (\s -> s { epAnn = a })


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

printStringAtMaybeAnn :: (Monad m, Monoid w) => KeywordId -> String -> EP w m ()
printStringAtMaybeAnn an str = printStringAtMaybeAnnThen an str (return ())

printStringAtMaybeAnnAll :: (Monad m, Monoid w) => KeywordId -> String -> EP w m ()
printStringAtMaybeAnnAll an str = go
  where
    go = printStringAtMaybeAnnThen an str go

printStringAtMaybeAnnThen :: (Monad m, Monoid w) => KeywordId -> String -> EP w m () -> EP w m ()
printStringAtMaybeAnnThen an str next = do
  annFinal <- getAnnFinal an
  case (annFinal, an) of
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

-- AZ:TODO: harvest the commonality between this and printStringAtLsDelta
printQueuedComment :: (Monad m, Monoid w) => Comment -> DeltaPos -> EP w m ()
printQueuedComment Comment{commentContents} dp = do
  p <- getPos
  colOffset <- getLayoutOffset
  let (dr,dc) = undelta (0,0) dp colOffset
  -- do not lose comments against the left margin
  when (isGoodDelta (DP (dr,max 0 dc)))
    (do
      printCommentAt (undelta p dp colOffset) commentContents
      setPos (undelta p (dp `addDP` dpFromString commentContents) colOffset))

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
  EPState{epPos = (l,c), epMarkLayout} <- get
  EPReader{epTokenPrint, epWhitespacePrint} <- ask
  when (epMarkLayout && layout) (
                      modify (\s -> s { epLHS = LayoutStartCol c, epMarkLayout = False } ))
  setPos (l, c + length str)
  --
  -- tell (mempty {output = Endo $ showString str })

  if not layout && c == 0
    then lift (epWhitespacePrint str) >>= \s -> tell (EPWriter { output = s})
    else lift (epTokenPrint str) >>= \s -> tell (EPWriter { output = s})


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
