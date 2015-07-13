{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.GHC.ExactPrint.Print
--
-----------------------------------------------------------------------------
module Language.Haskell.GHC.ExactPrint.Print
        (
          Anns
        , exactPrintWithAnns

        , exactPrint

        ) where

import Language.Haskell.GHC.ExactPrint.Internal.Types
import Language.Haskell.GHC.ExactPrint.Utils ( debug, undelta, isGoodDelta, ghead )
import Language.Haskell.GHC.ExactPrint.Annotate
  (AnnotationF(..), Annotated, Annotate(..), annotate)
import Language.Haskell.GHC.ExactPrint.Lookup (keywordToString, unicodeString)
import Language.Haskell.GHC.ExactPrint.Delta ( relativiseApiAnns )

import Control.Monad.RWS
import Data.Data (Data)
import Data.List (sortBy, elemIndex)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe)

import Control.Monad.Trans.Free


import qualified GHC

------------------------------------------------------------------------------
-- Printing of source elements

-- | Print an AST exactly as specified by the annotations on the nodes in the tree.
-- The output of this function should exactly match the source file.
exactPrint :: Annotate ast => GHC.Located ast -> GHC.ApiAnns -> String
exactPrint ast ghcAnns = exactPrintWithAnns ast relativeAnns
  where
    relativeAnns = relativiseApiAnns ast ghcAnns

-- | Print an AST with a map of potential modified `Anns`. The usual way to
-- generate such a map is by calling `relativiseApiAnns`.
exactPrintWithAnns :: Annotate ast
                     => GHC.Located ast
                     -> Anns
                     -> String
exactPrintWithAnns ast an = runEP (annotate ast) an


------------------------------------------------------
-- The EP monad and basic combinators

data EPReader = EPReader
            {  epLHS :: !LayoutStartCol -- ^ Marks the column of the LHS of
                                            -- the current layout block
            ,  epAnn :: !Annotation
            }

data EPWriter = EPWriter
              { output :: !(Endo String) }

instance Monoid EPWriter where
  mempty = EPWriter mempty
  (EPWriter a) `mappend` (EPWriter b) = EPWriter (a <> b)

data EPState = EPState
             { epPos    :: !Pos -- ^ Current output position
             , epAnns   :: !Anns
             , epAnnKds :: ![[(KeywordId, DeltaPos)]] -- MP: Could this be moved to the local state with suitable refactoring?
             }

---------------------------------------------------------

type EP a = RWS EPReader EPWriter EPState a

runEP :: Annotated () -> Anns -> String
runEP action ans =
  flip appEndo "" . output . snd
  . (\next -> execRWS next initialEPReader (defaultEPState ans))
  . printInterpret $ action

-- ---------------------------------------------------------------------

defaultEPState :: Anns -> EPState
defaultEPState as = EPState
             { epPos    = (1,1)
             , epAnns   = as
             , epAnnKds = []
             }

initialEPReader :: EPReader
initialEPReader  = EPReader
             { epLHS = 1
             , epAnn = annNone
             }

-- ---------------------------------------------------------------------

printInterpret :: Annotated a -> EP a
printInterpret = iterTM go
  where
    go :: AnnotationF (EP a) -> EP a
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
    go (MarkAfter akwid next) =
      justOne akwid >> next
    go (WithAST lss flag action next) =
      exactPC lss flag (printInterpret action) >> next
    go (CountAnns kwid next) =
      countAnnsEP (G kwid) >>= next
    go (SetLayoutFlag _ action next) =
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

storeOriginalSrcSpanPrint :: EP AnnKey
storeOriginalSrcSpanPrint = do
  Ann{..} <- asks epAnn
  case annCapturedSpan of
    Nothing -> error "Missing captured SrcSpan"
    Just v  -> return v

printStoredString :: EP ()
printStoredString = do
  kd <- gets epAnnKds

  let
    isAnnString (AnnString _,_) = True
    isAnnString _             = False

  case filter isAnnString (ghead "printStoredString" kd) of
    ((AnnString ss,_):_) -> printStringAtMaybeAnn (AnnString ss) ss
    _                    -> return ()

withSortKey :: [(GHC.SrcSpan, Annotated ())] -> EP ()
withSortKey xs = do
  Ann{..} <- asks epAnn
  let ordered = case annSortKey of
                  Nothing   -> map snd xs
                  Just keys -> match xs keys
                                `debug` ("withSortKey:" ++
                                         showGhc (map fst (sortBy (comparing (flip elemIndex keys . fst)) xs),
                                                 map fst xs,
                                                 keys)
                                         )
  mapM_ printInterpret ordered
  where
    -- AZ:TODO: if performance becomes a problem, consider a Map of the order
    -- SrcSpan to an index, and do a lookup instead of elemIndex.

    -- Items not in the ordering are placed to the start
    match :: [(GHC.SrcSpan, Annotated ())] -> [GHC.SrcSpan] -> [Annotated ()]
    match keys order =
       map snd (sortBy (comparing (flip elemIndex order . fst)) keys)

-------------------------------------------------------------------------

justOne, allAnns :: GHC.AnnKeywordId -> EP ()
justOne kwid = printStringAtMaybeAnn    (G kwid) (keywordToString (G kwid))
allAnns kwid = printStringAtMaybeAnnAll (G kwid) (keywordToString (G kwid))

-------------------------------------------------------------------------
-- |First move to the given location, then call exactP
exactPC :: Data ast => GHC.Located ast -> LayoutFlag -> EP a -> EP a
exactPC ast flag action =
    do
      return () `debug` ("exactPC entered for:" ++ show (mkAnnKey ast))
      ma <- getAndRemoveAnnotation ast
      let an@Ann{ annEntryDelta=edp
                , annPriorComments=comments
                , annFollowingComments=fcomments
                , annsDP=kds
                } = fromMaybe annNone ma
      r <- withContext kds an flag
       (mapM_ (uncurry printQueuedComment) comments
       >> advance edp
       >> action
       <* mapM_ (uncurry printQueuedComment) fcomments)
      return r `debug` ("leaving exactPCfor:" ++ show (mkAnnKey ast))

advance :: DeltaPos -> EP ()
advance cl = do
  p <- getPos
  colOffset <- getLayoutOffset
  printWhitespace (undelta p cl colOffset)

getAndRemoveAnnotation :: (Data a) => GHC.Located a -> EP (Maybe Annotation)
getAndRemoveAnnotation a = gets ((getAnnotationEP a) . epAnns)

markPrim :: KeywordId -> Maybe String -> EP ()
markPrim kwid mstr =
  let annString = fromMaybe (keywordToString kwid) mstr
  in printStringAtMaybeAnn kwid annString

withContext :: [(KeywordId, DeltaPos)]
            -> Annotation
            -> LayoutFlag
            -> EP a -> EP a
withContext kds an flag = withKds kds . withOffset an flag

-- ---------------------------------------------------------------------
--
-- | Given an annotation associated with a specific SrcSpan, determines a new offset relative to the previous
-- offset
--
withOffset :: Annotation -> LayoutFlag -> (EP a -> EP a)
withOffset a@Ann{annDelta, annTrueEntryDelta} flag k = do
  let DP (edLine, edColumn) = annTrueEntryDelta
  oldOffset <-  asks epLHS -- Shift from left hand column
  (_l, currentColumn) <- getPos
    -- Calculate the new offset
    -- 1. If the LayoutRules flag is set then we need to mark this position
    -- as the start of a new layout block.
    -- There are two cases (1) If we are on the same line  and (2) if we
    -- move to a new line.
    -- (1) The start of the layout block is the current position added to
    -- the delta
    -- (2) The start of the layout block is the old offset added to the
    -- "annOffset" (i.e., how far this annotation was from the edge)
  let offset = case flag of
                     LayoutRules -> LayoutStartCol $
                      if edLine == 0
                        then currentColumn + edColumn
                        else getLayoutStartCol oldOffset + getColDelta annDelta
                     NoLayoutRules -> oldOffset
  local (\s -> s { epLHS = offset
                 , epAnn = a }) k


-- ---------------------------------------------------------------------
--
-- Necessary as there are destructive gets of Kds across scopes
withKds :: [(KeywordId, DeltaPos)] -> EP a -> EP a
withKds kd action = do
  modify (\s -> s { epAnnKds = kd : epAnnKds s })
  r <- action
  modify (\s -> s { epAnnKds = tail (epAnnKds s) })
  return r

------------------------------------------------------------------------

setLayout :: EP () -> EP ()
setLayout k = do
  pos <- getPos
  lhs <- asks epLHS
  a@Ann{annLayoutStart} <- asks epAnn
  let newLHS = undelta pos (ghead "setLayout" annLayoutStart) lhs
  local
    (\s -> s { epLHS = LayoutStartCol (snd newLHS)
             , epAnn = a { annLayoutStart = tail annLayoutStart} } )
      k

getPos :: EP Pos
getPos = gets epPos

setPos :: Pos -> EP ()
setPos l = modify (\s -> s {epPos = l})

-- |Get the current column offset
getLayoutOffset :: EP LayoutStartCol
getLayoutOffset = asks epLHS

-- ---------------------------------------------------------------------

printStringAtMaybeAnn :: KeywordId -> String -> EP ()
printStringAtMaybeAnn an str = printStringAtMaybeAnnThen an str (return ())

printStringAtMaybeAnnAll :: KeywordId -> String -> EP ()
printStringAtMaybeAnnAll an str = go
  where
    go = printStringAtMaybeAnnThen an str go

printStringAtMaybeAnnThen :: KeywordId -> String -> EP () -> EP ()
printStringAtMaybeAnnThen an str next = do
  annFinal <- getAnnFinal an
  case (annFinal, an) of
    -- Could be unicode syntax
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
getAnnFinal :: KeywordId -> EP (Maybe ([(DComment, DeltaPos)], DeltaPos))
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
                    -> (Maybe ([(DComment, v)], v),[(KeywordId,v)])
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
printStringAtLsDelta :: [(DComment, DeltaPos)] -> DeltaPos -> String -> EP ()
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
printQueuedComment :: DComment -> DeltaPos -> EP ()
printQueuedComment Comment{commentPos, commentContents} dp = do
  p <- getPos
  colOffset <- getLayoutOffset
  let (dr,dc) = undelta (0,0) dp colOffset
  -- do not lose comments against the left margin
  when (isGoodDelta (DP (dr,max 0 dc)))
    (do
      printStringAt (undelta p dp colOffset) commentContents
      setPos (undelta p commentPos colOffset))

-- ---------------------------------------------------------------------

-- |non-destructive get
peekAnnFinal :: KeywordId -> EP (Maybe DeltaPos)
peekAnnFinal kw = do
  (r, _) <- (\kd -> destructiveGetFirst kw ([], kd)) <$> gets (ghead "peekAnnFinal" . epAnnKds)
  return (snd <$> r)

countAnnsEP :: KeywordId -> EP Int
countAnnsEP an = length <$> peekAnnFinal an

-- ---------------------------------------------------------------------


-- ---------------------------------------------------------------------
-- Printing functions

printString :: String -> EP ()
printString str = do
  (l,c) <- gets epPos
  setPos (l, c + length str)
  tell (mempty {output = Endo $ showString str })

newLine :: EP ()
newLine = do
    (l,_) <- getPos
    printString "\n"
    setPos (l+1,1)

padUntil :: Pos -> EP ()
padUntil (l,c) = do
    (l1,c1) <- getPos
    if | l1 == l && c1 <= c -> printString $ replicate (c - c1) ' '
       | l1 < l             -> newLine >> padUntil (l,c)
       | otherwise          -> return ()

printWhitespace :: Pos -> EP ()
printWhitespace = padUntil

printStringAt :: Pos -> String -> EP ()
printStringAt p str = printWhitespace p >> printString str
