{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE RecursiveDo #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.GHC.ExactPrint
--
-- Based on Language.Haskell.Exts.Annotated.ExactPrint
--
-----------------------------------------------------------------------------
module Language.Haskell.GHC.ExactPrint.Print
        (
          Anns
        , exactPrintWithAnns

        , exactPrint

        ) where

import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Utils ( debug, undelta, isGoodDelta, showGhc, )
import Language.Haskell.GHC.ExactPrint.Annotate
  (AnnotationF(..), Annotated, Annotate(..), markLocated)
import Language.Haskell.GHC.ExactPrint.Lookup (keywordToString)
import Language.Haskell.GHC.ExactPrint.Delta ( relativiseApiAnns )

import Control.Applicative
import Control.Monad.RWS
import Data.Data
import Data.List
import Data.Maybe (mapMaybe, fromMaybe, maybeToList)

import Control.Monad.Trans.Free

import qualified GHC           as GHC

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
exactPrintWithAnns ast an = runEP (markLocated ast) an


------------------------------------------------------
-- The EP monad and basic combinators

data EPState = EPState
             { epPos       :: Pos -- ^ Current output position
             , epAnns      :: Anns
             , epAnnKds    :: [[(KeywordId, DeltaPos)]] -- MP: Could this be moved to the local state with suitable refactoring?
                                                        -- AZ, it is already in the last element of Annotation, for withOffset
             }

data EPStack = EPStack
             {  epLHS      :: Int -- ^ Marks the column of the LHS of the i
                                  --   current layout block
             }

defaultEPState :: Anns -> EPState
defaultEPState as = EPState
             { epPos    = (1,1)
             , epAnns   = as
             , epAnnKds = []
             }

initialEPStack :: EPStack
initialEPStack  = EPStack
             { epLHS = 0
             }

data EPWriter = EPWriter
              { output :: Endo String }

instance Monoid EPWriter where
  mempty = EPWriter mempty
  (EPWriter a) `mappend` (EPWriter c) = EPWriter (a <> c)

---------------------------------------------------------

type EP a = RWS EPStack EPWriter EPState a

runEP :: Annotated () -> Anns -> String
runEP action ans =
  flip appEndo "" . output . snd
  . (\next -> execRWS next initialEPStack (defaultEPState ans))
  . printInterpret $ action




printInterpret :: Annotated a -> EP a
printInterpret = iterTM go
  where
    go :: AnnotationF (EP a) -> EP a
    go (MarkEOF next) =
      printStringAtMaybeAnn (G GHC.AnnEofPos) "" >> next
    go (MarkPrim kwid mstr next) =
      let annString = fromMaybe (keywordToString kwid) mstr in
        printStringAtMaybeAnn (G kwid) annString >> next
    go (MarkOutside _ kwid next) =
      printStringAtMaybeAnnAll kwid ";"  >> next
    go (MarkInside akwid next) =
      allAnns akwid >> next
    go (MarkMany akwid next) =
      allAnns akwid >> next
    go (MarkOffsetPrim kwid _ mstr next) =
      let annString = fromMaybe (keywordToString kwid) mstr in
        printStringAtMaybeAnn (G kwid) annString >> next
    go (MarkAfter akwid next) =
      justOne akwid >> next
    go (WithAST lss flag action next) =
      exactPC lss flag (NoLayoutRules <$ (printInterpret action)) >> next
    go (OutputKD _ next) =
      next
    go (CountAnns kwid next) =
      countAnnsEP (G kwid) >>= next
    go (SetLayoutFlag kwid action next) =
      setLayout kwid (printInterpret action) >> next
    go (MarkExternal _ akwid s next) =
      printStringAtMaybeAnn (G akwid) s >> next

justOne, allAnns :: GHC.AnnKeywordId -> EP ()
justOne kwid = printStringAtMaybeAnn (G kwid) (keywordToString kwid)
allAnns kwid = printStringAtMaybeAnnAll (G kwid) (keywordToString kwid)

-------------------------------------------------------------------------
-- |First move to the given location, then call exactP
exactPC :: Data ast => GHC.Located ast -> LayoutFlag -> EP LayoutFlag -> EP LayoutFlag
exactPC ast flag action =
    do return () `debug` ("exactPC entered for:" ++ showGhc (GHC.getLoc ast))
       ma <- getAndRemoveAnnotation ast
       let an@(Ann _ _ kds) = fromMaybe annNone ma
       withContext kds an flag action

getAndRemoveAnnotation :: (Data a) => GHC.Located a -> EP (Maybe Annotation)
getAndRemoveAnnotation a = do
  (r, an') <- gets (getAndRemoveAnnotationEP a . epAnns)
  modify (\s -> s { epAnns = an' })
  return r

withContext :: [(KeywordId, DeltaPos)]
            -> Annotation
            -> LayoutFlag
            -> EP LayoutFlag -> EP LayoutFlag
withContext kds an flag = withKds kds . withOffset an flag

-- ---------------------------------------------------------------------
--
-- | Given an annotation associated with a specific SrcSpan, determines a new offset relative to the previous
-- offset
--
withOffset :: Annotation -> LayoutFlag -> (EP LayoutFlag -> EP LayoutFlag)
withOffset (Ann (DP (edLine, edColumn)) annOffset  _) flag k = do
  oldOffset <-  asks epLHS -- Shift from left hand column
  (_l, currentColumn) <- getPos
  rec
    -- Calculate the new offset
    -- 1. If the LayoutRules flag is set then we need to mark this position
    -- as the start of a new layout block.
    -- There are two cases (1) If we are on the same line  and (2) if we
    -- move to a new line.
    -- (1) The start of the layout block is the current position added to
    -- the delta
    -- (2) The start of the layout block is the old offset added to the
    -- "annOffset" (i.e., how far this annotation was from the edge)
    let offset = case (flag <> f) of
                       LayoutRules ->
                        if edLine == 0
                          then currentColumn + edColumn
                          else oldOffset + annOffset
                       NoLayoutRules -> oldOffset
    f <-  (local (\s -> s { epLHS = offset }) k)
  return f


-- ---------------------------------------------------------------------
--
-- Necessary as there are destructive gets of Kds across scopes
withKds :: [(KeywordId, DeltaPos)] -> EP a -> EP a
withKds kd action = do
  modify (\s -> s { epAnnKds = kd : (epAnnKds s) })
  r <- action
  modify (\s -> s { epAnnKds = tail (epAnnKds s) })
  return r

------------------------------------------------------------------------

setLayout :: GHC.AnnKeywordId -> EP () -> EP LayoutFlag
setLayout akiwd k = do
  p <- gets epPos
  local (\s -> s { epLHS = snd p - (length (keywordToString akiwd))})
                  (LayoutRules <$ k)

getPos :: EP Pos
getPos = gets epPos

setPos :: Pos -> EP ()
setPos l = modify (\s -> s {epPos = l})

-- |Get the current column offset
getOffset :: EP ColOffset
getOffset = asks epLHS

-- ---------------------------------------------------------------------

printStringAtMaybeAnn :: KeywordId -> String -> EP ()
printStringAtMaybeAnn an str = do
  (comments, ma) <- getAnnFinal an
  printStringAtLsDelta comments (maybeToList ma) str
    -- ++AZ++: Enabling the following line causes a very weird error associated with AnnPackageName. I suspect it is because it is forcing the evaluation of a non-existent an or str
    -- `debug` ("printStringAtMaybeAnn:(an,ma,str)=" ++ show (an,ma,str))

printStringAtMaybeAnnAll :: KeywordId -> String -> EP ()
printStringAtMaybeAnnAll an str = go
  where
    go = do
      (comments, ma) <- getAnnFinal an
      case ma of
        Nothing -> return ()
        Just d  -> printStringAtLsDelta comments [d] str >> go

-- ---------------------------------------------------------------------


-- |destructive get, hence use an annotation once only
getAnnFinal :: KeywordId -> EP ([DComment], Maybe DeltaPos)
getAnnFinal kw = do
  kd <- gets epAnnKds
  let (r, kd', dcs) = case kd of
                  []    -> (Nothing ,[], [])
                  (k:kds) -> (r',kk:kds, dcs')
                    where (cs', r',kk) = destructiveGetFirst kw ([],k)
                          dcs' = mapMaybe keywordIdToDComment cs'
  modify (\s -> s { epAnnKds = kd' })
  return (dcs, r)

keywordIdToDComment :: (KeywordId, DeltaPos) -> Maybe DComment
keywordIdToDComment (AnnComment comment,_dp) = Just comment
keywordIdToDComment _                   = Nothing

-- | Get and remove the first item in the (k,v) list for which the k matches.
-- Return the value, together with any comments skipped over to get there.
destructiveGetFirst :: KeywordId -> ([(KeywordId,v)],[(KeywordId,v)])
                    -> ([(KeywordId,v)], Maybe v,[(KeywordId,v)])
destructiveGetFirst _key (acc,[]) = ([], Nothing ,acc)
destructiveGetFirst  key (acc,((k,v):kvs))
  | k == key = let (cs,others) = commentsAndOthers acc in (cs, Just v ,others++kvs)
  | otherwise = destructiveGetFirst key (acc++[(k,v)],kvs)
  where
    commentsAndOthers kvs' = partition isComment kvs'
    isComment ((AnnComment _),_) = True
    isComment _              = False

-- ---------------------------------------------------------------------

-- |This should be the final point where things are mode concrete,
-- before output. Hence the point where comments can be inserted
printStringAtLsDelta :: [DComment] -> [DeltaPos] -> String -> EP ()
printStringAtLsDelta cs mc s =
  case reverse mc of
    (cl:_) -> do
      p <- getPos
      colOffset <- getOffset
      if isGoodDeltaWithOffset cl colOffset
        then do
          mapM_ printQueuedComment cs
          printStringAt (undelta p cl colOffset) s
            `debug` ("printStringAtLsDelta:(pos,s):" ++ show (undelta p cl colOffset,s))
        else return () `debug` ("printStringAtLsDelta:bad delta for (mc,s):" ++ show (mc,s))
    _ -> return ()


isGoodDeltaWithOffset :: DeltaPos -> Int -> Bool
isGoodDeltaWithOffset dp colOffset = isGoodDelta (DP (undelta (0,0) dp colOffset))

-- AZ:TODO: harvest the commonality between this and printStringAtLsDelta
printQueuedComment :: DComment -> EP ()
printQueuedComment (DComment (dp,de) s) = do
  p <- getPos
  colOffset <- getOffset
  let (dr,dc) = undelta (0,0) dp colOffset
  if isGoodDelta (DP (dr,max 0 dc)) -- do not lose comments against the left margin
    then do
      printStringAt (undelta p dp colOffset) s
         `debug` ("printQueuedComment:(pos,s):" ++ show (undelta p dp colOffset,s))
      setPos (undelta p de colOffset)
    else return () `debug` ("printQueuedComment::bad delta for (dp,s):" ++ show (dp,s))

-- ---------------------------------------------------------------------

-- |non-destructive get
peekAnnFinal :: KeywordId -> EP (Maybe DeltaPos)
peekAnnFinal kw = do
  (_, r, _) <- (\kd -> destructiveGetFirst kw ([], kd)) <$> gets (head . epAnnKds)
  return r

countAnnsEP :: KeywordId -> EP Int
countAnnsEP an = length <$> peekAnnFinal an

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
    case  {- trace (show ((l,c), (l1,c1))) -} () of
     _ {-()-} | l1 >= l && c1 <= c -> printString $ replicate (c - c1) ' '
              | l1 < l             -> newLine >> padUntil (l,c)
              | otherwise          -> return ()

printWhitespace :: Pos -> EP ()
printWhitespace p = do
  -- mPrintComments p >> padUntil p
  padUntil p

printStringAt :: Pos -> String -> EP ()
printStringAt p str = printWhitespace p >> printString str
