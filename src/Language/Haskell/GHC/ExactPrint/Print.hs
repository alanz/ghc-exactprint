{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.GHC.ExactPrint
--
-- Based on Language.Haskell.Exts.Annotated.ExactPrint
--
-----------------------------------------------------------------------------
module Language.Haskell.GHC.ExactPrint.Print
        ( annotateAST
        , Anns
        , exactPrintAnnotated
        , exactPrintAnnotation

        , exactPrint

        ) where

import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Utils
import Language.Haskell.GHC.ExactPrint.Common
import Language.Haskell.GHC.ExactPrint.Lookup
import Language.Haskell.GHC.ExactPrint.Annotate

import Control.Applicative
import Control.Monad.RWS
import Data.Data
import Data.List
import Data.Maybe

import Control.Monad.Trans.Free

import qualified GHC           as GHC

import qualified Data.Map as Map

------------------------------------------------------
-- The EP monad and basic combinators

-- The (ColOffset,ColOffset) value carries the normal and current
-- column offset. The second one captures the difference between the
-- original col when the DP was captured and the current one.

data EPState = EPState
             { epPos       :: Pos -- ^ Current output position
             , epAnns      :: Anns
             , epAnnKds    :: [[(KeywordId, DeltaPos)]] -- MP: Could this be moved to the local state with suitable refactoring?
                                                        -- AZ, it is already in the last element of Annotation, for withOffset
             }

data EPLocal = EPLocal
             { epStack     :: (ColOffset,ColDelta) -- ^ stack of offsets that currently apply
             , epSrcSpan  :: GHC.SrcSpan
             }

type EP a = RWS EPLocal (Endo String) EPState a

runEP :: Wrapped () -> GHC.SrcSpan -> Anns -> String
runEP action ss ans =
  flip appEndo "" . snd
  . (\next -> execRWS next (initialEPStack ss) (defaultEPState ans))
  . printInterpret $ action


printInterpret :: Wrapped a -> EP a
printInterpret = iterTM go
  where
    go :: AnnotationF (AnnKey, Annotation) (EP a) -> EP a
    go (Output _ next) = next
    go (AddEofAnnotation next) = printStringAtMaybeAnn (G GHC.AnnEofPos) "" >> next
    go (AddDeltaAnnotation kwid next) =
      justOne kwid  >> next
    go (AddDeltaAnnotationsOutside _ kwid next) =
      printStringAtMaybeAnnAll kwid ";"  >> next
    go (AddDeltaAnnotationsInside akwid next) =
      allAnns akwid >> next
    go (AddDeltaAnnotations akwid next) =
      allAnns akwid >> next
    go (AddDeltaAnnotationLs akwid _ next) =
      justOne akwid >> next
    go (AddDeltaAnnotationAfter akwid next) =
      justOne akwid >> next
    go (AddDeltaAnnotationExt _ akwid next) =
      justOne akwid >> next
    go (WithAST lss _ action next) =
      exactPC lss (printInterpret action) >>= next
    go (OutputKD _ next) =
      next
    go (CountAnnsAP kwid next) =
      countAnnsEP (G kwid) >>= next
    go (SetLayoutFlag next) =
      next
    go (PrintAnnString akwid s next) = printStringAtMaybeAnn (G akwid) s >> next
    go (PrintAnnStringExt _ akwid s next) = printStringAtMaybeAnn (G akwid) s >> next
    go (PrintAnnStringLs akwid s _ next) = printStringAtMaybeAnn (G akwid) s >> next

justOne, allAnns :: GHC.AnnKeywordId -> EP ()
justOne kwid = printStringAtMaybeAnn (G kwid) (keywordToString kwid)
allAnns kwid = printStringAtMaybeAnnAll (G kwid) (keywordToString kwid)


defaultEPState :: Anns -> EPState
defaultEPState as = EPState
             { epPos    = (1,1)
             , epAnns   = as
             , epAnnKds = []
             }

initialEPStack :: GHC.SrcSpan -> EPLocal
initialEPStack ss = EPLocal
             { epStack     = (0,0)
             , epSrcSpan   = ss
             }

getPos :: EP Pos
getPos = gets epPos

setPos :: Pos -> EP ()
setPos l = modify (\s -> s {epPos = l})

-- ---------------------------------------------------------------------

-- | Given an annotation associated with a specific SrcSpan, determines a new offset relative to the previous
-- offset
--
withOffset :: Annotation -> (EP a -> EP a)
withOffset a@(Ann (DP (edLine, edColumn)) newline originalStartCol annDelta _) k = do
  -- The colOffset is the offset to be used when going to the next line.
  -- The colIndent is how far the current code block has been indented.
  (colOffset, colIndent) <- asks epStack
  (_l, currentColumn) <- getPos
  let
      -- Add extra indentation for this SrcSpan
     colOffset' = annDelta + colOffset
     -- Work out where new column starts, based on the entry delta
     newStartColumn = if edLine == 0
                          then edColumn + currentColumn -- same line, use entry delta
                          else colOffset -- different line, use current offset
     newColIndent        = newStartColumn - originalStartCol

     offsetValNewIndent  = (colOffset' + (newColIndent - colIndent), newColIndent)
     offsetValSameIndent = (colOffset'            ,                  colIndent)
     offsetValNewline    = (annDelta   + colIndent,                  colIndent)

     newOffset =
        case newline of
          -- For use by AST modifiers, to preserve the indentation
          -- level for the next line after an AST modification
          KeepOffset  -> offsetValNewline

          -- Generated during the annotation phase
          LineChanged       -> offsetValNewline
          LayoutLineChanged -> offsetValNewline

          LineSame          -> offsetValSameIndent
          LayoutLineSame    -> offsetValNewIndent
  local (\s -> s {epStack = newOffset }) k
    `debug` ("pushOffset:(a, colOffset, colIndent, currentColumn, newOffset)="
                 ++ show (a, colOffset, colIndent, currentColumn, newOffset))

-- |Get the current column offset
getOffset :: EP ColOffset
getOffset = asks (fst . epStack)

-- ---------------------------------------------------------------------

withSrcSpan :: GHC.SrcSpan -> (EP a -> EP a)
withSrcSpan ss = local (\s -> s {epSrcSpan = ss})

getAndRemoveAnnotation :: (Data a) => GHC.Located a -> EP (Maybe Annotation)
getAndRemoveAnnotation a = do
  (r, an') <- gets (getAndRemoveAnnotationEP a . epAnns)
  modify (\s -> s { epAnns = an' })
  return r

withKds :: [(KeywordId, DeltaPos)] -> EP a -> EP a
withKds kd action = do
  modify (\s -> s { epAnnKds = kd : (epAnnKds s) })
  r <- action
  modify (\s -> s { epAnnKds = tail (epAnnKds s) })
  return r

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

-- |destructive get, hence use an annotation once only
getAnnFinal :: KeywordId -> EP ([DComment], Maybe DeltaPos)
getAnnFinal kw = do
  kd <- gets epAnnKds
  let (r, kd', dcs) = case kd of
                  []    -> (Nothing ,[], [])
                  (k:kds) -> (r',kk:kds, dcs')
                    where (cs', r',kk) = destructiveGetFirst kw ([],k)
                          dcs' = concatMap keywordIdToDComment cs'
  modify (\s -> s { epAnnKds = kd' })
  return (dcs, r)

-- ---------------------------------------------------------------------

getStoredListSrcSpan :: EP GHC.SrcSpan
getStoredListSrcSpan = do
  kd <- gets epAnnKds
  let
    isAnnList ((AnnList _),_) = True
    isAnnList _               = False

    kdf = ghead "getStoredListSrcSpan.1" kd
    (AnnList ss,_) = ghead "getStoredListSrcSpan.2" $ filter isAnnList kdf
  return ss

-- ---------------------------------------------------------------------

keywordIdToDComment :: (KeywordId, DeltaPos) -> [DComment]
keywordIdToDComment (AnnComment comment,_dp) = [comment]
keywordIdToDComment _                   = []

-- |non-destructive get
peekAnnFinal :: KeywordId -> EP (Maybe DeltaPos)
peekAnnFinal kw = do
  (_, r, _) <- (\kd -> destructiveGetFirst kw ([], kd)) <$> gets (head . epAnnKds)
  return r

-- ---------------------------------------------------------------------

printString :: String -> EP ()
printString str = do
  (l,c) <- gets epPos
  setPos (l, c + length str)
  tell (Endo $ showString str)

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

printStringAtMaybeAnn :: KeywordId -> String -> EP ()
printStringAtMaybeAnn an str = do
  (comments, ma) <- getAnnFinal an
  printStringAtLsDelta comments (maybeToList ma) str
    `debug` ("printStringAtMaybeAnn:(an,ma,str)=" ++ show (an,ma,str))

printStringAtMaybeAnnAll :: KeywordId -> String -> EP ()
printStringAtMaybeAnnAll an str = go
  where
    go = do
      (comments, ma) <- getAnnFinal an
      case ma of
        Nothing -> return ()
        Just d  -> printStringAtLsDelta comments [d] str >> go

-- ---------------------------------------------------------------------

countAnnsEP :: KeywordId -> EP Int
countAnnsEP an = do
  ma <- peekAnnFinal an
  return (length ma)

------------------------------------------------------------------------------
-- Printing of source elements

-- | Print an AST exactly as specified by the annotations on the nodes in the tree.
-- exactPrint :: (ExactP ast) => ast -> [Comment] -> String
exactPrint :: (AnnotateGen ast) => GHC.Located ast -> String
exactPrint ast@(GHC.L l _) = runEP (annotatePC ast) l Map.empty


exactPrintAnnotated ::
     GHC.Located (GHC.HsModule GHC.RdrName) -> GHC.ApiAnns -> String
exactPrintAnnotated ast@(GHC.L l _) ghcAnns = runEP (annotatePC ast) l an
  where
    an = annotateLHsModule ast ghcAnns

exactPrintAnnotation :: AnnotateGen ast =>
  GHC.Located ast -> Anns -> String
exactPrintAnnotation ast@(GHC.L l _) an = runEP (annotatePC ast) l an
  -- `debug` ("exactPrintAnnotation:an=" ++ (concatMap (\(l,a) -> show (ss2span l,a)) $ Map.toList an ))



-- ---------------------------------------------------------------------

-- |First move to the given location, then call exactP
exactPC :: Data ast => GHC.Located ast -> EP a -> EP a
exactPC a@(GHC.L l _ast) action =
    do return () `debug` ("exactPC entered for:" ++ showGhc l)
       ma <- getAndRemoveAnnotation a
       let an@(Ann _edp _nl _sc _dc kds) = fromMaybe annNone ma
       withContext kds l an action

withContext :: [(KeywordId, DeltaPos)]
            -> GHC.SrcSpan
            -> Annotation
            -> EP a -> EP a
withContext kds l an = withKds kds . withSrcSpan l . withOffset an

-- ---------------------------------------------------------------------


