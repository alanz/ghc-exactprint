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
import Language.Haskell.GHC.ExactPrint.Delta ( relativiseApiAnns )

import Control.Monad.RWS
import Data.Data (Data)
import Data.List (sortBy, elemIndex)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe)

import Control.Monad.Trans.Free

import Debug.Trace
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
exactPrint = semanticPrint (\ast b -> b) id id


-- | A more general version of 'exactPrint' which allows the customisation
-- of the output whilst retaining the original source formatting. This is
-- useful for smarter syntax highlighting.
semanticPrint :: (Annotate ast, Monoid b) =>
              (forall a . Data a => GHC.Located a -> b -> b) -- ^ How to surround an AST fragment
              -> (String -> b) -- ^ How to output a token
              -> (String -> b) -- ^ How to output whitespace
              -> GHC.Located ast
              -> Anns
              -> b
semanticPrint astOut tokenOut whiteOut ast as =  runEP astOut tokenOut whiteOut (annotate ast) as


------------------------------------------------------
-- The EP monad and basic combinators

data EPReader a = EPReader
            {
              epAnn :: !Annotation
            , epAstPrint :: forall ast . Data ast => GHC.Located ast -> a -> a
            , epTokenPrint :: String -> a
            , epWhitespacePrint :: String -> a
            }

data EPWriter a = EPWriter
              { output :: !a }

instance Monoid w => Monoid (EPWriter w) where
  mempty = EPWriter mempty
  (EPWriter a) `mappend` (EPWriter b) = EPWriter (a <> b)

data EPState = EPState
             { epPos    :: !Pos -- ^ Current output position
             , epAnns   :: !Anns
             , epAnnKds :: ![[(KeywordId, DeltaPos)]] -- MP: Could this be moved to the local state with suitable refactoring?
             , epMarkLayout :: Bool
             , epLHS :: LayoutStartCol
             }

---------------------------------------------------------

type EP w a = RWS (EPReader w) (EPWriter w) EPState a



runEP :: Monoid a =>
      (forall ast . Data ast => GHC.Located ast -> a -> a)
      -> (String -> a)
      -> (String -> a)
      -> Annotated () -> Anns -> a
runEP astPrint wsPrint tokenPrint action ans =
  output . snd
  . (\next -> execRWS next (initialEPReader astPrint tokenPrint wsPrint) (defaultEPState ans))
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
      (forall ast . Data ast => GHC.Located ast -> a -> a)
      -> (String -> a)
      -> (String -> a)
      -> EPReader a
initialEPReader astPrint tokenPrint wsPrint  = EPReader
             {
               epAnn = annNone
             , epAstPrint = astPrint
             , epWhitespacePrint = wsPrint
             , epTokenPrint = tokenPrint
             }

-- ---------------------------------------------------------------------

printInterpret :: Monoid w => Annotated a -> EP w a
printInterpret = iterTM go
  where
    go :: Monoid w => AnnotationF (EP w a) -> EP w a
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

storeOriginalSrcSpanPrint :: Monoid w => EP w AnnKey
storeOriginalSrcSpanPrint = do
  Ann{..} <- asks epAnn
  case annCapturedSpan of
    Nothing -> error "Missing captured SrcSpan"
    Just v  -> return v

printStoredString :: Monoid w => EP w ()
printStoredString = do
  kd <- gets epAnnKds

  let
    isAnnString (AnnString _,_) = True
    isAnnString _             = False

  case filter isAnnString (ghead "printStoredString" kd) of
    ((AnnString ss,_):_) -> printStringAtMaybeAnn (AnnString ss) ss
    _                    -> return ()

withSortKey :: Monoid w => [(GHC.SrcSpan, Annotated ())] -> EP w ()
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

allAnns :: Monoid w => GHC.AnnKeywordId -> EP w ()
allAnns kwid = printStringAtMaybeAnnAll (G kwid) (keywordToString (G kwid))

-------------------------------------------------------------------------
-- |First move to the given location, then call exactP
exactPC :: (Data ast, Monoid w) => GHC.Located ast -> EP w a -> EP w a
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
       >> censor (\(EPWriter w) -> EPWriter (epAstPrint ast w)) action
       <* mapM_ (uncurry printQueuedComment) fcomments)
      return r `debug` ("leaving exactPCfor:" ++ show (mkAnnKey ast))

advance :: Monoid w => DeltaPos -> EP w ()
advance cl = do
  p <- getPos
  colOffset <- getLayoutOffset
  printWhitespace (undelta p cl colOffset)

getAndRemoveAnnotation :: (Monoid w, Data a) => GHC.Located a -> EP w (Maybe Annotation)
getAndRemoveAnnotation a = gets ((getAnnotationEP a) . epAnns)

markPrim :: Monoid w => KeywordId -> Maybe String -> EP w ()
markPrim kwid mstr =
  let annString = fromMaybe (keywordToString kwid) mstr
  in printStringAtMaybeAnn kwid annString

withContext :: Monoid w
            => [(KeywordId, DeltaPos)]
            -> Annotation
            -> EP w a -> EP w a
withContext kds an x = withKds kds (withOffset an x)

-- ---------------------------------------------------------------------
--
-- | Given an annotation associated with a specific SrcSpan, determines a new offset relative to the previous
-- offset
--
withOffset :: Monoid w => Annotation -> (EP w a -> EP w a)
withOffset a =
  local (\s -> s { epAnn = a })


-- ---------------------------------------------------------------------
--
-- Necessary as there are destructive gets of Kds across scopes
withKds :: Monoid w => [(KeywordId, DeltaPos)] -> EP w a -> EP w a
withKds kd action = do
  modify (\s -> s { epAnnKds = kd : epAnnKds s })
  r <- action
  modify (\s -> s { epAnnKds = tail (epAnnKds s) })
  return r

------------------------------------------------------------------------

setLayout :: Monoid w => EP w () -> EP w ()
setLayout k = do
  oldLHS <- gets epLHS
  modify (\a -> a { epMarkLayout = True } )
  let reset = modify (\a -> a { epMarkLayout = False
                              , epLHS = oldLHS } )
  k <* reset

getPos :: Monoid w => EP w Pos
getPos = gets epPos

setPos :: Monoid w => Pos -> EP w ()
setPos l = modify (\s -> s {epPos = l})

-- |Get the current column offset
getLayoutOffset :: Monoid w => EP w LayoutStartCol
getLayoutOffset = gets epLHS

-- ---------------------------------------------------------------------

printStringAtMaybeAnn :: Monoid w => KeywordId -> String -> EP w ()
printStringAtMaybeAnn an str = printStringAtMaybeAnnThen an str (return ())

printStringAtMaybeAnnAll :: Monoid w => KeywordId -> String -> EP w ()
printStringAtMaybeAnnAll an str = go
  where
    go = printStringAtMaybeAnnThen an str go

printStringAtMaybeAnnThen :: Monoid w => KeywordId -> String -> EP w () -> EP w ()
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
getAnnFinal :: Monoid w => KeywordId -> EP w (Maybe ([(Comment, DeltaPos)], DeltaPos))
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
printStringAtLsDelta :: Monoid w => [(Comment, DeltaPos)] -> DeltaPos -> String -> EP w ()
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
printQueuedComment :: Monoid w => Comment -> DeltaPos -> EP w ()
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
peekAnnFinal :: Monoid w => KeywordId -> EP w (Maybe DeltaPos)
peekAnnFinal kw = do
  (r, _) <- (\kd -> destructiveGetFirst kw ([], kd)) <$> gets (ghead "peekAnnFinal" . epAnnKds)
  return (snd <$> r)

countAnnsEP :: Monoid w => KeywordId -> EP w Int
countAnnsEP an = length <$> peekAnnFinal an

-- ---------------------------------------------------------------------


-- ---------------------------------------------------------------------
-- Printing functions

printString :: Monoid w => Bool -> String -> EP w ()
printString layout str = do
  EPState{epPos = (l,c), epMarkLayout} <- get
  EPReader{epTokenPrint, epWhitespacePrint} <- ask
  when (epMarkLayout && layout) (
                      modify (\s -> s { epLHS = LayoutStartCol c, epMarkLayout = False } ))
  setPos (l, c + length str)
  --
  -- tell (mempty {output = Endo $ showString str })

  if not layout && c == 0
    then tell (EPWriter { output = epWhitespacePrint str })
    else tell (EPWriter { output = epTokenPrint str } )


newLine :: Monoid w => EP w ()
newLine = do
    (l,_) <- getPos
    printString False "\n"
    setPos (l+1,1)

padUntil :: Monoid w => Pos -> EP w ()
padUntil (l,c) = do
    (l1,c1) <- getPos
    if | l1 == l && c1 <= c -> printString False $ replicate (c - c1) ' '
       | l1 < l             -> newLine >> padUntil (l,c)
       | otherwise          -> return ()

printWhitespace :: Monoid w => Pos -> EP w ()
printWhitespace = padUntil

printCommentAt :: Monoid w => Pos -> String -> EP w ()
printCommentAt p str = printWhitespace p >> printString False str

printStringAt :: Monoid w => Pos -> String -> EP w ()
printStringAt p str = printWhitespace p >> printString True str
