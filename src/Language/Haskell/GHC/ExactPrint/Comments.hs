{-# LANGUAGE RecordWildCards, NamedFieldPuns #-}
module Language.Haskell.GHC.ExactPrint.Comments (balanceComments) where

import qualified SrcLoc as GHC
import SrcLoc (SrcSpan, isSubspanOf)
import Data.Data
import Language.Haskell.GHC.ExactPrint.Utils
import Language.Haskell.GHC.ExactPrint.Types
import Control.Monad.State
import qualified Data.Generics as SYB
import qualified Data.Map as Map
import Data.Maybe
import Debug.Trace

-- | We process all comments individually.
-- If the comment trails a node we associate it with the innermost,
-- rightmost node which ends before it on the same line.
--
-- If the comment is on a new line then we associate it with the
-- immediately following node.
--
balanceComments :: Data ast => GHC.Located ast -> Anns -> Anns
balanceComments ast as =
  execState (SYB.everywhereM (return `SYB.ext2M` reallocate) ast) as
  where
    reallocate :: (Data a, Data b) => GHC.GenLocated a b -> State Anns (GHC.GenLocated a b)
    reallocate l@(GHC.L ss body) =
      case cast ss of
        Nothing -> return l
        Just newL -> do
            let cn = CN . show . toConstr $ body
                ak = AnnKey newL cn
                Ann{..} = fromMaybe annNone (Map.lookup ak as)
            newPrior <- reallocateCs annPriorComments
            -- Alter as it may change during realloc
            modify (Map.adjust (\a -> a {annPriorComments = newPrior}) ak )
            return l

    reallocateCs :: [(Comment, DeltaPos)] -> State Anns [(Comment, DeltaPos)]
    reallocateCs cs = (++ toKeep) . catMaybes <$> mapM moveComment toMove
      where
        (toMove, toKeep) = span (\(c, DP (l,_)) -> l == 0 && isNothing (commentOrigin c)) cs


    moveComment :: (Comment, DeltaPos) -> State Anns (Maybe (Comment, DeltaPos))
    moveComment com@(c,_) =
      case execState (collect After c ast) Nothing of
        Nothing -> return (Just com)
        Just ak@(AnnKey ss _) ->
          if (fst $ ss2posEnd ss) == fst (ss2pos (commentIdentifier c))
              then do
                    modify (Map.adjust (\a -> a {annFollowingComments = annFollowingComments a ++ [com]}) ak)
                    return Nothing
              else return (Just com)



{-
  where processComment ::
                       Anns
                       -> Anns
        -- Add in a single comment to the ast.
        processComment c@(Comment cont cspan _) as  =
          -- Try to find the node after which this comment lies.
          case (traceShowId (execState (collect After c ast) Nothing)) of
            -- When no node is found, the comment is on its own line.
            Nothing -> undefined

            -- We found the node that this comment follows.
            -- Check whether the node is on the same line.
            Just k@(AnnKey l _)
              | traceShow (k, c) False -> undefined
              -- If it's on a different line than the node, but the node has an
              -- EOL comment, and the EOL comment and this comment are aligned,
              -- attach this comment to the preceding node.
--              | ownLine && alignedWithPrevious -> trace "ownLine, aligned" (insertAfter k)

              -- If it's on a different line than the node, look for the following node to attach it to.
              | ownLine ->
                  case traceShowId (execState (collect Before c ast) Nothing) of
                    -- If we don't find a node after the comment, leave it with the previous node.
                    Nothing   -> insertAfter k
                    Just ak -> insertBefore k ak


              -- If it's on the same line, insert this comment into that node.
              | otherwise -> insertAfter k
              where
                ownLine = srcSpanStartLine cspan /= srcSpanEndLine l
                -- Quadratic
                insertBefore (AnnKey ss _) akey =
                  let commentDelta = adjustDeltaForOffset (LayoutStartCol 1) $
                                      ss2delta (ss2posEnd ss) (commentIdentifier c)
                  in traceShow commentDelta (Map.adjust (\a -> a {  annEntryDelta = commentDelta `stepDP` (annEntryDelta a),
                                            annPriorComments =
                                              annPriorComments a ++ [(c, commentDelta)]})
                             akey as)
                insertAfter akey@(AnnKey ss _) =
                  let commentDelta = ss2delta (ss2posEnd ss) (commentIdentifier c)
                  in Map.adjust (\a -> a { annFollowingComments =
                                          annFollowingComments a ++ [(c, commentDelta)]})
                             akey as
                alignedWithPrevious =
                  case Map.lookup k as of
                    Nothing -> False
                    Just (Ann{..})
                      | null annFollowingComments -> False
                      | otherwise -> case fst $ last annFollowingComments of
                      -- Require single line comment after the node.
                        (Comment _ prevSpan _)  ->
                          srcSpanStartLine prevSpan == srcSpanStartLine cspan - 1 &&
                          srcSpanStartColumn prevSpan == srcSpanStartColumn cspan
                        _       -> False


-}

adjustDeltaForOffset :: LayoutStartCol -> DeltaPos -> DeltaPos
adjustDeltaForOffset _colOffset              dp@(DP (0,_)) = dp -- same line
adjustDeltaForOffset (LayoutStartCol colOffset) (DP (l,c)) = DP (l,c - colOffset)



collect :: (Data a,Typeable a) => ComInfoLocation -> Comment -> a -> State (Maybe AnnKey) a
collect loc' c ast =
  SYB.everywhereM (return `SYB.ext2M` collectOne) ast
  where
    collectOne :: (Data a, Data b) => GHC.GenLocated a b -> State (Maybe AnnKey) (GHC.GenLocated a b)
    collectOne l@(GHC.L ss body) =
      case cast ss of
        Nothing -> return l
        Just newL -> do
            let cn = CN . show . toConstr $ body
                ak = AnnKey newL cn
            when (commentLocated loc' newL c) (do
              (modify (maybe (Just ak)
                            (\oldak@(AnnKey oldL _) ->
                             Just (if (test loc' oldL newL)
                                      then  ak
                                      else  oldak)))))
            return l

test After = testBefore
test Before = testAfter

-- Locate with the previous biggest closest thing
testAfter old new =
                     (srcSpanStartLine new < srcSpanStartLine old) ||
                      (srcSpanStartLine new == srcSpanStartLine old &&
                       srcSpanStartColumn new < srcSpanStartColumn old) ||
                      (GHC.srcSpanStart new == GHC.srcSpanStart old &&
                        old `isSubspanOf` new)


testBefore old new =
                     (srcSpanEndLine new > srcSpanEndLine old) ||
                      (srcSpanEndLine new == srcSpanEndLine old &&
                       srcSpanEndColumn new > srcSpanEndColumn old) ||
                      (GHC.srcSpanEnd new == GHC.srcSpanEnd old &&
                        old `isSubspanOf` new)

-- | Is the comment after the node?
commentLocated :: ComInfoLocation -> SrcSpan -> Comment -> Bool
commentLocated loc' ss (Comment _ c _) =
  spanTest loc' ss c




data ComInfoLocation = Before | After deriving (Show)

-- | For @After@, does the first span end before the second starts?
-- For @Before@, does the first span start after the second ends?
spanTest :: ComInfoLocation -> SrcSpan -> SrcSpan -> Bool
spanTest loc' first second =
  (srcSpanStartLine after > srcSpanEndLine before) ||
  ((srcSpanStartLine after == srcSpanEndLine before) &&
   (srcSpanStartColumn after > srcSpanEndColumn before))
  where (before,after) =
          case loc' of
            After -> (first,second)
            Before -> (second,first)

