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

-- |
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

