{-# LANGUAGE RecordWildCards #-}
module Language.Haskell.GHC.ExactPrint.Comments (balanceComments) where

import qualified SrcLoc as GHC
import SrcLoc (SrcSpan)
import Data.Data
import Language.Haskell.GHC.ExactPrint.Utils
import Language.Haskell.GHC.ExactPrint.Types
import Control.Monad.State
import qualified Data.Generics as SYB
import qualified Data.Map as Map

-- | We process all comments individually.
-- If the comment trails a node we associate it with the innermost,
-- rightmost node which ends before it on the same line.
--
-- If the comment is on a new line then we associate it with the
-- immediately following node.
--
balanceComments :: Data ast => GHC.Located ast -> [Comment] -> Anns -> Anns
balanceComments ast cs as = foldr processComment as cs

  where processComment :: Comment
                       -> Anns
                       -> Anns
        -- Add in a single comment to the ast.
        processComment c@(Comment _ cspan _) as  =
          -- Try to find the node after which this comment lies.
          case execState (collect After c ast) Nothing of
            -- When no node is found, the comment is on its own line.
            Nothing -> undefined

            -- We found the node that this comment follows.
            -- Check whether the node is on the same line.
            Just k@(AnnKey l _)
              -- If it's on a different line than the node, but the node has an
              -- EOL comment, and the EOL comment and this comment are aligned,
              -- attach this comment to the preceding node.
              | ownLine && alignedWithPrevious -> insertBefore k

              -- If it's on a different line than the node, look for the following node to attach it to.
              | ownLine ->
                  case execState (collect Before c ast) Nothing of
                    -- If we don't find a node after the comment, leave it with the previous node.
                    Nothing   -> insertBefore k
                    Just ak -> insertAfter k ak


              -- If it's on the same line, insert this comment into that node.
              | otherwise -> insertBefore k
              where
                ownLine = srcSpanStartLine cspan /= srcSpanEndLine l
                -- Quadratic
                insertBefore akey =
                  Map.adjust (\a -> a { annPriorComments =
                                          annPriorComments a ++ [(c, DP(0,0))]})
                             akey as
                insertAfter bkey akey =
                  Map.adjust (\a -> a { annFollowingComments =
                                          annFollowingComments a ++ [(c, DP(0,0))]})
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







collect :: (Data a,Typeable a) => ComInfoLocation -> Comment -> a -> State (Maybe AnnKey) a
collect loc' c ast =
  SYB.everywhereM (return `SYB.ext2M` collectOne) ast
  where
    collectOne :: (Data a, Data b) => GHC.GenLocated a b -> State (Maybe AnnKey) (GHC.GenLocated a b)
    collectOne l@(GHC.L ss body) =
      case cast ss of
        Nothing -> return l
        Just newL -> do
            let cn = gmapQi 1 (CN . show . toConstr) body
                ak = AnnKey newL cn
            when (commentLocated loc' newL c)
              (modify (maybe (Just ak)
                            (\oldak@(AnnKey oldL _) ->
                             Just (if (spanTest loc' oldL newL)
                                      then ak
                                      else oldak))))
            return l


-- | Is the comment after the node?
commentLocated :: ComInfoLocation -> SrcSpan -> Comment -> Bool
commentLocated loc' ss (Comment _ c _) =
  spanTest loc' ss c




data ComInfoLocation = Before | After

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

