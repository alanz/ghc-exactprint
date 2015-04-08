{-# LANGUAGE GADTs #-}
module GADTRecords (H1(..)) where

-- | h1
data H1 a b where
  C1 :: H1 a b
  C2 :: Ord a => [a] -> H1 a a
  C3 :: { field :: Int -- ^ hello docs
        } -> H1 Int Int
  C4 :: { field2 :: a -- ^ hello2 docs
        } -> H1 Int a

  FwdDataflowAnalysis :: (Eq f, Monad m) => { analysisTop :: f
                                            , analysisMeet :: f -> f -> f
                                            , analysisTransfer :: f -> Instruction -> m f
                                            , analysisFwdEdgeTransfer :: Maybe (f -> Instruction -> m [(BasicBlock, f)])
                                            } -> DataflowAnalysis m f
