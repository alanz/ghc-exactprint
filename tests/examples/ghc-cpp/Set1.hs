{-# LANGUAGE GHC_CPP #-}
module ADP.Fusion.SynVar.Indices.Set1 where

instance
  ( IndexHdr s x0 i0 us (BS1 k I) cs c is (BS1 k I)
  ) => AddIndexDense s (us:.BS1 k I) (cs:.c) (is:.BS1 k I) where
  addIndexDenseGo (cs:.c) (vs:.IVariable rb) (lbs:._) (ubs:._) (us:.BS1 uSet uBnd) (is:.BS1 set bnd)
    = flatten mk step . addIndexDenseGo cs vs lbs ubs us is . assert (rb==1) -- only works with one element
    where mk (SvS s t y') =
            let
            in
#if ADPFUSION_DEBUGOUTPUT
                traceShow (set,bnd,rb) $
#endif
                return (SvS s t y', Just $ set `clearBit` getBoundary bnd)
          step (_, Nothing) = return Done

