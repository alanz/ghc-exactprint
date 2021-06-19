{-# LANGUAGE CPP #-}
module Test.Consistency where

import Data.Data
import GHC
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Generics (everything, mkQ)

import Language.Haskell.GHC.ExactPrint.Types (AnnSpan)
import Language.Haskell.GHC.ExactPrint.Utils (isPointSrcSpan, rs)

-- import Debug.Trace

checkConsistency :: Data a => GHC.ApiAnns -> a -> [(AnnSpan, (AnnKeywordId, [AnnSpan]))]
checkConsistency anns ast =
  let srcspans = Set.fromList $ map rs $ getAllSrcSpans ast
      cons (s, (_, vs)) = Set.member s srcspans || (all (isPointSrcSpan) vs)
  in filter (\s -> not (cons s)) (getAnnSrcSpans anns)

getAnnSrcSpans :: ApiAnns -> [(AnnSpan,(AnnKeywordId,[AnnSpan]))]
#if __GLASGOW_HASKELL__ >= 808
getAnnSrcSpans anns = map (\((ss,k),v) -> (ss,(k,v))) $ Map.toList (GHC.apiAnnItems anns)
#else
getAnnSrcSpans (anns,_) = map (\((ss,k),v) -> (ss,(k,v))) $ Map.toList anns
#endif

getAllSrcSpans :: (Data t) => t -> [SrcSpan]
getAllSrcSpans ast = everything (++) ([] `mkQ` getSrcSpan) ast
  where
    getSrcSpan :: SrcSpan -> [SrcSpan]
    getSrcSpan ss = [ss]
