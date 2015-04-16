module Consistency where

import Data.Data
import GHC
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Generics (everything, mkQ)

import Language.Haskell.GHC.ExactPrint.Utils (isPointSrcSpan)

import Debug.Trace

checkConsistency :: Data a => GHC.ApiAnns -> a -> [(SrcSpan, (AnnKeywordId, [SrcSpan]))]
checkConsistency anns ast =
  let srcspans = Set.fromList $ getAllSrcSpans ast
      cons (s, (_, vs)) = Set.member s srcspans || (all (isPointSrcSpan) vs)
  in filter (\s -> not (cons s)) (getAnnSrcSpans anns)

getAnnSrcSpans :: ApiAnns -> [(SrcSpan,(AnnKeywordId,[SrcSpan]))]
getAnnSrcSpans (anns,_) = map (\((ss,k),v) -> (ss,(k,v))) $ Map.toList anns

getAllSrcSpans :: (Data t) => t -> [SrcSpan]
getAllSrcSpans ast = everything (++) ([] `mkQ` getSrcSpan) ast
  where
    getSrcSpan :: SrcSpan -> [SrcSpan]
    getSrcSpan ss = [ss]

