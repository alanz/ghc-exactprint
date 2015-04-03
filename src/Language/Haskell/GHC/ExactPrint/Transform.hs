-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.GHC.ExactPrint.Transform
--
-----------------------------------------------------------------------------
module Language.Haskell.GHC.ExactPrint.Transform
        (
          Transform
        , runTransform
        , logTr
        , getAnnsT, putAnnsT

        , uniqueSrcSpan
        , isUniqueSrcSpan

        , adjustAnnOffset
        , mergeAnns
        , setLocatedAnns
        ) where

import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Utils

import Control.Monad.RWS
import Data.List
import Data.Maybe

import qualified GHC
import qualified FastString    as GHC

import qualified Data.Generics as SYB

import qualified Data.Map as Map

------------------------------------------------------------------------------
-- Transformoation of source elements

-- | Monad type for updating the AST and managing the annotations at the same
-- time. The W state is used to generate logging information if required.
type Transform a = RWS () [String] (Anns,Int) a

runTransform :: Anns ->Transform a -> (a,(Anns,Int),[String])
runTransform ans f = runRWS f () (ans,0)

logTr :: String -> Transform ()
logTr str = tell [str]

getAnnsT :: Transform Anns
getAnnsT = gets fst

putAnnsT :: Anns -> Transform ()
putAnnsT ans = do
  (_,col) <- get
  put (ans,col)

-- ---------------------------------------------------------------------

-- TODO: do we have to match the filename for GHC compare functions?
uniqueSrcSpan :: Transform GHC.SrcSpan
uniqueSrcSpan = do
  (an,col) <- get
  put (an,col + 1 )
  let pos = GHC.mkSrcLoc (GHC.mkFastString "ghc-exactprint") (-1) col
  return $ GHC.mkSrcSpan pos pos

isUniqueSrcSpan :: GHC.SrcSpan -> Bool
isUniqueSrcSpan ss = srcSpanStartLine ss == -1

-- ---------------------------------------------------------------------

adjustAnnOffset :: ColDelta -> Annotation -> Annotation
adjustAnnOffset (ColDelta cd) (Ann (DP (ro,co)) (ColDelta ad) edp cs kds) = Ann edp cd' edp cs kds'
  where
    edp = case ro of
      0 -> DP (ro,co)
      _ -> DP (ro,co - cd)
    cd' = ColDelta (ad - cd)
    kds' = fmap adjustEntrySpan kds
    adjustEntrySpan (AnnSpanEntry,dp) =
      case dp of
        DP (0,c) -> (AnnSpanEntry,DP (0,c))
        DP (r,c) -> (AnnSpanEntry,DP (r, c - cd))
    adjustEntrySpan x = x

-- ---------------------------------------------------------------------

mergeAnns :: Anns -> Anns -> Anns
mergeAnns = Map.unionWith (<>)

-- ---------------------------------------------------------------------

-- |Update the DeltaPos for the given annotation keys
setLocatedAnns :: (SYB.Data a) => Anns -> [(GHC.Located a,Annotation)] -> Anns
setLocatedAnns anne kvs = foldl' setLocatedAnn anne kvs

setLocatedAnn :: (SYB.Data a) => Anns -> (GHC.Located a, Annotation) ->  Anns
setLocatedAnn aane (loc, annVal) = setAnn aane (mkAnnKey loc,annVal)

-- |Update the DeltaPos for the given annotation key/val
setAnn :: Anns -> (AnnKey, Annotation) -> Anns
setAnn anne (k, Ann dp col edp cs _) = case
  Map.lookup k anne of
    Nothing               -> Map.insert k (Ann dp col edp cs []) anne
    Just (Ann _ _ _ _ ks) -> Map.insert k (Ann dp col edp cs ks) anne

