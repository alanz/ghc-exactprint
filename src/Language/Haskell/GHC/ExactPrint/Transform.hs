{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ViewPatterns #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.GHC.ExactPrint.Transform
--
-- This module is currently under heavy development, and no promises are made
-- about API stability. Use with care.
--
-- We welcome any feedback / contributions on this, as it is the main point of
-- the library.
--
-----------------------------------------------------------------------------
module Language.Haskell.GHC.ExactPrint.Transform
        (
        -- * The Transform Monad
          Transform
        , TransformT(..)
        , hoistTransform
        , runTransform
        , runTransformT
        , runTransformFrom
        , runTransformFromT

        -- * Transform monad operations
        , logTr
        , logDataWithAnnsTr
        , uniqueSrcSpanT

        -- ** Managing declarations, in Transform monad
        , HasTransform (..)
        , HasDecls (..)
        , hsDeclsPatBind, hsDeclsPatBindD
        , replaceDeclsPatBind, replaceDeclsPatBindD
        , modifyDeclsT
        , modifyValD
        -- *** Utility, does not manage layout
        , hsDeclsValBinds, replaceDeclsValbinds
        , WithWhere(..)

        -- ** New gen functions
        , noAnnSrcSpanDP
        , noAnnSrcSpanDP0
        , noAnnSrcSpanDP1
        , noAnnSrcSpanDPn
        , d0, d1, dn
        , addComma

        -- ** Managing lists, Transform monad
        , insertAt
        , insertAtStart
        , insertAtEnd
        , insertAfter
        , insertBefore

        -- *** Low level operations used in 'HasDecls'
        , balanceComments
        , balanceCommentsList
        , balanceCommentsListA
        , anchorEof

        -- ** Managing lists, pure functions
        , captureOrderBinds
        , captureLineSpacing
        , captureMatchLineSpacing
        , captureTypeSigSpacing

        -- * Operations
        , isUniqueSrcSpan

        -- * Pure functions
        , setEntryDP, setEntryDPDecl
        , getEntryDP
        , transferEntryDP
        , transferEntryDP'
        , wrapSig, wrapDecl
        , decl2Sig, decl2Bind

        -- * Appending patterns to case expressions
        , appendMissingPats
        , Matches(..)
        , MatchLayout(..)
        ) where

import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Utils

import Control.Monad.RWS
import qualified Control.Monad.Fail as Fail

import GHC  hiding (parseModule, parsedSource)
import GHC.Data.FastString
import GHC.Types.SrcLoc

import Data.Data
import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty.Extra ((|:))
import Data.Function (on, (&))
import Data.Functor.Identity
import Data.Generics
import Data.List.Extra (chunksOf, dropEnd, takeEnd)
import Data.Maybe
import Data.Semigroup (sconcat)

import Control.Applicative (ZipList(ZipList), getZipList)
import Control.Monad.State

------------------------------------------------------------------------------
-- Transformation of source elements

-- | Monad type for updating the AST and managing the annotations at the same
-- time. The W state is used to generate logging information if required.
type Transform = TransformT Identity

-- |Monad transformer version of 'Transform' monad
newtype TransformT m a = TransformT { unTransformT :: RWST () [String] Int m a }
                deriving (Monad,Applicative,Functor
                         ,MonadReader ()
                         ,MonadWriter [String]
                         ,MonadState Int
                         ,MonadTrans
                         )

instance Fail.MonadFail m => Fail.MonadFail (TransformT m) where
    fail msg = TransformT $ RWST $ \_ _ -> Fail.fail msg

-- | Run a transformation in the 'Transform' monad, returning the updated
-- annotations and any logging generated via 'logTr'
runTransform :: Transform a -> (a,Int,[String])
runTransform f = runTransformFrom 0 f

runTransformT :: TransformT m a -> m (a,Int,[String])
runTransformT f = runTransformFromT 0 f

-- | Run a transformation in the 'Transform' monad, returning the updated
-- annotations and any logging generated via 'logTr', allocating any new
-- SrcSpans from the provided initial value.
runTransformFrom :: Int -> Transform a -> (a,Int,[String])
runTransformFrom seed f = runRWS (unTransformT f) () seed

-- |Run a monad transformer stack for the 'TransformT' monad transformer
runTransformFromT :: Int -> TransformT m a -> m (a,Int,[String])
runTransformFromT seed f = runRWST (unTransformT f) () seed

-- | Change inner monad of 'TransformT'.
hoistTransform :: (forall x. m x -> n x) -> TransformT m a -> TransformT n a
hoistTransform nt (TransformT m) = TransformT (mapRWST nt m)

-- |Log a string to the output of the Monad
logTr :: (Monad m) => String -> TransformT m ()
logTr str = tell [str]

-- |Log a representation of the given AST with annotations to the output of the
-- Monad
logDataWithAnnsTr :: (Monad m) => (Data a) => String -> a -> TransformT m ()
logDataWithAnnsTr str ast = do
  logTr $ str ++ showAst ast

-- ---------------------------------------------------------------------

-- |If we need to add new elements to the AST, they need their own
-- 'SrcSpan' for this.
-- This should no longer be needed, we use an @EpaDelta@ location instead.
uniqueSrcSpanT :: (Monad m) => TransformT m SrcSpan
uniqueSrcSpanT = do
  col <- get
  put (col + 1 )
  let pos = mkSrcLoc (mkFastString "ghc-exactprint") (-1) col
  return $ mkSrcSpan pos pos

-- |Test whether a given 'SrcSpan' was generated by 'uniqueSrcSpanT'
isUniqueSrcSpan :: SrcSpan -> Bool
isUniqueSrcSpan ss = srcSpanStartLine' ss == -1

srcSpanStartLine' :: SrcSpan -> Int
srcSpanStartLine' (RealSrcSpan s _) = srcSpanStartLine s
srcSpanStartLine' _ = 0

-- ---------------------------------------------------------------------

captureMatchLineSpacing :: LHsDecl GhcPs -> LHsDecl GhcPs
captureMatchLineSpacing (L l (ValD x (FunBind a b (MG c (L d ms )))))
                       = L l (ValD x (FunBind a b (MG c (L d ms'))))
    where
      ms' :: [LMatch GhcPs (LHsExpr GhcPs)]
      ms' = captureLineSpacing ms
captureMatchLineSpacing d = d

captureLineSpacing :: [LocatedA e] -> [LocatedA e]
captureLineSpacing [] = []
captureLineSpacing [d] = [d]
captureLineSpacing ds = map (\(_,_,x) -> x) $ go (map to ds)
  where
    to :: LocatedA e -> (Int, Int, LocatedA e)
    to d = (fst $ ss2pos rss, fst $ ss2posEnd rss,d)
      where
        rss = rs $ getHasLoc d

    go :: [(Int, Int, LocatedA e)] -> [(Int, Int, LocatedA e)]
    go [] = []
    go [d] = [d]
    go ((ls1,le1,de1):(ls2,le2,d2):ds0) = (ls1,le1,de1):go ((ls2,le2,d2'):ds0)
         `debug` ("captureLineSpacing: (le1,ls2,getLoc d2,getLoc d2')=" ++ showAst (le1,ls2,getLoc d2,getLoc d2'))
      where
        d2' = setEntryDP d2 (deltaPos (ls2-le1) 0)

-- ---------------------------------------------------------------------

captureTypeSigSpacing :: LHsDecl GhcPs -> LHsDecl GhcPs
captureTypeSigSpacing (L l (SigD x (TypeSig (AnnSig NoEpUniTok mp md) ns (HsWC xw ty))))
  = (L l (SigD x (TypeSig (AnnSig NoEpUniTok mp md) ns (HsWC xw ty))))
captureTypeSigSpacing (L l (SigD x (TypeSig (AnnSig (EpUniTok dca u) mp md) ns (HsWC xw ty))))
  = (L l (SigD x (TypeSig (AnnSig (EpUniTok dca' u) mp md) ns (HsWC xw ty'))))
  where
    -- we want DPs for the distance from the end of the ns to the
    -- AnnDColon, and to the start of the ty
    rd = case last ns of
      L (EpAnn anc' _ _) _ -> epaLocationRealSrcSpan anc'
    dca' = case dca of
          EpaSpan ss@(RealSrcSpan r _) -> (EpaDelta ss (ss2delta (ss2posEnd rd) r) [])
          _                            -> dca

    -- ---------------------------------

    ty' :: LHsSigType GhcPs
    ty' = case ty of
      (L (EpAnn anc0 a c) b)
        -> let
              anc' = case anc0 of
                EpaDelta _ _ _ -> anc0
                _ -> case dca of
                  EpaSpan ss -> EpaDelta ss (SameLine 1) []
                  EpaDelta ss _ cs0 -> EpaDelta ss (SameLine 1) cs0
           in (L (EpAnn anc' a c) b)

captureTypeSigSpacing s = s

-- ---------------------------------------------------------------------

setEntryDPDecl :: LHsDecl GhcPs -> DeltaPos -> LHsDecl GhcPs
setEntryDPDecl decl@(L _  (ValD x (FunBind a b (MG c (L d ms ))))) dp
                   = L l' (ValD x (FunBind a b (MG c (L d ms'))))
    where
      L l' _ = setEntryDP decl dp
      ms' :: [LMatch GhcPs (LHsExpr GhcPs)]
      ms' = case ms of
        [] -> []
        (m0':ms0) -> setEntryDP m0' dp : ms0
setEntryDPDecl d dp = setEntryDP d dp

-- ---------------------------------------------------------------------

-- |Set the true entry 'DeltaPos' from the annotation for a given AST
-- element. This is the 'DeltaPos' ignoring any comments.
setEntryDP :: LocatedAn t a -> DeltaPos -> LocatedAn t a
setEntryDP (L (EpAnn (EpaSpan ss@(UnhelpfulSpan _)) an cs) a) dp
  = L (EpAnn (EpaDelta ss dp []) an cs) a
setEntryDP (L (EpAnn (EpaSpan ss) an (EpaComments [])) a) dp
  = L (EpAnn (EpaDelta ss dp []) an (EpaComments [])) a
setEntryDP (L (EpAnn (EpaDelta ss d csd) an cs) a) dp
  = L (EpAnn (EpaDelta ss d' csd') an cs') a
  where
    -- I suspect we should assume the comments are already in the
    -- right place, and just set the entry DP for this case. This
    -- avoids surprises from the caller.
    (d', csd', cs') = case cs of
      EpaComments (h:t) ->
        let
          (dp0,c') = go h
        in
          (dp0, csd, EpaComments (c':t))
      EpaComments [] ->
          (dp, csd, cs)
      EpaCommentsBalanced (h:t) ts ->
        let
          (dp0,c') = go h
        in
          (dp0, c':t++csd, EpaCommentsBalanced [] ts)
      EpaCommentsBalanced [] ts ->
           case csd of
             [] -> (d, csd, EpaCommentsBalanced [] ts)
             (h:t) ->
                let
                  (dp0,c') = go h
                in
                  (dp0, c':t, EpaCommentsBalanced [] ts)
    go :: GenLocated NoCommentsLocation e -> (DeltaPos, GenLocated NoCommentsLocation e)
    go (L (EpaDelta ss0 _ c0) c) = (d,  L (EpaDelta ss0 dp c0) c)
    go (L (EpaSpan ss0)       c) = (d,  L (EpaDelta ss0 dp NoComments) c)
setEntryDP (L (EpAnn (EpaSpan ss@(RealSrcSpan r _)) an cs) a) dp
  = case sortEpaComments (priorComments cs) of
      [] ->
        L (EpAnn (EpaDelta ss dp []) an cs) a
      (L ca c:cs') ->
        L (EpAnn (EpaDelta ss edp csd) an cs'') a
              where
                cs'' = setPriorComments cs []
                csd = L (EpaDelta ss dp NoComments) c:commentOrigDeltas cs'
                lc = last $ (L ca c:cs')
                delta = case getLoc lc of
                          EpaSpan (RealSrcSpan rr _) -> ss2delta (ss2pos rr) r
                          EpaSpan _ -> (SameLine 0)
                          EpaDelta _ _dp _ -> DifferentLine 1 0
                line = getDeltaLine delta
                col = deltaColumn delta
                edp' = if line == 0 then SameLine col
                                    else DifferentLine line (col - 1)
                                         -- At the top level the layout offset is 1, adjust for it
                                         -- TODO: what about the layout offset for nested items?
                edp = edp' `debug` ("setEntryDP :" ++ showGhc (edp', (ss2pos $ epaLocationRealSrcSpan $ getLoc lc), r))


-- ---------------------------------------------------------------------

getEntryDP :: LocatedAn t a -> DeltaPos
getEntryDP (L (EpAnn (EpaDelta _ dp _) _ _) _) = dp
getEntryDP _ = SameLine 1

-- ---------------------------------------------------------------------

addEpaLocationDelta :: LayoutStartCol -> RealSrcSpan -> EpaLocation -> EpaLocation
addEpaLocationDelta _off _anc (EpaDelta ss d cs) = EpaDelta ss d cs
addEpaLocationDelta _off _anc (EpaSpan ss@(UnhelpfulSpan _)) = EpaDelta ss (SameLine 0) []
addEpaLocationDelta  off  anc (EpaSpan ss@(RealSrcSpan r _))
  = EpaDelta ss (adjustDeltaForOffset off (ss2deltaEnd anc r)) []

-- Set the entry DP for an element coming after an existing keyword annotation
setEntryDPFromAnchor :: LayoutStartCol -> EpaLocation -> LocatedA t -> LocatedA t
setEntryDPFromAnchor _off (EpaDelta _ _ _) (L la a) = L la a
setEntryDPFromAnchor _off (EpaSpan (UnhelpfulSpan _)) (L la a) = L la a
setEntryDPFromAnchor  off (EpaSpan (RealSrcSpan anc _)) ll@(L la _) = setEntryDP ll dp'
  where
    dp' = case la of
      (EpAnn (EpaSpan (RealSrcSpan r' _)) _ _) -> adjustDeltaForOffset off (ss2deltaEnd anc r')
      (EpAnn (EpaSpan _) _ _)                  -> adjustDeltaForOffset off (SameLine 0)
      (EpAnn (EpaDelta _ dp _) _ _)            -> adjustDeltaForOffset off dp

-- ---------------------------------------------------------------------

-- |Take the annEntryDelta associated with the first item and
-- associate it with the second. Also transfer any comments occurring
-- before it.
transferEntryDP :: (Typeable t1, Typeable t2)
  => LocatedAn t1 a -> LocatedAn t2 b -> (LocatedAn t2 b)
transferEntryDP (L (EpAnn anc1 an1 cs1) _) (L (EpAnn anc2 an2 cs2) b) =
  -- Note: the EpaDelta version of an EpaLocation contains the original
  -- SrcSpan. We must preserve that.
  let anc1' = case (anc1,anc2) of
          (EpaDelta _ dp cs, EpaDelta ss2 _ _) -> EpaDelta ss2 dp cs
          (_, _) -> anc1
  -- Problem: if the original had preceding comments, blindly
  -- transferring the location is not correct
  in case priorComments cs1 of
    [] -> (L (EpAnn anc1' (combine an1 an2) cs2) b)
    -- TODO: what happens if the receiving side already has comments?
    (L _ _:_) -> (L (EpAnn anc1' (combine an1 an2) (cs1 <> cs2)) b)


-- |If a and b are the same type return first arg, else return second
combine :: (Typeable a, Typeable b) => a -> b -> b
combine x y = fromMaybe y (cast x)

-- |Take the annEntryDelta associated with the first item and associate it with the second.
-- Also transfer any comments occurring before it.
-- TODO: call transferEntryDP, and use pushDeclDP
transferEntryDP' :: LHsDecl GhcPs -> LHsDecl GhcPs -> (LHsDecl GhcPs)
transferEntryDP' la lb =
  let
    (L l2 b) = transferEntryDP la lb
  in (L l2 (pushDeclDP b (SameLine 0)))


pushDeclDP :: HsDecl GhcPs -> DeltaPos -> HsDecl GhcPs
pushDeclDP (ValD x (FunBind a b (MG c (L d  ms )))) dp
          = ValD x (FunBind a b (MG c (L d' ms')))
    where
      L d' _ = setEntryDP (L d ms) dp
      ms' :: [LMatch GhcPs (LHsExpr GhcPs)]
      ms' = case ms of
        [] -> []
        (m0':ms0) -> setEntryDP m0' dp : ms0
pushDeclDP d _dp = d

-- ---------------------------------------------------------------------

-- | If we compile in haddock mode, the haddock processing inserts
-- DocDecls to carry the Haddock Documentation. We ignore these in
-- exact printing, as all the comments are also available in their
-- normal location, and the haddock processing is lossy, in that it
-- does not preserve all haddock-like comments. When we balance
-- comments in a list, we migrate some to preceding or following
-- declarations in the list. We must make sure we do not move any to
-- these DocDecls, which are not printed.
balanceCommentsList :: [LHsDecl GhcPs] -> [LHsDecl GhcPs]
balanceCommentsList decls = balanceCommentsList' (filter notDocDecl decls)

balanceCommentsList' :: [LHsDecl GhcPs] -> [LHsDecl GhcPs]
balanceCommentsList' [] = []
balanceCommentsList' [x] = [x]
balanceCommentsList' (a:b:ls) = (a':r)
  where
    (a',b') = balanceComments a b
    r = balanceCommentsList' (b':ls)

-- |The GHC parser puts all comments appearing between the end of one AST
-- item and the beginning of the next as 'annPriorComments' for the second one.
-- This function takes two adjacent AST items and moves any 'annPriorComments'
-- from the second one to the 'annFollowingComments' of the first if they belong
-- to it instead. This is typically required before deleting or duplicating
-- either of the AST elements.
balanceComments :: LHsDecl GhcPs -> LHsDecl GhcPs
                -> (LHsDecl GhcPs, LHsDecl GhcPs)
balanceComments first second =
  case first of
    (L l (ValD x fb@(FunBind{}))) ->
      let
        (L l' fb',second') = balanceCommentsFB (L l fb) second
      in (L l' (ValD x fb'), second')
    _ -> balanceCommentsA first second

-- |Once 'balanceCommentsA has been called to move trailing comments to a
-- 'FunBind', these need to be pushed down from the top level to the last
-- 'Match' if that 'Match' needs to be manipulated.
balanceCommentsFB :: LHsBind GhcPs -> LocatedA b -> (LHsBind GhcPs, LocatedA b)
balanceCommentsFB (L lf (FunBind x n (MG o (L lm matches)))) second
  = balanceCommentsA (packFunBind bind) second'
  -- There are comments on lf.  We need to
  -- + Keep the prior ones here
  -- + move the interior ones to the first match,
  -- + move the trailing ones to the last match.
  where
    (before,middle,after) = case entry lf of
        EpaSpan (RealSrcSpan ss _) ->
          let
            split = splitCommentsEnd ss (comments lf)
            split2 = splitCommentsStart ss  (EpaComments (sortEpaComments $ priorComments split))

            before0 = sortEpaComments $ priorComments split2
            middle0 = sortEpaComments $ getFollowingComments split2
            after0  = sortEpaComments $ getFollowingComments split
          in (before0,middle0,after0)
        _ -> (priorComments $ comments lf,
              [],
              getFollowingComments $ comments lf)

    lf' = setCommentsEpAnn lf (EpaComments before)
    matches' :: [LocatedA (Match GhcPs (LHsExpr GhcPs))]
    matches' = case matches of
                  (L lm' m0:ms') ->
                    (L (addCommentsToEpAnn lm' (EpaComments middle )) m0:ms')
                  _ -> error "balanceCommentsFB"
    matches'' = balanceCommentsListA matches'
    (m,ms) = case reverse matches'' of
               (L lm' m0:ms') ->
                 (L (addCommentsToEpAnn lm' (EpaCommentsBalanced [] after)) m0,ms')
               _ -> error "balanceCommentsFB4"
    (m',second') = balanceCommentsA m second
    m'' = balanceCommentsMatch m'
    (m''',lf'') = case ms of
      [] -> moveLeadingComments m'' lf'
      _  -> (m'',lf')
    bind = L lf'' (FunBind x n (MG o (L lm (reverse (m''':ms)))))
balanceCommentsFB f s = balanceCommentsA f s

-- | Move comments on the same line as the end of the match into the
-- GRHS, prior to the binds
balanceCommentsMatch :: LMatch GhcPs (LHsExpr GhcPs) -> (LMatch GhcPs (LHsExpr GhcPs))
balanceCommentsMatch (L l (Match am mctxt pats (GRHSs xg grhss binds)))
  = (L l'' (Match am mctxt pats (GRHSs xg grhss' binds')))
  where
    simpleBreak (r,_) = r /= 0
    an1 = l
    anc1 = comments an1
    cs1f = getFollowingComments anc1
    (move',stay') = break simpleBreak (trailingCommentsDeltas (anchorFromLocatedA (L l ())) cs1f)
    move = map snd move'
    stay = map snd stay'
    (l'', grhss', binds', _logInfo)
      = case NE.reverse grhss of
          L lg (GRHS ag grs rhs):|gs ->
            let
              anc1' = setFollowingComments anc1 stay
              an1' = setCommentsEpAnn l anc1'

              -- ---------------------------------
              (moved,bindsm) = pushTrailingComments WithWhere (EpaCommentsBalanced [] move) binds
              -- ---------------------------------

              (EpAnn anc an lgc) = ag
              lgc' = splitCommentsEnd (realSrcSpan $ locA lg) lgc
              ag' = if moved
                      then EpAnn anc an lgc'
                      else EpAnn anc an (lgc' <> (EpaCommentsBalanced [] move))

            in (an1', (NE.reverse $ L lg (GRHS ag' grs rhs):|gs), bindsm, (anc1',an1'))

pushTrailingComments :: WithWhere -> EpAnnComments -> HsLocalBinds GhcPs -> (Bool, HsLocalBinds GhcPs)
pushTrailingComments _ _cs b@EmptyLocalBinds{} = (False, b)
pushTrailingComments _ _cs (HsIPBinds _ _) = error "TODO: pushTrailingComments:HsIPBinds"
pushTrailingComments w cs lb@(HsValBinds an _) = (True, HsValBinds an' vb)
  where
    decls = hsDeclsLocalBinds lb
    (an', decls') = case reverse decls of
      [] -> (addCommentsToEpAnn an cs, decls)
      (L la d:ds) -> (an, L (addCommentsToEpAnn la cs) d:ds)
    vb = case replaceDeclsValbinds w lb (reverse decls') of
      (HsValBinds _ vb') -> vb'
      _ -> ValBinds NoAnnSortKey [] []


balanceCommentsListA :: [LocatedA a] -> [LocatedA a]
balanceCommentsListA [] = []
balanceCommentsListA [x] = [x]
balanceCommentsListA (a:b:ls) = (a':r)
  where
    (a',b') = balanceCommentsA a b
    r = balanceCommentsListA (b':ls)

-- |Prior to moving an AST element, make sure any trailing comments belonging to
-- it are attached to it, and not the following element. Of necessity this is a
-- heuristic process, to be tuned later. Possibly a variant should be provided
-- with a passed-in decision function.
-- The initial situation is that all comments for a given anchor appear as prior comments
-- Many of these should in fact be following comments for the previous anchor
balanceCommentsA :: LocatedA a -> LocatedA b -> (LocatedA a, LocatedA b)
balanceCommentsA la1 la2 = (la1', la2')
  where
    simpleBreak n (r,_) = r > n
    L an1 f = la1
    L an2 s = la2
    anc1 = comments an1
    anc2 = comments an2

    (p1,m1,f1) = splitComments (anchorFromLocatedA la1) anc1
    cs1p = priorCommentsDeltas (anchorFromLocatedA la1) p1

    -- Split cs1 following comments into those before any
    -- TrailingAnn's on an1, and any after
    cs1f = splitCommentsEnd (fullSpanFromLocatedA la1) $ EpaComments f1
    cs1fp = priorCommentsDeltas    (anchorFromLocatedA la1) (priorComments        cs1f)
    cs1ff = trailingCommentsDeltas (anchorFromLocatedA la1) (getFollowingComments cs1f)

    -- Split cs1ff into those that belong on an1 and ones that must move to an2
    (cs1move,cs1stay) = break (simpleBreak 1) cs1ff

    (p2,m2,f2) = splitComments (anchorFromLocatedA la2) anc2
    cs2p = priorCommentsDeltas    (anchorFromLocatedA la2) p2
    cs2f = trailingCommentsDeltas (anchorFromLocatedA la2) f2

    (stay'',move') = break (simpleBreak 1) cs2p
    -- Need to also check for comments more closely attached to la1,
    -- ie trailing on the same line
    (move'',stay') = break (simpleBreak 0) (trailingCommentsDeltas (anchorFromLocatedA la1) (map snd stay''))
    move = sortEpaComments $ map snd (cs1fp ++ cs1move ++ move'' ++ move')
    stay = sortEpaComments $ m2 ++ map snd (cs1stay ++ stay')

    an1' = setCommentsEpAnn (getLoc la1) (epaCommentsBalanced (m1 ++ map snd cs1p) move)
    an2' = setCommentsEpAnn (getLoc la2) (epaCommentsBalanced stay (map snd cs2f))
    la1' = L an1' f
    la2' = L an2' s

-- | Like commentsDeltas, but calculates the delta from the end of the anchor, not the start
trailingCommentsDeltas :: RealSrcSpan -> [LEpaComment]
               -> [(Int, LEpaComment)]
trailingCommentsDeltas _ [] = []
trailingCommentsDeltas r (la@(L (EpaDelta _ dp _) _):las)
  = (getDeltaLine dp, la): trailingCommentsDeltas r las
trailingCommentsDeltas r (la@(L l _):las)
  = deltaComment r la : trailingCommentsDeltas (epaLocationRealSrcSpan l) las
  where
    deltaComment rs' (L loc c) = (abs(ll - al), L loc c)
      where
        (al,_) = ss2posEnd rs'
        (ll,_) = ss2pos (epaLocationRealSrcSpan loc)

priorCommentsDeltas :: RealSrcSpan -> [LEpaComment]
                    -> [(Int, LEpaComment)]
priorCommentsDeltas r cs = go r (sortEpaComments cs)
  where
    go :: RealSrcSpan -> [LEpaComment] -> [(Int, LEpaComment)]
    go _   [] = []
    go _   (la@(L l@(EpaDelta _ dp _) _):las) = (getDeltaLine dp, la) : go (epaLocationRealSrcSpan l) las
    go rs' (la@(L l _):las) = deltaComment rs' la : go (epaLocationRealSrcSpan l) las

    deltaComment :: RealSrcSpan -> LEpaComment -> (Int, LEpaComment)
    deltaComment rs' (L loc c) = (abs(ll - al), L loc c)
      where
        (al,_) = ss2pos rs'
        (ll,_) = ss2pos (epaLocationRealSrcSpan loc)


-- ---------------------------------------------------------------------

-- | Split comments into ones occurring before the end of the reference
-- span, and those after it.
splitComments :: RealSrcSpan -> EpAnnComments -> ([LEpaComment], [LEpaComment], [LEpaComment])
splitComments p cs = (before, middle, after)
  where
    cmpe (L (EpaSpan (RealSrcSpan l _)) _) = ss2pos l > ss2posEnd p
    cmpe (L _ _) = True

    cmpb (L (EpaSpan (RealSrcSpan l _)) _) = ss2pos l > ss2pos p
    cmpb (L _ _) = True

    (beforeEnd, after) = break cmpe ((priorComments cs) ++ (getFollowingComments cs))
    (before, middle) = break cmpb beforeEnd


-- | Split comments into ones occurring before the end of the reference
-- span, and those after it.
splitCommentsEnd :: RealSrcSpan -> EpAnnComments -> EpAnnComments
splitCommentsEnd p (EpaComments cs) = cs'
  where
    cmp (L (EpaSpan (RealSrcSpan l _)) _) = ss2pos l > ss2posEnd p
    cmp (L _ _) = True
    (before, after) = break cmp cs
    cs' = case after of
      [] -> EpaComments cs
      _ -> epaCommentsBalanced before after
splitCommentsEnd p (EpaCommentsBalanced cs ts) = epaCommentsBalanced cs' ts'
  where
    cmp (L (EpaSpan (RealSrcSpan l _)) _) = ss2pos l > ss2posEnd p
    cmp (L _ _) = True
    (before, after) = break cmp cs
    cs' = before
    ts' = after <> ts

-- | Split comments into ones occurring before the start of the reference
-- span, and those after it.
splitCommentsStart :: RealSrcSpan -> EpAnnComments -> EpAnnComments
splitCommentsStart p (EpaComments cs) = cs'
  where
    cmp (L (EpaSpan (RealSrcSpan l _)) _) = ss2pos l > ss2posEnd p
    cmp (L _ _) = True
    (before, after) = break cmp cs
    cs' = case after of
      [] -> EpaComments cs
      _ -> epaCommentsBalanced before after
splitCommentsStart p (EpaCommentsBalanced cs ts) = epaCommentsBalanced cs' ts'
  where
    cmp (L (EpaSpan (RealSrcSpan l _)) _) = ss2pos l > ss2posEnd p
    cmp (L _ _) = True
    (before, after) = break cmp cs
    cs' = before
    ts' = after <> ts

moveLeadingComments :: (Data t, Data u, NoAnn t, NoAnn u)
  => LocatedAn t a -> EpAnn u -> (LocatedAn t a, EpAnn u)
moveLeadingComments (L la a) lb = (L la' a, lb')
  `debug` ("moveLeadingComments: (before, after, la', lb'):" ++ showAst (before, after, la', lb'))
  where
    split = splitCommentsEnd (realSrcSpan $ locA la) (comments la)
    before = sortEpaComments $ priorComments split
    after = sortEpaComments $ getFollowingComments split

    -- TODO: need to set an entry delta on lb' to zero, and move the
    -- original spacing to the first comment.

    la' = setCommentsEpAnn la (epaCommentsBalanced [] after)
    lb' = addCommentsToEpAnn lb (epaCommentsBalanced before [])

-- | A GHC comment includes the span of the preceding (non-comment)
-- token.  Takes an original list of comments, and converts the
-- 'Anchor's to have a have a `MovedAnchor` operation based on the
-- original locations.
commentOrigDeltas :: [LEpaComment] -> [LEpaComment]
commentOrigDeltas [] = []
commentOrigDeltas lcs = map commentOrigDelta lcs

addCommentOrigDeltas :: EpAnnComments -> EpAnnComments
addCommentOrigDeltas (EpaComments cs) = EpaComments (commentOrigDeltas cs)
addCommentOrigDeltas (EpaCommentsBalanced pcs fcs)
  = EpaCommentsBalanced (commentOrigDeltas pcs) (commentOrigDeltas fcs)

addCommentOrigDeltasAnn :: (EpAnn a) -> (EpAnn a)
addCommentOrigDeltasAnn (EpAnn e a cs) = EpAnn e a (addCommentOrigDeltas cs)

-- TODO: this is replicating functionality in ExactPrint. Sort out the
-- import loop`
anchorFromLocatedA :: LocatedA a -> RealSrcSpan
anchorFromLocatedA (L (EpAnn anc _ _) _) = epaLocationRealSrcSpan anc

-- | Get the full span of interest for comments from a LocatedA.
-- This extends up to the last TrailingAnn
fullSpanFromLocatedA :: LocatedA a -> RealSrcSpan
fullSpanFromLocatedA (L (EpAnn anc (AnnListItem tas)  _) _) = rr
  where
    r = epaLocationRealSrcSpan anc
    trailing_loc ta = case ta_location ta of
        EpaSpan (RealSrcSpan s _) -> [s]
        _ -> []
    rr = case reverse (concatMap trailing_loc tas) of
        [] -> r
        (s:_) -> combineRealSrcSpans r s

-- ---------------------------------------------------------------------

balanceSameLineComments :: LMatch GhcPs (LHsExpr GhcPs) -> (LMatch GhcPs (LHsExpr GhcPs))
balanceSameLineComments (L la (Match anm mctxt pats (GRHSs x grhss lb)))
  = (L la' (Match anm mctxt pats (GRHSs x grhss' lb)))
  where
    simpleBreak n (r,_) = r > n
    (la',grhss', _logInfo) = case NE.reverse grhss of
      L lg (GRHS ga gs rhs):|grs -> (la'',NE.reverse $ L lg (GRHS ga' gs rhs):|grs,[(gac,(csp,csf))])
        where
          anc1 = comments la
          (EpAnn anc an _) = ga :: EpAnn GrhsAnn
          (csp,csf) = case anc1 of
            EpaComments cs -> ([],cs)
            EpaCommentsBalanced p f -> (p,f)
          (move',stay') = break (simpleBreak 0) (trailingCommentsDeltas (epaLocationRealSrcSpan anc) csf)
          move = map snd move'
          stay = map snd stay'
          cs1 = epaCommentsBalanced csp stay

          gac = epAnnComments ga
          gfc = getFollowingComments gac
          gac' = setFollowingComments gac (sortEpaComments $ gfc ++ move)
          ga' = (EpAnn anc an gac')

          la'' = setCommentsEpAnn la cs1

-- ---------------------------------------------------------------------

anchorEof :: ParsedSource -> ParsedSource
anchorEof (L l m@(HsModule (XModulePs an _lo _ _) _mn _exps _imps _decls)) = L l (m { hsmodExt = (hsmodExt m){ hsmodAnn = an' } })
  where
    an' = addCommentOrigDeltasAnn an

-- ---------------------------------------------------------------------

-- | Create a @SrcSpanAnn@ with a @MovedAnchor@ operation using the
-- given @DeltaPos@.
noAnnSrcSpanDP :: (NoAnn ann) => DeltaPos -> EpAnn ann
noAnnSrcSpanDP dp = EpAnn (EpaDelta noSrcSpan dp []) noAnn emptyComments

noAnnSrcSpanDP0 :: (NoAnn ann) => EpAnn ann
noAnnSrcSpanDP0 = noAnnSrcSpanDP (SameLine 0)

noAnnSrcSpanDP1 :: (NoAnn ann) => EpAnn ann
noAnnSrcSpanDP1 = noAnnSrcSpanDP (SameLine 1)

noAnnSrcSpanDPn :: (NoAnn ann) => Int -> EpAnn ann
noAnnSrcSpanDPn s = noAnnSrcSpanDP (SameLine s)

d0 :: EpaLocation
d0 = EpaDelta noSrcSpan (SameLine 0) []

d1 :: EpaLocation
d1 = EpaDelta noSrcSpan (SameLine 1) []

dn :: Int -> EpaLocation
dn n = EpaDelta noSrcSpan (SameLine n) []

addComma :: SrcSpanAnnA -> SrcSpanAnnA
addComma (EpAnn anc (AnnListItem as) cs)
  = EpAnn anc (AnnListItem (AddCommaAnn (EpTok d0):as)) cs

-- ---------------------------------------------------------------------

-- | Insert a declaration into an AST element having sub-declarations
-- (@HasDecls@) according to the given location function.
insertAt :: (HasDecls ast)
         => (LHsDecl GhcPs
              -> [LHsDecl GhcPs]
              -> [LHsDecl GhcPs])
         -> ast
         -> LHsDecl GhcPs
         -> ast
insertAt f t decl = replaceDecls t (f decl oldDecls')
  where
    oldDecls = hsDecls t
    oldDeclsb = balanceCommentsList oldDecls
    oldDecls' = oldDeclsb

-- |Insert a declaration at the beginning or end of the subdecls of the given
-- AST item
insertAtStart, insertAtEnd :: HasDecls ast => ast -> LHsDecl GhcPs -> ast

insertAtEnd   = insertAt (\x xs -> xs ++ [x])

insertAtStart = insertAt insertFirst
  where
    insertFirst x xs =
      case xs of
        [] -> [x]
        (h:t) -> x:setEntryDP h (DifferentLine 2 0):t
                   `debug` ("insertAtStart:h=" ++ showAst h)


-- |Insert a declaration at a specific location in the subdecls of the given
-- AST item
insertAfter, insertBefore :: HasDecls (LocatedA ast)
                          => LocatedA old
                          -> LocatedA ast
                          -> LHsDecl GhcPs
                          -> LocatedA ast
insertAfter (getLocA -> k) = insertAt findAfter
  where
    findAfter x xs =
      case span (\(L l _) -> locA l /= k) xs of
        ([],[]) -> [x]
        (fs,[]) -> fs++[x]
        (fs, b:bs) -> fs ++ (b : x : bs)
insertBefore (getLocA -> k) = insertAt findBefore
  where
    findBefore x xs =
      let (fs, bs) = span (\(L l _) -> locA l /= k) xs
      in fs ++ (x : bs)

-- =====================================================================
-- start of HasDecls instances
-- =====================================================================

-- |Provide a means to get and process the immediate child declarations of a
-- given AST element.
class (Data t) => HasDecls t where
-- ++AZ++: TODO: add tests to confirm that hsDecls followed by replaceDecls is idempotent

    -- | Return the 'HsDecl's that are directly enclosed in the
    -- given syntax phrase. They are always returned in the wrapped 'HsDecl'
    -- form, even if orginating in local decls. This is safe, as annotations
    -- never attach to the wrapper, only to the wrapped item.
    hsDecls :: t -> [LHsDecl GhcPs]

    -- | Replace the directly enclosed decl list by the given
    --  decl list. As part of replacing it will update list order
    --  annotations, and rebalance comments and other layout changes as needed.
    --
    -- For example, a call on replaceDecls for a wrapped 'FunBind' having no
    -- where clause will convert
    --
    -- @
    -- -- |This is a function
    -- foo = x -- comment1
    -- @
    -- in to
    --
    -- @
    -- -- |This is a function
    -- foo = x -- comment1
    --   where
    --     nn = 2
    -- @
    replaceDecls :: t -> [LHsDecl GhcPs] -> t

-- ---------------------------------------------------------------------

instance HasDecls ParsedSource where
  hsDecls (L _ (HsModule (XModulePs _ _lo _ _) _mn _exps _imps decls)) = decls

  replaceDecls (L l (HsModule (XModulePs a lo deps haddocks) mname exps imps _decls)) decls
    = (L l (HsModule (XModulePs a lo deps haddocks) mname exps imps decls))

-- ---------------------------------------------------------------------

instance HasDecls (LocatedA (HsDecl GhcPs)) where
  hsDecls (L _ (TyClD _ c@ClassDecl{}))  = hsDeclsClassDecl c
  hsDecls decl = error $ "hsDecls:decl=" ++ showAst decl
  replaceDecls (L l (TyClD e dec@ClassDecl{})) decls =
    let
        decl' = replaceDeclsClassDecl dec decls
    in (L l (TyClD e decl'))
  replaceDecls decl _decls
      = error $ "replaceDecls:decl=" ++ showAst decl

-- ---------------------------------------------------------------------

instance HasDecls (LocatedA (Match GhcPs (LocatedA (HsExpr GhcPs)))) where
  hsDecls (L _ (Match _ _ _ (GRHSs _ _ lb))) = hsDeclsLocalBinds lb

  replaceDecls (L l (Match xm c p (GRHSs xr rhs binds))) []
    = let
        binds'' = replaceDeclsValbinds WithoutWhere binds []
      in (L l (Match xm c p (GRHSs xr rhs binds'')))

  replaceDecls m@(L l (Match xm c p (GRHSs xr rhs binds))) newBinds
    = let
        -- Need to throw in a fresh where clause if the binds were empty,
        -- in the annotations.
        (l', rhs') = case binds of
          EmptyLocalBinds{} ->
            let
              L l0 m' = balanceSameLineComments m
            in (l0, grhssGRHSs $ m_grhss m')
          _ -> (l, rhs)
        binds'' = replaceDeclsValbinds WithWhere binds newBinds
      in (L l' (Match xm c p (GRHSs xr rhs' binds'')))

-- ---------------------------------------------------------------------

instance HasDecls (LocatedA (HsExpr GhcPs)) where
  hsDecls (L _ (HsLet _ decls _ex)) = hsDeclsLocalBinds decls
  hsDecls _                         = []

  replaceDecls (L ll (HsLet (tkLet, tkIn) binds ex)) newDecls
    = let
        lastAnc = realSrcSpan $ spanHsLocaLBinds binds
        -- TODO: may be an intervening comment, take account for lastAnc
        (tkLet', tkIn', ex',newDecls') = case (tkLet, tkIn) of
          (EpTok l, EpTok i) ->
            let
              off = case l of
                      (EpaSpan (RealSrcSpan r _)) -> LayoutStartCol $ snd $ ss2pos r
                      (EpaSpan (UnhelpfulSpan _)) -> LayoutStartCol 0
                      (EpaDelta _ (SameLine _) _) -> LayoutStartCol 0
                      (EpaDelta _ (DifferentLine _ c) _) -> LayoutStartCol c
              ex'' = setEntryDPFromAnchor off i ex
              newDecls'' = case newDecls of
                [] -> newDecls
                (d:ds) -> setEntryDPDecl d (SameLine 0) : ds
            in ( EpTok l
               , EpTok (addEpaLocationDelta off lastAnc i)
               , ex''
               , newDecls'')
          (_,_) -> (tkLet, tkIn, ex, newDecls)
        binds' = replaceDeclsValbinds WithoutWhere binds newDecls'
      in (L ll (HsLet (tkLet', tkIn') binds' ex'))

  -- TODO: does this make sense? Especially as no hsDecls for HsPar
  replaceDecls (L l (HsPar x e)) newDecls
    = let
        e' = replaceDecls e newDecls
      in (L l (HsPar x e'))
  replaceDecls old _new = error $ "replaceDecls (LHsExpr GhcPs) undefined for:" ++ showGhc old

-- ---------------------------------------------------------------------

-- | Extract the immediate declarations for a 'PatBind' wrapped in a 'ValD'. This
-- cannot be a member of 'HasDecls' because a 'FunBind' is not idempotent
-- for 'hsDecls' \/ 'replaceDecls'. 'hsDeclsPatBindD' \/ 'replaceDeclsPatBindD' is
-- idempotent.
hsDeclsPatBindD :: LHsDecl GhcPs -> [LHsDecl GhcPs]
hsDeclsPatBindD (L l (ValD _ d)) = hsDeclsPatBind (L l d)
hsDeclsPatBindD x = error $ "hsDeclsPatBindD called for:" ++ showGhc x

-- | Extract the immediate declarations for a 'PatBind'. This
-- cannot be a member of 'HasDecls' because a 'FunBind' is not idempotent
-- for 'hsDecls' \/ 'replaceDecls'. 'hsDeclsPatBind' \/ 'replaceDeclsPatBind' is
-- idempotent.
hsDeclsPatBind :: LHsBind GhcPs -> [LHsDecl GhcPs]
hsDeclsPatBind (L _ (PatBind _ _ _ (GRHSs _ _grhs lb))) = hsDeclsLocalBinds lb
hsDeclsPatBind x = error $ "hsDeclsPatBind called for:" ++ showGhc x

-- -------------------------------------

-- | Replace the immediate declarations for a 'PatBind' wrapped in a 'ValD'. This
-- cannot be a member of 'HasDecls' because a 'FunBind' is not idempotent
-- for 'hsDecls' \/ 'replaceDecls'. 'hsDeclsPatBindD' \/ 'replaceDeclsPatBindD' is
-- idempotent.
replaceDeclsPatBindD :: LHsDecl GhcPs -> [LHsDecl GhcPs] -> (LHsDecl GhcPs)
replaceDeclsPatBindD (L l (ValD x d)) newDecls =
  let
    (L _ d') = replaceDeclsPatBind (L l d) newDecls
  in (L l (ValD x d'))
replaceDeclsPatBindD x _ = error $ "replaceDeclsPatBindD called for:" ++ showGhc x

-- | Replace the immediate declarations for a 'PatBind'. This
-- cannot be a member of 'HasDecls' because a 'FunBind' is not idempotent
-- for 'hsDecls' \/ 'replaceDecls'. 'hsDeclsPatBind' \/ 'replaceDeclsPatBind' is
-- idempotent.
replaceDeclsPatBind :: LHsBind GhcPs -> [LHsDecl GhcPs] -> (LHsBind GhcPs)
replaceDeclsPatBind (L l (PatBind x a p (GRHSs xr rhss binds))) newDecls
  =  (L l (PatBind x a p (GRHSs xr rhss binds'')))
  where
    binds'' = replaceDeclsValbinds WithWhere binds newDecls
replaceDeclsPatBind x _ = error $ "replaceDeclsPatBind called for:" ++ showGhc x

-- ---------------------------------------------------------------------

instance HasDecls (LocatedA (Stmt GhcPs (LocatedA (HsExpr GhcPs)))) where
  hsDecls (L _ (LetStmt _ lb))      = hsDeclsLocalBinds lb
  hsDecls (L _ (LastStmt _ e _ _))  = hsDecls e
  hsDecls (L _ (BindStmt _ _pat e)) = hsDecls e
  hsDecls (L _ (BodyStmt _ e _ _))  = hsDecls e
  hsDecls _                         = []

  replaceDecls (L l (LetStmt x lb)) newDecls
    = let
        lb'' = replaceDeclsValbinds WithWhere lb newDecls
      in (L l (LetStmt x lb''))
  replaceDecls (L l (LastStmt x e d se)) newDecls
    = let
        e' = replaceDecls e newDecls
      in (L l (LastStmt x e' d se))
  replaceDecls (L l (BindStmt x pat e)) newDecls
    = let
        e' = replaceDecls e newDecls
      in (L l (BindStmt x pat e'))

  replaceDecls (L l (BodyStmt x e a b)) newDecls
    = let
        e' = replaceDecls e newDecls
      in (L l (BodyStmt x e' a b))
  replaceDecls x _newDecls = x

-- =====================================================================
-- end of HasDecls instances
-- =====================================================================

-- ---------------------------------------------------------------------
-- A @FunBind@ is a container for @[LMatch GhcPs]@
--
-- When being used as a Bind (or Decl), the surrounding context
-- annotations must appear at the FunBind level, so it can be
-- manipulated in the context of other Binds or Decls.
--
-- Surrounding context annotations are specifically prior comments,
-- following comments and trailing annotations.
--
-- But when we unpack the container, by calling @hsDecls@ on a
-- @FunBind@, we must make sure that the component parts fully
-- represent the relationship between them and the surrounding
-- declarations.
--
-- This means pushing the prior context annotations into the first
-- match, and the following ones into the last match when returning
-- @hsDecls@, and undoing this for @replaceDecls@.

-- |Push leading and trailing top level annotations into the @[LMatch GhcPs]@
unpackFunBind :: LHsBind GhcPs -> LHsBind GhcPs
unpackFunBind (L loc (FunBind x1 fid (MG x2 (L lg (L lm m:matches)))))
  = (L loc'' (FunBind x1 fid (MG x2 (L lg (reverse (L llm' lmtch:ms))))))
     -- `debug` ("unpackFunBind: ="
     --          ++ showAst (("loc",loc), ("loc'",loc'), ("loc''",loc''),
     --                      ("lm'",lm'), ("llm",llm), ("llm'", llm')))
  where
    (loc', lm') = transferPriorCommentsA loc lm
    matches' = reverse $ L lm' m:matches
    (L llm lmtch, ms) = case matches' of
                      mm:ms0 -> (mm,ms0)
                      _ -> error "unpackFunBind"

    (loc'', llm') = transferFollowingA loc' llm

unpackFunBind d = d

-- |Pull leading and trailing annotations from the @[LMatch GhcPs]@ to
-- the top level.
packFunBind :: LHsBind GhcPs -> LHsBind GhcPs
packFunBind (L loc (FunBind x1 fid (MG x2 (L lg (L lm m:matches)))))
  = (L loc'' (FunBind x1 fid (MG x2 (L lg (reverse (L llm' lmtch:ms))))))
     -- `debug` ("packFunBind: ="
     --          ++ showAst (("loc",loc), ("loc'",loc'), ("loc''",loc''),
     --                      ("lm'",lm'), ("llm",llm), ("llm'", llm')))
  where
    (lm', loc') = transferPriorCommentsA lm loc
    matches' = reverse $ L lm' m:matches
    (L llm lmtch, ms) = case matches' of
                      mm:ms0 -> (mm,ms0)
                      _ -> error "packFunBind"
    (llm', loc'') = transferFollowingA llm loc'
packFunBind d = d

packFunDecl :: LHsDecl GhcPs -> LHsDecl GhcPs
packFunDecl (L l (ValD x b)) = L l' (ValD x b')
  where
    L l' b' = packFunBind (L l b)
packFunDecl x = x

unpackFunDecl :: LHsDecl GhcPs -> LHsDecl GhcPs
unpackFunDecl (L l (ValD x b)) = L l' (ValD x b')
  where
    L l' b' = unpackFunBind (L l b)
unpackFunDecl x = x

-- ---------------------------------------------------------------------

data WithWhere = WithWhere
               | WithoutWhere
               deriving (Eq,Show)

-- | Utility function for returning decls to 'HsLocalBinds'. Use with
-- care, as this does not manage the declaration order, the
-- ordering should be done by the calling function from the 'HsLocalBinds'
-- context in the AST.
replaceDeclsValbinds :: WithWhere
                     -> HsLocalBinds GhcPs -> [LHsDecl GhcPs]
                     -> HsLocalBinds GhcPs
replaceDeclsValbinds _ _ [] = EmptyLocalBinds NoExtField
replaceDeclsValbinds w b@(HsValBinds a _) new
    = let
        oldSpan = spanHsLocaLBinds b
        an = oldWhereAnnotation a w (realSrcSpan oldSpan)
        decs = concatMap decl2Bind new
        sigs = concatMap decl2Sig new
        sortKey = captureOrderBinds new
      in (HsValBinds an (ValBinds sortKey decs sigs))
replaceDeclsValbinds _ (HsIPBinds {}) _new    = error "undefined replaceDecls HsIPBinds"
replaceDeclsValbinds w (EmptyLocalBinds _) new
    = let
        an = newWhereAnnotation w
        decs = concatMap decl2Bind new
        sigs = concatMap decl2Sig  new
        sortKey = captureOrderBinds new
      in (HsValBinds an (ValBinds sortKey decs sigs))

oldWhereAnnotation :: EpAnn (AnnList (EpToken "where"))
  -> WithWhere -> RealSrcSpan -> (EpAnn (AnnList (EpToken "where")))
oldWhereAnnotation (EpAnn anc an cs) ww _oldSpan = an'
  -- TODO: when we set DP (0,0) for the HsValBinds EpEpaLocation,
  -- change the AnnList anchor to have the correct DP too
  where
    (AnnList ancl p s _r t) = an
    w = case ww of
      WithWhere -> EpTok (EpaDelta noSrcSpan (SameLine 0) [])
      WithoutWhere -> NoEpTok
    (anc', ancl') =
          case ww of
            WithWhere -> (anc, ancl)
            WithoutWhere -> (anc, ancl)
    an' = EpAnn anc'
                (AnnList ancl' p s w t)
                cs

newWhereAnnotation :: WithWhere -> (EpAnn (AnnList (EpToken "where")))
newWhereAnnotation ww = an
  where
  anc  = EpaDelta noSrcSpan (DifferentLine 1 2) []
  anc2 = EpaDelta noSrcSpan (DifferentLine 1 4) []
  w = case ww of
    WithWhere -> EpTok (EpaDelta noSrcSpan (SameLine 0) [])
    WithoutWhere -> NoEpTok
  an = EpAnn anc
              (AnnList (Just anc2) ListNone [] w [])
              emptyComments

-- ---------------------------------------------------------------------

type Decl  = LHsDecl GhcPs
type PMatch = LMatch GhcPs (LHsExpr GhcPs)

-- |Modify a 'LHsBind' wrapped in a 'ValD'. For a 'PatBind' the
-- declarations are extracted and returned after modification. For a
-- 'FunBind' the supplied 'SrcSpan' is used to identify the specific
-- 'Match' to be transformed, for when there are multiple of them.
modifyValD :: forall t.
                   SrcSpan
                -> Decl
                -> (PMatch -> [Decl] -> ([Decl], Maybe t))
                -> (Decl,Maybe t)
modifyValD p pb@(L ss (ValD _ (PatBind {} ))) f =
  if (locA ss) == p
     then
       let
           ds = hsDeclsPatBindD pb
           (ds',r) = f (error "modifyValD.PatBind should not touch Match") ds
           pb' = replaceDeclsPatBindD pb ds'
       in (pb',r)
     else (pb,Nothing)
modifyValD p decl f = (packFunDecl decl', r)
  where
    (decl',r) = runState (everywhereM (mkM doModLocal) (unpackFunDecl decl)) Nothing
    doModLocal :: PMatch -> State (Maybe t) PMatch
    doModLocal  (match@(L ss _) :: PMatch) = do
         if (locA ss) == p
           then do
             let
               ds = hsDecls match
               (ds',r0) = f match ds
             put r0
             let match' = replaceDecls match ds'
             return match'
           else return match

-- ---------------------------------------------------------------------

-- |Used to integrate a @Transform@ into other Monad stacks
class (Monad m) => (HasTransform m) where
  liftT :: Transform a -> m a

instance Monad m => HasTransform (TransformT m) where
  liftT = hoistTransform (return . runIdentity)

-- ---------------------------------------------------------------------

-- | Apply a transformation to the decls contained in @t@
modifyDeclsT :: (HasDecls t,HasTransform m)
             => ([LHsDecl GhcPs] -> m [LHsDecl GhcPs])
             -> t -> m t
modifyDeclsT action t = do
  let decls = hsDecls t
  decls' <- action decls
  return $ replaceDecls t decls'

-- | Isomorphic to @'Maybe' 'Matches'@, this type encodes whether a @case@-like
-- expression has braces; if it does, the type also records whether there are
-- pre-existing matches.
data MatchLayout = NonBraced | Braced Matches

-- | Isomorphic to @Maybe Int@, this type encodes whether there are
-- pre-existing matches in a @case@-like expression **with braces**, and - if
-- there are - what's the indentation of the first of them.
--
-- Note: it could also model the same concept for the non-braced case, but that's
-- not needed (see also 'MatchLayout').
data Matches = NoMatches | SomeMatches !Int

-- | Given a 'MatchGroup' and a 'NonEmpty' list of 'LMatch'es, this function
-- inserts the latter matches in the former group, honor the existing
-- 'MatchLayout', returning the new 'MatchGroup'.
--
-- For the meaning of the first argument of type @Maybe Int@, see
-- 'getIndentation'.
--
-- Honoring the existing layout means two things:
--
--   1. producing valid code, which means:
--
--      - adding semicolons wherever they are needed, i.e.
--
--        - if matches are braced, for every matches,
--
--        - otherwise, for all but the last matches for groups of matches
--          that are not aligned vertically, e.g.
--
--            - matches shown on the same line, which this plugin can produce,
--
--            - matches shown on different lines but in a "staircase" way,
--              which this plugin never produces).
--
--      - using the correct indentation when matches are not braced (when
--        matches are braced, the code will stay valid irrespective of the
--        indentation of the alternatives).
--
--   2. such valid code tries to adhere to the existing layout, which means:
--
--      - don't alter position of existing matches nor of the opening @{@;
--
--      - when matches are not braced, we align the first match we insert
--        with the pre-existing previous match
--
--      - we have to make some arbitrary decision
--
--        - when matches are not braced and no previous match exists,
--          we indent by @indentation def@ with respect to whatever layout
--          context is the current one;
--
--        - as regards the number of matches to print per line, we inspect the
--          last group of matches appearing on one line, to determine how many
--          matches per line we insert.
--
--        - when matches are braced, we also align them vertically (it would
--          not be necessary, in principle).
--
--
-- Refer to test cases to see practical examples.
appendMissingPats :: MatchGroup GhcPs (LHsExpr GhcPs)
                  -> NonEmpty (LMatch GhcPs (LHsExpr GhcPs))
                  -> MatchLayout
                  -> MatchGroup GhcPs (LHsExpr GhcPs)
appendMissingPats mg@(MG { mg_alts = L altsLoc existingMatches }) missingMatches matchLayout
  = let -- Choose how many patterns per line we are emitting:
        chunkSize = case existingMatches of
                 [] -> 1 -- trivially 1 if there's no existing matches,
                      -- otherwise, set the size equal to the length
                      -- of the last group of @existingMatches@ that
                      -- are on the same line:
                 _ -> NE.length
                    $ NE.last
                    $ NE.groupBy1 startSameLine (NE.fromList existingMatches)

        -- Chunkify the matches to be inserted:
        missingGroup :| missingGroups = prettyChunksOf chunkSize missingMatches

        -- Detect if the list of alternatives is between @{@ and @}@:
        isBraced = isJust $ getOpeningBraceCol altsLoc

        -- Finally, lay out the missing matches:
        missingMatchesEP = -- indent the first group and the following ones (see discussion above)
                           mapFirst indentHead missingGroup :| map (mapFirst indentTail) missingGroups
                           -- add a semicolon to the end of each group only if the alternatives are braced
                         & (if isBraced then addSemicols else id)
                           -- put each group on its own line
                         & NE.map (mapFirst putOnNewLine)
                           -- concatenate the groups
                         & sconcat
                           -- turn into an ordinary list
                         & NE.toList
          where
            -- add semicolons:
            addSemicols = NE.zipWith ($)
                                      -- for each one-line group of matches,
                                     (replicate (length missingGroups)
                                                -- only to the last match of the group,
                                                (mapLast addSemiCol)
                                      -- except for the last group
                                      |: id)

            -- Indentation is complicated.
            --
            -- For a non-braced @case@-like expression, the first match **of the
            -- whole expression** (I mean, not the first match **to be inserted**)
            -- has some anchor that depends on the surrounding code, while the
            -- following matches all use their own predecessor as the anchor.
            --
            -- Otherwise (i.e. for a braced @case@-like expression), all matches
            -- including the first one have the same anchor that depends on the
            -- surrounding code.
            --
            -- Therefore, here's how we set the DeltaPos for the first and
            -- following matches:
            (setDPCol -> indentHead, setDPCol -> indentTail)
               = case matchLayout of
                   NonBraced | null existingMatches  -> (indentation def, 0)
                   NonBraced                         -> (0, 0)
                   Braced (SomeMatches indent)       -> (indent, indent)
                   Braced NoMatches                  -> let indent = indentation def
                                                        in (indent, indent)

        -- Only if there's braces do we need to make sure the last of the
        -- existing matches ends with @;@:
        existingMatchesEP = if isBraced
                               then dropEnd 1 existingMatches <> (addSemiCol <$> takeEnd 1 existingMatches)
                               else existingMatches

    in mg { mg_alts = L altsLoc (existingMatchesEP <> missingMatchesEP) }

-- | Accepts a @NonEmpty (LocatedA a)@ and chunkifies it by the given 'size',
-- putting all matches of each chunk on the same line, leaving 1 space in between, and
-- keeping the code valid by adding semicolons to all but the last match of each chunk.
prettyChunksOf :: Int -> NonEmpty (LocatedA a) -> NonEmpty (NonEmpty (LocatedA a))
prettyChunksOf size allMatches = do
  -- For each chunk
  chunk <- chunksOf1 size allMatches
  pure $ fromZipList
       $ do -- of all the matches of chunk
            match       <- toZipList chunk
            -- from the second match onwards, they go the same line, one space apart
            putBeside   <- toZipList $ id :| repeat (setDP 0 1)
            -- all but the last match get a semicolon
            addSemicols <- toZipList $ replicate (length chunk - 1) addSemiCol |: id
            -- apply
            pure $ addSemicols $ putBeside match
  where
    toZipList = ZipList . NE.toList
    fromZipList = NE.fromList . getZipList

-- Other things that we could store here are:
--
--    - the maximum number of alternatives on one line
--    - whether or not to put the @;@ for the last alternative
data Default = Default {
  -- | Max number of underscores to show for the constructor of an alternative.
  -- Beyond this, the record syntax with empty braces is used.
  maxUnderscores :: Int
  -- | Indentation used when there's no existing alternatives to refer to.
  -- Such indentation is with respect to the current layout context.
, indentation    :: Int
}

def :: Default
def = Default { maxUnderscores = 3
              , indentation = 2 }

-- | Predicate telling if two located annotations are (actually, start) on the
-- same line.
startSameLine :: LocatedAn ann e -> LocatedAn ann e -> Bool
startSameLine = (==) `on` getStartLine
  where
    -- | Get the starting line of an 'HasSrcSpan'.
    getStartLine :: GHC.LocatedAn ann e -> Int
    getStartLine = srcSpanStartLine . realSrcSpan . getHasLoc


-- | Given an @EpAnn (AnnList a)@ return the starting column of
-- its opening brace, if any, otherwise 'Nothing'.
getOpeningBraceCol :: EpAnn (AnnList a) -> Maybe Int
getOpeningBraceCol (EpAnn _ (AnnList _ (ListBraces (EpTok col) _) _ _ _) _) = Just $ getStartCol $ getHasLoc col
getOpeningBraceCol _ = Nothing

-- | Get the starting column of an 'HasSrcSpan'.
getStartCol :: SrcSpan -> Int
getStartCol = srcSpanStartCol . realSrcSpan . getHasLoc

-- | Set the DeltaPos for the given annotation.
setDP :: Int -> Int -> LocatedAn t a -> LocatedAn t a
setDP deltaLine deltaColumn lann = setEntryDP lann $ deltaPos deltaLine deltaColumn

-- | Set the deltaColumn for the given annotation.
setDPCol :: Int -> LocatedAn t a -> LocatedAn t a
setDPCol deltaColumn lann = setEntryDP lann
                          $ (\d -> deltaPos (getDeltaLine d) deltaColumn)
                          $ getEntryDP lann

-- | Set the deltaLine for the given annotation.
setDPLine :: Int -> LocatedAn t a -> LocatedAn t a
setDPLine deltaLine lann = setEntryDP lann
                          $ (\d -> deltaPos deltaLine (deltaColumn d))
                          $ getEntryDP lann

-- | Useful helper.
putOnNewLine :: LocatedAn t a -> LocatedAn t a
putOnNewLine = setDPLine 1

-- | Add semicolon, unless one is already present.
addSemiCol :: LocatedA a -> LocatedA a
addSemiCol (L l@(EpAnn _ ls _) e)
  | none isSemiCol (lann_trailing ls)
  = L (addTrailingAnnToA (AddSemiAnn (EpTok d0)) emptyComments l) e
  where
    isSemiCol :: TrailingAnn -> Bool
    isSemiCol (AddSemiAnn _) = True
    isSemiCol _              = False
addSemiCol l = l

-- | Version of 'Data.List.Extra.chunksOf' (**not** to be confused with
-- 'Data.List.Split.chunksOf') for a 'NonEmpty' lists.
chunksOf1 :: Int -> NonEmpty a -> NonEmpty (NonEmpty a)
chunksOf1 n xs
  | n >= 1
  , (b:before, after) <- NE.splitAt n xs
    = (b :| before) :| case after of
                         [] -> []
                         _  -> map NE.fromList $ chunksOf n after
  | otherwise = error "chunksOf1: the `Int` argument should be ≥ 1"

-- | Maps a function @f@ over the first element of a 'NonEmpty' list.
mapFirst :: (a -> a) -> NonEmpty a -> NonEmpty a
mapFirst f (a :| as) = f a :| as

-- | Maps a function @f@ over the last element of a 'NonEmpty' list.
mapLast :: (a -> a) -> NonEmpty a -> NonEmpty a
mapLast f (a :| [])     = f a :| []
mapLast f (a :| b : cs) = a :| NE.toList (mapLast f $ b :| cs)

-- | Convenient negation of 'any'.
none :: Foldable t => (a -> Bool) -> t a -> Bool
none p xs = not $ any p xs
