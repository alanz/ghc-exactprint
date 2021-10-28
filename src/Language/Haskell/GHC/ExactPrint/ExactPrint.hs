{-# LANGUAGE DeriveDataTypeable   #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiWayIf           #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns         #-}
{-# LANGUAGE UndecidableInstances  #-} -- For the (StmtLR GhcPs GhcPs (LocatedA (body GhcPs))) ExactPrint instance

module Language.Haskell.GHC.ExactPrint.ExactPrint
  (
    ExactPrint(..)
  , exactPrint
  , exactPrintWithOptions
  , makeDeltaAst

  -- * Configuration
  , EPOptions(epRigidity, epAstPrint, epTokenPrint, epWhitespacePrint, epUpdateAnchors)
  , stringOptions
  , epOptions
  , deltaOptions
  ) where

import GHC
import GHC.Base (NonEmpty(..))
import GHC.Core.Coercion.Axiom (Role(..))
import GHC.Data.Bag
import qualified GHC.Data.BooleanFormula as BF
import GHC.Data.FastString
import GHC.Types.Basic hiding (EP)
import GHC.Types.Fixity
import GHC.Types.ForeignCall
import GHC.Types.SourceText
import GHC.Types.Var
import GHC.Utils.Outputable hiding ( (<>) )
import GHC.Unit.Module.Warnings
import GHC.Utils.Misc
import GHC.Utils.Panic

import Control.Monad.Identity
import qualified Control.Monad.Reader as Reader
import Control.Monad.RWS
import Data.Data ( Data )
import Data.Dynamic
import Data.Foldable
import Data.Functor.Const
import qualified Data.Set.Ordered as OSet
import qualified Data.Set as Set
import Data.Typeable
import Data.List ( partition, sortBy)
import Data.Maybe ( isJust, mapMaybe )

import Data.Void

import Language.Haskell.GHC.ExactPrint.Lookup
import Language.Haskell.GHC.ExactPrint.Utils
import Language.Haskell.GHC.ExactPrint.Types

-- import Debug.Trace

-- ---------------------------------------------------------------------

exactPrint :: ExactPrint ast => ast -> String
exactPrint ast = snd $ runIdentity (runEP stringOptions (markAnnotated ast))

-- | The additional option to specify the rigidity and printing
-- configuration.
exactPrintWithOptions :: (ExactPrint ast, Monoid b, Monad m)
                      => EPOptions m b
                      -> ast
                      -> m (ast, b)
exactPrintWithOptions r ast =
    runEP r (markAnnotated ast)

makeDeltaAst :: ExactPrint ast => ast -> ast
makeDeltaAst ast = fst $ runIdentity (runEP deltaOptions (markAnnotated ast))

------------------------------------------------------

type EP w m a = RWST (EPOptions m w) (EPWriter w) EPState m a

runEP :: (Monad m)
      => EPOptions m w
      -> EP w m a -> m (a, w)
runEP epReader action = do
  (ast, w) <- evalRWST action epReader defaultEPState
  return (ast, output w)

-- ---------------------------------------------------------------------

defaultEPState :: EPState
defaultEPState = EPState
             { epPos      = (1,1)
             , dLHS       = 0
             , pMarkLayout = False
             , pLHS = 0
             , dMarkLayout = False
             , dPriorEndPosition = (1,1)
             , uAnchorSpan = badRealSrcSpan
             , uExtraDP = Nothing
             , epComments = []
             , epCommentsApplied = []
             }


-- ---------------------------------------------------------------------
-- The EP monad and basic combinators

-- | The R part of RWS. The environment. Updated via 'local' as we
-- enter a new AST element, having a different anchor point.
data EPOptions m a = EPOptions
            {
              epAstPrint :: forall ast . Data ast => GHC.Located ast -> a -> m a
            , epTokenPrint :: String -> m a
            , epWhitespacePrint :: String -> m a
            , epRigidity :: Rigidity
            , epUpdateAnchors :: Bool
            }

-- | Helper to create a 'EPOptions'
epOptions ::
      (forall ast . Data ast => GHC.Located ast -> a -> m a)
      -> (String -> m a)
      -> (String -> m a)
      -> Rigidity
      -> Bool
      -> EPOptions m a
epOptions astPrint tokenPrint wsPrint rigidity delta = EPOptions
             {
               epAstPrint = astPrint
             , epWhitespacePrint = wsPrint
             , epTokenPrint = tokenPrint
             , epRigidity = rigidity
             , epUpdateAnchors = delta
             }

-- | Options which can be used to print as a normal String.
stringOptions :: EPOptions Identity String
stringOptions = epOptions (\_ b -> return b) return return NormalLayout False

-- | Options which can be used to simply update the AST to be in delta
-- form, without generating output
deltaOptions :: EPOptions Identity ()
deltaOptions = epOptions (\_ _ -> return ()) (\_ -> return ()) (\_ -> return ()) NormalLayout True

data EPWriter a = EPWriter
              { output :: !a }

instance Monoid w => Semigroup (EPWriter w) where
  (EPWriter a) <> (EPWriter b) = EPWriter (a <> b)

instance Monoid w => Monoid (EPWriter w) where
  mempty = EPWriter mempty

data EPState = EPState
             { uAnchorSpan :: !RealSrcSpan -- ^ in pre-changed AST
                                          -- reference frame, from
                                          -- Annotation
             , uExtraDP :: !(Maybe Anchor) -- ^ Used to anchor a
                                             -- list

             -- Print phase
             , epPos        :: !Pos -- ^ Current output position
             , pMarkLayout  :: !Bool
             , pLHS   :: !LayoutStartCol

             -- Delta phase
             , dPriorEndPosition :: !Pos -- ^ End of Position reached
                                         -- when processing the
                                         -- preceding element
             , dMarkLayout :: !Bool
             , dLHS        :: !LayoutStartCol

             -- Shared
             , epComments :: ![Comment]
             , epCommentsApplied :: ![[Comment]]
             }

-- ---------------------------------------------------------------------

-- AZ:TODO: this can just be a function :: (EpAnn a) -> Entry
class HasEntry ast where
  fromAnn :: ast -> Entry

-- ---------------------------------------------------------------------

-- type Annotated = FreeT AnnotationF Identity
-- type Annotated a = EP w m a

-- ---------------------------------------------------------------------

-- | Key entry point.  Switches to an independent AST element with its
-- own annotation, calculating new offsets, etc
markAnnotated :: (Monad m, Monoid w, ExactPrint a) => a -> EP w m a
markAnnotated a = enterAnn (getAnnotationEntry a) a

-- | For HsModule, because we do not have a proper SrcSpan, we must
-- indicate to flush trailing comments when done.
data FlushComments = FlushComments
                   | NoFlushComments
                   deriving (Eq, Show)

-- | For GenLocated SrcSpan, we construct an entry location but cannot update it.
data CanUpdateAnchor = CanUpdateAnchor
                     | CanUpdateAnchorOnly
                     | NoCanUpdateAnchor
                   deriving (Eq, Show)

data Entry = Entry Anchor EpAnnComments FlushComments CanUpdateAnchor
           | NoEntryVal

-- | For flagging whether to capture comments in an EpaDelta or not
data CaptureComments = CaptureComments
                     | NoCaptureComments

mkEntry :: Anchor -> EpAnnComments -> Entry
mkEntry anc cs = Entry anc cs NoFlushComments CanUpdateAnchor

instance HasEntry (SrcSpanAnn' (EpAnn an)) where
  fromAnn (SrcSpanAnn EpAnnNotUsed ss) = mkEntry (spanAsAnchor ss) emptyComments
  fromAnn (SrcSpanAnn an _) = fromAnn an

instance HasEntry (EpAnn a) where
  fromAnn (EpAnn anchor _ cs) = mkEntry anchor cs
  fromAnn EpAnnNotUsed = NoEntryVal

-- ---------------------------------------------------------------------

fromAnn' :: (HasEntry a) => a -> Entry
fromAnn' an = case fromAnn an of
  NoEntryVal -> NoEntryVal
  Entry a c _ u -> Entry a c FlushComments u

-- ---------------------------------------------------------------------

astId :: (Typeable a) => a -> String
astId a = show (typeOf a)

cua :: (Monad m, Monoid w) => CanUpdateAnchor -> EP w m [a] -> EP w m [a]
cua CanUpdateAnchor f = f
cua CanUpdateAnchorOnly _ = return []
cua NoCanUpdateAnchor _ = return []

-- | "Enter" an annotation, by using the associated 'anchor' field as
-- the new reference point for calculating all DeltaPos positions.
--
-- This is combination of the ghc=exactprint Delta.withAST and
-- Print.exactPC functions and effectively does the delta processing
-- immediately followed by the print processing.  JIT ghc-exactprint.
enterAnn :: (Monad m, Monoid w, ExactPrint a) => Entry -> a -> EP w m a
enterAnn NoEntryVal a = do
  p <- getPosP
  debugM $ "enterAnn:starting:NO ANN:(p,a) =" ++ show (p, astId a)
  r <- exact a
  debugM $ "enterAnn:done:NO ANN:p =" ++ show (p, astId a)
  return r
enterAnn (Entry anchor' cs flush canUpdateAnchor) a = do
  p <- getPosP
  debugM $ "enterAnn:starting:(p,a) =" ++ show (p, astId a)
  -- debugM $ "enterAnn:(cs) =" ++ showGhc (cs)
  let curAnchor = anchor anchor' -- As a base for the current AST element
  debugM $ "enterAnn:(curAnchor):=" ++ show (rs2range curAnchor)
  case canUpdateAnchor of
    CanUpdateAnchor -> pushAppliedComments
    _ -> return ()
  addCommentsA (priorComments cs)
  debugM $ "enterAnn:Added comments"
  printComments curAnchor
  priorCs <- cua canUpdateAnchor takeAppliedComments -- no pop
  -- -------------------------
  case anchor_op anchor' of
    MovedAnchor dp -> do
      debugM $ "enterAnn: MovedAnchor:" ++ show dp
      -- Set the original anchor as prior end, so the rest of this AST
      -- fragment has a reference
      -- BUT: this means the entry DP can be calculated incorrectly too,
      -- for immediately nested items.
      setPriorEndNoLayoutD (ss2pos curAnchor)
    _ -> do
      return ()
  -- -------------------------
  setAnchorU curAnchor
  -- -------------------------------------------------------------------
  -- The first part corresponds to the delta phase, so should only use
  -- delta phase variables
  -- -----------------------------------
  -- Calculate offset required to get to the start of the SrcSPan
  off <- gets dLHS
  let spanStart = ss2pos curAnchor
  priorEndAfterComments <- getPriorEndD
  let edp' = adjustDeltaForOffset
               -- Use the propagated offset if one is set
               -- Note that we need to use the new offset if it has
               -- changed.
               off (ss2delta priorEndAfterComments curAnchor)
  debugM $ "enterAnn: (edp',off,priorEndAfterComments,curAnchor):" ++ show (edp',off,priorEndAfterComments,rs2range curAnchor)
  let edp'' = case anchor_op anchor' of
        MovedAnchor dp -> dp
        _ -> edp'
  -- ---------------------------------------------
  -- let edp = edp''
  med <- getExtraDP
  setExtraDP Nothing
  let edp = case med of
        Nothing -> edp''
        Just (Anchor _ (MovedAnchor dp)) -> dp
                   -- Replace original with desired one. Allows all
                   -- list entry values to be DP (1,0)
        Just (Anchor r _) -> dp
          where
            dp = adjustDeltaForOffset
                   off (ss2delta priorEndAfterComments r)
  when (isJust med) $ debugM $ "enterAnn:(med,edp)=" ++ show (med,edp)
  -- ---------------------------------------------
  -- Preparation complete, perform the action
  when (priorEndAfterComments < spanStart) (do
    debugM $ "enterAnn.dPriorEndPosition:spanStart=" ++ show spanStart
    modify (\s -> s { dPriorEndPosition    = spanStart } ))

  debugM $ "enterAnn: (anchor_op, curAnchor):" ++ show (anchor_op anchor', rs2range curAnchor)
  debugM $ "enterAnn: (dLHS,spanStart,pec,edp)=" ++ show (off,spanStart,priorEndAfterComments,edp)

  -- end of delta phase processing
  -- -------------------------------------------------------------------
  -- start of print phase processing

  let mflush = when (flush == FlushComments) $ do
        debugM $ "flushing comments in enterAnn"
        flushComments (getFollowingComments cs)

  -- let
  --   st = annNone
  -- withOffset st (advance edp >> exact a >> mflush)
-- local :: (r -> r) -> RWST r w s m a -> RWST r w s m a
  advance edp
  a' <- exact a
  mflush

  postCs <- cua canUpdateAnchor takeAppliedCommentsPop
  when (flush == NoFlushComments) $ do
    when ((getFollowingComments cs) /= []) $ do
      debugM $ "starting trailing comments:" ++ showAst (getFollowingComments cs)
      mapM_ printOneComment (map tokComment $ getFollowingComments cs)
      debugM $ "ending trailing comments"

  let newAchor = anchor' { anchor_op = MovedAnchor edp }
  let r = case canUpdateAnchor of
            CanUpdateAnchor -> setAnnotationAnchor a' newAchor (mkEpaComments (priorCs++ postCs) [])
            CanUpdateAnchorOnly -> setAnnotationAnchor a' newAchor emptyComments
            NoCanUpdateAnchor -> a'
  -- let r = (setAnnotationAnchor a' newAchor (mkEpaComments [] postCs))
  pure () -- monadic action to flush debugM
  debugM $ "calling setAnnotationAnchor:(curAnchor, newAchor,priorCs,postCs)=" ++ showAst (show (rs2range curAnchor), newAchor, priorCs, postCs)
  -- debugM $ "calling setAnnotationAnchor:(newAchor,postCs)=" ++ showAst (newAchor, postCs)
  debugM $ "enterAnn:done:(p,a) =" ++ show (p, astId a')
  return r

-- ---------------------------------------------------------------------

-- withComments :: (Monad m, Monoid w) => Annotation -> (EP w m a -> EP w m a)
-- withComments a =
--   local (\s -> s { epAnn = a, epContext = pushAcs (epContext s) })


-- ---------------------------------------------------------------------

addCommentsA :: (Monad m, Monoid w) => [LEpaComment] -> EP w m ()
addCommentsA csNew = addComments (map tokComment csNew)

{-
TODO: When we addComments, some may have an anchor that is no longer
valid, as it has been moved and has an anchor_op.

Does an Anchor even make sense for a comment, perhaps it should be an
EpaLocation?

How do we sort them? do we assign a location based on when we add them
to the list, based on the current output pos?  Except the offset is a
delta compared to a reference location.  Need to nail the concept of
the reference location.

By definition it is the current anchor, so work against that. And that
also means that the first entry comment that has moved should not have
a line offset.
-}
addComments :: (Monad m, Monoid w) => [Comment] -> EP w m ()
addComments csNew = do
  debugM $ "addComments:" ++ show csNew
  cs <- getUnallocatedComments
  -- Make sure we merge duplicates while sorting, needed until
  -- https://gitlab.haskell.org/ghc/ghc/-/issues/20239 is resolved
  let ocs = OSet.fromList cs
  let ncs = OSet.fromList csNew
  putUnallocatedComments (OSet.toAscList (ocs OSet.<>| ncs))


-- ---------------------------------------------------------------------

-- | Just before we print out the EOF comments, flush the remaining
-- ones in the state.
flushComments :: (Monad m, Monoid w) => [LEpaComment] -> EP w m ()
flushComments trailing = do
  addCommentsA trailing
  cs <- getUnallocatedComments
  -- Must compare without span filenames, for CPP injected comments with fake filename
  let cmp (Comment _ l1 _ _) (Comment _ l2 _ _) = compare (ss2pos $ anchor l1) (ss2pos $ anchor l2)
  debugM $ "flushing comments starting"
  mapM_ printOneComment (sortBy cmp cs)
  debugM $ "flushing comments done"

-- ---------------------------------------------------------------------

-- |In order to interleave annotations into the stream, we turn them into
-- comments. They are removed from the annotation to avoid duplication.
annotationsToComments :: (Monad m, Monoid w)
  => EpAnn a -> Lens a [AddEpAnn] -> [AnnKeywordId] -> EP w m (EpAnn a)
annotationsToComments EpAnnNotUsed _ _kws = return EpAnnNotUsed
annotationsToComments (EpAnn anc a cs) l kws = do
  let (newComments, newAnns) = go ([],[]) (view l a)
  addComments newComments
  return (EpAnn anc (set l (reverse newAnns) a) cs)
  where
    keywords = Set.fromList kws

    go :: ([Comment], [AddEpAnn]) -> [AddEpAnn] -> ([Comment], [AddEpAnn])
    go acc [] = acc
    go (cs',ans) ((AddEpAnn k ss) : ls)
      | Set.member k keywords = go ((mkKWComment k ss):cs', ans) ls
      | otherwise             = go (cs', (AddEpAnn k ss):ans)    ls


-- -- |In order to interleave annotations into the stream, we turn them into
-- -- comments.
-- annotationsToComments' :: (Monad m, Monoid w) => [AddEpAnn] -> [AnnKeywordId] -> EP w m ()
-- annotationsToComments' ans kws = do
--   let
--     getSpans _ [] = []
--     getSpans k1 (AddEpAnn k2 ss:as)
--       | k1 == k2 = ss : getSpans k1 as
--       | otherwise = getSpans k1 as
--     doOne :: (Monad m, Monoid w) => AnnKeywordId -> EP w m [Comment]
--     doOne kw = do
--       let sps =getSpans kw ans
--       return $ map (mkKWComment kw ) sps
--     -- TODO:AZ make sure these are sorted/merged properly when the invariant for
--     -- allocateComments is re-established.
--   newComments <- mapM doOne kws
--   addComments (concat newComments)

-- annotationsToCommentsA :: (Monad m, Monoid w) => EpAnn [AddEpAnn] -> [AnnKeywordId] -> EP w m ()
-- annotationsToCommentsA EpAnnNotUsed _ = return ()
-- annotationsToCommentsA an kws = annotationsToComments' (anns an) kws

-- ---------------------------------------------------------------------

-- Temporary function to simply reproduce the "normal" pretty printer output
withPpr :: (Monad m, Monoid w, Outputable a) => a -> EP w m a
withPpr a = do
  ss <- getAnchorU
  debugM $ "withPpr: ss=" ++ show ss
  printStringAtRs' ss (showPprUnsafe a)
  return a

-- ---------------------------------------------------------------------

-- | An AST fragment with an annotation must be able to return the
-- requirements for nesting another one, captured in an 'Entry', and
-- to be able to use the rest of the exactprint machinery to print the
-- element.  In the analogy to Outputable, 'exact' plays the role of
-- 'ppr'.
class (Typeable a) => ExactPrint a where
  getAnnotationEntry :: a -> Entry
  setAnnotationAnchor :: a -> Anchor -> EpAnnComments -> a
  exact :: (Monad m, Monoid w) => a -> EP w m a

-- ---------------------------------------------------------------------
-- Start of utility functions
-- ---------------------------------------------------------------------

printSourceText :: (Monad m, Monoid w) => SourceText -> String -> EP w m ()
printSourceText (NoSourceText) txt   =  printStringAdvance txt >> return ()
printSourceText (SourceText   txt) _ =  printStringAdvance txt >> return ()

-- ---------------------------------------------------------------------

printStringAtSs :: (Monad m, Monoid w) => SrcSpan -> String -> EP w m ()
printStringAtSs ss str = printStringAtRs (realSrcSpan ss) str >> return ()

printStringAtRs :: (Monad m, Monoid w) => RealSrcSpan -> String -> EP w m EpaLocation
printStringAtRs pa str = printStringAtRsC CaptureComments pa str

printStringAtRsC :: (Monad m, Monoid w)
  => CaptureComments -> RealSrcSpan -> String -> EP w m EpaLocation
printStringAtRsC capture pa str = do
  printComments pa
  pe <- getPriorEndD
  debugM $ "printStringAtRs:pe=" ++ show pe
  let p = ss2delta pe pa
  p' <- adjustDeltaForOffsetM p
  printStringAtLsDelta p' str
  setPriorEndASTD True pa
  cs' <- case capture of
    CaptureComments -> takeAppliedComments
    NoCaptureComments -> return []
  debugM $ "printStringAtRs:cs'=" ++ show cs'
  -- return (EpaDelta p' (map comment2LEpaComment (noKWComments cs')))
  return (EpaDelta p' (map comment2LEpaComment cs'))

printStringAtRs' :: (Monad m, Monoid w) => RealSrcSpan -> String -> EP w m ()
printStringAtRs' pa str = printStringAtRsC NoCaptureComments pa str >> return ()

-- ---------------------------------------------------------------------

-- AZ:TODO get rid of this
printStringAtMLoc :: (Monad m, Monoid w) => Maybe EpaLocation -> String -> EP w m ()
printStringAtMLoc (Just aa) s = printStringAtAA aa s >> return ()
printStringAtMLoc Nothing s = printStringAtLsDelta (SameLine 1) s >> return ()

printStringAtMLoc' :: (Monad m, Monoid w)
  => Maybe EpaLocation -> String -> EP w m (Maybe EpaLocation)
printStringAtMLoc' (Just aa) s = Just <$> printStringAtAA aa s
printStringAtMLoc' Nothing s = do
  printStringAtLsDelta (SameLine 1) s
  return (Just (EpaDelta (SameLine 1) []))

printStringAtMLocL :: (Monad m, Monoid w)
  => EpAnn a -> Lens a (Maybe EpaLocation) -> String -> EP w m (EpAnn a)
printStringAtMLocL EpAnnNotUsed _ _ = return EpAnnNotUsed
printStringAtMLocL (EpAnn anc an cs) l s = do
  r <- go (view l an) s
  return (EpAnn anc (set l r an) cs)
  where
    go (Just aa) str = Just <$> printStringAtAA aa str
    go Nothing str = do
      printStringAtLsDelta (SameLine 1) str
      return (Just (EpaDelta (SameLine 1) []))

printStringAtAA :: (Monad m, Monoid w) => EpaLocation -> String -> EP w m EpaLocation
printStringAtAA el str = printStringAtAAC CaptureComments el str

printStringAtAAL :: (Monad m, Monoid w)
  => EpAnn a -> Lens a EpaLocation -> String -> EP w m (EpAnn a)
printStringAtAAL EpAnnNotUsed _ _ = return EpAnnNotUsed
printStringAtAAL (EpAnn anc an cs) l str = do
  r <- printStringAtAAC CaptureComments (view l an) str
  return (EpAnn anc (set l r an) cs)

printStringAtAAC :: (Monad m, Monoid w)
  => CaptureComments -> EpaLocation -> String -> EP w m EpaLocation
printStringAtAAC capture (EpaSpan r) s = printStringAtRsC capture r s
printStringAtAAC capture (EpaDelta d cs) s = do
  mapM_ (printOneComment . tokComment) cs
  pe <- getPriorEndD
  p1 <- getPosP
  printStringAtLsDelta d s
  p2 <- getPosP
  -- debugM $ "printStringAtAA:(pe,p1,p2)=" ++ show (pe,p1,p2)
  setPriorEndASTPD True (p1,p2)
  cs' <- case capture of
    CaptureComments -> takeAppliedComments
    NoCaptureComments -> return []
  debugM $ "printStringAtAA:(pe,p1,p2,cs')=" ++ show (pe,p1,p2,cs')
  -- return (EpaDelta d (map comment2LEpaComment (noKWComments cs')))
  return (EpaDelta d (map comment2LEpaComment cs'))


-- ---------------------------------------------------------------------

markExternalSourceText :: (Monad m, Monoid w) => SrcSpan -> SourceText -> String -> EP w m ()
markExternalSourceText l NoSourceText txt   = printStringAtRs (realSrcSpan l) txt >> return ()
markExternalSourceText l (SourceText txt) _ = printStringAtRs (realSrcSpan l) txt >> return ()

-- ---------------------------------------------------------------------

-- -- TODO: remove in favour of markLensMAA
-- markLocatedMAA :: (Monad m, Monoid w) => EpAnn a -> (a -> Maybe AddEpAnn) -> EP w m ()
-- markLocatedMAA EpAnnNotUsed  _  = return ()
-- markLocatedMAA (EpAnn _ a _) f =
--   case f a of
--     Nothing -> return ()
--     Just aa -> markAddEpAnn aa >> return ()

markLensMAA :: (Monad m, Monoid w) => EpAnn a -> Lens a (Maybe AddEpAnn) -> EP w m (EpAnn a)
markLensMAA EpAnnNotUsed  _  = return EpAnnNotUsed
markLensMAA (EpAnn anc a cs) l =
  case view l a of
    Nothing -> return (EpAnn anc a cs)
    Just aa -> do
      aa' <- markAddEpAnn aa
      return (EpAnn anc (set l (Just aa') a) cs)

-- TODO: remove in favour of markLensAA
markLocatedAA :: (Monad m, Monoid w) => EpAnn a -> (a -> AddEpAnn) -> EP w m Int
markLocatedAA EpAnnNotUsed  _  = return 1
markLocatedAA (EpAnn _ a _) f = markKw (f a) >> return 1

markLensAA :: (Monad m, Monoid w) => EpAnn a -> Lens a AddEpAnn -> EP w m (EpAnn a)
markLensAA EpAnnNotUsed  _  = return EpAnnNotUsed
markLensAA (EpAnn anc a cs) l = do
  a' <- markKw (view l a)
  return (EpAnn anc (set l a' a) cs)


-- TODO: removein favour of markEpAnnL
markLocatedAAL :: (Monad m, Monoid w) => EpAnn a -> (a -> [AddEpAnn]) -> AnnKeywordId -> EP w m Int
markLocatedAAL EpAnnNotUsed  _ _ = return 1
markLocatedAAL (EpAnn _ a _) f kw = go (f a)
  where
    go [] = return 1
    go (aa@(AddEpAnn kw' _):as)
      | kw' == kw = mark [aa] kw
      | otherwise = go as


markEpAnnLMS :: (Monad m, Monoid w)
  => EpAnn a -> Lens a [AddEpAnn] -> AnnKeywordId -> Maybe String -> EP w m (EpAnn a)
markEpAnnLMS an l kw Nothing = markEpAnnL an l kw
markEpAnnLMS EpAnnNotUsed  _ _ _ = return EpAnnNotUsed
markEpAnnLMS (EpAnn anc a cs) l kw (Just str) = do
  anns <- mapM go (view l a)
  return (EpAnn anc (set l anns a) cs)
  where
    go :: (Monad m, Monoid w) => AddEpAnn -> EP w m AddEpAnn
    go (AddEpAnn kw' r)
      | kw' == kw = do
          r' <- printStringAtAA r str
          return (AddEpAnn kw' r')
      | otherwise = return (AddEpAnn kw' r)

markEpAnnLMS' :: (Monad m, Monoid w)
                => EpAnn a -> Lens a AddEpAnn -> AnnKeywordId -> Maybe String -> EP w m (EpAnn a)
markEpAnnLMS' an l _kw Nothing = markLensKwA an l
markEpAnnLMS' EpAnnNotUsed  _ _ _ = return EpAnnNotUsed
markEpAnnLMS' (EpAnn anc a cs) l kw (Just str) = do
  anns <- go (view l a)
  return (EpAnn anc (set l anns a) cs)
  where
    go :: (Monad m, Monoid w) => AddEpAnn -> EP w m AddEpAnn
    go (AddEpAnn kw' r)
      | kw' == kw = do
          r' <- printStringAtAA r str
          return (AddEpAnn kw' r')
      | otherwise = return (AddEpAnn kw' r)

-- Deprecate in favour of markEpAnnLMS
markLocatedAALS :: (Monad m, Monoid w)
                => EpAnn a -> (a -> [AddEpAnn]) -> AnnKeywordId -> Maybe String -> EP w m Int
markLocatedAALS an f kw Nothing = markLocatedAAL an f kw
markLocatedAALS EpAnnNotUsed  _ _ _ = return 1
markLocatedAALS (EpAnn _ a _) f kw (Just str) = go (f a) >> return 1
  where
    go [] = return (1::Int)
    go (AddEpAnn kw' r:as)
      | kw' == kw = printStringAtAA r str >> return 1
      | otherwise = go as

-- ---------------------------------------------------------------------

markArrow :: (Monad m, Monoid w)
  => EpAnn TrailingAnn -> HsArrow GhcPs -> EP w m (EpAnn TrailingAnn, HsArrow GhcPs)
markArrow an arr = do
  arr' <-
    case arr of
      HsUnrestrictedArrow _u ->
        return arr
      HsLinearArrow u ma -> do
        ma' <- mapM markAddEpAnn ma
        return (HsLinearArrow u ma')
      HsExplicitMult u ma t  -> do
        ma' <- mapM markAddEpAnn ma
        t' <- markAnnotated t
        return (HsExplicitMult u ma' t')

  an' <- case an of
           EpAnnNotUsed -> pure EpAnnNotUsed
           EpAnn anc a cs -> do
             a' <- markKwT a
             return (EpAnn anc a' cs)
  return (an', arr')

-- ---------------------------------------------------------------------

markAnnCloseP :: (Monad m, Monoid w) => EpAnn AnnPragma -> EP w m (EpAnn AnnPragma)
markAnnCloseP an = markEpAnnLMS' an lapr_close AnnClose (Just "#-}")

markAnnOpenP :: (Monad m, Monoid w) => EpAnn AnnPragma -> SourceText -> String -> EP w m (EpAnn AnnPragma)
markAnnOpenP an NoSourceText txt   = markEpAnnLMS' an lapr_open AnnOpen (Just txt)
markAnnOpenP an (SourceText txt) _ = markEpAnnLMS' an lapr_open AnnOpen (Just txt)

markAnnOpen :: (Monad m, Monoid w) => EpAnn [AddEpAnn] -> SourceText -> String -> EP w m (EpAnn [AddEpAnn])
markAnnOpen an NoSourceText txt   = markEpAnnLMS an lidl AnnOpen (Just txt)
markAnnOpen an (SourceText txt) _ = markEpAnnLMS an lidl AnnOpen (Just txt)

markAnnOpen' :: (Monad m, Monoid w)
  => Maybe EpaLocation -> SourceText -> String -> EP w m (Maybe EpaLocation)
markAnnOpen' ms NoSourceText txt   = printStringAtMLoc' ms txt
markAnnOpen' ms (SourceText txt) _ = printStringAtMLoc' ms txt

markAnnOpen'' :: (Monad m, Monoid w)
  => EpaLocation -> SourceText -> String -> EP w m EpaLocation
markAnnOpen'' el NoSourceText txt   = printStringAtAA el txt
markAnnOpen'' el (SourceText txt) _ = printStringAtAA el txt

-- ---------------------------------------------------------------------
{-
data AnnParen
  = AnnParen {
      ap_adornment :: ParenType,
      ap_open      :: EpaLocation,
      ap_close     :: EpaLocation
      } deriving (Data)
-}
markOpeningParen, markClosingParen :: (Monad m, Monoid w) => EpAnn AnnParen -> EP w m (EpAnn AnnParen)
markOpeningParen an = markParen an lfst
markClosingParen an = markParen an lsnd

markParen :: (Monad m, Monoid w) => EpAnn AnnParen -> (forall a. Lens (a,a) a) -> EP w m (EpAnn AnnParen)
markParen EpAnnNotUsed _ = return (EpAnnNotUsed)
markParen (EpAnn anc (AnnParen pt o c) cs) l = do
  loc' <- markKwA (view l $ kw pt) (view l (o, c))
  let (o',c') = set l loc' (o,c)
  return (EpAnn anc (AnnParen pt o' c') cs)
  where
    kw AnnParens       = (AnnOpenP,  AnnCloseP)
    kw AnnParensHash   = (AnnOpenPH, AnnClosePH)
    kw AnnParensSquare = (AnnOpenS, AnnCloseS)
-- markParen' :: (Monad m, Monoid w) => EpAnn AnnParen -> (forall a. (a,a) -> a) -> EP w m (EpAnn AnnParen)
-- markParen' EpAnnNotUsed _ = return (EpAnnNotUsed)
-- markParen' (EpAnn anc (AnnParen pt o c) cs) f = do
--   markKwA (f $ kw pt) (f (o, c))
--   return (EpAnn anc (AnnParen pt o c) cs)
--   where
--     kw AnnParens       = (AnnOpenP,  AnnCloseP)
--     kw AnnParensHash   = (AnnOpenPH, AnnClosePH)
--     kw AnnParensSquare = (AnnOpenS, AnnCloseS)

-- ---------------------------------------------------------------------
-- Bare bones Optics
-- Base on From https://hackage.haskell.org/package/lens-tutorial-1.0.3/docs/Control-Lens-Tutorial.html

type Lens    a b = forall f . Functor f => (b -> f        b) -> (a -> f        a)
type Getting a b =                         (b -> Const  b b) -> (a -> Const b  a)
type ASetter a b =                         (b -> Identity b) -> (a -> Identity a)

view :: MonadReader s m => Getting s a -> m a
-- view l = Reader.asks (getConst #. l Const)
view l = Reader.asks (getConst . l Const)
{-# INLINE view #-}

over :: ASetter a b -> (b -> b) -> (a -> a)
-- over l f = runIdentity #. l (Identity #. f)
over l f = runIdentity . l (Identity . f)
{-# INLINE over #-}

set  :: Lens a b -> b -> a -> a
set lens b = over lens (\_ -> b)
{-# INLINE set #-}

{-
Question: How do I combine lenses?

Answer: You compose them, using function composition (Yes, really!)

You can think of the function composition operator as having this type:

(.) :: Lens' a b -> Lens' b c -> Lens' a c
-}

-- ---------------------------------------------------------------------
-- Lenses

lalLet :: Lens AnnsLet EpaLocation
lalLet k annsLet = fmap (\newLoc -> annsLet { alLet = newLoc })
                        (k (alLet annsLet))

lalIn :: Lens AnnsLet EpaLocation
lalIn k annsLet = fmap (\newLoc -> annsLet { alIn = newLoc })
                       (k (alIn annsLet))
-- data AnnsModule
--   = AnnsModule {
--     am_main :: [AddEpAnn],
--     am_decls :: AnnList
--     } deriving (Data, Eq)

lam_main :: Lens AnnsModule [AddEpAnn]
lam_main k annsModule = fmap (\newAnns -> annsModule { am_main = newAnns })
                             (k (am_main annsModule))

-- lam_decls :: Lens AnnsModule AnnList
-- lam_decls k annsModule = fmap (\newAnns -> annsModule { am_decls = newAnns })
--                               (k (am_decls annsModule))


-- data EpAnnImportDecl = EpAnnImportDecl
--   { importDeclAnnImport    :: EpaLocation
--   , importDeclAnnPragma    :: Maybe (EpaLocation, EpaLocation)
--   , importDeclAnnSafe      :: Maybe EpaLocation
--   , importDeclAnnQualified :: Maybe EpaLocation
--   , importDeclAnnPackage   :: Maybe EpaLocation
--   , importDeclAnnAs        :: Maybe EpaLocation
--   } deriving (Data)

limportDeclAnnImport :: Lens EpAnnImportDecl EpaLocation
limportDeclAnnImport k annImp = fmap (\new -> annImp { importDeclAnnImport = new })
                                     (k (importDeclAnnImport annImp))

-- limportDeclAnnPragma :: Lens EpAnnImportDecl (Maybe (EpaLocation, EpaLocation))
-- limportDeclAnnPragma k annImp = fmap (\new -> annImp { importDeclAnnPragma = new })
--                                      (k (importDeclAnnPragma annImp))

limportDeclAnnSafe :: Lens EpAnnImportDecl (Maybe EpaLocation)
limportDeclAnnSafe k annImp = fmap (\new -> annImp { importDeclAnnSafe = new })
                                     (k (importDeclAnnSafe annImp))

limportDeclAnnQualified :: Lens EpAnnImportDecl (Maybe EpaLocation)
limportDeclAnnQualified k annImp = fmap (\new -> annImp { importDeclAnnQualified = new })
                                     (k (importDeclAnnQualified annImp))

limportDeclAnnPackage :: Lens EpAnnImportDecl (Maybe EpaLocation)
limportDeclAnnPackage k annImp = fmap (\new -> annImp { importDeclAnnPackage = new })
                                     (k (importDeclAnnPackage annImp))

-- limportDeclAnnAs :: Lens EpAnnImportDecl (Maybe EpaLocation)
-- limportDeclAnnAs k annImp = fmap (\new -> annImp { importDeclAnnAs = new })
--                                      (k (importDeclAnnAs annImp))

-- -------------------------------------

-- data AnnList
--   = AnnList {
--       al_anchor    :: Maybe Anchor, -- ^ start point of a list having layout
--       al_open      :: Maybe AddEpAnn,
--       al_close     :: Maybe AddEpAnn,
--       al_rest      :: [AddEpAnn], -- ^ context, such as 'where' keyword
--       al_trailing  :: [TrailingAnn] -- ^ items appearing after the
--                                     -- list, such as '=>' for a
--                                     -- context
--       } deriving (Data,Eq)

lal_open :: Lens AnnList (Maybe AddEpAnn)
lal_open k parent = fmap (\new -> parent { al_open = new })
                           (k (al_open parent))

lal_close :: Lens AnnList (Maybe AddEpAnn)
lal_close k parent = fmap (\new -> parent { al_close = new })
                           (k (al_close parent))

lal_rest :: Lens AnnList [AddEpAnn]
lal_rest k parent = fmap (\new -> parent { al_rest = new })
                           (k (al_rest parent))

-- -------------------------------------

lapr_rest :: Lens AnnPragma [AddEpAnn]
lapr_rest k parent = fmap (\newAnns -> parent { apr_rest = newAnns })
                          (k (apr_rest parent))

lapr_open :: Lens AnnPragma AddEpAnn
lapr_open k parent = fmap (\new -> parent { apr_open = new })
                          (k (apr_open parent))

lapr_close :: Lens AnnPragma AddEpAnn
lapr_close k parent = fmap (\new -> parent { apr_close = new })
                          (k (apr_close parent))

lidl :: Lens [AddEpAnn] [AddEpAnn]
lidl k parent = fmap (\new -> new)
                     (k parent)

lid :: Lens a a
lid k parent = fmap (\new -> new)
                    (k parent)

lfst :: Lens (a,a) a
lfst k parent = fmap (\new -> (new, snd parent))
                     (k (fst parent))

lsnd :: Lens (a,a) a
lsnd k parent = fmap (\new -> (fst parent, new))
                     (k (snd parent))

-- -------------------------------------
-- data AnnExplicitSum
--   = AnnExplicitSum {
--       aesOpen       :: EpaLocation,
--       aesBarsBefore :: [EpaLocation],
--       aesBarsAfter  :: [EpaLocation],
--       aesClose      :: EpaLocation
--       } deriving Data

laesOpen :: Lens AnnExplicitSum EpaLocation
laesOpen k parent = fmap (\new -> parent { aesOpen = new })
                         (k (aesOpen parent))

laesBarsBefore :: Lens AnnExplicitSum [EpaLocation]
laesBarsBefore k parent = fmap (\new -> parent { aesBarsBefore = new })
                               (k (aesBarsBefore parent))

laesBarsAfter :: Lens AnnExplicitSum [EpaLocation]
laesBarsAfter k parent = fmap (\new -> parent { aesBarsAfter = new })
                               (k (aesBarsAfter parent))

laesClose :: Lens AnnExplicitSum EpaLocation
laesClose k parent = fmap (\new -> parent { aesClose = new })
                               (k (aesClose parent))

-- -------------------------------------
-- data AnnFieldLabel
--   = AnnFieldLabel {
--       afDot :: Maybe EpaLocation
--       } deriving Data

lafDot :: Lens AnnFieldLabel (Maybe EpaLocation)
lafDot k parent = fmap (\new -> parent { afDot = new })
                         (k (afDot parent))

-- -------------------------------------
-- data AnnProjection
--   = AnnProjection {
--       apOpen  :: EpaLocation, -- ^ '('
--       apClose :: EpaLocation  -- ^ ')'
--       } deriving Data

lapOpen :: Lens AnnProjection EpaLocation
lapOpen k parent = fmap (\new -> parent { apOpen = new })
                         (k (apOpen parent))

lapClose :: Lens AnnProjection EpaLocation
lapClose k parent = fmap (\new -> parent { apClose = new })
                         (k (apClose parent))

-- -------------------------------------
-- data AnnsIf
--   = AnnsIf {
--       aiIf       :: EpaLocation,
--       aiThen     :: EpaLocation,
--       aiElse     :: EpaLocation,
--       aiThenSemi :: Maybe EpaLocation,
--       aiElseSemi :: Maybe EpaLocation
--       } deriving Data

laiIf :: Lens AnnsIf EpaLocation
laiIf k parent = fmap (\new -> parent { aiIf = new })
                      (k (aiIf parent))

laiThen :: Lens AnnsIf EpaLocation
laiThen k parent = fmap (\new -> parent { aiThen = new })
                        (k (aiThen parent))

laiElse :: Lens AnnsIf EpaLocation
laiElse k parent = fmap (\new -> parent { aiElse = new })
                        (k (aiElse parent))

laiThenSemi :: Lens AnnsIf (Maybe EpaLocation)
laiThenSemi k parent = fmap (\new -> parent { aiThenSemi = new })
                            (k (aiThenSemi parent))

laiElseSemi :: Lens AnnsIf (Maybe EpaLocation)
laiElseSemi k parent = fmap (\new -> parent { aiElseSemi = new })
                            (k (aiElseSemi parent))

-- -------------------------------------

-- data AnnParen
--   = AnnParen {
--       ap_adornment :: ParenType,
--       ap_open      :: EpaLocation,
--       ap_close     :: EpaLocation
--       } deriving (Data)

lap_open :: Lens AnnParen EpaLocation
lap_open k parent = fmap (\new -> parent { ap_open = new })
                         (k (ap_open parent))

lap_close :: Lens AnnParen EpaLocation
lap_close k parent = fmap (\new -> parent { ap_close = new })
                          (k (ap_close parent))

-- -------------------------------------
-- data EpAnnHsCase = EpAnnHsCase
--       { hsCaseAnnCase :: EpaLocation
--       , hsCaseAnnOf   :: EpaLocation
--       , hsCaseAnnsRest :: [AddEpAnn]
--       } deriving Data

lhsCaseAnnCase :: Lens EpAnnHsCase EpaLocation
lhsCaseAnnCase k parent = fmap (\new -> parent { hsCaseAnnCase = new })
                               (k (hsCaseAnnCase parent))

lhsCaseAnnOf :: Lens EpAnnHsCase EpaLocation
lhsCaseAnnOf k parent = fmap (\new -> parent { hsCaseAnnOf = new })
                               (k (hsCaseAnnOf parent))

lhsCaseAnnsRest :: Lens EpAnnHsCase [AddEpAnn]
lhsCaseAnnsRest k parent = fmap (\new -> parent { hsCaseAnnsRest = new })
                                (k (hsCaseAnnsRest parent))

-- ---------------------------------------------------------------------

-- data HsRuleAnn
--   = HsRuleAnn
--        { ra_tyanns :: Maybe (AddEpAnn, AddEpAnn)
--                  -- ^ The locations of 'forall' and '.' for forall'd type vars
--                  -- Using AddEpAnn to capture possible unicode variants
--        , ra_tmanns :: Maybe (AddEpAnn, AddEpAnn)
--                  -- ^ The locations of 'forall' and '.' for forall'd term vars
--                  -- Using AddEpAnn to capture possible unicode variants
--        , ra_rest :: [AddEpAnn]
--        } deriving (Data, Eq)

lra_tyanns :: Lens HsRuleAnn (Maybe (AddEpAnn, AddEpAnn))
lra_tyanns k parent = fmap (\new -> parent { ra_tyanns = new })
                               (k (ra_tyanns parent))

ff :: Maybe (a,b) -> (Maybe a,Maybe b)
ff Nothing = (Nothing, Nothing)
ff (Just (a,b)) = (Just a, Just b)


gg :: (Maybe a,Maybe b) -> Maybe (a,b)
gg (Nothing, Nothing) = Nothing
gg (Just a, Just b) = Just (a,b)
gg _ = error "gg:expecting two Nothing or two Just"

lff :: Lens (Maybe (a,b)) (Maybe a,Maybe b)
lff k parent = fmap (\new -> gg new)
                    (k (ff parent))

-- (.) :: Lens' a b -> Lens' b c -> Lens' a c
lra_tyanns_fst :: Lens HsRuleAnn (Maybe AddEpAnn)
lra_tyanns_fst = lra_tyanns . lff . lfst

lra_tyanns_snd :: Lens HsRuleAnn (Maybe AddEpAnn)
lra_tyanns_snd = lra_tyanns . lff . lsnd

lra_tmanns :: Lens HsRuleAnn (Maybe (AddEpAnn, AddEpAnn))
lra_tmanns k parent = fmap (\new -> parent { ra_tmanns = new })
                               (k (ra_tmanns parent))

lra_tmanns_fst :: Lens HsRuleAnn (Maybe AddEpAnn)
lra_tmanns_fst = lra_tmanns . lff . lfst

lra_tmanns_snd :: Lens HsRuleAnn (Maybe AddEpAnn)
lra_tmanns_snd = lra_tmanns . lff . lsnd

lra_rest :: Lens HsRuleAnn [AddEpAnn]
lra_rest k parent = fmap (\new -> parent { ra_rest = new })
                                (k (ra_rest parent))


-- ---------------------------------------------------------------------
-- data GrhsAnn
--   = GrhsAnn {
--       ga_vbar :: Maybe EpaLocation, -- TODO:AZ do we need this?
--       ga_sep  :: AddEpAnn -- ^ Match separator location
--       } deriving (Data)

lga_vbar :: Lens GrhsAnn (Maybe EpaLocation)
lga_vbar k parent = fmap (\new -> parent { ga_vbar = new })
                                (k (ga_vbar parent))

lga_sep :: Lens GrhsAnn AddEpAnn
lga_sep k parent = fmap (\new -> parent { ga_sep = new })
                                (k (ga_sep parent))

-- ---------------------------------------------------------------------
-- data AnnSig
--   = AnnSig {
--       asDcolon :: AddEpAnn, -- Not an EpaAnchor to capture unicode option
--       asRest   :: [AddEpAnn]
--       } deriving Data

lasDcolon :: Lens AnnSig AddEpAnn
lasDcolon k parent = fmap (\new -> parent { asDcolon = new })
                                (k (asDcolon parent))

lasRest :: Lens AnnSig [AddEpAnn]
lasRest k parent = fmap (\new -> parent { asRest = new })
                                (k (asRest parent))

-- ---------------------------------------------------------------------
-- data EpAnnSumPat = EpAnnSumPat
--       { sumPatParens      :: [AddEpAnn]
--       , sumPatVbarsBefore :: [EpaLocation]
--       , sumPatVbarsAfter  :: [EpaLocation]
--       } deriving Data

lsumPatParens :: Lens EpAnnSumPat [AddEpAnn]
lsumPatParens k parent = fmap (\new -> parent { sumPatParens = new })
                              (k (sumPatParens parent))

lsumPatVbarsBefore :: Lens EpAnnSumPat [EpaLocation]
lsumPatVbarsBefore k parent = fmap (\new -> parent { sumPatVbarsBefore = new })
                              (k (sumPatVbarsBefore parent))

lsumPatVbarsAfter :: Lens EpAnnSumPat [EpaLocation]
lsumPatVbarsAfter k parent = fmap (\new -> parent { sumPatVbarsAfter = new })
                              (k (sumPatVbarsAfter parent))

-- End of lenses
-- ---------------------------------------------------------------------

markLensKwA :: (Monad m, Monoid w)
  => EpAnn a -> Lens a AddEpAnn -> EP w m (EpAnn a)
markLensKwA EpAnnNotUsed  _    = return EpAnnNotUsed
markLensKwA (EpAnn anc a cs) l = do
  loc <- markKw (view l a)
  return (EpAnn anc (set l loc a) cs)

markLensKw :: (Monad m, Monoid w)
  => EpAnn a -> Lens a EpaLocation -> AnnKeywordId -> EP w m (EpAnn a)
markLensKw EpAnnNotUsed  _ _  = return EpAnnNotUsed
markLensKw (EpAnn anc a cs) l kw = do
  loc <- markKwA kw (view l a)
  return (EpAnn anc (set l loc a) cs)

-- TODO: get rid of this, in favour of markAnnKwL (or markLensKw).
markAnnKw :: (Monad m, Monoid w) => EpAnn a -> (a -> EpaLocation) -> AnnKeywordId -> EP w m Int
markAnnKw EpAnnNotUsed  _ _  = return 1
markAnnKw (EpAnn _anc a _cs) f kw = markKwA kw (f a) >> return 1

markAnnKwL :: (Monad m, Monoid w)
  => EpAnn a -> Lens a EpaLocation -> AnnKeywordId -> EP w m (EpAnn a)
markAnnKwL = markLensKw

-- TODO: get rid of this, in favour of markAnnKwAllL.
markAnnKwAll :: (Monad m, Monoid w) => EpAnn a -> (a -> [EpaLocation]) -> AnnKeywordId -> EP w m Int
markAnnKwAll EpAnnNotUsed  _ _  = return 1
-- markAnnKwAll (EpAnn _ a _) f kw = mapM_ (markKwA kw) (sort (f a))
markAnnKwAll (EpAnn _ a _) f kw = mapM_ (markKwA kw) (f a) >> return 1

markAnnKwAllL :: (Monad m, Monoid w)
  => EpAnn a -> Lens a [EpaLocation] -> AnnKeywordId -> EP w m (EpAnn a)
markAnnKwAllL EpAnnNotUsed  _ _  = return EpAnnNotUsed
-- markAnnKwAllL (EpAnn _ a _) f kw = mapM_ (markKwA kw) (sort (f a))
markAnnKwAllL (EpAnn anc a cs) l kw = do
  -- anns <- mapM (markKwA kw) (sort (view l a))
  anns <- mapM (markKwA kw) (view l a)
  return (EpAnn anc (set l anns a) cs)

-- TODO: get rid of this, in favour of markLensKwM.
markAnnKwM :: (Monad m, Monoid w) => EpAnn a -> (a -> Maybe EpaLocation) -> AnnKeywordId -> EP w m Int
markAnnKwM EpAnnNotUsed  _ _ = return 1
markAnnKwM (EpAnn _ a _) f kw = go (f a) >> return 1
  where
    go Nothing = return 1
    go (Just s) = markKwA kw s >> return 1

markLensKwM :: (Monad m, Monoid w)
  => EpAnn a -> Lens a (Maybe EpaLocation) -> AnnKeywordId -> EP w m (EpAnn a)
markLensKwM EpAnnNotUsed  _ _ = return EpAnnNotUsed
markLensKwM (EpAnn anc a cs) l kw = do
  new <- go (view l a)
  return (EpAnn anc (set l new a) cs)
  where
    go Nothing = return Nothing
    go (Just s) = Just <$> markKwA kw s

-- ---------------------------------------------------------------------

markALocatedA :: (Monad m, Monoid w) => EpAnn AnnListItem -> EP w m (EpAnn AnnListItem)
markALocatedA EpAnnNotUsed  = return EpAnnNotUsed
markALocatedA (EpAnn anc a cs) = do
  t <- markTrailing (lann_trailing a)
  return (EpAnn anc (a { lann_trailing = t }) cs)

-- Deprecate in favour of markEpAnnL
markEpAnn :: (Monad m, Monoid w)
  => EpAnn [AddEpAnn] -> AnnKeywordId -> EP w m Int -- Return something to trigger not used warning
markEpAnn EpAnnNotUsed _ = return 1
markEpAnn (EpAnn _ a _) kw = mark a kw >> return 1

-- -- Deprecate in favour of markEpAnnL
-- markEpAnn' :: (Monad m, Monoid w)
--   => EpAnn ann -> (ann -> [AddEpAnn]) -> AnnKeywordId -> EP w m ()
-- markEpAnn' EpAnnNotUsed _ _ = return ()
-- markEpAnn' (EpAnn _anc a _cs) f kw = mark' (f a) kw >> return ()

markEpAnnL :: (Monad m, Monoid w)
  => EpAnn ann -> Lens ann [AddEpAnn] -> AnnKeywordId -> EP w m (EpAnn ann)
markEpAnnL EpAnnNotUsed _ _ = return EpAnnNotUsed
markEpAnnL (EpAnn anc a cs) l kw = do
  anns <- mark' (view l a) kw
  return (EpAnn anc (set l anns a) cs)

-- Deprecate in favour of markEpAnnAllL
markEpAnnAll :: (Monad m, Monoid w)
  => EpAnn ann -> (ann -> [AddEpAnn]) -> AnnKeywordId -> EP w m Int
markEpAnnAll EpAnnNotUsed _ _ = return 1
-- markEpAnnAll (EpAnn _ a _) f kw = mapM_ markKw (sort anns)
markEpAnnAll (EpAnn _ a _) f kw = mapM_ markKw anns >> return 1
  where
    anns = filter (\(AddEpAnn ka _) -> ka == kw) (f a)

markEpAnnAllL :: (Monad m, Monoid w)
  => EpAnn ann -> Lens ann [AddEpAnn] -> AnnKeywordId -> EP w m (EpAnn ann)
markEpAnnAllL EpAnnNotUsed _ _ = return EpAnnNotUsed
markEpAnnAllL (EpAnn anc a cs) l kw = do
  anns <- mapM doit (view l a)
  return (EpAnn anc (set l anns a) cs)
  where
    doit an@(AddEpAnn ka _)
      = if ka == kw
          then markKw an
          else return an

markAnnAll :: (Monad m, Monoid w) => [AddEpAnn] -> AnnKeywordId -> EP w m [AddEpAnn]
markAnnAll a kw = mapM doit a
  where
    doit an@(AddEpAnn ka _)
      = if ka == kw
          then markKw an
          else return an

markAddEpAnn :: (Monad m, Monoid w) => AddEpAnn -> EP w m AddEpAnn
markAddEpAnn a@(AddEpAnn kw _) = do
  r <- mark' [a] kw
  case r of
    [a'] -> return a'
    _ -> error "Should not happen: markAddEpAnn"

mark :: (Monad m, Monoid w) => [AddEpAnn] -> AnnKeywordId -> EP w m Int
mark anns kw = do
  case find (\(AddEpAnn k _) -> k == kw) anns of
    Just aa -> markKw aa >> return 1
    Nothing -> case find (\(AddEpAnn k _) -> k == (unicodeAnn kw)) anns of
      Just aau -> markKw aau >> return 1
      Nothing -> return 1

mark' :: (Monad m, Monoid w) => [AddEpAnn] -> AnnKeywordId -> EP w m [AddEpAnn]
mark' anns kw = do
  case find' kw anns of
    (lead, Just aa, end) -> do
      aa' <- markKw aa
      return (lead ++ [aa'] ++ end)
    (_lead, Nothing, _end) -> case find' (unicodeAnn kw) anns of
      (leadu, Just aau, endu) -> do
        aau' <- markKw aau
        return (leadu ++ [aau'] ++ endu)
      (_,Nothing,_) -> return anns

-- | Find for update, returning lead section of the list, item if
-- found, and tail of the list
find' :: AnnKeywordId -> [AddEpAnn] -> ([AddEpAnn], Maybe AddEpAnn, [AddEpAnn])
find' kw anns = (lead, middle, end)
  where
    (lead, rest) = break (\(AddEpAnn k _) -> k == kw) anns
    (middle,end) = case rest of
      [] -> (Nothing, [])
      (x:xs) -> (Just x, xs)

markKw :: (Monad m, Monoid w) => AddEpAnn -> EP w m AddEpAnn
markKw an = markKwC CaptureComments an

markKwC :: (Monad m, Monoid w) => CaptureComments -> AddEpAnn -> EP w m AddEpAnn
markKwC capture (AddEpAnn kw ss) = do
  ss' <- markKwAC capture kw ss
  return (AddEpAnn kw ss')

-- | This should be the main driver of the process, managing printing keywords.
-- It returns the 'EpaDelta' variant of the passed in 'EpaLocation'
markKwA :: (Monad m, Monoid w) => AnnKeywordId -> EpaLocation -> EP w m EpaLocation
markKwA kw aa = markKwAC CaptureComments kw aa

markKwAC :: (Monad m, Monoid w)
  => CaptureComments -> AnnKeywordId -> EpaLocation -> EP w m EpaLocation
markKwAC capture kw aa = printStringAtAAC capture aa (keywordToString kw)

-- | Print a keyword encoded in a 'TrailingAnn'
markKwT :: (Monad m, Monoid w) => TrailingAnn -> EP w m TrailingAnn
markKwT (AddSemiAnn ss)    = AddSemiAnn    <$> markKwA AnnSemi ss
markKwT (AddCommaAnn ss)   = AddCommaAnn   <$> markKwA AnnComma ss
markKwT (AddVbarAnn ss)    = AddVbarAnn    <$> markKwA AnnVbar ss
markKwT (AddRarrowAnn ss)  = AddRarrowAnn  <$> markKwA AnnRarrow ss
markKwT (AddRarrowAnnU ss) = AddRarrowAnnU <$> markKwA AnnRarrowU ss
markKwT (AddLollyAnnU ss)  = AddLollyAnnU  <$> markKwA AnnLollyU ss

-- ---------------------------------------------------------------------

markAnnList :: (Monad m, Monoid w)
  => Bool -> EpAnn AnnList -> EP w m a -> EP w m (EpAnn AnnList, a)
markAnnList _ EpAnnNotUsed action = do
  a <- action
  return (EpAnnNotUsed, a)
markAnnList reallyTrail (EpAnn anc ann cs) action = do
  (ann', a') <- markAnnList' reallyTrail ann action
  return (EpAnn anc ann' cs, a')

markAnnList' :: (Monad m, Monoid w)
  => Bool -> AnnList -> EP w m a -> EP w m (AnnList, a)
markAnnList' reallyTrail ann action = do
  p <- getPosP
  debugM $ "markAnnList : " ++ showPprUnsafe (p, ann)
  al_open' <- mapM markAddEpAnn (al_open ann)
  ann0 <- markTrailIf (not reallyTrail) ann -- Only makes sense for HsModule.
  -- al_rest' <- markAnnAll (sort $ al_rest ann) AnnSemi
  al_rest' <- markAnnAll (al_rest ann) AnnSemi
  r <- action
  al_close' <- mapM markAddEpAnn (al_close ann)
  debugM $ "markAnnList: calling markTrailing with:" ++ showPprUnsafe (al_trailing ann)
  ann1 <- markTrailIf reallyTrail ann0 -- normal case
  return (ann1 { al_open = al_open', al_rest = al_rest', al_close = al_close'}
         , r)

markTrailIf :: (Monad m, Monoid w) => Bool -> AnnList -> EP w m AnnList
markTrailIf cond ann =
  if cond
    then do
      at' <- markTrailing (al_trailing ann) -- Only makes sense for HsModule.
      return (ann{ al_trailing = at'})
    else return ann

-- ---------------------------------------------------------------------

printComments :: (Monad m, Monoid w) => RealSrcSpan -> EP w m ()
printComments ss = do
  cs <- commentAllocation ss
  debugM $ "printComments: (ss): " ++ showPprUnsafe (rs2range ss)
  -- debugM $ "printComments: (ss,comment locations): " ++ showPprUnsafe (rs2range ss,map commentAnchor cs)
  mapM_ printOneComment cs

-- ---------------------------------------------------------------------

printOneComment :: (Monad m, Monoid w) => Comment -> EP w m ()
printOneComment c@(Comment _str loc _r _mo) = do
  debugM $ "printOneComment:c=" ++ showGhc c
  dp <-case anchor_op loc of
    MovedAnchor dp -> return dp
    _ -> do
        pe <- getPriorEndD
        let dp = ss2delta pe (anchor loc)
        -- debugM $ "printOneComment:(dp,pe,anchor loc)=" ++ showGhc (dp,pe,ss2pos $ anchor loc)
        adjustDeltaForOffsetM dp
  mep <- getExtraDP
  dp' <- case mep of
    Just (Anchor _ (MovedAnchor edp)) -> do
      debugM $ "printOneComment:edp=" ++ show edp
      adjustDeltaForOffsetM edp
    _ -> return dp
  -- Start of debug printing
  -- LayoutStartCol dOff <- gets dLHS
  -- debugM $ "printOneComment:(dp,dp',dOff)=" ++ showGhc (dp,dp',dOff)
  -- End of debug printing
  setPriorEndD (ss2posEnd (anchor loc))
  updateAndApplyComment c dp'
  printQueuedComment (anchor loc) c dp'

updateAndApplyComment :: (Monad m, Monoid w) => Comment -> DeltaPos -> EP w m ()
updateAndApplyComment co@(Comment str anc pp mo) dp = do
  debugM $ "updateAndApplyComment: (dp,anc',co)=" ++ showAst (dp,anc',co)
  applyComment (Comment str anc' pp mo)
  where
    -- anc' = anc { anchor_op = MovedAnchor dp }
    anc' = anc { anchor_op = op}

    (r,c) = ss2posEnd pp
    la = anchor anc
    dp'' = if r == 0
           then (ss2delta (r,c+1) la)
           else (ss2delta (r,c)   la)
    dp' = if pp == anchor anc
             then dp
             else dp''
    op' = case dp' of
            SameLine n -> if n >= 0
                            then MovedAnchor dp'
                            else MovedAnchor dp
            _ -> MovedAnchor dp'
    op = if str == "" && op' == MovedAnchor (SameLine 0) -- EOF comment
           then MovedAnchor dp
           -- else op'
           else MovedAnchor dp

-- ---------------------------------------------------------------------

commentAllocation :: (Monad m, Monoid w) => RealSrcSpan -> EP w m [Comment]
commentAllocation ss = do
  cs <- getUnallocatedComments
  -- Note: The CPP comment injection may change the file name in the
  -- RealSrcSpan, which affects comparison, as the Ord instance for
  -- RealSrcSpan compares the file first. So we sort via ss2pos
  -- TODO: this is inefficient, use Pos all the way through
  let (earlier,later) = partition (\(Comment _str loc _r _mo) -> (ss2pos $ anchor loc) <= (ss2pos ss)) cs
  putUnallocatedComments later
  -- debugM $ "commentAllocation:(ss,earlier,later)" ++ show (rs2range ss,earlier,later)
  return earlier

-- ---------------------------------------------------------------------

markAnnotatedWithLayout :: (Monad m, Monoid w) => ExactPrint ast => ast -> EP w m ast
markAnnotatedWithLayout a = setLayoutBoth $ markAnnotated a

-- ---------------------------------------------------------------------

markTopLevelList :: (Monad m, Monoid w) => ExactPrint ast => [ast] -> EP w m [ast]
markTopLevelList ls = mapM (\a -> setLayoutTopLevelP $ markAnnotated a) ls

-- ---------------------------------------------------------------------
-- End of utility functions
-- ---------------------------------------------------------------------
-- Start of ExactPrint instances
-- ---------------------------------------------------------------------


-- | Bare Located elements are simply stripped off without further
-- processing.
instance (ExactPrint a) => ExactPrint (Located a) where
  -- getAnnotationEntry (L l _) = Entry (spanAsAnchor l) emptyComments NoFlushComments NoCanUpdateAnchor
  getAnnotationEntry (L l _) = Entry (hackSrcSpanToAnchor l) emptyComments NoFlushComments CanUpdateAnchorOnly
  -- getAnnotationEntry (L l _) = NoEntryVal

  -- setAnnotationAnchor _la _anc _cs = error "should not be called:setAnnotationAnchor (Located a)"
  setAnnotationAnchor (L l a) anc cs = L (hackAnchorToSrcSpan anc) a

  exact (L l a) = L l <$> markAnnotated a

instance (ExactPrint a) => ExactPrint (LocatedA a) where
  getAnnotationEntry = entryFromLocatedA
  setAnnotationAnchor la anc cs = setAnchorAn la anc cs
  exact (L la a) = do
    debugM $ "LocatedA a:la loc=" ++ show (ss2range $ locA la)
    a' <- markAnnotated a
    ann' <- markALocatedA (ann la)
    return (L (la { ann = ann'}) a')

instance (ExactPrint a) => ExactPrint [a] where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor ls _ _ = ls
  exact ls = mapM markAnnotated ls

instance (ExactPrint a) => ExactPrint (Maybe a) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor ma _ _ = ma
  exact ma = mapM markAnnotated ma

-- ---------------------------------------------------------------------

-- | 'Located (HsModule GhcPs)' corresponds to 'ParsedSource'
instance ExactPrint HsModule where
  getAnnotationEntry hsmod = fromAnn' (hsmodAnn hsmod)
  -- A bit pointless actually changing anything here
  setAnnotationAnchor hsmod anc cs = setAnchorHsModule hsmod anc cs
                   `debug` ("setAnnotationAnchor hsmod called" ++ showAst (anc,cs))

  exact hsmod@(HsModule EpAnnNotUsed _ _ _ _ _ _ _) = withPpr hsmod >> return hsmod
  exact (HsModule an lo mmn mexports imports decls mdeprec mbDoc) = do

    debugM "HsModule Entered"

    mbDoc' <- markAnnotated mbDoc

    (an0, mmn' , mdeprec', mexports') <-
      case mmn of
        Nothing -> return (an, mmn, mdeprec, mexports)
        Just m -> do
          an0 <- markEpAnnL an lam_main AnnModule
          m' <- markAnnotated m

          mdeprec' <- setLayoutTopLevelP $ markAnnotated mdeprec

          mexports' <- setLayoutTopLevelP $ markAnnotated mexports

          debugM $ "HsModule.AnnWhere coming"
          an1 <- setLayoutTopLevelP $ markEpAnnL an0 lam_main AnnWhere
          debugM $ "HsModule.AnnWhere done, an1=" ++ showAst (anns an1)

          return (an1, Just m', mdeprec', mexports')

    debugM $ "After HsModule.AnnWhere done, an0=" ++ showAst (anns an0)
    (am_decls', (decls', imports')) <- markAnnList' False (am_decls $ anns an0) $ do
      imports' <- markTopLevelList imports
      decls' <- markTopLevelList decls
      return (decls', imports')

    let anf = an0 { anns = (anns an0) { am_decls = am_decls' }}
    debugM $ "HsModule, anf=" ++ showAst anf

    return (HsModule anf lo mmn' mexports' imports' decls' mdeprec' mbDoc')

-- ---------------------------------------------------------------------

instance ExactPrint ModuleName where
  getAnnotationEntry _ = NoEntryVal
  setAnnotationAnchor n _anc cs = n
     `debug` ("ModuleName.setAnnotationAnchor:cs=" ++ showAst cs)
  exact n = do
    debugM $ "ModuleName: " ++ showPprUnsafe n
    withPpr n

-- ---------------------------------------------------------------------

instance ExactPrint (LocatedP WarningTxt) where
  getAnnotationEntry = entryFromLocatedA
  setAnnotationAnchor = setAnchorAn

  exact (L (SrcSpanAnn an l) (WarningTxt (L la src) ws)) = do
    an0 <- markAnnOpenP an src "{-# WARNING"
    an1 <- markEpAnnL an0 lapr_rest AnnOpenS
    ws' <- markAnnotated ws
    an2 <- markEpAnnL an1 lapr_rest AnnCloseS
    an3 <- markAnnCloseP an2
    return (L (SrcSpanAnn an3 l) (WarningTxt (L la src) ws'))

  exact (L (SrcSpanAnn an l) (DeprecatedTxt (L ls src) ws)) = do
    an0 <- markAnnOpenP an src "{-# DEPRECATED"
    an1 <- markEpAnnL an0 lapr_rest AnnOpenS
    ws' <- markAnnotated ws
    an2 <- markEpAnnL an1 lapr_rest AnnCloseS
    an3 <- markAnnCloseP an2
    return (L (SrcSpanAnn an3 l) (DeprecatedTxt (L ls src) ws'))

-- ---------------------------------------------------------------------

instance ExactPrint (ImportDecl GhcPs) where
  getAnnotationEntry idecl = fromAnn (ideclExt idecl)
  setAnnotationAnchor idecl anc cs = idecl { ideclExt = setAnchorEpa (ideclExt idecl) anc cs }

  exact x@(ImportDecl EpAnnNotUsed _ _ _ _ _ _ _ _ _) = withPpr x
  exact (ImportDecl ann msrc m mpkg src safeflag qualFlag impl mAs hiding) = do

    ann0 <- markLensKw ann limportDeclAnnImport AnnImport
    let (EpAnn _anc an _cs) = ann0

    -- "{-# SOURCE" and "#-}"
    importDeclAnnPragma' <-
      case msrc of
        SourceText _txt -> do
          debugM $ "ImportDecl sourcetext"
          case importDeclAnnPragma an of
            Just (mo, mc) -> do
              mo' <- markAnnOpen'' mo msrc "{-# SOURCE"
              mc' <- printStringAtAA mc "#-}"
              return $ Just (mo', mc')
            Nothing ->  do
              _ <- markAnnOpen' Nothing msrc "{-# SOURCE"
              printStringAtMLoc Nothing "#-}"
              return Nothing
        NoSourceText -> return (importDeclAnnPragma an)
    ann1 <- if safeflag
      then (markLensKwM ann0 limportDeclAnnSafe AnnSafe)
      else return ann0
    ann2 <-
      case qualFlag of
        QualifiedPre  -- 'qualified' appears in prepositive position.
          -> printStringAtMLocL ann1 limportDeclAnnQualified "qualified"
        _ -> return ann1
    ann3 <-
      case mpkg of
       Just (StringLiteral src' v _) ->
         printStringAtMLocL ann2 limportDeclAnnPackage (sourceTextToString src' (show v))
       _ -> return ann2

    m' <- markAnnotated m

    ann4 <-
      case qualFlag of
        QualifiedPost  -- 'qualified' appears in postpositive position.
          -> printStringAtMLocL ann3 limportDeclAnnQualified "qualified"
        _ -> return ann3

    (importDeclAnnAs', mAs') <-
      case mAs of
        Nothing -> return (importDeclAnnAs an, Nothing)
        Just m0 -> do
          a <- printStringAtMLoc' (importDeclAnnAs an) "as"
          m'' <- markAnnotated m0
          return (a, Just m'')

    hiding' <-
      case hiding of
        Nothing -> return hiding
        Just (isHiding,lie) -> do
          lie' <- markAnnotated lie
          return (Just (isHiding, lie'))

    let (EpAnn anc' an' cs') = ann4
    let an2 = an' { importDeclAnnAs = importDeclAnnAs'
                  , importDeclAnnPragma = importDeclAnnPragma'
                  }

    return (ImportDecl (EpAnn anc' an2 cs') msrc m' mpkg src safeflag qualFlag impl mAs' hiding')


-- ---------------------------------------------------------------------

instance ExactPrint HsDocString where
  getAnnotationEntry _ = NoEntryVal
  setAnnotationAnchor a _ _ = a
  exact = withPpr -- TODO:AZ use annotations

-- ---------------------------------------------------------------------

instance ExactPrint (HsDecl GhcPs) where
  getAnnotationEntry (TyClD      _ _) = NoEntryVal
  getAnnotationEntry (InstD      _ _) = NoEntryVal
  getAnnotationEntry (DerivD     _ _) = NoEntryVal
  getAnnotationEntry (ValD       _ _) = NoEntryVal
  getAnnotationEntry (SigD       _ _) = NoEntryVal
  getAnnotationEntry (KindSigD   _ _) = NoEntryVal
  getAnnotationEntry (DefD       _ _) = NoEntryVal
  getAnnotationEntry (ForD       _ _) = NoEntryVal
  getAnnotationEntry (WarningD   _ _) = NoEntryVal
  getAnnotationEntry (AnnD       _ _) = NoEntryVal
  getAnnotationEntry (RuleD      _ _) = NoEntryVal
  getAnnotationEntry (SpliceD    _ _) = NoEntryVal
  getAnnotationEntry (DocD       _ _) = NoEntryVal
  getAnnotationEntry (RoleAnnotD _ _) = NoEntryVal

  -- We do not recurse, the generic traversal using this feature
  -- should do that for us.
  setAnnotationAnchor d _ _ = d

  exact (TyClD       x d) = TyClD       x <$> markAnnotated d
  exact (InstD       x d) = InstD       x <$> markAnnotated d
  exact (DerivD      x d) = DerivD      x <$> markAnnotated d
  exact (ValD        x d) = ValD        x <$> markAnnotated d
  exact (SigD        x d) = SigD        x <$> markAnnotated d
  exact (KindSigD    x d) = KindSigD    x <$> markAnnotated d
  exact (DefD        x d) = DefD        x <$> markAnnotated d
  exact (ForD        x d) = ForD        x <$> markAnnotated d
  exact (WarningD    x d) = WarningD    x <$> markAnnotated d
  exact (AnnD        x d) = AnnD        x <$> markAnnotated d
  exact (RuleD       x d) = RuleD       x <$> markAnnotated d
  exact (SpliceD     x d) = SpliceD     x <$> markAnnotated d
  exact (DocD        x d) = DocD        x <$> markAnnotated d
  exact (RoleAnnotD  x d) = RoleAnnotD  x <$> markAnnotated d

-- ---------------------------------------------------------------------

instance ExactPrint (InstDecl GhcPs) where
  getAnnotationEntry (ClsInstD     _  _) = NoEntryVal
  getAnnotationEntry (DataFamInstD an _) = fromAnn an
  getAnnotationEntry (TyFamInstD   _  _) = NoEntryVal

  setAnnotationAnchor (DataFamInstD an d) anc cs = DataFamInstD (setAnchorEpa an anc cs) d
  setAnnotationAnchor d _ _ = d


  exact (ClsInstD     a  cid) = do
    cid' <- markAnnotated cid
    return (ClsInstD     a  cid')
  exact (DataFamInstD an decl) = do
    d' <- markAnnotated (DataFamInstDeclWithContext an TopLevel decl)
    return (DataFamInstD an (dc_d d'))
  exact (TyFamInstD a eqn) = do
    eqn' <- markAnnotated eqn
    return (TyFamInstD a eqn')

-- ---------------------------------------------------------------------

data DataFamInstDeclWithContext
  = DataFamInstDeclWithContext
    { _dc_a :: EpAnn [AddEpAnn]
    , _dc_f :: TopLevelFlag
    , dc_d :: DataFamInstDecl GhcPs
    }

instance ExactPrint DataFamInstDeclWithContext where
  getAnnotationEntry (DataFamInstDeclWithContext _ _ (DataFamInstDecl (FamEqn { feqn_ext = an})))
    = fromAnn an
  setAnnotationAnchor (DataFamInstDeclWithContext a c (DataFamInstDecl fe)) anc cs
    = (DataFamInstDeclWithContext a c (DataFamInstDecl (fe { feqn_ext = (setAnchorEpa (feqn_ext fe) anc cs)})))
  exact (DataFamInstDeclWithContext an c d) = do
    debugM $ "starting DataFamInstDeclWithContext:an=" ++ showAst an
    (an', d') <- exactDataFamInstDecl an c d
    return (DataFamInstDeclWithContext an' c d')

-- ---------------------------------------------------------------------

exactDataFamInstDecl :: (Monad m, Monoid w)
                     => EpAnn [AddEpAnn] -> TopLevelFlag -> DataFamInstDecl GhcPs
                     -> EP w m (EpAnn [AddEpAnn], DataFamInstDecl GhcPs)
exactDataFamInstDecl an top_lvl
  (DataFamInstDecl (FamEqn { feqn_ext    = an2
                           , feqn_tycon  = tycon
                           , feqn_bndrs  = bndrs
                           , feqn_pats   = pats
                           , feqn_fixity = fixity
                           , feqn_rhs    = defn })) = do
    (an', an2', tycon', bndrs', _,  _mc, defn') <- exactDataDefn an2 pp_hdr defn
    return
      (an',
       DataFamInstDecl ( FamEqn { feqn_ext    = an2'
                                , feqn_tycon  = tycon'
                                , feqn_bndrs  = bndrs'
                                , feqn_pats   = pats
                                , feqn_fixity = fixity
                                , feqn_rhs    = defn' }))
                    `debug` ("exactDataFamInstDecl: defn' derivs:" ++ showAst (dd_derivs defn'))
  where
    pp_hdr :: (Monad m, Monoid w)
           => Maybe (LHsContext GhcPs)
           -> EP w m ( EpAnn [AddEpAnn]
                     , LocatedN RdrName
                     , HsOuterTyVarBndrs () GhcPs
                     , HsTyPats GhcPs
                     , Maybe (LHsContext GhcPs))
    pp_hdr mctxt = do
      an0 <- case top_lvl of
               TopLevel -> markEpAnnL an lidl AnnInstance -- TODO: maybe in toplevel
               NotTopLevel -> return an
      exactHsFamInstLHS an0 tycon bndrs pats fixity mctxt

{-
Note [an and an2 in exactDataFamInstDecl]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The exactDataFamInstDecl function is called to render a
DataFamInstDecl within its surrounding context. This context is
rendered via the 'pp_hdr' function, which uses the exact print
annotations from that context, named 'an'.  The EPAs used for
rendering the DataDefn are contained in the FamEqn, and are called
'an2'.

-}

-- ---------------------------------------------------------------------

instance ExactPrint (DerivDecl GhcPs) where
  getAnnotationEntry (DerivDecl {deriv_ext = an} ) = fromAnn an
  setAnnotationAnchor dd anc cs = dd { deriv_ext = setAnchorEpa (deriv_ext dd) anc cs }
  exact (DerivDecl an typ ms mov) = do
    markEpAnn an AnnDeriving
    ms' <- mapM markAnnotated ms
    markEpAnn an AnnInstance
    mov' <- mapM markAnnotated mov
    typ' <- markAnnotated typ
    return (DerivDecl an typ' ms' mov')

-- ---------------------------------------------------------------------

instance ExactPrint (ForeignDecl GhcPs) where
  getAnnotationEntry (ForeignImport an _ _  _) = fromAnn an
  getAnnotationEntry (ForeignExport an _ _  _) = fromAnn an

  setAnnotationAnchor (ForeignImport an a b c) anc cs = ForeignImport (setAnchorEpa an anc cs) a b c
  setAnnotationAnchor (ForeignExport an a b c) anc cs = ForeignExport (setAnchorEpa an anc cs) a b c

  exact (ForeignImport an n ty fimport) = do
    markEpAnn an AnnForeign
    markEpAnn an AnnImport

    fimport' <- markAnnotated fimport

    n' <- markAnnotated n
    markEpAnn an AnnDcolon
    ty' <- markAnnotated ty
    return (ForeignImport an n' ty' fimport')

  exact (ForeignExport an n ty fexport) = do
    markEpAnn an AnnForeign
    markEpAnn an AnnExport
    fexport' <- markAnnotated fexport
    n' <- markAnnotated n
    markEpAnn an AnnDcolon
    ty' <- markAnnotated ty
    return (ForeignExport an n' ty' fexport')

-- ---------------------------------------------------------------------

instance ExactPrint ForeignImport where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a
  exact (CImport cconv safety@(L ll _) mh imp (L ls src)) = do
    cconv' <- markAnnotated cconv
    unless (ll == noSrcSpan) $ markAnnotated safety >> return ()
    unless (ls == noSrcSpan) $ markExternalSourceText ls src "" >> return ()
    return (CImport cconv' safety mh imp (L ls src))

-- ---------------------------------------------------------------------

instance ExactPrint ForeignExport where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a
  exact (CExport spec (L ls src)) = do
    debugM $ "CExport starting"
    spec' <- markAnnotated spec
    unless (ls == noSrcSpan) $ markExternalSourceText ls src ""
    return (CExport spec' (L ls src))

-- ---------------------------------------------------------------------

instance ExactPrint CExportSpec where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a
  exact (CExportStatic st lbl cconv) = do
    debugM $ "CExportStatic starting"
    cconv' <- markAnnotated cconv
    return (CExportStatic st lbl cconv')

-- ---------------------------------------------------------------------

instance ExactPrint Safety where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a
  exact = withPpr

-- ---------------------------------------------------------------------

instance ExactPrint CCallConv where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a
  exact = withPpr

-- ---------------------------------------------------------------------

instance ExactPrint (WarnDecls GhcPs) where
  getAnnotationEntry (Warnings an _ _) = fromAnn an
  setAnnotationAnchor (Warnings an a b) anc cs = Warnings (setAnchorEpa an anc cs) a b

  exact (Warnings an src warns) = do
    an0 <- markAnnOpen an src "{-# WARNING" -- Note: might be {-# DEPRECATED
    warns' <- markAnnotated warns
    an1 <- markEpAnnLMS an0 lidl AnnClose (Just "#-}")
    return (Warnings an1 src warns')

-- ---------------------------------------------------------------------

instance ExactPrint (WarnDecl GhcPs) where
  getAnnotationEntry (Warning an _ _) = fromAnn an
  setAnnotationAnchor (Warning an a b) anc cs = Warning (setAnchorEpa an anc cs) a b

  exact (Warning an lns txt) = do
    lns' <- markAnnotated lns
    markEpAnn an AnnOpenS -- "["
    txt' <-
      case txt of
        WarningTxt    src ls -> do
          ls' <- markAnnotated ls
          return (WarningTxt    src ls')
        DeprecatedTxt src ls -> do
          ls' <- markAnnotated ls
          return (DeprecatedTxt src ls')
    markEpAnn an AnnCloseS -- "]"
    return (Warning an lns' txt')

-- ---------------------------------------------------------------------

instance ExactPrint StringLiteral where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a

  exact l@(StringLiteral src fs mcomma) = do
    printSourceText src (show (unpackFS fs))
    mapM_ (\r -> printStringAtRs r ",") mcomma
    return l

-- ---------------------------------------------------------------------

instance ExactPrint FastString where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a

  -- TODO: https://ghc.haskell.org/trac/ghc/ticket/10313 applies.
  -- exact fs = printStringAdvance (show (unpackFS fs))
  exact fs = printStringAdvance (unpackFS fs) >> return fs


-- ---------------------------------------------------------------------

instance ExactPrint (RuleDecls GhcPs) where
  getAnnotationEntry (HsRules an _ _) = fromAnn an
  setAnnotationAnchor (HsRules an a b) anc cs = HsRules (setAnchorEpa an anc cs) a b
  exact (HsRules an src rules) = do
    an0 <-
      case src of
        NoSourceText      -> markEpAnnLMS an lidl AnnOpen  (Just "{-# RULES")
        SourceText srcTxt -> markEpAnnLMS an lidl AnnOpen  (Just srcTxt)
    rules' <- markAnnotated rules
    an1 <- markEpAnnLMS an0 lidl AnnClose (Just "#-}")
    return (HsRules an1 src rules')

-- ---------------------------------------------------------------------

instance ExactPrint (RuleDecl GhcPs) where
  getAnnotationEntry (HsRule {rd_ext = an}) = fromAnn an
  setAnnotationAnchor r anc cs = r { rd_ext = setAnchorEpa (rd_ext r) anc cs}
  exact (HsRule an ln act mtybndrs termbndrs lhs rhs) = do
    debugM "HsRule entered"
    ln' <- markAnnotated ln
    debugM "HsRule after ln"
    an0 <- markActivation an lra_rest act
    debugM "HsRule after act"
    (an1, mtybndrs') <-
      case mtybndrs of
        Nothing -> return (an0, Nothing)
        Just bndrs -> do
          -- an1 <-  markLensMAA an0 (\a -> fmap fst (ra_tyanns a))  -- AnnForall
          an1 <-  markLensMAA an0 lra_tyanns_fst  -- AnnForall
          bndrs' <- mapM markAnnotated bndrs
          -- an2 <- markLensMAA an1 (\a -> fmap snd (ra_tyanns a))  -- AnnDot
          an2 <- markLensMAA an1 lra_tyanns_snd  -- AnnDot
          return (an2, Just bndrs')

    -- an2 <- markLensMAA an1 (\a -> fmap fst (ra_tmanns a))  -- AnnForall
    an2 <- markLensMAA an1 lra_tmanns_fst  -- AnnForall
    termbndrs' <- mapM markAnnotated termbndrs
    -- an3 <- markLensMAA an2 (\a -> fmap snd (ra_tmanns a))  -- AnnDot
    an3 <- markLensMAA an2 lra_tmanns_snd  -- AnnDot

    lhs' <- markAnnotated lhs
    an4 <- markEpAnnL an3 lra_rest AnnEqual
    rhs' <- markAnnotated rhs
    return (HsRule an4 ln' act mtybndrs' termbndrs' lhs' rhs')

markActivation :: (Monad m, Monoid w)
  => EpAnn a -> Lens a [AddEpAnn] -> Activation -> EP w m (EpAnn a)
markActivation an l act = do
  case act of
    ActiveBefore src phase -> do
      an0 <- markEpAnnL an l AnnOpenS --  '['
      an1 <- markEpAnnL an0 l AnnTilde -- ~
      an2 <- markEpAnnLMS an1 l AnnVal (Just (toSourceTextWithSuffix src (show phase) ""))
      an3 <- markEpAnnL an2 l AnnCloseS -- ']'
      return an3
    ActiveAfter src phase -> do
      an0 <- markEpAnnL an l AnnOpenS --  '['
      an1 <- markEpAnnLMS an0 l AnnVal (Just (toSourceTextWithSuffix src (show phase) ""))
      an2 <- markEpAnnL an1 l AnnCloseS -- ']'
      return an2
    NeverActive -> do
      an0 <- markEpAnnL an l AnnOpenS --  '['
      an1 <- markEpAnnL an0 l AnnTilde -- ~
      an2 <- markEpAnnL an1 l AnnCloseS -- ']'
      return an2
    _ -> return an

-- ---------------------------------------------------------------------

instance ExactPrint (SpliceDecl GhcPs) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a

  exact (SpliceDecl x splice flag) = do
    splice' <- markAnnotated splice
    return (SpliceDecl x splice' flag)

-- ---------------------------------------------------------------------

instance ExactPrint DocDecl where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a

  exact v =
    let str =
          case v of
            (DocCommentNext ds)     -> unpackHDS ds
            (DocCommentPrev ds)     -> unpackHDS ds
            (DocCommentNamed _s ds) -> unpackHDS ds
            (DocGroup _i ds)        -> unpackHDS ds
    in
      printStringAdvance str >> return v

-- ---------------------------------------------------------------------

instance ExactPrint (RoleAnnotDecl GhcPs) where
  getAnnotationEntry (RoleAnnotDecl an _ _) = fromAnn an
  setAnnotationAnchor (RoleAnnotDecl an a b) anc cs = RoleAnnotDecl (setAnchorEpa an anc cs) a b
  exact (RoleAnnotDecl an ltycon roles) = do
    markEpAnn an AnnType
    markEpAnn an AnnRole
    ltycon' <- markAnnotated ltycon
    let markRole (L l (Just r)) = do
          (L _ r') <- markAnnotated (L l r)
          return (L l (Just r'))
        markRole (L l Nothing) = do
          printStringAtSs l "_"
          return (L l Nothing)
    roles' <- mapM markRole roles
    return (RoleAnnotDecl an ltycon' roles')

-- ---------------------------------------------------------------------

instance ExactPrint Role where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a
  exact = withPpr

-- ---------------------------------------------------------------------

instance ExactPrint (RuleBndr GhcPs) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a

  exact (RuleBndr x ln) = do
    ln' <- markAnnotated ln
    return (RuleBndr x ln')
  exact (RuleBndrSig an ln (HsPS x ty)) = do
    markEpAnn an AnnOpenP -- "("
    ln' <- markAnnotated ln
    markEpAnn an AnnDcolon
    ty' <- markAnnotated ty
    markEpAnn an AnnCloseP -- ")"
    return (RuleBndrSig an ln' (HsPS x ty'))

-- ---------------------------------------------------------------------

instance (ExactPrint body) => ExactPrint (FamEqn GhcPs body) where
  getAnnotationEntry (FamEqn { feqn_ext = an}) = fromAnn an
  setAnnotationAnchor fe anc cs = fe {feqn_ext = setAnchorEpa (feqn_ext fe) anc cs}
  exact (FamEqn { feqn_ext = an
                , feqn_tycon  = tycon
                , feqn_bndrs  = bndrs
                , feqn_pats   = pats
                , feqn_fixity = fixity
                , feqn_rhs    = rhs }) = do
    (an', tycon', bndrs', pats', _) <- exactHsFamInstLHS an tycon bndrs pats fixity Nothing
    markEpAnn an AnnEqual
    rhs' <- markAnnotated rhs
    return (FamEqn { feqn_ext = an'
                   , feqn_tycon  = tycon'
                   , feqn_bndrs  = bndrs'
                   , feqn_pats   = pats'
                   , feqn_fixity = fixity
                   , feqn_rhs    = rhs' })

-- ---------------------------------------------------------------------

exactHsFamInstLHS ::
      (Monad m, Monoid w)
   => EpAnn [AddEpAnn]
   -> LocatedN RdrName
   -> HsOuterTyVarBndrs () GhcPs
   -> HsTyPats GhcPs
   -> LexicalFixity
   -> Maybe (LHsContext GhcPs)
   -> EP w m ( EpAnn [AddEpAnn]
             , LocatedN RdrName
             , HsOuterTyVarBndrs () GhcPs
             , HsTyPats GhcPs, Maybe (LHsContext GhcPs))
exactHsFamInstLHS an thing bndrs typats fixity mb_ctxt = do
  an0 <- markEpAnnL an lidl AnnForall
  bndrs' <- markAnnotated bndrs
  an1 <- markEpAnnL an0 lidl AnnDot
  mb_ctxt' <- mapM markAnnotated mb_ctxt
  (an2, thing', typats') <- exact_pats an1 typats
  return (an2, thing', bndrs', typats', mb_ctxt')
  where
    exact_pats :: (Monad m, Monoid w)
      => EpAnn [AddEpAnn] -> HsTyPats GhcPs -> EP w m (EpAnn [AddEpAnn], LocatedN RdrName, HsTyPats GhcPs)
    exact_pats an' (patl:patr:pats)
      | Infix <- fixity
      = let exact_op_app = do
              an0 <- markEpAnnAllL an' lidl AnnOpenP
              patl' <- markAnnotated patl
              thing' <- markAnnotated thing
              patr' <- markAnnotated patr
              an1 <- markEpAnnAllL an0 lidl AnnCloseP
              return (an1, thing', [patl',patr'])
        in case pats of
             [] -> exact_op_app
             _  -> do
               (an0, thing', p) <- exact_op_app
               pats' <- mapM markAnnotated pats
               return (an0, thing', p++pats')

    exact_pats an' pats = do
      an0 <- markEpAnnAllL an' lidl AnnOpenP
      thing' <- markAnnotated thing
      pats' <- markAnnotated pats
      an1 <- markEpAnnAllL an0 lidl AnnCloseP
      return (an1, thing', pats')

-- ---------------------------------------------------------------------

-- instance ExactPrint (LHsTypeArg GhcPs) where
instance (ExactPrint tm, ExactPrint ty, Outputable tm, Outputable ty)
     =>  ExactPrint (HsArg tm ty) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a

  exact a@(HsValArg tm)    = markAnnotated tm >> return a
  exact a@(HsTypeArg ss ty) = printStringAtSs ss "@" >> markAnnotated ty >> return a
  exact x@(HsArgPar _sp)   = withPpr x -- Does not appear in original source

-- ---------------------------------------------------------------------

instance ExactPrint (ClsInstDecl GhcPs) where
  getAnnotationEntry cid = fromAnn (fst $ cid_ext cid)
  setAnnotationAnchor cid anc cs
    = cid { cid_ext = (setAnchorEpa (fst $ cid_ext cid) anc cs, (snd $ cid_ext cid)) }

  exact (ClsInstDecl { cid_ext = (an, sortKey)
                     , cid_poly_ty = inst_ty, cid_binds = binds
                     , cid_sigs = sigs, cid_tyfam_insts = ats
                     , cid_overlap_mode = mbOverlap
                     , cid_datafam_insts = adts })
      = do
          (mbOverlap', inst_ty') <- top_matter
          markEpAnn an AnnWhere
          markEpAnn an AnnOpenC
          markEpAnnAll an id AnnSemi
          ds <- withSortKey sortKey
                               (prepareListAnnotationA ats
                             ++ prepareListAnnotationF an adts
                             ++ prepareListAnnotationA (bagToList binds)
                             ++ prepareListAnnotationA sigs
                               )
          markEpAnn an AnnCloseC -- '}'
          let
            ats'   = undynamic ds
            adts'  = undynamic ds
            binds' = listToBag $ undynamic ds
            sigs'  = undynamic ds
          return (ClsInstDecl { cid_ext = (an, sortKey)
                              , cid_poly_ty = inst_ty', cid_binds = binds'
                              , cid_sigs = sigs', cid_tyfam_insts = ats'
                              , cid_overlap_mode = mbOverlap'
                              , cid_datafam_insts = adts' })

      where
        top_matter = do
          markEpAnn an AnnInstance
          mo <- mapM markAnnotated mbOverlap
          it <- markAnnotated inst_ty
          markEpAnn an AnnWhere -- Optional
          return (mo,it)

-- ---------------------------------------------------------------------

instance ExactPrint (TyFamInstDecl GhcPs) where
  getAnnotationEntry (TyFamInstDecl an _) = fromAnn an
  setAnnotationAnchor (TyFamInstDecl an a) anc cs = TyFamInstDecl (setAnchorEpa an anc cs) a

  exact d@(TyFamInstDecl { tfid_xtn = an, tfid_eqn = eqn }) = do
    markEpAnn an AnnType
    markEpAnn an AnnInstance
    eqn' <- markAnnotated eqn
    return (d { tfid_eqn = eqn' })

-- ---------------------------------------------------------------------

instance ExactPrint (LocatedP OverlapMode) where
  getAnnotationEntry = entryFromLocatedA
  setAnnotationAnchor = setAnchorAn

  -- NOTE: NoOverlap is only used in the typechecker
  exact (L (SrcSpanAnn an l) (NoOverlap src)) = do
    an0 <- markAnnOpenP an src "{-# NO_OVERLAP"
    an1 <- markAnnCloseP an0
    return (L (SrcSpanAnn an1 l) (NoOverlap src))

  exact (L (SrcSpanAnn an l) (Overlappable src)) = do
    an0 <- markAnnOpenP an src "{-# OVERLAPPABLE"
    an1 <- markAnnCloseP an0
    return (L (SrcSpanAnn an1 l) (Overlappable src))

  exact (L (SrcSpanAnn an l) (Overlapping src)) = do
    an0 <- markAnnOpenP an src "{-# OVERLAPPING"
    an1 <- markAnnCloseP an0
    return (L (SrcSpanAnn an1 l) (Overlapping src))

  exact (L (SrcSpanAnn an l) (Overlaps src)) = do
    an0 <- markAnnOpenP an src "{-# OVERLAPS"
    an1 <- markAnnCloseP an0
    return (L (SrcSpanAnn an1 l) (Overlaps src))

  exact (L (SrcSpanAnn an l) (Incoherent src)) = do
    an0 <- markAnnOpenP an src "{-# INCOHERENT"
    an1 <- markAnnCloseP an0
    return (L (SrcSpanAnn an1 l) (Incoherent src))

-- ---------------------------------------------------------------------

instance ExactPrint (HsBind GhcPs) where
  getAnnotationEntry FunBind{} = NoEntryVal
  getAnnotationEntry PatBind{pat_ext=an} = fromAnn an
  getAnnotationEntry VarBind{} = NoEntryVal
  getAnnotationEntry AbsBinds{} = NoEntryVal
  getAnnotationEntry PatSynBind{} = NoEntryVal

  setAnnotationAnchor pb@PatBind{} anc cs = pb { pat_ext = setAnchorEpa (pat_ext pb) anc cs}
  setAnnotationAnchor a _ _ = a

  exact b@(FunBind _ _ matches _) = do
    matches' <- markAnnotated matches
    return b { fun_matches = matches' }
  exact (PatBind x pat grhss t) = do
    pat' <- markAnnotated pat
    grhss' <- markAnnotated grhss
    return (PatBind x pat' grhss' t)
  exact (PatSynBind x bind) = do
    bind' <- markAnnotated bind
    return (PatSynBind x bind')

  exact x = error $ "HsBind: exact for " ++ showAst x

-- ---------------------------------------------------------------------

instance ExactPrint (PatSynBind GhcPs GhcPs) where
  getAnnotationEntry (PSB { psb_ext = an}) = fromAnn an
  setAnnotationAnchor p anc cs = p { psb_ext = setAnchorEpa (psb_ext p) anc cs}

  exact (PSB{ psb_ext = an
            , psb_id = psyn, psb_args = details
            , psb_def = pat
            , psb_dir = dir }) = do
    markEpAnn an AnnPattern
    (psyn', details') <-
      case details of
        InfixCon v1 v2 -> do
          v1' <- markAnnotated v1
          psyn' <- markAnnotated psyn
          v2' <- markAnnotated v2
          return (psyn',InfixCon v1' v2')
        PrefixCon tvs vs -> do
          psyn' <- markAnnotated psyn
          tvs' <- markAnnotated tvs
          vs' <- markAnnotated vs
          return (psyn', PrefixCon tvs' vs')
        RecCon vs -> do
          psyn' <- markAnnotated psyn
          markEpAnn an AnnOpenC  -- '{'
          vs' <- markAnnotated vs
          markEpAnn an AnnCloseC -- '}'
          return (psyn', RecCon vs')

    (pat', dir') <-
      case dir of
        Unidirectional           -> do
          markEpAnn an AnnLarrow
          pat' <- markAnnotated pat
          return (pat', dir)
        ImplicitBidirectional    -> do
          markEpAnn an AnnEqual
          pat' <- markAnnotated pat
          return (pat', dir)
        ExplicitBidirectional mg -> do
          markEpAnn an AnnLarrow
          pat' <- markAnnotated pat
          markEpAnn an AnnWhere
          mg' <- markAnnotated mg
          return (pat', ExplicitBidirectional mg')

    return (PSB{ psb_ext = an
               , psb_id = psyn', psb_args = details'
               , psb_def = pat'
               , psb_dir = dir' })


-- ---------------------------------------------------------------------

instance ExactPrint (RecordPatSynField GhcPs) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a
  exact r@(RecordPatSynField { recordPatSynField = v }) = markAnnotated v
        >> return r

-- ---------------------------------------------------------------------

instance ExactPrint (Match GhcPs (LocatedA (HsCmd GhcPs))) where
  getAnnotationEntry (Match ann _ _ _) = fromAnn ann
  setAnnotationAnchor (Match an a b c) anc cs = Match (setAnchorEpa an anc cs) a b c

  exact (Match an mctxt pats grhss) =
    exactMatch (Match an mctxt pats grhss)

-- -------------------------------------

instance ExactPrint (Match GhcPs (LocatedA (HsExpr GhcPs))) where
  getAnnotationEntry (Match ann _ _ _) = fromAnn ann
  setAnnotationAnchor (Match an a b c) anc cs = Match (setAnchorEpa an anc cs) a b c

  exact (Match an mctxt pats grhss) =
    exactMatch (Match an mctxt pats grhss)

-- ---------------------------------------------------------------------

exactMatch :: (Monad m, Monoid w) => (ExactPrint (GRHSs GhcPs body)) => (Match GhcPs body) -> EP w m (Match GhcPs body)
exactMatch (Match an mctxt pats grhss) = do

  debugM $ "exact Match entered"

  (an0, mctxt', pats') <-
    case mctxt of
      FunRhs fun fixity strictness -> do
        debugM $ "exact Match FunRhs:" ++ showPprUnsafe fun
        an0' <-
          case strictness of
            SrcStrict -> markEpAnnL an lidl AnnBang
            _ -> pure an
        case fixity of
          Prefix -> do
            an' <- annotationsToComments an0' lidl [AnnOpenP,AnnCloseP]
            fun' <- markAnnotated fun
            pats' <- markAnnotated pats
            return (an', FunRhs fun' fixity strictness, pats')
          Infix ->
            case pats of
              (p1:p2:rest)
                | null rest -> do
                    p1'  <- markAnnotated p1
                    fun' <- markAnnotated fun
                    p2'  <- markAnnotated p2
                    return (an0', FunRhs fun' fixity strictness, [p1',p2'])
                | otherwise -> do
                    an0  <- markEpAnnL an0' lidl AnnOpenP
                    p1'  <- markAnnotated p1
                    fun' <- markAnnotated fun
                    p2'  <- markAnnotated p2
                    an1  <- markEpAnnL an0 lidl AnnCloseP
                    rest' <- mapM markAnnotated rest
                    return (an1, FunRhs fun' fixity strictness, p1':p2':rest')
              _ -> panic "FunRhs"
      LambdaExpr -> do
        an0' <- markEpAnnL an lidl AnnLam
        pats' <- markAnnotated pats
        return (an0', LambdaExpr, pats')
      CaseAlt -> do
        pats' <- markAnnotated pats
        return (an, CaseAlt, pats')
      _ -> do
        mctxt' <- withPpr mctxt
        return (an, mctxt', pats)

  grhss' <- markAnnotated grhss

  return (Match an0 mctxt' pats' grhss')

-- ---------------------------------------------------------------------

instance ExactPrint (GRHSs GhcPs (LocatedA (HsExpr GhcPs))) where
  getAnnotationEntry (GRHSs _ _ _) = NoEntryVal
  setAnnotationAnchor a _ _ = a

  exact (GRHSs cs grhss binds) = do
    addCommentsA $ priorComments cs
    addCommentsA $ getFollowingComments cs
    grhss' <- markAnnotated grhss
    binds' <- markAnnotated binds
    -- The comments will be added back as they are printed
    return (GRHSs emptyComments grhss' binds')


instance ExactPrint (GRHSs GhcPs (LocatedA (HsCmd GhcPs))) where
  getAnnotationEntry (GRHSs _ _ _) = NoEntryVal
  setAnnotationAnchor a _ _ = a

  exact (GRHSs cs grhss binds) = do
    addCommentsA $ priorComments cs
    addCommentsA $ getFollowingComments cs
    grhss' <- markAnnotated grhss
    binds' <- markAnnotated binds
    -- The comments will be added back as they are printed
    return (GRHSs emptyComments grhss' binds')

-- ---------------------------------------------------------------------

-- Temporary until https://gitlab.haskell.org/ghc/ghc/-/issues/20247
-- is fixed
fixValbindsAnn :: EpAnn AnnList -> EpAnn AnnList
fixValbindsAnn EpAnnNotUsed = EpAnnNotUsed
fixValbindsAnn (EpAnn anchor (AnnList ma o c r t) cs)
  = (EpAnn (widenAnchor anchor (map trailingAnnToAddEpAnn t)) (AnnList ma o c r t) cs)


-- See https://gitlab.haskell.org/ghc/ghc/-/issues/20256
fixAnnListAnn :: EpAnn AnnList -> EpAnn AnnList
fixAnnListAnn EpAnnNotUsed = EpAnnNotUsed
fixAnnListAnn (EpAnn anchor (AnnList ma o c r t) cs)
  = (EpAnn (widenAnchor anchor r) (AnnList ma o c r t) cs)

-- See https://gitlab.haskell.org/ghc/ghc/-/issues/20256
fixSrcAnnL :: SrcSpanAnnL -> SrcSpanAnnL
fixSrcAnnL (SrcSpanAnn an l) = SrcSpanAnn (fixAnnListAnn an) l

-- ---------------------------------------------------------------------

instance ExactPrint (HsLocalBinds GhcPs) where
  getAnnotationEntry (HsValBinds an _) = fromAnn (fixValbindsAnn an)
  getAnnotationEntry (HsIPBinds{}) = NoEntryVal
  getAnnotationEntry (EmptyLocalBinds{}) = NoEntryVal

  setAnnotationAnchor (HsValBinds an a) anc cs = HsValBinds (setAnchorEpaL an anc cs) a
  setAnnotationAnchor a _ _ = a

  exact (HsValBinds an' valbinds) = do
    let an = fixValbindsAnn an'
    -- markLocatedAAL an al_rest AnnWhere
    an0 <- markEpAnnL an lal_rest AnnWhere

    let manc = case an of
                 EpAnnNotUsed -> Nothing
                 _ -> al_anchor $ anns an

    case manc of
      Just anc -> do
        when (not $ isEmptyValBinds valbinds) $ setExtraDP (Just anc)
      _ -> return ()

    (an1, valbinds') <- markAnnList False an0 $ markAnnotatedWithLayout valbinds
    return (HsValBinds an1 valbinds')

  exact (HsIPBinds an bs) = do
    (as, ipb) <- markAnnList True an (markLocatedAAL an al_rest AnnWhere
                           >> markAnnotated bs
                           >>= \bs' -> return (HsIPBinds an bs'::HsLocalBinds GhcPs))
    case ipb of
      HsIPBinds _ bs' -> return (HsIPBinds as bs'::HsLocalBinds GhcPs)
      _ -> error "should not happen HsIPBinds"
  exact b@(EmptyLocalBinds _) = return b


-- ---------------------------------------------------------------------
instance ExactPrint (HsValBindsLR GhcPs GhcPs) where
  getAnnotationEntry _ = NoEntryVal
  setAnnotationAnchor a _ _ = a

  exact (ValBinds sortKey binds sigs) = do
    ds <- setLayoutBoth $ withSortKey sortKey
       (prepareListAnnotationA (bagToList binds)
     ++ prepareListAnnotationA sigs
       )
    let
      binds' = listToBag $ undynamic ds
      sigs'  = undynamic ds
    return (ValBinds sortKey binds' sigs')
  exact (XValBindsLR _) = panic "XValBindsLR"

undynamic :: Typeable a => [Dynamic] -> [a]
undynamic ds = mapMaybe fromDynamic ds

-- ---------------------------------------------------------------------

instance ExactPrint (HsIPBinds GhcPs) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a

  exact b@(IPBinds _ binds) = setLayoutBoth $ markAnnotated binds >> return b

-- ---------------------------------------------------------------------

instance ExactPrint (IPBind GhcPs) where
  getAnnotationEntry (IPBind an _ _) = fromAnn an
  setAnnotationAnchor (IPBind an a b) anc cs = IPBind (setAnchorEpa an anc cs) a b

  exact (IPBind an (Left lr) rhs) = do
    lr' <- markAnnotated lr
    markEpAnn an AnnEqual
    rhs' <- markAnnotated rhs
    return (IPBind an (Left lr') rhs')

  exact (IPBind _ (Right _) _) = error $ "ExactPrint IPBind: Right only after typechecker"

-- ---------------------------------------------------------------------

instance ExactPrint HsIPName where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a

  exact i@(HsIPName fs) = printStringAdvance ("?" ++ (unpackFS fs)) >> return i

-- ---------------------------------------------------------------------
-- Managing lists which have been separated, e.g. Sigs and Binds

prepareListAnnotationF :: (Monad m, Monoid w) =>
  EpAnn [AddEpAnn] -> [LDataFamInstDecl GhcPs] -> [(RealSrcSpan,EP w m Dynamic)]
prepareListAnnotationF an ls = map (\b -> (realSrcSpan $ getLocA b, go b)) ls
  where
    go (L l a) = do
      -- d' <- markAnnotated (DataFamInstDeclWithContext an TopLevel a)
      d' <- markAnnotated (DataFamInstDeclWithContext an NotTopLevel a)
      return (toDyn (L l (dc_d d')))

prepareListAnnotationA :: (Monad m, Monoid w, ExactPrint (LocatedAn an a))
  => [LocatedAn an a] -> [(RealSrcSpan,EP w m Dynamic)]
prepareListAnnotationA ls = map (\b -> (realSrcSpan $ getLocA b,go b)) ls
  where
    go b = do
      b' <- markAnnotated b
      return (toDyn b')

withSortKey :: (Monad m, Monoid w) => AnnSortKey -> [(RealSrcSpan, EP w m Dynamic)] -> EP w m [Dynamic]
withSortKey annSortKey xs = do
  debugM $ "withSortKey:annSortKey=" ++ showAst annSortKey
  let ordered = case annSortKey of
                  NoAnnSortKey -> sortBy orderByFst xs
                  -- Just keys -> error $ "withSortKey: keys" ++ show keys
                  AnnSortKey keys -> orderByKey xs keys
                                -- `debug` ("withSortKey:" ++
                                --          showPprUnsafe (map fst (sortBy (comparing (flip elemIndex keys . fst)) xs),
                                --                  map fst xs,
                                --                  keys)
                                --          )
  mapM snd ordered

orderByFst :: Ord a => (a, b1) -> (a, b2) -> Ordering
orderByFst (a,_) (b,_) = compare a b

-- ---------------------------------------------------------------------

instance ExactPrint (Sig GhcPs) where
  getAnnotationEntry (TypeSig a _ _)  = fromAnn a
  getAnnotationEntry (PatSynSig a _ _) = fromAnn a
  getAnnotationEntry (ClassOpSig a _ _ _) = fromAnn a
  getAnnotationEntry (IdSig {}) = NoEntryVal
  getAnnotationEntry (FixSig a _) = fromAnn a
  getAnnotationEntry (InlineSig a _ _) = fromAnn a
  getAnnotationEntry (SpecSig a _ _ _) = fromAnn a
  getAnnotationEntry (SpecInstSig a _ _) = fromAnn a
  getAnnotationEntry (MinimalSig a _ _) = fromAnn a
  getAnnotationEntry (SCCFunSig a _ _ _) = fromAnn a
  getAnnotationEntry (CompleteMatchSig a _ _ _) = fromAnn a

  setAnnotationAnchor (TypeSig a x y)  anc           cs = (TypeSig (setAnchorEpa a anc cs) x y)
  setAnnotationAnchor (PatSynSig a x y) anc          cs = (PatSynSig (setAnchorEpa a anc cs) x y)
  setAnnotationAnchor (ClassOpSig a x y z) anc       cs = (ClassOpSig (setAnchorEpa a anc cs) x y z)
  setAnnotationAnchor i@(IdSig {}) _ _s = i
  setAnnotationAnchor (FixSig a x) anc               cs = (FixSig (setAnchorEpa a anc cs) x)
  setAnnotationAnchor (InlineSig a x y) anc          cs = (InlineSig (setAnchorEpa a anc cs) x y)
  setAnnotationAnchor (SpecSig a x y z) anc          cs = (SpecSig (setAnchorEpa a anc cs) x y z)
  setAnnotationAnchor (SpecInstSig a x y) anc        cs = (SpecInstSig (setAnchorEpa a anc cs) x y)
  setAnnotationAnchor (MinimalSig a x y) anc         cs = (MinimalSig (setAnchorEpa a anc cs) x y)
  setAnnotationAnchor (SCCFunSig a x y z) anc        cs = (SCCFunSig (setAnchorEpa a anc cs) x y z)
  setAnnotationAnchor (CompleteMatchSig a x y z) anc cs = (CompleteMatchSig (setAnchorEpa a anc cs) x y z)

  exact (TypeSig an vars ty)  = do
    (an', vars', ty') <- exactVarSig an vars ty
    return (TypeSig an' vars' ty')

  exact (PatSynSig an lns typ) = do
    an0 <- markEpAnnL an lasRest AnnPattern
    lns' <- markAnnotated lns
    an1 <- markLensAA an0 lasDcolon
    typ' <- markAnnotated typ
    return (PatSynSig an1 lns' typ')

  exact (ClassOpSig an is_deflt vars ty)
    | is_deflt  = do
        an0 <- markEpAnnL an lasRest AnnDefault
        (an1, vars',ty') <- exactVarSig an0 vars ty
        return (ClassOpSig an1 is_deflt vars' ty')
    | otherwise = do
        (an0, vars',ty') <- exactVarSig an vars ty
        return (ClassOpSig an0 is_deflt vars' ty')

  exact (FixSig an (FixitySig x names (Fixity src v fdir))) = do
    let fixstr = case fdir of
         InfixL -> "infixl"
         InfixR -> "infixr"
         InfixN -> "infix"
    an0 <- markEpAnnLMS an  lidl AnnInfix (Just fixstr)
    an1 <- markEpAnnLMS an0 lidl AnnVal (Just (sourceTextToString src (show v)))
    names' <- markAnnotated names
    return (FixSig an1 (FixitySig x names' (Fixity src v fdir)))

  exact (InlineSig an ln inl) = do
    an0 <- markAnnOpen an (inl_src inl) "{-# INLINE"
    an1 <- markActivation an0 id (inl_act inl)
    ln' <- markAnnotated ln
    debugM $ "InlineSig:an=" ++ showAst an
    p <- getPosP
    debugM $ "InlineSig: p=" ++ show p
    an2 <- markEpAnnLMS an1 lidl AnnClose (Just "#-}")
    debugM $ "InlineSig:done"
    return (InlineSig an2 ln' inl)

  exact (SpecSig an ln typs inl) = do
    an0 <- markAnnOpen an (inl_src inl) "{-# SPECIALISE" -- Note: may be {-# SPECIALISE_INLINE
    an1 <- markActivation an0 lidl (inl_act inl)
    ln' <- markAnnotated ln
    an2 <- markEpAnnL an1 lidl AnnDcolon
    typs' <- markAnnotated typs
    an3 <- markEpAnnLMS an2 lidl AnnClose (Just "#-}")
    return (SpecSig an3 ln' typs' inl)

  exact (SpecInstSig an src typ) = do
    an0 <- markAnnOpen an src "{-# SPECIALISE"
    an1 <- markEpAnnL an0 lidl AnnInstance
    typ' <- markAnnotated typ
    an2 <- markEpAnnLMS an1 lidl AnnClose (Just "#-}")
    return (SpecInstSig an2 src typ')

  exact (MinimalSig an src formula) = do
    an0 <- markAnnOpen an src "{-# MINIMAL"
    formula' <- markAnnotated formula
    an1 <- markEpAnnLMS an0 lidl AnnClose (Just "#-}")
    return (MinimalSig an1 src formula')

  exact (SCCFunSig an src ln ml) = do
    an0 <- markAnnOpen an src "{-# SCC"
    ln' <- markAnnotated ln
    ml' <- markAnnotated ml
    an1 <- markEpAnnLMS an0 lidl AnnClose (Just "#-}")
    return (SCCFunSig an1 src ln' ml')

  exact (CompleteMatchSig an src cs mty) = do
    an0 <- markAnnOpen an src "{-# COMPLETE"
    cs' <- markAnnotated cs
    (an1, mty') <-
      case mty of
        Nothing -> return (an0, mty)
        Just ty -> do
          an1 <- markEpAnnL an0 lidl AnnDcolon
          ty' <- markAnnotated ty
          return (an1, Just ty')
    an2 <- markEpAnnLMS an1 lidl AnnClose (Just "#-}")
    return (CompleteMatchSig an2 src cs' mty')

  exact x = error $ "exact Sig for:" ++ showAst x

-- ---------------------------------------------------------------------

exactVarSig :: (Monad m, Monoid w, ExactPrint a)
  => EpAnn AnnSig -> [LocatedN RdrName] -> a -> EP w m (EpAnn AnnSig, [LocatedN RdrName], a)
exactVarSig an vars ty = do
  vars' <- mapM markAnnotated vars
  an0 <- markLensAA an lasDcolon
  ty' <- markAnnotated ty
  return (an0, vars', ty')

-- ---------------------------------------------------------------------

instance ExactPrint (StandaloneKindSig GhcPs) where
  getAnnotationEntry (StandaloneKindSig an _ _) = fromAnn an
  setAnnotationAnchor (StandaloneKindSig an a b) anc cs = StandaloneKindSig (setAnchorEpa an anc cs) a b

  exact (StandaloneKindSig an vars sig) = do
    an0 <- markEpAnnL an lidl AnnType
    vars' <- markAnnotated vars
    an1 <- markEpAnnL an0 lidl AnnDcolon
    sig' <- markAnnotated sig
    return (StandaloneKindSig an1 vars' sig')

-- ---------------------------------------------------------------------

instance ExactPrint (DefaultDecl GhcPs) where
  getAnnotationEntry (DefaultDecl an _) = fromAnn an
  setAnnotationAnchor (DefaultDecl an a) anc cs = DefaultDecl (setAnchorEpa an anc cs) a

  exact (DefaultDecl an tys) = do
    an0 <- markEpAnnL an lidl AnnDefault
    an1 <- markEpAnnL an0 lidl AnnOpenP
    tys' <- markAnnotated tys
    an2 <- markEpAnnL an1 lidl AnnCloseP
    return (DefaultDecl an2 tys')

-- ---------------------------------------------------------------------

instance ExactPrint (AnnDecl GhcPs) where
  getAnnotationEntry (HsAnnotation an _ _ _) = fromAnn an
  setAnnotationAnchor (HsAnnotation an a b c) anc cs = HsAnnotation (setAnchorEpa an anc cs) a b c

  exact (HsAnnotation an src prov e) = do
    an0 <- markAnnOpenP an src "{-# ANN"
    (an1, prov') <-
      case prov of
        (ValueAnnProvenance n) -> do
          n' <- markAnnotated n
          return (an0, ValueAnnProvenance n')
        (TypeAnnProvenance n) -> do
          an1 <- markEpAnnL an0 lapr_rest AnnType
          n' <- markAnnotated n
          return (an1, TypeAnnProvenance n')
        ModuleAnnProvenance -> do
          an1 <- markEpAnnL an lapr_rest AnnModule
          return (an1, prov)

    e' <- markAnnotated e
    an2 <- markAnnCloseP an1
    return (HsAnnotation an2 src prov' e')

-- ---------------------------------------------------------------------

instance ExactPrint (BF.BooleanFormula (LocatedN RdrName)) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a

  exact (BF.Var x)  = do
    x' <- markAnnotated x
    return (BF.Var x')
  exact (BF.Or ls)  = do
    ls' <- markAnnotated ls
    return (BF.Or ls')
  exact (BF.And ls) = do
    ls' <- markAnnotated ls
    return (BF.And ls')
  exact (BF.Parens x)  = do
    x' <- markAnnotated x
    return (BF.Parens x')

-- ---------------------------------------------------------------------

instance (ExactPrint body) => ExactPrint (HsWildCardBndrs GhcPs body) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a
  exact (HsWC x ty) = do
    ty' <- markAnnotated ty
    return (HsWC x ty')

-- ---------------------------------------------------------------------

instance ExactPrint (GRHS GhcPs (LocatedA (HsExpr GhcPs))) where
  getAnnotationEntry (GRHS an _ _) = fromAnn an
  setAnnotationAnchor (GRHS an a b) anc cs = GRHS (setAnchorEpa an anc cs) a b

  exact (GRHS an guards expr) = do
    debugM $ "GRHS comments:" ++ showGhc (comments an)
    an0 <- markLensKwM an lga_vbar AnnVbar
    guards' <- markAnnotated guards
    debugM $ "GRHS before matchSeparator"
    an1 <- markLensAA an0 lga_sep -- Mark the matchSeparator for these GRHSs
    debugM $ "GRHS after matchSeparator"
    expr' <- markAnnotated expr
    return (GRHS an1 guards' expr')

instance ExactPrint (GRHS GhcPs (LocatedA (HsCmd GhcPs))) where
  getAnnotationEntry (GRHS ann _ _) = fromAnn ann
  setAnnotationAnchor (GRHS an a b) anc cs = GRHS (setAnchorEpa an anc cs) a b

  exact (GRHS an guards expr) = do
    an0 <- markLensKwM an lga_vbar AnnVbar
    guards' <- markAnnotated guards
    an1 <- markLensAA an0 lga_sep -- Mark the matchSeparator for these GRHSs
    expr' <- markAnnotated expr
    return (GRHS an1 guards' expr')

-- ---------------------------------------------------------------------

instance ExactPrint (HsExpr GhcPs) where
  getAnnotationEntry (HsVar{})                    = NoEntryVal
  getAnnotationEntry (HsUnboundVar an _)          = fromAnn an
  getAnnotationEntry (HsConLikeOut{})             = NoEntryVal
  getAnnotationEntry (HsRecFld{})                 = NoEntryVal
  getAnnotationEntry (HsOverLabel an _)           = fromAnn an
  getAnnotationEntry (HsIPVar an _)               = fromAnn an
  getAnnotationEntry (HsOverLit an _)             = fromAnn an
  getAnnotationEntry (HsLit an _)                 = fromAnn an
  getAnnotationEntry (HsLam _ _)                  = NoEntryVal
  getAnnotationEntry (HsLamCase an _)             = fromAnn an
  getAnnotationEntry (HsApp an _ _)               = fromAnn an
  getAnnotationEntry (HsAppType _ _ _)            = NoEntryVal
  getAnnotationEntry (OpApp an _ _ _)             = fromAnn an
  getAnnotationEntry (NegApp an _ _)              = fromAnn an
  getAnnotationEntry (HsPar an _)                 = fromAnn an
  getAnnotationEntry (SectionL an _ _)            = fromAnn an
  getAnnotationEntry (SectionR an _ _)            = fromAnn an
  getAnnotationEntry (ExplicitTuple an _ _)       = fromAnn an
  getAnnotationEntry (ExplicitSum an _ _ _)       = fromAnn an
  getAnnotationEntry (HsCase an _ _)              = fromAnn an
  getAnnotationEntry (HsIf an _ _ _)              = fromAnn an
  getAnnotationEntry (HsMultiIf an _)             = fromAnn an
  getAnnotationEntry (HsLet an _ _)               = fromAnn an
  getAnnotationEntry (HsDo an _ _)                = fromAnn an
  getAnnotationEntry (ExplicitList an _)          = fromAnn an
  getAnnotationEntry (RecordCon an _ _)           = fromAnn an
  getAnnotationEntry (RecordUpd an _ _)           = fromAnn an
  getAnnotationEntry (HsGetField an _ _)          = fromAnn an
  getAnnotationEntry (HsProjection an _)          = fromAnn an
  getAnnotationEntry (ExprWithTySig an _ _)       = fromAnn an
  getAnnotationEntry (ArithSeq an _ _)            = fromAnn an
  getAnnotationEntry (HsBracket an _)             = fromAnn an
  getAnnotationEntry (HsRnBracketOut{})           = NoEntryVal
  getAnnotationEntry (HsTcBracketOut{})           = NoEntryVal
  getAnnotationEntry (HsSpliceE an _)             = fromAnn an
  getAnnotationEntry (HsProc an _ _)              = fromAnn an
  getAnnotationEntry (HsStatic an _)              = fromAnn an
  getAnnotationEntry (HsTick {})                  = NoEntryVal
  getAnnotationEntry (HsBinTick {})               = NoEntryVal
  getAnnotationEntry (HsPragE{})                  = NoEntryVal

  setAnnotationAnchor a@(HsVar{})              _ _s = a
  setAnnotationAnchor (HsUnboundVar an a)    anc cs = (HsUnboundVar (setAnchorEpa an anc cs) a)
  setAnnotationAnchor a@(HsConLikeOut{})       _ _s = a
  setAnnotationAnchor a@(HsRecFld{})           _ _s = a
  setAnnotationAnchor (HsOverLabel an a)     anc cs = (HsOverLabel (setAnchorEpa an anc cs) a)
  setAnnotationAnchor (HsIPVar an a)         anc cs = (HsIPVar (setAnchorEpa an anc cs) a)
  setAnnotationAnchor (HsOverLit an a)       anc cs = (HsOverLit (setAnchorEpa an anc cs) a)
  setAnnotationAnchor (HsLit an a)           anc cs = (HsLit (setAnchorEpa an anc cs) a)
  setAnnotationAnchor a@(HsLam _ _)            _ _s = a
  setAnnotationAnchor (HsLamCase an a)       anc cs = (HsLamCase (setAnchorEpa an anc cs) a)
  setAnnotationAnchor (HsApp an a b)         anc cs = (HsApp (setAnchorEpa an anc cs) a b)
  setAnnotationAnchor a@(HsAppType _ _ _)      _ _s = a
  setAnnotationAnchor (OpApp an a b c)       anc cs = (OpApp (setAnchorEpa an anc cs) a b c)
  setAnnotationAnchor (NegApp an a b)        anc cs = (NegApp (setAnchorEpa an anc cs) a b)
  setAnnotationAnchor (HsPar an a)           anc cs = (HsPar (setAnchorEpa an anc cs) a)
  setAnnotationAnchor (SectionL an a b)      anc cs = (SectionL (setAnchorEpa an anc cs) a b)
  setAnnotationAnchor (SectionR an a b)      anc cs = (SectionR (setAnchorEpa an anc cs) a b)
  setAnnotationAnchor (ExplicitTuple an a b) anc cs = (ExplicitTuple (setAnchorEpa an anc cs) a b)
  setAnnotationAnchor (ExplicitSum an a b c) anc cs = (ExplicitSum (setAnchorEpa an anc cs) a b c)
  setAnnotationAnchor (HsCase an a b)        anc cs = (HsCase (setAnchorEpa an anc cs) a b)
  setAnnotationAnchor (HsIf an a b c)        anc cs = (HsIf (setAnchorEpa an anc cs) a b c)
  setAnnotationAnchor (HsMultiIf an a)       anc cs = (HsMultiIf (setAnchorEpa an anc cs) a)
  setAnnotationAnchor (HsLet an a b)         anc cs = (HsLet (setAnchorEpa an anc cs) a b)
  setAnnotationAnchor (HsDo an a b)          anc cs = (HsDo (setAnchorEpa an anc cs) a b)
  setAnnotationAnchor (ExplicitList an a)    anc cs = (ExplicitList (setAnchorEpa an anc cs) a)
  setAnnotationAnchor (RecordCon an a b)     anc cs = (RecordCon (setAnchorEpa an anc cs) a b)
  setAnnotationAnchor (RecordUpd an a b)     anc cs = (RecordUpd (setAnchorEpa an anc cs) a b)
  setAnnotationAnchor (HsGetField an a b)    anc cs = (HsGetField (setAnchorEpa an anc cs) a b)
  setAnnotationAnchor (HsProjection an a)    anc cs = (HsProjection (setAnchorEpa an anc cs) a)
  setAnnotationAnchor (ExprWithTySig an a b) anc cs = (ExprWithTySig (setAnchorEpa an anc cs) a b)
  setAnnotationAnchor (ArithSeq an a b)      anc cs = (ArithSeq (setAnchorEpa an anc cs) a b)
  setAnnotationAnchor (HsBracket an a)       anc cs = (HsBracket (setAnchorEpa an anc cs) a)
  setAnnotationAnchor a@(HsRnBracketOut{})     _ _s = a
  setAnnotationAnchor a@(HsTcBracketOut{})     _ _s = a
  setAnnotationAnchor (HsSpliceE an a)       anc cs = (HsSpliceE (setAnchorEpa an anc cs) a)
  setAnnotationAnchor (HsProc an a b)        anc cs = (HsProc (setAnchorEpa an anc cs) a b)
  setAnnotationAnchor (HsStatic an a)        anc cs = (HsStatic (setAnchorEpa an anc cs) a)
  setAnnotationAnchor a@(HsTick {})            _ _s = a
  setAnnotationAnchor a@(HsBinTick {})         _ _s = a
  setAnnotationAnchor a@(HsPragE{})            _ _s = a

  exact (HsVar x n) = do
    n' <- markAnnotated n
    return (HsVar x n')
  exact x@(HsUnboundVar an _) = do
    case an of
      EpAnnNotUsed -> withPpr x
      EpAnn _ (EpAnnUnboundVar (ob,cb) l) _ -> do
        printStringAtAA ob "`" >> return ()
        printStringAtAA l  "_" >> return ()
        printStringAtAA cb "`" >> return ()
        return x
  -- exact x@(HsConLikeOut{})             = withPpr x
  -- exact x@(HsRecFld{})                 = withPpr x
  exact x@(HsOverLabel _ _) = withPpr x

  exact x@(HsIPVar _ (HsIPName n))
    = printStringAdvance ("?" ++ unpackFS n) >> return x

  exact x@(HsOverLit _an ol) = do
    let str = case ol_val ol of
                HsIntegral   (IL src _ _) -> src
                HsFractional (FL { fl_text = src }) -> src
                HsIsString src _          -> src
    -- markExternalSourceText l str ""
    case str of
      SourceText s -> printStringAdvance s >> return ()
      NoSourceText -> withPpr x >> return ()
    return x

  exact (HsLit an lit) = do
    lit' <- withPpr lit
    return (HsLit an lit')
  exact (HsLam x mg) = do
    mg' <- markAnnotated mg
    return (HsLam x mg')

  exact (HsLamCase an mg) = do
    an0 <- markEpAnnL an lidl AnnLam
    an1 <- markEpAnnL an0 lidl AnnCase
    mg' <- markAnnotated mg
    return (HsLamCase an1 mg')

  exact (HsApp an e1 e2) = do
    p <- getPosP
    debugM $ "HsApp entered. p=" ++ show p
    e1' <- markAnnotated e1
    e2' <- markAnnotated e2
    return (HsApp an e1' e2')
  exact (HsAppType ss fun arg) = do
    fun' <- markAnnotated fun
    printStringAtSs ss "@"
    arg' <- markAnnotated arg
    return (HsAppType ss fun' arg')
  exact (OpApp an e1 e2 e3) = do
    e1' <- markAnnotated e1
    e2' <- markAnnotated e2
    e3' <- markAnnotated e3
    return (OpApp an e1' e2' e3')

  exact (NegApp an e s) = do
    an0 <- markEpAnnL an lidl AnnMinus
    e' <- markAnnotated e
    return (NegApp an0 e' s)

  exact (HsPar an e) = do
    an0 <- markOpeningParen an
    e' <- markAnnotated e
    debugM $ "HsPar closing paren"
    an1 <- markClosingParen an0
    debugM $ "HsPar done"
    return (HsPar an1 e')

  exact (SectionL an expr op) = do
    expr' <- markAnnotated expr
    op' <- markAnnotated op
    return (SectionL an expr' op')

  exact (SectionR an op expr) = do
    op' <- markAnnotated op
    expr' <- markAnnotated expr
    return (SectionR an op' expr')

  exact (ExplicitTuple an args b) = do
    an0 <- if b == Boxed then markEpAnnL an lidl AnnOpenP
                         else markEpAnnL an lidl AnnOpenPH

    args' <- mapM markAnnotated args

    an1 <- if b == Boxed then markEpAnnL an0 lidl AnnCloseP
                         else markEpAnnL an0 lidl AnnClosePH
    debugM $ "ExplicitTuple done"
    return (ExplicitTuple an1 args' b)

  exact (ExplicitSum an alt arity expr) = do
    an0 <- markLensKw an laesOpen AnnOpenPH
    an1 <- markAnnKwAllL an0 laesBarsBefore AnnVbar
    expr' <- markAnnotated expr
    an2 <- markAnnKwAllL an1 laesBarsAfter AnnVbar
    an3 <- markLensKw an2 laesClose AnnClosePH
    return (ExplicitSum an3 alt arity expr')

  exact (HsCase an e alts) = do
    an0 <- markAnnKwL an lhsCaseAnnCase AnnCase
    e' <- markAnnotated e
    an1 <- markAnnKwL an0 lhsCaseAnnOf AnnOf
    an2 <- markEpAnnL an1 lhsCaseAnnsRest AnnOpenC
    an3 <- markEpAnnAllL an2 lhsCaseAnnsRest AnnSemi
    alts' <- setLayoutBoth $ markAnnotated alts
    an4 <- markEpAnnL an3 lhsCaseAnnsRest AnnCloseC
    return (HsCase an4 e' alts')

  exact (HsIf an e1 e2 e3) = do
    an0 <- markAnnKwL an laiIf AnnIf
    e1' <- markAnnotated e1
    an1 <- markLensKwM an0 laiThenSemi AnnSemi
    an2 <- markAnnKwL an1 laiThen AnnThen
    e2' <- markAnnotated e2
    an3 <- markLensKwM an2 laiElseSemi AnnSemi
    an4 <- markAnnKwL an3 laiElse AnnElse
    e3' <- markAnnotated e3
    return (HsIf an4 e1' e2' e3')

  exact (HsMultiIf an mg) = do
    an0 <- markEpAnnL an lidl AnnIf
    an1 <- markEpAnnL an0 lidl AnnOpenC -- optional
    mg' <- markAnnotated mg
    an2 <- markEpAnnL an1 lidl AnnCloseC -- optional
    return (HsMultiIf an2 mg')

  exact (HsLet an binds e) = do
    setLayoutBoth $ do -- Make sure the 'in' gets indented too
      an0 <- markLensKw an lalLet AnnLet
      debugM $ "HSlet:binds coming"
      binds' <- setLayoutBoth $ markAnnotated binds
      debugM $ "HSlet:binds done"
      an1 <- markLensKw an0 lalIn AnnIn
      debugM $ "HSlet:expr coming"
      e' <- markAnnotated e
      return (HsLet an1 binds' e')

  exact (HsDo an do_or_list_comp stmts) = do
    debugM $ "HsDo"
    (an',stmts') <- markAnnList True an $ exactDo an do_or_list_comp stmts
    return (HsDo an' do_or_list_comp stmts')

  exact (ExplicitList an es) = do
    debugM $ "ExplicitList start"
    an0 <- markLensMAA an lal_open
    es' <- markAnnotated es
    an1 <- markLensMAA an0 lal_close
    debugM $ "ExplicitList end"
    return (ExplicitList an1 es')
  exact (RecordCon an con_id binds) = do
    con_id' <- markAnnotated con_id
    an0 <- markEpAnnL an lidl AnnOpenC
    binds' <- markAnnotated binds
    an1 <- markEpAnnL an0 lidl AnnCloseC
    return (RecordCon an1 con_id' binds')
  exact (RecordUpd an expr fields) = do
    expr' <- markAnnotated expr
    an0 <- markEpAnnL an lidl AnnOpenC
    fields' <- markAnnotated fields
    an1 <- markEpAnnL an0 lidl AnnCloseC
    return (RecordUpd an1 expr' fields')
  exact (HsGetField an expr field) = do
    expr' <- markAnnotated expr
    field' <- markAnnotated field
    return (HsGetField an expr' field')
  exact (HsProjection an flds) = do
    an0 <- markAnnKwL an lapOpen AnnOpenP
    flds' <- markAnnotated flds
    an1 <- markAnnKwL an0 lapClose AnnCloseP
    return (HsProjection an1 flds')
  exact (ExprWithTySig an expr sig) = do
    expr' <- markAnnotated expr
    an0 <- markEpAnnL an lidl AnnDcolon
    sig' <- markAnnotated sig
    return (ExprWithTySig an0 expr' sig')
  exact (ArithSeq an s seqInfo) = do
    an0 <- markEpAnnL an lidl AnnOpenS -- '['
    (an1, seqInfo') <-
      case seqInfo of
        From e -> do
          e' <- markAnnotated e
          an' <- markEpAnnL an0 lidl AnnDotdot
          return (an', From e')
        FromTo e1 e2 -> do
          e1' <- markAnnotated e1
          an' <- markEpAnnL an0 lidl AnnDotdot
          e2' <- markAnnotated e2
          return (an', FromTo e1' e2')
        FromThen e1 e2 -> do
          e1' <- markAnnotated e1
          an' <- markEpAnnL an0 lidl AnnComma
          e2' <- markAnnotated e2
          an'' <- markEpAnnL an' lidl AnnDotdot
          return (an'', FromThen e1' e2')
        FromThenTo e1 e2 e3 -> do
          e1' <- markAnnotated e1
          an' <- markEpAnnL an0 lidl AnnComma
          e2' <- markAnnotated e2
          an'' <- markEpAnnL an' lidl AnnDotdot
          e3' <- markAnnotated e3
          return (an'', FromThenTo e1' e2' e3')
    an2 <- markEpAnnL an1 lidl AnnCloseS -- ']'
    return (ArithSeq an2 s seqInfo')

  exact (HsBracket an (ExpBr a e)) = do
    an0 <- markEpAnnL an lidl AnnOpenEQ -- "[|"
    an1 <- markEpAnnL an0 lidl AnnOpenE  -- "[e|" -- optional
    e' <- markAnnotated e
    an2 <- markEpAnnL an1 lidl AnnCloseQ -- "|]"
    return (HsBracket an2 (ExpBr a e'))
  exact (HsBracket an (PatBr a e)) = do
    an0 <- markEpAnnLMS an lidl AnnOpen (Just "[p|")
    e' <- markAnnotated e
    an1 <- markEpAnnL an0 lidl AnnCloseQ -- "|]"
    return (HsBracket an1 (PatBr a e'))
  exact (HsBracket an (DecBrL a e)) = do
    an0 <- markEpAnnLMS an lidl AnnOpen (Just "[d|")
    -- See https://gitlab.haskell.org/ghc/ghc/-/issues/20257, we need
    -- to mark braces here for the time being
    an1 <- markEpAnnL an0 lidl AnnOpenC -- "{"
    e' <- markAnnotated e
    an2 <- markEpAnnL an1 lidl AnnCloseC -- "}"
    an3 <- markEpAnnL an2 lidl AnnCloseQ -- "|]"
    return (HsBracket an3 (DecBrL a e'))
  -- -- exact (HsBracket an (DecBrG _ _)) =
  -- --   traceM "warning: DecBrG introduced after renamer"
  exact (HsBracket an (TypBr a e)) = do
    an0 <- markEpAnnLMS an lidl AnnOpen (Just "[t|")
    e' <- markAnnotated e
    an1 <- markEpAnnL an0 lidl AnnCloseQ -- "|]"
    return (HsBracket an1 (TypBr a e'))
  exact (HsBracket an (VarBr a b e)) = do
    (an0, e') <-
      if b
      then do
        an' <- markEpAnnL an lidl AnnSimpleQuote
        e' <- markAnnotated e
        return (an', e')
      else do
        an' <- markEpAnnL an lidl AnnThTyQuote
        e' <- markAnnotated e
        return (an', e')
    return (HsBracket an0 (VarBr a b e'))
  exact (HsBracket an (TExpBr a e)) = do
    an0 <- markEpAnnLMS an lidl AnnOpen (Just "[||")
    an1 <- markEpAnnLMS an0 lidl AnnOpenE (Just "[e||")
    e' <- markAnnotated e
    an2 <- markEpAnnLMS an1 lidl AnnClose (Just "||]")
    return (HsBracket an2 (TExpBr a e'))


  -- exact x@(HsRnBracketOut{})           = withPpr x
  -- exact x@(HsTcBracketOut{})           = withPpr x
  exact (HsSpliceE a sp) = do
    sp' <- markAnnotated sp
    return (HsSpliceE a sp')

  exact (HsProc an p c) = do
    debugM $ "HsProc start"
    an0 <- markEpAnnL an lidl AnnProc
    p' <- markAnnotated p
    an1 <- markEpAnnL an0 lidl AnnRarrow
    debugM $ "HsProc after AnnRarrow"
    c' <- markAnnotated c
    return (HsProc an1 p' c')

  exact (HsStatic an e) = do
    an0 <- markEpAnnL an lidl AnnStatic
    e' <- markAnnotated e
    return (HsStatic an0 e')

  -- exact x@(HsTick {})                  = withPpr x
  -- exact x@(HsBinTick {})               = withPpr x
  exact (HsPragE a prag e) = do
    prag' <- markAnnotated prag
    e' <- markAnnotated e
    return (HsPragE a prag' e')
  exact x = error $ "exact HsExpr for:" ++ showAst x

-- ---------------------------------------------------------------------

exactDo :: (Monad m, Monoid w, ExactPrint (LocatedAn an a))
        => EpAnn AnnList -> (HsStmtContext any) -> (LocatedAn an a) -> EP w m (LocatedAn an a)
exactDo an (DoExpr m)    stmts = exactMdo an m AnnDo             >> markMaybeDodgyStmts stmts
exactDo an GhciStmtCtxt  stmts = markLocatedAAL an al_rest AnnDo >> markMaybeDodgyStmts stmts
exactDo an ArrowExpr     stmts = markLocatedAAL an al_rest AnnDo >> markMaybeDodgyStmts stmts
exactDo an (MDoExpr m)   stmts = exactMdo an m AnnMdo            >> markMaybeDodgyStmts stmts
exactDo _  ListComp      stmts = markMaybeDodgyStmts stmts
exactDo _  MonadComp     stmts = markMaybeDodgyStmts stmts
exactDo _  _             _     = panic "pprDo" -- PatGuard, ParStmtCxt

exactMdo :: (Monad m, Monoid w) => EpAnn AnnList -> Maybe ModuleName -> AnnKeywordId -> EP w m Int
exactMdo an Nothing            kw = markLocatedAAL  an al_rest kw
exactMdo an (Just module_name) kw = markLocatedAALS an al_rest kw (Just n)
    where
      n = (moduleNameString module_name) ++ "." ++ (keywordToString kw)

markMaybeDodgyStmts :: (Monad m, Monoid w, ExactPrint (LocatedAn an a))
  => LocatedAn an a -> EP w m (LocatedAn an a)
markMaybeDodgyStmts stmts =
  if isGoodSrcSpan (getLocA stmts)
    then markAnnotatedWithLayout stmts
    else return stmts

-- ---------------------------------------------------------------------
instance ExactPrint (HsPragE GhcPs) where
  getAnnotationEntry HsPragSCC{}  = NoEntryVal
  setAnnotationAnchor a _ _ = a

  exact (HsPragSCC an st sl) = do
    an0 <- markAnnOpenP an st "{-# SCC"
    let txt = sourceTextToString (sl_st sl) (unpackFS $ sl_fs sl)
    an1 <- markEpAnnLMS an0 lapr_rest AnnVal    (Just txt) -- optional
    an2 <- markEpAnnLMS an1 lapr_rest AnnValStr (Just txt) -- optional
    an3 <- markAnnCloseP an2
    return (HsPragSCC an3 st sl)


-- ---------------------------------------------------------------------

instance ExactPrint (HsSplice GhcPs) where
  getAnnotationEntry (HsTypedSplice an _ _ _)   = fromAnn an
  getAnnotationEntry (HsUntypedSplice an _ _ _) = fromAnn an
  getAnnotationEntry (HsQuasiQuote _ _ _ _ _)   = NoEntryVal
  getAnnotationEntry (HsSpliced _ _ _)          = NoEntryVal

  setAnnotationAnchor (HsTypedSplice an a b c)   anc cs = (HsTypedSplice (setAnchorEpa an anc cs) a b c)
  setAnnotationAnchor (HsUntypedSplice an a b c) anc cs = (HsUntypedSplice (setAnchorEpa an anc cs) a b c)
  setAnnotationAnchor a@(HsQuasiQuote _ _ _ _ _) _ _ = a
  setAnnotationAnchor a@(HsSpliced _ _ _)        _ _ = a

  exact (HsTypedSplice an DollarSplice n e) = do
    markEpAnn an AnnDollarDollar
    e' <- markAnnotated e
    return (HsTypedSplice an DollarSplice n e')

  exact (HsUntypedSplice an decoration n b) = do
    an0 <- if (decoration == DollarSplice)
             then markEpAnnL an lidl AnnDollar
             else return an
    b' <- markAnnotated b
    return (HsUntypedSplice an0 decoration n b')

  exact (HsQuasiQuote a b q ss fs) = do
    -- The quasiquote string does not honour layout offsets. Store
    -- the colOffset for now.
    -- TODO: use local?
    oldOffset <- getLayoutOffsetP
    EPState{pMarkLayout} <- get
    unless pMarkLayout $ setLayoutOffsetP 0
    printStringAdvance
            -- Note: Lexer.x does not provide unicode alternative. 2017-02-26
            ("[" ++ (showPprUnsafe q) ++ "|" ++ (unpackFS fs) ++ "|]")
    unless pMarkLayout $ setLayoutOffsetP oldOffset
    p <- getPosP
    debugM $ "HsQuasiQuote:after:(p,ss)=" ++ show (p,ss2range ss)
    return (HsQuasiQuote a b q ss fs)

  exact x = error $ "exact HsSplice for:" ++ showAst x

-- ---------------------------------------------------------------------

-- TODO:AZ: combine these instances
instance ExactPrint (MatchGroup GhcPs (LocatedA (HsExpr GhcPs))) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a
  exact (MG x matches o) = do
    -- TODO:AZ use SortKey, in MG ann.
    -- matches' <- markAnnotated matches
    matches' <- if isGoodSrcSpan (getLocA matches)
      then markAnnotated matches
      else return matches
    return (MG x matches' o)

instance ExactPrint (MatchGroup GhcPs (LocatedA (HsCmd GhcPs))) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a
  exact (MG x matches o) = do
    -- TODO:AZ use SortKey, in MG ann.
    matches' <- if isGoodSrcSpan (getLocA matches)
      then markAnnotated matches
      else return matches
    return (MG x matches' o)

-- ---------------------------------------------------------------------

instance (ExactPrint body) => ExactPrint (HsRecFields GhcPs body) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a
  exact (HsRecFields fields mdot) = do
    fields' <- markAnnotated fields
    case mdot of
      Nothing -> return ()
      Just (L ss _) ->
        printStringAtSs ss ".." >> return ()
      -- Note: mdot contains the SrcSpan where the ".." appears, if present
    return (HsRecFields fields' mdot)

-- ---------------------------------------------------------------------

instance (ExactPrint body)
    => ExactPrint (HsRecField' (FieldOcc GhcPs) body) where
  getAnnotationEntry x = fromAnn (hsRecFieldAnn x)
  setAnnotationAnchor x anc cs = x { hsRecFieldAnn = setAnchorEpa (hsRecFieldAnn x) anc cs}
  exact (HsRecField an f arg isPun) = do
    debugM $ "HsRecField"
    f' <- markAnnotated f
    arg' <- if isPun
      then return arg
      else do
        markEpAnn an AnnEqual
        markAnnotated arg
    return (HsRecField an f' arg' isPun)

-- ---------------------------------------------------------------------

instance (ExactPrint body)
    => ExactPrint (HsRecField' (FieldLabelStrings GhcPs) body) where
  getAnnotationEntry x = fromAnn (hsRecFieldAnn x)
  setAnnotationAnchor x anc cs = x { hsRecFieldAnn = setAnchorEpa (hsRecFieldAnn x) anc cs}
  exact (HsRecField an f arg isPun) = do
    debugM $ "HsRecField FieldLabelStrings"
    f' <- markAnnotated f
    arg' <- if isPun
      then return arg
      else do
        markEpAnn an AnnEqual
        markAnnotated arg
    return (HsRecField an f' arg' isPun)

-- ---------------------------------------------------------------------

instance (ExactPrint (LocatedA body))
    => ExactPrint (HsRecField' (AmbiguousFieldOcc GhcPs) (LocatedA body)) where
  getAnnotationEntry x = fromAnn (hsRecFieldAnn x)
  setAnnotationAnchor x anc cs = x { hsRecFieldAnn = setAnchorEpa (hsRecFieldAnn x) anc cs}
  exact (HsRecField an f arg isPun) = do
    debugM $ "HsRecUpdField"
    f' <- markAnnotated f
    an0 <- if isPun then return an
                    else markEpAnnL an lidl AnnEqual
    arg' <- if ((locA $ getLoc arg) == noSrcSpan )
      then return arg
      else markAnnotated arg
    return (HsRecField an0 f' arg' isPun)

-- ---------------------------------------------------------------------

instance
    (ExactPrint (HsRecField' (a GhcPs) body),
     ExactPrint (HsRecField' (b GhcPs) body))
    => ExactPrint
         (Either [LocatedA (HsRecField' (a GhcPs) body)]
                 [LocatedA (HsRecField' (b GhcPs) body)]) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a
  exact (Left rbinds) = do
    rbinds' <- markAnnotated rbinds
    return (Left rbinds')
  exact (Right pbinds) = do
    pbinds' <- markAnnotated pbinds
    return (Right pbinds')

-- ---------------------------------------------------------------------

instance ExactPrint (FieldLabelStrings GhcPs) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a
  exact (FieldLabelStrings fs) = FieldLabelStrings <$> markAnnotated fs

-- ---------------------------------------------------------------------

instance ExactPrint (NonEmpty (Located (HsFieldLabel GhcPs))) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a
  exact (h :| t) = do
    h' <- markAnnotated h
    t' <- markAnnotated t
    return (h :| t)

-- ---------------------------------------------------------------------

instance ExactPrint (HsFieldLabel GhcPs) where
  getAnnotationEntry (HsFieldLabel an _) = fromAnn an
  setAnnotationAnchor (HsFieldLabel an a) anc cs = HsFieldLabel (setAnchorEpa an anc cs) a

  exact (HsFieldLabel an fs) = do
    an0 <- markLensKwM an lafDot  AnnDot
    fs' <- markAnnotated fs
    return (HsFieldLabel an0 fs')

-- ---------------------------------------------------------------------

instance ExactPrint (HsTupArg GhcPs) where
  getAnnotationEntry (Present an _) = fromAnn an
  getAnnotationEntry (Missing an)   = fromAnn an

  setAnnotationAnchor (Present an a) anc cs = Present (setAnchorEpa an anc cs) a
  setAnnotationAnchor (Missing an)   anc cs = Missing (setAnchorEpa an anc cs)

  exact (Present a e) = Present a <$> markAnnotated e

  exact a@(Missing EpAnnNotUsed) = return a
  exact a@(Missing _) = printStringAdvance "," >> return a

-- ---------------------------------------------------------------------

instance ExactPrint (HsCmdTop GhcPs) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a
  exact (HsCmdTop a cmd) = HsCmdTop a <$> markAnnotated cmd

-- ---------------------------------------------------------------------

instance ExactPrint (HsCmd GhcPs) where
  getAnnotationEntry (HsCmdArrApp an _ _ _ _)   = fromAnn an
  getAnnotationEntry (HsCmdArrForm an _ _ _ _ ) = fromAnn an
  getAnnotationEntry (HsCmdApp an _ _ )         = fromAnn an
  getAnnotationEntry (HsCmdLam {})              = NoEntryVal
  getAnnotationEntry (HsCmdPar an _)            = fromAnn an
  getAnnotationEntry (HsCmdCase an _ _)         = fromAnn an
  getAnnotationEntry (HsCmdLamCase an _)        = fromAnn an
  getAnnotationEntry (HsCmdIf an _ _ _ _)       = fromAnn an
  getAnnotationEntry (HsCmdLet an _ _)          = fromAnn an
  getAnnotationEntry (HsCmdDo an _)             = fromAnn an

  setAnnotationAnchor (HsCmdArrApp an a b c d)   anc cs = (HsCmdArrApp (setAnchorEpa an anc cs) a b c d)
  setAnnotationAnchor (HsCmdArrForm an a b c d ) anc cs = (HsCmdArrForm (setAnchorEpa an anc cs) a b c d )
  setAnnotationAnchor (HsCmdApp an a b )         anc cs = (HsCmdApp (setAnchorEpa an anc cs) a b )
  setAnnotationAnchor a@(HsCmdLam {})              _ _s = a
  setAnnotationAnchor (HsCmdPar an a)            anc cs = (HsCmdPar (setAnchorEpa an anc cs) a)
  setAnnotationAnchor (HsCmdCase an a b)         anc cs = (HsCmdCase (setAnchorEpa an anc cs) a b)
  setAnnotationAnchor (HsCmdLamCase an a)        anc cs = (HsCmdLamCase (setAnchorEpa an anc cs) a)
  setAnnotationAnchor (HsCmdIf an a b c d)       anc cs = (HsCmdIf (setAnchorEpa an anc cs) a b c d)
  setAnnotationAnchor (HsCmdLet an a b)          anc cs = (HsCmdLet (setAnchorEpa an anc cs) a b)
  setAnnotationAnchor (HsCmdDo an a)             anc cs = (HsCmdDo (setAnchorEpa an anc cs) a)

  exact (HsCmdArrApp an arr arg o isRightToLeft) = do
    if isRightToLeft
      then do
        arr' <- markAnnotated arr
        an0 <- markKw (anns an)
        arg' <- markAnnotated arg
        let an1 = an{anns = an0}
        return (HsCmdArrApp an1 arr' arg' o isRightToLeft)
      else do
        arg' <- markAnnotated arg
        an0 <- markKw (anns an)
        arr' <- markAnnotated arr
        let an1 = an{anns = an0}
        return (HsCmdArrApp an1 arr' arg' o isRightToLeft)

  exact (HsCmdArrForm an e fixity mf cs) = do
    -- markLocatedMAA an al_open
    an0 <- markLensMAA an lal_open
    (e',cs') <- case (fixity, cs) of
      (Infix, (arg1:argrest)) -> do
        arg1' <- markAnnotated arg1
        e' <- markAnnotated e
        argrest' <- markAnnotated argrest
        return (e', arg1':argrest')
      (Prefix, _) -> do
        e' <- markAnnotated e
        cs' <- markAnnotated cs
        return (e', cs')
      (Infix, []) -> error "Not possible"
    an1 <- markLensMAA an0 lal_close
    return (HsCmdArrForm an1 e' fixity mf cs')

  exact (HsCmdApp an e1 e2) = do
    e1' <- markAnnotated e1
    e2' <- markAnnotated e2
    return (HsCmdApp an e1' e2')

  exact (HsCmdLam a match) = do
    match' <- markAnnotated match
    return (HsCmdLam a match')

  exact (HsCmdPar an e) = do
    an0 <- markOpeningParen an
    e' <- markAnnotated e
    an1 <- markClosingParen an0
    return (HsCmdPar an1 e')

  exact (HsCmdCase an e alts) = do
    an0 <- markLensKw an lhsCaseAnnCase AnnCase
    e' <- markAnnotated e
    an1 <- markLensKw an0 lhsCaseAnnOf AnnOf
    an2 <- markEpAnnL an1 lhsCaseAnnsRest AnnOpenC
    an3 <- markEpAnnAllL an2 lhsCaseAnnsRest AnnSemi
    alts' <- markAnnotated alts
    an4 <- markEpAnnL an3 lhsCaseAnnsRest AnnCloseC
    return (HsCmdCase an4 e' alts')

  exact (HsCmdLamCase an matches) = do
    an0 <- markEpAnnL an lidl AnnLam
    an1 <- markEpAnnL an0 lidl AnnCase
    matches' <- markAnnotated matches
    return (HsCmdLamCase an1 matches')

  exact (HsCmdIf an a e1 e2 e3) = do
    an0 <- markLensKw an laiIf AnnIf
    e1' <- markAnnotated e1
    markAnnKwM an0 aiThenSemi AnnSemi
    an2 <- markLensKw an0 laiThen AnnThen
    e2' <- markAnnotated e2
    markAnnKwM an aiElseSemi AnnSemi
    an4 <- markLensKw an2 laiElse AnnElse
    e3' <- markAnnotated e3
    return (HsCmdIf an4 a e1' e2' e3')

  exact (HsCmdLet an binds e) = do
    an0 <- markLensKw an lalLet AnnLet
    binds' <- markAnnotated binds
    an1 <- markLensKw an0 lalIn AnnIn
    e' <- markAnnotated e
    return (HsCmdLet an1 binds' e')

  exact (HsCmdDo an es) = do
    debugM $ "HsCmdDo"
    an0 <- markEpAnnL an lal_rest AnnDo
    es' <- markAnnotated es
    return (HsCmdDo an0 es')

  -- exact x = error $ "exact HsCmd for:" ++ showAst x

-- ---------------------------------------------------------------------

instance (
  ExactPrint (LocatedA (body GhcPs)),
                 Anno (StmtLR GhcPs GhcPs (LocatedA (body GhcPs))) ~ SrcSpanAnnA,
           Anno [GenLocated SrcSpanAnnA (StmtLR GhcPs GhcPs (LocatedA (body GhcPs)))] ~ SrcSpanAnnL,
           (ExactPrint (LocatedL [LocatedA (StmtLR GhcPs GhcPs (LocatedA (body GhcPs)))])))
   => ExactPrint (StmtLR GhcPs GhcPs (LocatedA (body GhcPs))) where
  getAnnotationEntry (LastStmt _ _ _ _)             = NoEntryVal
  getAnnotationEntry (BindStmt an _ _)              = fromAnn an
  getAnnotationEntry (ApplicativeStmt _ _ _)        = NoEntryVal
  getAnnotationEntry (BodyStmt _ _ _ _)             = NoEntryVal
  getAnnotationEntry (LetStmt an _)                 = fromAnn an
  getAnnotationEntry (ParStmt _ _ _ _)              = NoEntryVal
  getAnnotationEntry (TransStmt an _ _ _ _ _ _ _ _) = fromAnn an
  getAnnotationEntry (RecStmt an _ _ _ _ _ _)       = fromAnn an

  -----------------------------------------------------------------

  setAnnotationAnchor a@(LastStmt _ _ _ _)             _ _s = a
  setAnnotationAnchor (BindStmt an a b)              anc cs = (BindStmt (setAnchorEpa an anc cs) a b)
  setAnnotationAnchor a@(ApplicativeStmt _ _ _)        _ _s = a
  setAnnotationAnchor a@(BodyStmt _ _ _ _)             _ _s = a
  setAnnotationAnchor (LetStmt an a)                 anc cs = (LetStmt (setAnchorEpa an anc cs) a)
  setAnnotationAnchor a@(ParStmt _ _ _ _)              _ _s = a
  setAnnotationAnchor (TransStmt an a b c d e f g h) anc cs = (TransStmt (setAnchorEpa an anc cs) a b c d e f g h)
  setAnnotationAnchor (RecStmt an a b c d e f)       anc cs = (RecStmt (setAnchorEpa an anc cs) a b c d e f)

  -----------------------------------------------------------------

  exact (LastStmt a body b c) = do
    debugM $ "LastStmt"
    body' <- markAnnotated body
    return (LastStmt a body' b c)

  exact (BindStmt an pat body) = do
    debugM $ "BindStmt"
    pat' <- markAnnotated pat
    an0 <- markEpAnnL an lidl AnnLarrow
    body' <- markAnnotated body
    return (BindStmt an0 pat' body')

  exact (ApplicativeStmt _ _body _) = do
    debugM $ "ApplicativeStmt"
    -- TODO: ApplicativeStmt
    -- markAnnotated body
    error $ "need to complete ApplicativeStmt"

  exact (BodyStmt a body b c) = do
    debugM $ "BodyStmt"
    body' <- markAnnotated body
    return (BodyStmt a body' b c)

  exact (LetStmt an binds) = do
    debugM $ "LetStmt"
    an0 <- markEpAnnL an lidl AnnLet
    binds' <- markAnnotated binds
    return (LetStmt an0 binds')

  exact (ParStmt a pbs b c) = do
    debugM $ "ParStmt"
    pbs' <- markAnnotated pbs
    return (ParStmt a pbs' b c)

  exact (TransStmt an form stmts b using by c d e) = do
    debugM $ "TransStmt"
    stmts' <- markAnnotated stmts
    (by', using') <- exactTransStmt an by using form
    return (TransStmt an form stmts' b using' by' c d e)


  exact (RecStmt an stmts a b c d e) = do
    debugM $ "RecStmt"
    -- markLocatedAAL an al_rest AnnRec
    an0 <- markEpAnnL an lal_rest AnnRec
    (an1, stmts') <- markAnnList True an0 (markAnnotated stmts)
    return (RecStmt an1 stmts' a b c d e)

  -- exact x = error $ "exact CmdLStmt for:" ++ showAst x
  -- exact x = error $ "exact CmdLStmt for:"


-- ---------------------------------------------------------------------

instance ExactPrint (ParStmtBlock GhcPs GhcPs) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a
  exact (ParStmtBlock a stmts b c) = do
    stmts' <- markAnnotated stmts
    return (ParStmtBlock a stmts' b c)

exactTransStmt :: (Monad m, Monoid w)
  => EpAnn [AddEpAnn] -> Maybe (LHsExpr GhcPs) -> (LHsExpr GhcPs) -> TransForm
  -> EP w m (Maybe (LHsExpr GhcPs), (LHsExpr GhcPs))
exactTransStmt an by using ThenForm = do
  debugM $ "exactTransStmt:ThenForm"
  markEpAnn an AnnThen
  using' <- markAnnotated using
  case by of
    Nothing -> return (by, using')
    Just b -> do
      markEpAnn an AnnBy
      b' <- markAnnotated b
      return (Just b', using')
exactTransStmt an by using GroupForm = do
  debugM $ "exactTransStmt:GroupForm"
  markEpAnn an AnnThen
  markEpAnn an AnnGroup
  by' <- case by of
    Nothing -> return by
    Just b -> do
      markEpAnn an AnnBy
      b' <- markAnnotated b
      return (Just b')
  markEpAnn an AnnUsing
  using' <- markAnnotated using
  return (by', using')

-- ---------------------------------------------------------------------

instance ExactPrint (TyClDecl GhcPs) where
  getAnnotationEntry (FamDecl   { })                      = NoEntryVal
  getAnnotationEntry (SynDecl   { tcdSExt = an })         = fromAnn an
  getAnnotationEntry (DataDecl  { tcdDExt = an })         = fromAnn an
  getAnnotationEntry (ClassDecl { tcdCExt = (an, _, _) }) = fromAnn an

  setAnnotationAnchor a@FamDecl{}     _ _s = a
  setAnnotationAnchor x@SynDecl{}   anc cs = x { tcdSExt = setAnchorEpa (tcdSExt x) anc cs }
  setAnnotationAnchor x@DataDecl{}  anc cs = x { tcdDExt = setAnchorEpa (tcdDExt x) anc cs }
  setAnnotationAnchor x@ClassDecl{} anc cs = x { tcdCExt = (setAnchorEpa an anc cs, a, b) }
    where
      (an,a,b) = tcdCExt x

  exact (FamDecl a decl) = do
    decl' <- markAnnotated decl
    return (FamDecl a decl')

  exact (SynDecl { tcdSExt = an
                 , tcdLName = ltycon, tcdTyVars = tyvars, tcdFixity = fixity
                 , tcdRhs = rhs }) = do
    -- There may be arbitrary parens around parts of the constructor
    -- that are infix.  Turn these into comments so that they feed
    -- into the right place automatically
    an0 <- annotationsToComments an lidl [AnnOpenP,AnnCloseP]
    an1 <- markEpAnnL an0 lidl AnnType

    (_anx, ltycon', tyvars',_,_) <- exactVanillaDeclHead ltycon tyvars fixity Nothing
    an2 <- markEpAnnL an1 lidl AnnEqual
    rhs' <- markAnnotated rhs
    return (SynDecl { tcdSExt = an2
                    , tcdLName = ltycon', tcdTyVars = tyvars', tcdFixity = fixity
                    , tcdRhs = rhs' })

  -- TODO: add a workaround for https://gitlab.haskell.org/ghc/ghc/-/issues/20452
  exact (DataDecl { tcdDExt = an, tcdLName = ltycon, tcdTyVars = tyvars
                  , tcdFixity = fixity, tcdDataDefn = defn }) = do
    (_, an', ltycon', tyvars', _, _mctxt', defn') <-
      exactDataDefn an (exactVanillaDeclHead ltycon tyvars fixity) defn
    return (DataDecl { tcdDExt = an', tcdLName = ltycon', tcdTyVars = tyvars'
                     , tcdFixity = fixity, tcdDataDefn = defn' })

  -- -----------------------------------

  exact (ClassDecl {tcdCExt = (an, sortKey, lo),
                    tcdCtxt = context, tcdLName = lclas, tcdTyVars = tyvars,
                    tcdFixity = fixity,
                    tcdFDs  = fds,
                    tcdSigs = sigs, tcdMeths = methods,
                    tcdATs = ats, tcdATDefs = at_defs,
                    tcdDocs = _docs})
      -- TODO: add a test that demonstrates tcdDocs
      | null sigs && isEmptyBag methods && null ats && null at_defs -- No "where" part
      = do
          (an0, fds', lclas', tyvars',context') <- top_matter
          an1 <- markEpAnnL an0 lidl AnnOpenC
          an2 <- markEpAnnL an1 lidl AnnCloseC
          return (ClassDecl {tcdCExt = (an2, sortKey, lo),
                             tcdCtxt = context', tcdLName = lclas', tcdTyVars = tyvars',
                             tcdFixity = fixity,
                             tcdFDs  = fds',
                             tcdSigs = sigs, tcdMeths = methods,
                             tcdATs = ats, tcdATDefs = at_defs,
                             tcdDocs = _docs})

      | otherwise       -- Laid out
      = do
          (an0, fds', lclas', tyvars',context') <- top_matter
          an1 <- markEpAnnL    an0 lidl AnnOpenC
          an2 <- markEpAnnAllL an1 lidl AnnSemi
          ds <- withSortKey sortKey
                               (prepareListAnnotationA sigs
                             ++ prepareListAnnotationA (bagToList methods)
                             ++ prepareListAnnotationA ats
                             ++ prepareListAnnotationA at_defs
                             -- ++ prepareListAnnotation docs
                               )
          an3 <- markEpAnnL an2 lidl AnnCloseC
          let
            sigs'    = undynamic ds
            methods' = listToBag $ undynamic ds
            ats'     = undynamic ds
            at_defs' = undynamic ds
          return (ClassDecl {tcdCExt = (an3, sortKey, lo),
                             tcdCtxt = context', tcdLName = lclas', tcdTyVars = tyvars',
                             tcdFixity = fixity,
                             tcdFDs  = fds',
                             tcdSigs = sigs', tcdMeths = methods',
                             tcdATs = ats', tcdATDefs = at_defs',
                             tcdDocs = _docs})
      where
        top_matter = do
          an' <- annotationsToComments an lidl  [AnnOpenP, AnnCloseP]
          an0 <- markEpAnnL an' lidl AnnClass
          (_, lclas', tyvars',_,context') <-  exactVanillaDeclHead lclas tyvars fixity context
          (an1, fds') <- if (null fds)
            then return (an0, fds)
            else do
              an1 <- markEpAnnL an0 lidl AnnVbar
              fds' <- markAnnotated fds
              return (an1, fds')
          an2 <- markEpAnnL an1 lidl AnnWhere
          return (an2, fds', lclas', tyvars',context')


-- ---------------------------------------------------------------------

instance ExactPrint (FunDep GhcPs) where
  getAnnotationEntry (FunDep an _ _) = fromAnn an
  setAnnotationAnchor (FunDep an a b) anc cs = FunDep (setAnchorEpa an anc cs) a b

  exact (FunDep an ls rs') = do
    ls' <- markAnnotated ls
    markEpAnn an AnnRarrow
    rs'' <- markAnnotated rs'
    return (FunDep an ls' rs'')

-- ---------------------------------------------------------------------

instance ExactPrint (FamilyDecl GhcPs) where
  getAnnotationEntry (FamilyDecl { fdExt = an }) = fromAnn an
  setAnnotationAnchor x anc cs = x { fdExt = setAnchorEpa (fdExt x) anc cs}

  exact (FamilyDecl { fdExt = an
                    , fdInfo = info
                    , fdTopLevel = top_level
                    , fdLName = ltycon
                    , fdTyVars = tyvars
                    , fdFixity = fixity
                    , fdResultSig = L lr result
                    , fdInjectivityAnn = mb_inj }) = do
    an0 <- exactFlavour an info
    an1 <- exact_top_level an0
    an2 <- annotationsToComments an1 lidl [AnnOpenP,AnnCloseP]
    (_, ltycon', tyvars',_,_) <- exactVanillaDeclHead ltycon tyvars fixity Nothing
    (an3, result') <- exact_kind an2
    (an4, mb_inj') <-
      case mb_inj of
        Nothing -> return (an3, mb_inj)
        Just inj -> do
          an4 <- markEpAnnL an3 lidl AnnVbar
          inj' <- markAnnotated inj
          return (an4, Just inj')
    (an5, info') <-
             case info of
               ClosedTypeFamily mb_eqns -> do
                 an5 <- markEpAnnL an4 lidl AnnWhere
                 an6 <- markEpAnnL an5 lidl AnnOpenC
                 (an7, mb_eqns') <-
                   case mb_eqns of
                     Nothing -> do
                       an7 <- markEpAnnL an6 lidl AnnDotdot
                       return (an7, mb_eqns)
                     Just eqns -> do
                       eqns' <- markAnnotated eqns
                       return (an6, Just eqns')
                 an8 <- markEpAnnL an7 lidl AnnCloseC
                 return (an8, ClosedTypeFamily mb_eqns')
               _ -> return (an4, info)
    return (FamilyDecl { fdExt = an5
                       , fdInfo = info'
                       , fdTopLevel = top_level
                       , fdLName = ltycon'
                       , fdTyVars = tyvars'
                       , fdFixity = fixity
                       , fdResultSig = L lr result'
                       , fdInjectivityAnn = mb_inj' })
    where
      exact_top_level an' =
        case top_level of
          TopLevel    -> markEpAnnL an' lidl AnnFamily
          NotTopLevel -> do
            -- It seems that in some kind of legacy
            -- mode the 'family' keyword is still
            -- accepted.
            markEpAnnL an' lidl AnnFamily

      exact_kind an' =
        case result of
          NoSig    _         -> return (an', result)
          KindSig  x kind    -> do
            an0 <- markEpAnnL an' lidl AnnDcolon
            kind' <- markAnnotated kind
            return (an0, KindSig  x kind')
          TyVarSig x tv_bndr -> do
            an0 <- markEpAnnL an' lidl AnnEqual
            tv_bndr' <- markAnnotated tv_bndr
            return (an0, TyVarSig x tv_bndr')


exactFlavour :: (Monad m, Monoid w) => EpAnn [AddEpAnn] -> FamilyInfo GhcPs -> EP w m (EpAnn [AddEpAnn])
exactFlavour an DataFamily            = markEpAnnL an lidl AnnData
exactFlavour an OpenTypeFamily        = markEpAnnL an lidl AnnType
exactFlavour an (ClosedTypeFamily {}) = markEpAnnL an lidl AnnType

-- ---------------------------------------------------------------------

exactDataDefn
  :: (Monad m, Monoid w)
  => EpAnn [AddEpAnn]
  -> (Maybe (LHsContext GhcPs) -> EP w m (EpAnn [AddEpAnn]
                                         , LocatedN RdrName
                                         , a
                                         , b
                                         , Maybe (LHsContext GhcPs))) -- Printing the header
  -> HsDataDefn GhcPs
  -> EP w m ( EpAnn [AddEpAnn] -- ^ from exactHdr
            , EpAnn [AddEpAnn] -- ^ updated one passed in
            , LocatedN RdrName, a, b, Maybe (LHsContext GhcPs), HsDataDefn GhcPs)
exactDataDefn an exactHdr
                 (HsDataDefn { dd_ext = x, dd_ND = new_or_data, dd_ctxt = context
                             , dd_cType = mb_ct
                             , dd_kindSig = mb_sig
                             , dd_cons = condecls, dd_derivs = derivings }) = do
  an' <- annotationsToComments an lidl [AnnOpenP, AnnCloseP]
  an0 <- if new_or_data == DataType
    then markEpAnnL an' lidl AnnData
    else markEpAnnL an' lidl AnnNewtype
  an1 <- markEpAnnL an0 lidl AnnInstance -- optional
  mb_ct' <- mapM markAnnotated mb_ct
  (anx, ln', tvs', b, mctxt') <- exactHdr context
  (an2, mb_sig') <- case mb_sig of
    Nothing -> return (an1, Nothing)
    Just kind -> do
      an2 <- markEpAnnL an1 lidl AnnDcolon
      kind' <- markAnnotated kind
      return (an2, Just kind')
  an3 <- if (isGadt condecls)
    then markEpAnnL an2 lidl AnnWhere
    else return an2
  an4 <- markEpAnnL an3 lidl AnnOpenC
  (an5, condecls') <- exact_condecls an4 condecls
  an6 <- markEpAnnL an5 lidl AnnCloseC
  derivings' <- mapM markAnnotated derivings
  return (anx, an6, ln', tvs', b, mctxt',
                 (HsDataDefn { dd_ext = x, dd_ND = new_or_data, dd_ctxt = context
                             , dd_cType = mb_ct'
                             , dd_kindSig = mb_sig'
                             , dd_cons = condecls', dd_derivs = derivings' }))


exactVanillaDeclHead :: (Monad m, Monoid w)
                     => LocatedN RdrName
                     -> LHsQTyVars GhcPs
                     -> LexicalFixity
                     -> Maybe (LHsContext GhcPs)
                     -> EP w m ( EpAnn [AddEpAnn]
                               , LocatedN RdrName
                               , LHsQTyVars GhcPs
                               , (), Maybe (LHsContext GhcPs))
exactVanillaDeclHead thing tvs@(HsQTvs { hsq_explicit = tyvars }) fixity context = do
  let
    exact_tyvars (varl:varsr)
      | fixity == Infix && length varsr > 1 = do
          varl' <- markAnnotated varl
          thing' <- markAnnotated thing
          hvarsr <- markAnnotated (head varsr)
          tvarsr <- markAnnotated (tail varsr)
          return (thing', varl':hvarsr:tvarsr)
      | fixity == Infix = do
          varl' <- markAnnotated varl
          thing' <- markAnnotated thing
          varsr' <- markAnnotated varsr
          return (thing', varl':varsr')
      | otherwise = do
          thing' <- markAnnotated thing
          vs <- mapM markAnnotated (varl:varsr)
          return (thing', vs)
    exact_tyvars [] = do
      thing' <- markAnnotated thing
      return (thing', [])
  context' <- mapM markAnnotated context
  (thing', tyvars') <- exact_tyvars tyvars
  return (EpAnnNotUsed, thing', tvs { hsq_explicit = tyvars' }, (), context')

-- ---------------------------------------------------------------------

instance ExactPrint (InjectivityAnn GhcPs) where
  getAnnotationEntry (InjectivityAnn an _ _) = fromAnn an
  setAnnotationAnchor (InjectivityAnn an a b) anc cs = InjectivityAnn (setAnchorEpa an anc cs) a b
  exact (InjectivityAnn an lhs rhs) = do
    markEpAnn an AnnVbar
    lhs' <- markAnnotated lhs
    markEpAnn an AnnRarrow
    rhs' <- mapM markAnnotated rhs
    return (InjectivityAnn an lhs' rhs')

-- ---------------------------------------------------------------------

class Typeable flag => ExactPrintTVFlag flag where
  exactTVDelimiters :: (Monad m, Monoid w) => EpAnn [AddEpAnn] -> flag -> EP w m a -> EP w m a

instance ExactPrintTVFlag () where
  exactTVDelimiters an _ thing_inside = do
    markEpAnnAll an id AnnOpenP
    r <- thing_inside
    markEpAnnAll an id AnnCloseP
    return r

instance ExactPrintTVFlag Specificity where
  exactTVDelimiters an s thing_inside = do
    markEpAnnAll an id open
    r <- thing_inside
    markEpAnnAll an id close
    return r
    where
      (open, close) = case s of
        SpecifiedSpec -> (AnnOpenP, AnnCloseP)
        InferredSpec  -> (AnnOpenC, AnnCloseC)

instance ExactPrintTVFlag flag => ExactPrint (HsTyVarBndr flag GhcPs) where
  getAnnotationEntry (UserTyVar an _ _)     = fromAnn an
  getAnnotationEntry (KindedTyVar an _ _ _) = fromAnn an

  setAnnotationAnchor (UserTyVar an a b)     anc cs = UserTyVar (setAnchorEpa an anc cs) a b
  setAnnotationAnchor (KindedTyVar an a b c) anc cs = KindedTyVar (setAnchorEpa an anc cs) a b c

  exact (UserTyVar an flag n) = do
    exactTVDelimiters an flag $ do
      n' <- markAnnotated n
      return (UserTyVar an flag n')
  exact (KindedTyVar an flag n k) = exactTVDelimiters an flag $ do
    n' <- markAnnotated n
    markEpAnn an AnnDcolon
    k' <- markAnnotated k
    return (KindedTyVar an flag n' k')

-- ---------------------------------------------------------------------

instance ExactPrint (HsType GhcPs) where
  getAnnotationEntry (HsForAllTy _ _ _)        = NoEntryVal
  getAnnotationEntry (HsQualTy _ _ _)          = NoEntryVal
  getAnnotationEntry (HsTyVar an _ _)          = fromAnn an
  getAnnotationEntry (HsAppTy _ _ _)           = NoEntryVal
  getAnnotationEntry (HsAppKindTy _ _ _)       = NoEntryVal
  getAnnotationEntry (HsFunTy an _ _ _)        = fromAnn an
  getAnnotationEntry (HsListTy an _)           = fromAnn an
  getAnnotationEntry (HsTupleTy an _ _)        = fromAnn an
  getAnnotationEntry (HsSumTy an _)            = fromAnn an
  getAnnotationEntry (HsOpTy _ _ _ _)          = NoEntryVal
  getAnnotationEntry (HsParTy an _)            = fromAnn an
  getAnnotationEntry (HsIParamTy an _ _)       = fromAnn an
  getAnnotationEntry (HsStarTy _ _)            = NoEntryVal
  getAnnotationEntry (HsKindSig an _ _)        = fromAnn an
  getAnnotationEntry (HsSpliceTy _ _)          = NoEntryVal
  getAnnotationEntry (HsDocTy an _ _)          = fromAnn an
  getAnnotationEntry (HsBangTy an _ _)         = fromAnn an
  getAnnotationEntry (HsRecTy an _)            = fromAnn an
  getAnnotationEntry (HsExplicitListTy an _ _) = fromAnn an
  getAnnotationEntry (HsExplicitTupleTy an _)  = fromAnn an
  getAnnotationEntry (HsTyLit _ _)             = NoEntryVal
  getAnnotationEntry (HsWildCardTy _)          = NoEntryVal
  getAnnotationEntry (XHsType _)               = NoEntryVal

  setAnnotationAnchor a@(HsForAllTy _ _ _)        _ _s = a
  setAnnotationAnchor a@(HsQualTy _ _ _)          _ _s = a
  setAnnotationAnchor (HsTyVar an a b)          anc cs = (HsTyVar (setAnchorEpa an anc cs) a b)
  setAnnotationAnchor a@(HsAppTy _ _ _)           _ _s = a
  setAnnotationAnchor a@(HsAppKindTy _ _ _)       _ _s = a
  setAnnotationAnchor (HsFunTy an a b c)        anc cs = (HsFunTy (setAnchorEpa an anc cs) a b c)
  setAnnotationAnchor (HsListTy an a)           anc cs = (HsListTy (setAnchorEpa an anc cs) a)
  setAnnotationAnchor (HsTupleTy an a b)        anc cs = (HsTupleTy (setAnchorEpa an anc cs) a b)
  setAnnotationAnchor (HsSumTy an a)            anc cs = (HsSumTy (setAnchorEpa an anc cs) a)
  setAnnotationAnchor a@(HsOpTy _ _ _ _)          _ _s = a
  setAnnotationAnchor (HsParTy an a)            anc cs = (HsParTy (setAnchorEpa an anc cs) a)
  setAnnotationAnchor (HsIParamTy an a b)       anc cs = (HsIParamTy (setAnchorEpa an anc cs) a b)
  setAnnotationAnchor a@(HsStarTy _ _)            _ _s = a
  setAnnotationAnchor (HsKindSig an a b)        anc cs = (HsKindSig (setAnchorEpa an anc cs) a b)
  setAnnotationAnchor a@(HsSpliceTy _ _)          _ _s = a
  setAnnotationAnchor (HsDocTy an a b)          anc cs = (HsDocTy (setAnchorEpa an anc cs) a b)
  setAnnotationAnchor (HsBangTy an a b)         anc cs = (HsBangTy (setAnchorEpa an anc cs) a b)
  setAnnotationAnchor (HsRecTy an a)            anc cs = (HsRecTy (setAnchorEpa an anc cs) a)
  setAnnotationAnchor (HsExplicitListTy an a b) anc cs = (HsExplicitListTy (setAnchorEpa an anc cs) a b)
  setAnnotationAnchor (HsExplicitTupleTy an a)  anc cs = (HsExplicitTupleTy (setAnchorEpa an anc cs) a)
  setAnnotationAnchor a@(HsTyLit _ _)             _ _s = a
  setAnnotationAnchor a@(HsWildCardTy _)          _ _s = a
  setAnnotationAnchor a@(XHsType _)               _ _s = a

  exact (HsForAllTy { hst_xforall = an
                    , hst_tele = tele, hst_body = ty }) = do
    tele' <- markAnnotated tele
    ty' <- markAnnotated ty
    return (HsForAllTy { hst_xforall = an
                       , hst_tele = tele', hst_body = ty' })

  exact (HsQualTy an ctxt ty) = do
    ctxt' <- markAnnotated ctxt
    ty' <- markAnnotated ty
    return (HsQualTy an ctxt' ty')
  exact (HsTyVar an promoted name) = do
    an0 <- if (promoted == IsPromoted)
             then markEpAnnL an lidl AnnSimpleQuote
             else return an
    name' <- markAnnotated name
    return (HsTyVar an0 promoted name')
  exact (HsAppTy an t1 t2) = do
    t1' <- markAnnotated t1
    t2' <- markAnnotated t2
    return (HsAppTy an t1' t2')
  exact (HsAppKindTy ss ty ki) = do
    ty' <- markAnnotated ty
    printStringAtSs ss "@"
    ki' <- markAnnotated ki
    return (HsAppKindTy ss ty' ki')
  exact (HsFunTy an mult ty1 ty2) = do
    ty1' <- markAnnotated ty1
    (an', mult') <- markArrow an mult
    ty2' <- markAnnotated ty2
    return (HsFunTy an' mult' ty1' ty2')
  exact (HsListTy an tys) = do
    an0 <- markOpeningParen an
    tys' <- markAnnotated tys
    an1 <- markClosingParen an0
    return (HsListTy an1 tys')
  exact (HsTupleTy an con tys) = do
    an0 <- markOpeningParen an
    tys' <- markAnnotated tys
    an1 <- markClosingParen an0
    return (HsTupleTy an1 con tys')
  exact (HsSumTy an tys) = do
    an0 <- markOpeningParen an
    tys' <- markAnnotated tys
    an1 <- markClosingParen an0
    return (HsSumTy an1 tys')
  exact (HsOpTy an t1 lo t2) = do
    t1' <- markAnnotated t1
    lo' <- markAnnotated lo
    t2' <- markAnnotated t2
    return (HsOpTy an t1' lo' t2')
  exact (HsParTy an ty) = do
    an0 <- markOpeningParen an
    ty' <- markAnnotated ty
    an1 <- markClosingParen an0
    return (HsParTy an1 ty')
  exact (HsIParamTy an n t) = do
    n' <- markAnnotated n
    markEpAnn an AnnDcolon
    t' <- markAnnotated t
    return (HsIParamTy an n' t')
  exact (HsStarTy an isUnicode)
    = do
    if isUnicode
        then printStringAdvance "\x2605" -- Unicode star
        else printStringAdvance "*"
    return (HsStarTy an isUnicode)
  exact (HsKindSig an ty k) = do
    ty' <- markAnnotated ty
    markEpAnn an AnnDcolon
    k' <- markAnnotated k
    return (HsKindSig an ty' k')
  exact (HsSpliceTy a splice) = do
    splice' <- markAnnotated splice
    return (HsSpliceTy a splice')
  -- exact x@(HsDocTy an _ _)          = withPpr x
  exact (HsBangTy an (HsSrcBang mt up str) ty) = do
    an0 <-
      case mt of
        NoSourceText -> return an
        SourceText src -> do
          debugM $ "HsBangTy: src=" ++ showAst src
          an0 <- markEpAnnLMS an lid AnnOpen  (Just src)
          an1 <- markEpAnnLMS an0 lid AnnClose (Just "#-}")
          debugM $ "HsBangTy: done unpackedness"
          return an1
    an1 <-
      case str of
        SrcLazy     -> markEpAnnL an0 lidl AnnTilde
        SrcStrict   -> markEpAnnL an0 lidl AnnBang
        NoSrcStrict -> return an0
    ty' <- markAnnotated ty
    return (HsBangTy an1 (HsSrcBang mt up str) ty')
  -- exact x@(HsRecTy an _)            = withPpr x
  exact (HsExplicitListTy an prom tys) = do
    an0 <- if (isPromoted prom)
             then markEpAnnL an lidl AnnSimpleQuote
             else return an
    an1 <- markEpAnnL an0 lidl AnnOpenS
    tys' <- markAnnotated tys
    an2 <- markEpAnnL an1 lidl AnnCloseS
    return (HsExplicitListTy an2 prom tys')
  exact (HsExplicitTupleTy an tys) = do
    an0 <- markEpAnnL an lidl AnnSimpleQuote
    an1 <- markEpAnnL an0 lidl AnnOpenP
    tys' <- markAnnotated tys
    an2 <- markEpAnnL an1 lidl AnnCloseP
    return (HsExplicitTupleTy an2 tys')
  exact (HsTyLit a lit) = do
    case lit of
      (HsNumTy src v) -> printSourceText src (show v)
      (HsStrTy src v) -> printSourceText src (show v)
      (HsCharTy src v) -> printSourceText src (show v)
    return (HsTyLit a lit)
  exact t@(HsWildCardTy _) = printStringAdvance "_" >> return t
  exact x = error $ "missing match for HsType:" ++ showAst x

-- ---------------------------------------------------------------------

instance ExactPrint (HsForAllTelescope GhcPs) where
  getAnnotationEntry (HsForAllVis an _)   = fromAnn an
  getAnnotationEntry (HsForAllInvis an _) = fromAnn an

  setAnnotationAnchor (HsForAllVis an a) anc cs = HsForAllVis (setAnchorEpa an anc cs) a
  setAnnotationAnchor (HsForAllInvis an a) anc cs = HsForAllInvis (setAnchorEpa an anc cs) a

  exact (HsForAllVis an bndrs)   = do
    markLocatedAA an fst -- AnnForall
    bndrs' <- markAnnotated bndrs
    markLocatedAA an snd -- AnnRarrow
    return (HsForAllVis an bndrs')

  exact (HsForAllInvis an bndrs) = do
    markLocatedAA an fst -- AnnForall
    bndrs' <- markAnnotated bndrs
    markLocatedAA an snd -- AnnDot
    return (HsForAllInvis an bndrs')

-- ---------------------------------------------------------------------

instance ExactPrint (HsDerivingClause GhcPs) where
  getAnnotationEntry d@(HsDerivingClause{}) = fromAnn (deriv_clause_ext d)
  setAnnotationAnchor x anc cs = (x { deriv_clause_ext = setAnchorEpa (deriv_clause_ext x) anc cs})
                                   `debug` ("setAnnotationAnchor HsDerivingClause: (anc,cs):" ++ showAst (anc,cs))

  exact (HsDerivingClause { deriv_clause_ext      = an
                          , deriv_clause_strategy = dcs
                          , deriv_clause_tys      = dct }) = do
    markEpAnn an AnnDeriving
    exact_strat_before
    dct' <- markAnnotated dct
    exact_strat_after
    return (HsDerivingClause { deriv_clause_ext      = an
                             , deriv_clause_strategy = dcs
                             , deriv_clause_tys      = dct' })
      where
        -- -- This complexity is to distinguish between
        -- --    deriving Show
        -- --    deriving (Show)
        -- pp_dct [HsIB { hsib_body = ty }]
        --          = ppr (parenthesizeHsType appPrec ty)
        -- pp_dct _ = parens (interpp'SP dct)

        -- @via@ is unique in that in comes /after/ the class being derived,
        -- so we must special-case it.
        (exact_strat_before, exact_strat_after) =
          case dcs of
            Just v@(L _ ViaStrategy{}) -> (pure (), markAnnotated v >> pure ())
            _                          -> (mapM_ markAnnotated dcs, pure ())

-- ---------------------------------------------------------------------

instance ExactPrint (DerivStrategy GhcPs) where
  getAnnotationEntry (StockStrategy an)    = fromAnn an
  getAnnotationEntry (AnyclassStrategy an) = fromAnn an
  getAnnotationEntry (NewtypeStrategy an)  = fromAnn an
  getAnnotationEntry (ViaStrategy (XViaStrategyPs an  _)) = fromAnn an

  setAnnotationAnchor (StockStrategy an)    anc cs = (StockStrategy (setAnchorEpa an anc cs))
  setAnnotationAnchor (AnyclassStrategy an) anc cs = (AnyclassStrategy (setAnchorEpa an anc cs))
  setAnnotationAnchor (NewtypeStrategy an)  anc cs = (NewtypeStrategy (setAnchorEpa an anc cs))
  setAnnotationAnchor (ViaStrategy (XViaStrategyPs an  a)) anc cs = (ViaStrategy (XViaStrategyPs (setAnchorEpa an anc cs)  a))

  exact s@(StockStrategy an)    = markEpAnn an AnnStock >> return s
  exact s@(AnyclassStrategy an) = markEpAnn an AnnAnyclass >> return s
  exact s@(NewtypeStrategy an)  = markEpAnn an AnnNewtype >> return s
  exact s@(ViaStrategy (XViaStrategyPs an ty))
    = markEpAnn an AnnVia >> markAnnotated ty >> return s

-- ---------------------------------------------------------------------

instance (ExactPrint a) => ExactPrint (LocatedC a) where
  getAnnotationEntry (L sann _) = fromAnn sann
  setAnnotationAnchor = setAnchorAn

  exact (L (SrcSpanAnn EpAnnNotUsed l) a) = do
    a' <- markAnnotated a
    return (L (SrcSpanAnn EpAnnNotUsed l) a')
  exact (L (SrcSpanAnn (EpAnn anc (AnnContext ma opens closes) cs) l) a) = do
    -- mapM_ (markKwA AnnOpenP) (sort opens)
    mapM_ (markKwA AnnOpenP) opens
    a' <- markAnnotated a
    -- mapM_ (markKwA AnnCloseP) (sort closes)
    mapM_ (markKwA AnnCloseP) closes
    case ma of
      Just (UnicodeSyntax, r) -> markKwA AnnDarrowU r >> pure ()
      Just (NormalSyntax,  r) -> markKwA AnnDarrow  r >> pure ()
      Nothing -> pure ()
    return (L (SrcSpanAnn (EpAnn anc (AnnContext ma opens closes) cs) l) a')

-- ---------------------------------------------------------------------

instance ExactPrint (DerivClauseTys GhcPs) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a

  exact (DctSingle x ty) = do
    ty' <- markAnnotated ty
    return (DctSingle x ty')
  exact (DctMulti x tys) = do
    tys' <- markAnnotated tys
    return (DctMulti x tys')

-- ---------------------------------------------------------------------

instance ExactPrint (HsSigType GhcPs) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a

  exact (HsSig a bndrs ty) = do
    bndrs' <- markAnnotated bndrs
    ty' <- markAnnotated ty
    return (HsSig a bndrs' ty')

-- ---------------------------------------------------------------------

instance ExactPrint (LocatedN RdrName) where
  getAnnotationEntry (L sann _) = fromAnn sann
  setAnnotationAnchor = setAnchorAn

  exact x@(L (SrcSpanAnn EpAnnNotUsed l) n) = do
    printUnicode (spanAsAnchor l) n
    return x
  exact (L (SrcSpanAnn (EpAnn anc ann cs) ll) n) = do
    ann' <-
      case ann of
        NameAnn a o l c t -> do
          mn <- markName a o (Just (l,n)) c
          let (o', (Just (l',_)), c') =
                case mn of
                 (o', (Just (l',n')), c') -> (o', (Just (l',n')), c')
                 _ -> error "ExactPrint (LocatedN RdrName)"
          t' <- markTrailing t
          return (NameAnn a o' l' c' t')
        NameAnnCommas a o commas c t -> do
          let (kwo,kwc) = adornments a
          (AddEpAnn _ o') <- markKwC NoCaptureComments (AddEpAnn kwo o)
          commas' <- forM commas (\loc -> locFromAdd <$> markKwC NoCaptureComments (AddEpAnn AnnComma loc))
          (AddEpAnn _ c') <- markKwC NoCaptureComments (AddEpAnn kwc c)
          t' <- markTrailing t
          return (NameAnnCommas a o' commas' c' t')
        NameAnnOnly a o c t -> do
          (o',_,c') <- markName a o Nothing c
          t' <- markTrailing t
          return (NameAnnOnly a o' c' t')
        NameAnnRArrow nl t -> do
          (AddEpAnn _ nl') <- markKwC NoCaptureComments (AddEpAnn AnnRarrow nl)
          t' <- markTrailing t
          return (NameAnnRArrow nl' t')
        NameAnnQuote q name t -> do
          debugM $ "NameAnnQuote"
          (AddEpAnn _ q') <- markKwC NoCaptureComments (AddEpAnn AnnSimpleQuote q)
          (L name' _) <- markAnnotated (L name n)
          t' <- markTrailing t
          return (NameAnnQuote q' name' t')
        NameAnnTrailing t -> do
          anc' <- printUnicode anc n
          t' <- markTrailing t
          return (NameAnnTrailing t')
    return (L (SrcSpanAnn (EpAnn anc ann' cs) ll) n)

locFromAdd :: AddEpAnn -> EpaLocation
locFromAdd (AddEpAnn _ loc) = loc

printUnicode :: (Monad m, Monoid w) => Anchor -> RdrName -> EP w m Anchor
printUnicode anc n = do
  let str = case (showPprUnsafe n) of
            -- TODO: unicode support?
              "forall" -> if spanLength (anchor anc) == 1 then "∀" else "forall"
              s -> s
  -- printStringAtSs l str
  -- loc <- printStringAtAAC NoCaptureComments (anchorToEpaLocation anc) str
  -- case loc of
  --   EpaSpan _ -> return anc
  --   EpaDelta dp [] -> return anc { anchor_op = MovedAnchor dp }
  loc <- printStringAtAAC NoCaptureComments (EpaDelta (SameLine 0) []) str
  case loc of
    EpaSpan _ -> return anc
    EpaDelta dp [] -> return anc { anchor_op = MovedAnchor dp }


markName :: (Monad m, Monoid w)
  => NameAdornment -> EpaLocation -> Maybe (EpaLocation,RdrName) -> EpaLocation
  -> EP w m (EpaLocation, Maybe (EpaLocation,RdrName), EpaLocation)
markName adorn open mname close = do
  let (kwo,kwc) = adornments adorn
  (AddEpAnn _ open') <- markKwC CaptureComments (AddEpAnn kwo open)
  mname' <-
    case mname of
      Nothing -> return Nothing
      Just (name, a) -> do
        name' <- printStringAtAAC CaptureComments name (showPprUnsafe a)
        return (Just (name',a))
  (AddEpAnn _ close') <- markKwC CaptureComments (AddEpAnn kwc close)
  return (open', mname', close')

adornments :: NameAdornment -> (AnnKeywordId, AnnKeywordId)
adornments NameParens     = (AnnOpenP, AnnCloseP)
adornments NameParensHash = (AnnOpenPH, AnnClosePH)
adornments NameBackquotes = (AnnBackquote, AnnBackquote)
adornments NameSquare     = (AnnOpenS, AnnCloseS)

markTrailing :: (Monad m, Monoid w) => [TrailingAnn] -> EP w m [TrailingAnn]
markTrailing ts = do
  p <- getPosP
  debugM $ "markTrailing:" ++ showPprUnsafe (p,ts)
  -- mapM markKwT (sort ts)
  mapM markKwT ts

-- ---------------------------------------------------------------------

-- based on pp_condecls in Decls.hs
exact_condecls :: (Monad m, Monoid w)
  => EpAnn [AddEpAnn] -> [LConDecl GhcPs] -> EP w m (EpAnn [AddEpAnn],[LConDecl GhcPs])
exact_condecls an cs
  | gadt_syntax                  -- In GADT syntax
  = do
      cs' <- mapM markAnnotated cs
      return (an, cs')
  | otherwise                    -- In H98 syntax
  = do
      an0 <- markEpAnnL an lidl AnnEqual
      cs' <- mapM markAnnotated cs
      return (an0, cs')
  where
    gadt_syntax = case cs of
      []                      -> False
      (L _ ConDeclH98{}  : _) -> False
      (L _ ConDeclGADT{} : _) -> True

-- ---------------------------------------------------------------------

instance ExactPrint (ConDecl GhcPs) where
  getAnnotationEntry x@(ConDeclGADT{}) = fromAnn (con_g_ext x)
  getAnnotationEntry x@(ConDeclH98{})  = fromAnn (con_ext x)

  setAnnotationAnchor x@ConDeclGADT{} anc cs = x { con_g_ext = setAnchorEpa (con_g_ext x) anc cs}
  setAnnotationAnchor x@ConDeclH98{}  anc cs = x { con_ext   = setAnchorEpa (con_ext x) anc cs}

-- based on pprConDecl
  exact (ConDeclH98 { con_ext = an
                    , con_name = con
                    , con_forall = has_forall
                    , con_ex_tvs = ex_tvs
                    , con_mb_cxt = mcxt
                    , con_args = args
                    , con_doc = doc }) = do
    doc' <- mapM markAnnotated doc
    an0 <- if has_forall
      then markEpAnnL an lidl AnnForall
      else return an
    ex_tvs' <- mapM markAnnotated ex_tvs
    an1 <- if has_forall
      then markEpAnnL an0 lidl AnnDot
      else return an0
    mcxt' <- mapM markAnnotated mcxt
    an2 <- if (isJust mcxt)
      then markEpAnnL an1 lidl AnnDarrow
      else return an1

    (con', args') <- exact_details args
    return (ConDeclH98 { con_ext = an2
                       , con_name = con'
                       , con_forall = has_forall
                       , con_ex_tvs = ex_tvs'
                       , con_mb_cxt = mcxt'
                       , con_args = args'
                       , con_doc = doc' })

    where
    --   -- In ppr_details: let's not print the multiplicities (they are always 1, by
    --   -- definition) as they do not appear in an actual declaration.
      exact_details (InfixCon t1 t2) = do
        t1' <- markAnnotated t1
        con' <- markAnnotated con
        t2' <- markAnnotated t2
        return (con', InfixCon t1' t2')
      exact_details (PrefixCon tyargs tys) = do
        con' <- markAnnotated con
        tyargs' <- markAnnotated tyargs
        tys' <- markAnnotated tys
        return (con', PrefixCon tyargs' tys')
      exact_details (RecCon fields) = do
        con' <- markAnnotated con
        fields' <- markAnnotated fields
        return (con', RecCon fields')

  -- -----------------------------------

  exact (ConDeclGADT { con_g_ext = an
                     , con_names = cons
                     , con_bndrs = bndrs
                     , con_mb_cxt = mcxt, con_g_args = args
                     , con_res_ty = res_ty, con_doc = doc }) = do
    doc' <- mapM markAnnotated doc
    cons' <- mapM markAnnotated cons
    an0 <- markEpAnnL an lidl AnnDcolon
    an1 <- annotationsToComments an0 lidl  [AnnOpenP, AnnCloseP]

    -- Work around https://gitlab.haskell.org/ghc/ghc/-/issues/20558
    bndrs' <- case bndrs of
      L _ (HsOuterImplicit _) -> return bndrs
      _ -> markAnnotated bndrs

    mcxt' <- mapM markAnnotated mcxt
    an2 <- if (isJust mcxt)
      then markEpAnnL an1 lidl AnnDarrow
      else return an1
    args' <-
      case args of
          (PrefixConGADT args0) -> do
            args0' <- mapM markScaled args0
            return (PrefixConGADT args0')
          (RecConGADT fields) -> do
            fields' <- markAnnotated fields
            return (RecConGADT fields')
    res_ty' <- markAnnotated res_ty
    return (ConDeclGADT { con_g_ext = an2
                        , con_names = cons'
                        , con_bndrs = bndrs'
                        , con_mb_cxt = mcxt', con_g_args = args'
                        , con_res_ty = res_ty', con_doc = doc' })

-- ---------------------------------------------------------------------

instance ExactPrint Void where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a
  exact x = return x

-- ---------------------------------------------------------------------

instance ExactPrintTVFlag flag => ExactPrint (HsOuterTyVarBndrs flag GhcPs) where
  getAnnotationEntry (HsOuterImplicit _) = NoEntryVal
  getAnnotationEntry (HsOuterExplicit an _) = fromAnn an

  setAnnotationAnchor (HsOuterImplicit a) _ _ = HsOuterImplicit a
  setAnnotationAnchor (HsOuterExplicit an a) anc cs = HsOuterExplicit (setAnchorEpa an anc cs) a

  exact b@(HsOuterImplicit _) = pure b
  exact (HsOuterExplicit an bndrs) = do
    markLocatedAA an fst -- "forall"
    bndrs' <- markAnnotated bndrs
    markLocatedAA an snd -- "."
    return (HsOuterExplicit an bndrs')

-- ---------------------------------------------------------------------

instance ExactPrint (ConDeclField GhcPs) where
  getAnnotationEntry f@(ConDeclField{}) = fromAnn (cd_fld_ext f)

  setAnnotationAnchor x anc cs = x { cd_fld_ext = setAnchorEpa (cd_fld_ext x) anc cs}

  exact (ConDeclField an names ftype mdoc) = do
    names' <- markAnnotated names
    markEpAnn an AnnDcolon
    ftype' <- markAnnotated ftype
    mdoc' <- mapM markAnnotated mdoc
    return (ConDeclField an names' ftype' mdoc')

-- ---------------------------------------------------------------------

instance ExactPrint (FieldOcc GhcPs) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a
  exact f@(FieldOcc _ n) = markAnnotated n >> return f

-- ---------------------------------------------------------------------

instance ExactPrint (AmbiguousFieldOcc GhcPs) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a
  exact f@(Unambiguous _ n) = markAnnotated n >> return f
  exact f@(Ambiguous   _ n) = markAnnotated n >> return f

-- ---------------------------------------------------------------------

markScaled :: (Monad m, Monoid w)
  => HsScaled GhcPs (LBangType GhcPs) -> EP w m (HsScaled GhcPs (LBangType GhcPs))
markScaled (HsScaled arr (L l c)) = do
  (L l1 (HsScaled arr' (L l2 c'))) <- markAnnotated (L l (HsScaled arr (L (noAnnSrcSpan $ locA l) c))
                 :: LocatedA (HsScaled GhcPs (LBangType GhcPs)))
  let l' = l1 <> l2
  return (HsScaled arr' (L l' c'))

instance (ExactPrint a) => ExactPrint (HsScaled GhcPs a) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a
  exact (HsScaled arr t) = do
    t' <- markAnnotated t
    (_, arr') <- markArrow EpAnnNotUsed arr
    return (HsScaled arr' t')

-- ---------------------------------------------------------------------

instance ExactPrint (LocatedP CType) where
  getAnnotationEntry = entryFromLocatedA
  setAnnotationAnchor = setAnchorAn

  exact x@(L (SrcSpanAnn EpAnnNotUsed _) ct) = withPpr ct >> return x
  exact (L (SrcSpanAnn an ll)
         (CType stp mh (stct,ct))) = do
    an0 <- markAnnOpenP an stp "{-# CTYPE"
    an1 <- case mh of
             Nothing -> return an0
             Just (Header srcH _h) ->
               markEpAnnLMS an0 lapr_rest AnnHeader (Just (toSourceTextWithSuffix srcH "" ""))
    an2 <- markEpAnnLMS an1 lapr_rest AnnVal (Just (toSourceTextWithSuffix stct (unpackFS ct) ""))
    an3 <- markAnnCloseP an2
    return (L (SrcSpanAnn an3 ll)
         (CType stp mh (stct,ct)))

-- ---------------------------------------------------------------------

instance ExactPrint (SourceText, RuleName) where
  -- We end up at the right place from the Located wrapper
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a

  exact (st, rn)
    = printStringAdvance (toSourceTextWithSuffix st (unpackFS rn) "")
      >> return (st, rn)


-- =====================================================================
-- LocatedL instances start --
--
-- Each is dealt with specifically, as they have
-- different wrapping annotations in the al_rest zone.
--
-- In future, the annotation could perhaps be improved, with an
-- 'al_pre' and 'al_post' set of annotations to be simply sorted and
-- applied.
-- ---------------------------------------------------------------------

instance ExactPrint (LocatedL [LocatedA (IE GhcPs)]) where
  getAnnotationEntry = entryFromLocatedA
  setAnnotationAnchor = setAnchorAn

  exact (L (SrcSpanAnn an l) ies) = do
    debugM $ "LocatedL [LIE"
    an0 <- markEpAnnL an lal_rest AnnHiding
    p <- getPosP
    debugM $ "LocatedL [LIE:p=" ++ showPprUnsafe p
    (an1, ies') <- markAnnList True an0 (markAnnotated ies)
    return (L (SrcSpanAnn an1 l) ies')

instance (ExactPrint (Match GhcPs (LocatedA body)))
   => ExactPrint (LocatedL [LocatedA (Match GhcPs (LocatedA body))]) where
  getAnnotationEntry = entryFromLocatedA
  setAnnotationAnchor = setAnchorAn
  exact (L la a) = do
    let an = ann la
    debugM $ "LocatedL [LMatch"
    -- TODO: markAnnList?
    an0 <- markEpAnnAllL an lal_rest AnnWhere
    an1 <- markLensMAA an0 lal_open
    an2 <- markEpAnnAllL an1 lal_rest AnnSemi
    a' <- markAnnotated a
    an3 <- markLensMAA an2 lal_close
    return (L (la { ann = an3}) a')

-- instance ExactPrint (LocatedL [ExprLStmt GhcPs]) where
instance ExactPrint (LocatedL [LocatedA (StmtLR GhcPs GhcPs (LocatedA (HsExpr GhcPs)))]) where
  getAnnotationEntry = entryFromLocatedAFixed
  setAnnotationAnchor = setAnchorAn
  exact (L (SrcSpanAnn an' l) stmts) = do
    let an = fixAnnListAnn an'
    debugM $ "LocatedL [ExprLStmt"
    (an'', stmts') <- markAnnList True an $ do
      -- markLocatedMAA an al_open
      case snocView stmts of
        Just (initStmts, ls@(L _ (LastStmt _ _body _ _))) -> do
          debugM $ "LocatedL [ExprLStmt: snocView"
          ls' <- markAnnotated ls
          initStmts' <- markAnnotated initStmts
          return (initStmts' ++ [ls'])
        _ -> do
          markAnnotated stmts
        -- x -> error $ "pprDo:ListComp" ++ showAst x
      -- markLocatedMAA an al_close
    return (L (SrcSpanAnn an'' l) stmts')

-- instance ExactPrint (LocatedL [CmdLStmt GhcPs]) where
instance ExactPrint (LocatedL [LocatedA (StmtLR GhcPs GhcPs (LocatedA (HsCmd GhcPs)))]) where
  getAnnotationEntry = entryFromLocatedAFixed
  setAnnotationAnchor = setAnchorAn
  exact (L (SrcSpanAnn ann' l) es) = do
    let ann = fixAnnListAnn ann'
    debugM $ "LocatedL [CmdLStmt"
    an0 <- markLensMAA ann lal_open
    es' <- mapM markAnnotated es
    an1 <- markLensMAA an0 lal_close
    return (L (SrcSpanAnn an1 l) es')

instance ExactPrint (LocatedL [LocatedA (ConDeclField GhcPs)]) where
  getAnnotationEntry = entryFromLocatedA
  setAnnotationAnchor = setAnchorAn
  exact (L (SrcSpanAnn an l) fs) = do
    debugM $ "LocatedL [LConDeclField"
    (an', fs') <- markAnnList True an (markAnnotated fs)
    return (L (SrcSpanAnn an' l) fs')

instance ExactPrint (LocatedL (BF.BooleanFormula (LocatedN RdrName))) where
  getAnnotationEntry = entryFromLocatedA
  setAnnotationAnchor = setAnchorAn
  exact (L (SrcSpanAnn an l) bf) = do
    debugM $ "LocatedL [LBooleanFormula"
    (an', bf') <- markAnnList True an (markAnnotated bf)
    return (L (SrcSpanAnn an' l) bf')

-- ---------------------------------------------------------------------
-- LocatedL instances end --
-- =====================================================================

instance ExactPrint (IE GhcPs) where
  getAnnotationEntry (IEVar _ _)            = NoEntryVal
  getAnnotationEntry (IEThingAbs an _)      = fromAnn an
  getAnnotationEntry (IEThingAll an _)      = fromAnn an
  getAnnotationEntry (IEThingWith an _ _ _) = fromAnn an
  getAnnotationEntry (IEModuleContents an _)= fromAnn an
  getAnnotationEntry (IEGroup _ _ _)        = NoEntryVal
  getAnnotationEntry (IEDoc _ _)            = NoEntryVal
  getAnnotationEntry (IEDocNamed _ _)       = NoEntryVal

  setAnnotationAnchor a@(IEVar _ _)             _ _s = a
  setAnnotationAnchor (IEThingAbs an a)       anc cs = (IEThingAbs (setAnchorEpa an anc cs) a)
  setAnnotationAnchor (IEThingAll an a)       anc cs = (IEThingAll (setAnchorEpa an anc cs) a)
  setAnnotationAnchor (IEThingWith an a b c)  anc cs = (IEThingWith (setAnchorEpa an anc cs) a b c)
  setAnnotationAnchor (IEModuleContents an a) anc cs = (IEModuleContents (setAnchorEpa an anc cs) a)
  setAnnotationAnchor a@(IEGroup _ _ _)         _ _s = a
  setAnnotationAnchor a@(IEDoc _ _)             _ _s = a
  setAnnotationAnchor a@(IEDocNamed _ _)        _ _s = a

  exact (IEVar x ln) = do
    ln' <- markAnnotated ln
    return (IEVar x ln')
  exact (IEThingAbs x thing) = do
    thing' <- markAnnotated thing
    return (IEThingAbs x thing')
  exact (IEThingAll an thing) = do
    thing' <- markAnnotated thing
    markEpAnn an AnnOpenP
    markEpAnn an AnnDotdot
    markEpAnn an AnnCloseP
    return (IEThingAll an thing')

  exact (IEThingWith an thing wc withs) = do
    thing' <- markAnnotated thing
    markEpAnn an AnnOpenP
    (wc', withs') <-
      case wc of
        NoIEWildcard -> do
          withs'' <- markAnnotated withs
          return (wc, withs'')
        IEWildcard pos -> do
          let (bs, as) = splitAt pos withs
          bs' <- markAnnotated bs
          markEpAnn an AnnDotdot
          markEpAnn an AnnComma
          as' <- markAnnotated as
          return (wc, bs'++as')
    markEpAnn an AnnCloseP
    return (IEThingWith an thing' wc' withs')

  exact (IEModuleContents an m) = do
    an0 <- markEpAnnL an lidl AnnModule
    m' <- markAnnotated m
    return (IEModuleContents an0 m')

  -- exact (IEGroup _ _ _)          = NoEntryVal
  -- exact (IEDoc _ _)              = NoEntryVal
  -- exact (IEDocNamed _ _)         = NoEntryVal
  exact x = error $ "missing match for IE:" ++ showAst x

-- ---------------------------------------------------------------------

instance ExactPrint (IEWrappedName RdrName) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a

  exact (IEName n) = do
    n' <- markAnnotated n
    return (IEName n')
  exact (IEPattern r n) = do
    r' <- printStringAtAA r "pattern"
    n' <- markAnnotated n
    return (IEPattern r' n')
  exact (IEType r n) = do
    r' <- printStringAtAA r "type"
    n' <- markAnnotated n
    return (IEType r' n')

-- ---------------------------------------------------------------------

instance ExactPrint (Pat GhcPs) where
  getAnnotationEntry (WildPat _)              = NoEntryVal
  getAnnotationEntry (VarPat _ _)             = NoEntryVal
  getAnnotationEntry (LazyPat an _)           = fromAnn an
  getAnnotationEntry (AsPat an _ _)           = fromAnn an
  getAnnotationEntry (ParPat an _)            = fromAnn an
  getAnnotationEntry (BangPat an _)           = fromAnn an
  getAnnotationEntry (ListPat an _)           = fromAnn an
  getAnnotationEntry (TuplePat an _ _)        = fromAnn an
  getAnnotationEntry (SumPat an _ _ _)        = fromAnn an
  getAnnotationEntry (ConPat an _ _)          = fromAnn an
  getAnnotationEntry (ViewPat an _ _)         = fromAnn an
  getAnnotationEntry (SplicePat _ _)          = NoEntryVal
  getAnnotationEntry (LitPat _ _)             = NoEntryVal
  getAnnotationEntry (NPat an _ _ _)          = fromAnn an
  getAnnotationEntry (NPlusKPat an _ _ _ _ _) = fromAnn an
  getAnnotationEntry (SigPat an _ _)          = fromAnn an

  setAnnotationAnchor a@(WildPat _)              _ _s = a
  setAnnotationAnchor a@(VarPat _ _)             _ _s = a
  setAnnotationAnchor (LazyPat an a)            anc cs = (LazyPat (setAnchorEpa an anc cs) a)
  setAnnotationAnchor (AsPat an a b)            anc cs = (AsPat (setAnchorEpa an anc cs) a b)
  setAnnotationAnchor (ParPat an a)             anc cs = (ParPat (setAnchorEpa an anc cs) a)
  setAnnotationAnchor (BangPat an a)            anc cs = (BangPat (setAnchorEpa an anc cs) a)
  setAnnotationAnchor (ListPat an a)            anc cs = (ListPat (setAnchorEpa an anc cs) a)
  setAnnotationAnchor (TuplePat an a b)         anc cs = (TuplePat (setAnchorEpa an anc cs) a b)
  setAnnotationAnchor (SumPat an a b c)         anc cs = (SumPat (setAnchorEpa an anc cs) a b c)
  setAnnotationAnchor (ConPat an a b)           anc cs = (ConPat (setAnchorEpa an anc cs) a b)
  setAnnotationAnchor (ViewPat an a b)          anc cs = (ViewPat (setAnchorEpa an anc cs) a b)
  setAnnotationAnchor a@(SplicePat _ _)         _ _s = a
  setAnnotationAnchor a@(LitPat _ _)            _ _s = a
  setAnnotationAnchor (NPat an a b c)          anc cs = (NPat (setAnchorEpa an anc cs) a b c)
  setAnnotationAnchor (NPlusKPat an a b c d e) anc cs = (NPlusKPat (setAnchorEpa an anc cs) a b c d e)
  setAnnotationAnchor (SigPat an a b)          anc cs = (SigPat (setAnchorEpa an anc cs) a b)

  exact (WildPat w) = do
    anchor <- getAnchorU
    debugM $ "WildPat:anchor=" ++ show anchor
    _ <- printStringAtRs anchor "_"
    return (WildPat w)
  exact (VarPat x n) = do
    -- The parser inserts a placeholder value for a record pun rhs. This must be
    -- filtered.
    let pun_RDR = "pun-right-hand-side"
    n' <- if (showPprUnsafe n /= pun_RDR)
      then markAnnotated n
      else return n
    return (VarPat x n')
  exact (LazyPat an pat) = do
    an0 <- markEpAnnL an lidl AnnTilde
    pat' <- markAnnotated pat
    return (LazyPat an0 pat')
  exact (AsPat an n pat) = do
    n' <- markAnnotated n
    an0 <- markEpAnnL an lidl AnnAt
    pat' <- markAnnotated pat
    return (AsPat an0 n' pat')
  exact (ParPat an pat) = do
    an0 <- markAnnKwL an lap_open AnnOpenP
    pat' <- markAnnotated pat
    an1 <- markAnnKwL an0 lap_close AnnCloseP
    return (ParPat an1 pat')
  exact (BangPat an pat) = do
    an0 <- markEpAnnL an lidl AnnBang
    pat' <- markAnnotated pat
    return (BangPat an0 pat')

  exact (ListPat an pats) = do
    (an', pats') <- markAnnList True an (markAnnotated pats)
    return (ListPat an' pats')

  exact (TuplePat an pats boxity) = do
    an0 <- case boxity of
             Boxed   -> markEpAnnL an lidl AnnOpenP
             Unboxed -> markEpAnnL an lidl AnnOpenPH
    pats' <- markAnnotated pats
    an1 <- case boxity of
             Boxed   -> markEpAnnL an0 lidl AnnCloseP
             Unboxed -> markEpAnnL an0 lidl AnnClosePH
    return (TuplePat an1 pats' boxity)

  exact (SumPat an pat alt arity) = do
    an0 <- markEpAnnL an lsumPatParens AnnOpenPH
    an1 <- markAnnKwAllL an0 lsumPatVbarsBefore AnnVbar
    pat' <- markAnnotated pat
    an2 <- markAnnKwAllL an1 lsumPatVbarsAfter AnnVbar
    an3 <- markEpAnnL an2 lsumPatParens AnnClosePH
    return (SumPat an3 pat' alt arity)

  -- | ConPat an con args)
  exact (ConPat an con details) = do
    (an', con', details') <- exactUserCon an con details
    return (ConPat an' con' details')
  exact (ViewPat an expr pat) = do
    expr' <- markAnnotated expr
    an0 <- markEpAnnL an lidl AnnRarrow
    pat' <- markAnnotated pat
    return (ViewPat an0 expr' pat')
  exact (SplicePat x splice) = do
    splice' <- markAnnotated splice
    return (SplicePat x splice')
  exact p@(LitPat _ lit) = printStringAdvance (hsLit2String lit) >> return p
  exact (NPat an ol mn z) = do
    an0 <- if (isJust mn)
      then markEpAnnL an lidl AnnMinus
      else return an
    ol' <- markAnnotated ol
    return (NPat an0 ol' mn z)

  -- | NPlusKPat an n lit1 lit2 _ _)
  exact (NPlusKPat an n k lit2 a b) = do
    n' <- markAnnotated n
    an' <- printStringAtAAL an lid "+"
    k' <- markAnnotated k
    return (NPlusKPat an' n' k' lit2 a b)

  exact (SigPat an pat sig) = do
    pat' <- markAnnotated pat
    an0 <- markEpAnnL an lidl AnnDcolon
    sig' <- markAnnotated sig
    return (SigPat an0 pat' sig')

-- ---------------------------------------------------------------------

instance ExactPrint (HsPatSigType GhcPs) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a

  exact (HsPS an ty) = do
    an0 <- markAnnKwL an lid AnnAt
    ty' <- markAnnotated ty
    return (HsPS an0 ty')

-- ---------------------------------------------------------------------

instance ExactPrint (HsOverLit GhcPs) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a

  exact ol =
    let str = case ol_val ol of
                HsIntegral   (IL src _ _) -> src
                HsFractional (FL{ fl_text = src }) -> src
                HsIsString src _ -> src
    in
      case str of
        SourceText s -> printStringAdvance s >> return ol
        NoSourceText -> return ol

-- ---------------------------------------------------------------------

hsLit2String :: HsLit GhcPs -> String
hsLit2String lit =
  case lit of
    HsChar       src v   -> toSourceTextWithSuffix src v ""
    -- It should be included here
    -- https://github.com/ghc/ghc/blob/master/compiler/parser/Lexer.x#L1471
    HsCharPrim   src p   -> toSourceTextWithSuffix src p "#"
    HsString     src v   -> toSourceTextWithSuffix src v ""
    HsStringPrim src v   -> toSourceTextWithSuffix src v ""
    HsInt        _ (IL src _ v)   -> toSourceTextWithSuffix src v ""
    HsIntPrim    src v   -> toSourceTextWithSuffix src v ""
    HsWordPrim   src v   -> toSourceTextWithSuffix src v ""
    HsInt64Prim  src v   -> toSourceTextWithSuffix src v ""
    HsWord64Prim src v   -> toSourceTextWithSuffix src v ""
    HsInteger    src v _ -> toSourceTextWithSuffix src v ""
    HsRat        _ fl@(FL{fl_text = src }) _ -> toSourceTextWithSuffix src fl ""
    HsFloatPrim  _ fl@(FL{fl_text = src })   -> toSourceTextWithSuffix src fl "#"
    HsDoublePrim _ fl@(FL{fl_text = src })   -> toSourceTextWithSuffix src fl "##"
    -- (XLit x) -> error $ "got XLit for:" ++ showPprUnsafe x

toSourceTextWithSuffix :: (Show a) => SourceText -> a -> String -> String
toSourceTextWithSuffix (NoSourceText)    alt suffix = show alt ++ suffix
toSourceTextWithSuffix (SourceText txt) _alt suffix = txt ++ suffix

sourceTextToString :: SourceText -> String -> String
sourceTextToString NoSourceText alt   = alt
sourceTextToString (SourceText txt) _ = txt

-- ---------------------------------------------------------------------

exactUserCon :: (Monad m, Monoid w, ExactPrint con)
  => EpAnn [AddEpAnn] -> con -> HsConPatDetails GhcPs
  -> EP w m (EpAnn [AddEpAnn], con, HsConPatDetails GhcPs)
exactUserCon an c (InfixCon p1 p2) = do
  p1' <- markAnnotated p1
  c' <- markAnnotated c
  p2' <- markAnnotated p2
  return (an, c', InfixCon p1' p2')
exactUserCon an c details = do
  c' <- markAnnotated c
  an0 <- markEpAnnL an lidl AnnOpenC
  details' <- exactConArgs details
  an1 <- markEpAnnL an0 lidl AnnCloseC
  return (an1, c', details')

exactConArgs :: (Monad m, Monoid w)
  => HsConPatDetails GhcPs -> EP w m (HsConPatDetails GhcPs)
exactConArgs (PrefixCon tyargs pats) = do
  tyargs' <- markAnnotated tyargs
  pats' <- markAnnotated pats
  return (PrefixCon tyargs' pats')
exactConArgs (InfixCon p1 p2) = do
  p1' <- markAnnotated p1
  p2' <- markAnnotated p2
  return (InfixCon p1' p2')
exactConArgs (RecCon rpats) = do
  rpats' <- markAnnotated rpats
  return (RecCon rpats')

-- ---------------------------------------------------------------------

entryFromLocatedA :: LocatedAn ann a -> Entry
entryFromLocatedA (L la _) = fromAnn la

-- See https://gitlab.haskell.org/ghc/ghc/-/issues/20256
entryFromLocatedAFixed :: LocatedL a -> Entry
entryFromLocatedAFixed (L la _)
  = fromAnn (fixSrcAnnL la)

-- =====================================================================
-- Utility stuff
-- ---------------------------------------------------------------------

-- |This should be the final point where things are mode concrete,
-- before output.
-- NOTE: despite the name, this is the ghc-exactprint final output for
-- the PRINT phase.
printStringAtLsDelta :: (Monad m, Monoid w) => DeltaPos -> String -> EP w m ()
printStringAtLsDelta cl s = do
  p <- getPosP
  colOffset <- getLayoutOffsetP
  if isGoodDeltaWithOffset cl colOffset
    then do
      printStringAt (undelta p cl colOffset) s
        -- `debug` ("printStringAtLsDelta:(pos,s):" ++ show (undelta p cl colOffset,s))
      p' <- getPosP
      debugM $ "printStringAtLsDelta:(pos,p',s):" ++ show (undelta p cl colOffset,p',s)
    else return () `debug` ("printStringAtLsDelta:bad delta for (mc,s):" ++ show (cl,s))

-- ---------------------------------------------------------------------

isGoodDeltaWithOffset :: DeltaPos -> LayoutStartCol -> Bool
isGoodDeltaWithOffset dp colOffset = isGoodDelta (deltaPos l c)
  where (l,c) = undelta (0,0) dp colOffset

-- | Print a comment, using the current layout offset to convert the
-- @DeltaPos@ to an absolute position.
printQueuedComment :: (Monad m, Monoid w) => RealSrcSpan -> Comment -> DeltaPos -> EP w m ()
printQueuedComment loc Comment{commentContents} dp = do
  p <- getPosP
  colOffset <- getLayoutOffsetP
  let (dr,dc) = undelta (0,0) dp colOffset
  -- do not lose comments against the left margin
  when (isGoodDelta (deltaPos dr (max 0 dc))) $ do
    printCommentAt (undelta p dp colOffset) commentContents
    setPriorEndASTD False loc
  p' <- getPosP
  debugM $ "printQueuedComment: (p,p',dp,colOffset,undelta)=" ++ show (p,p',dp,colOffset,undelta p dp colOffset)

------------------------------------------------------------------------

setLayoutBoth :: (Monad m, Monoid w) => EP w m a -> EP w m a
setLayoutBoth k = do
  oldLHS <- gets dLHS
  oldAnchorOffset <- getLayoutOffsetP
  debugM $ "setLayoutBoth: (oldLHS,oldAnchorOffset)=" ++ show (oldLHS,oldAnchorOffset)
  modify (\a -> a { dMarkLayout = True
                  , pMarkLayout = True } )
  let reset = do
        debugM $ "setLayoutBoth:reset: (oldLHS,oldAnchorOffset)=" ++ show (oldLHS,oldAnchorOffset)
        modify (\a -> a { dMarkLayout = False
                        , dLHS = oldLHS
                        , pMarkLayout = False
                        , pLHS = oldAnchorOffset} )
  k <* reset

-- Use 'local', designed for this
setLayoutTopLevelP :: (Monad m, Monoid w) => EP w m a -> EP w m a
setLayoutTopLevelP k = do
  debugM $ "setLayoutTopLevelP entered"
  oldAnchorOffset <- getLayoutOffsetP
  modify (\a -> a { pMarkLayout = False
                  , pLHS = 0} )
  r <- k
  debugM $ "setLayoutTopLevelP:resetting"
  setLayoutOffsetP oldAnchorOffset
  return r

------------------------------------------------------------------------

getPosP :: (Monad m, Monoid w) => EP w m Pos
getPosP = gets epPos

setPosP :: (Monad m, Monoid w) => Pos -> EP w m ()
setPosP l = do
  -- debugM $ "setPosP:" ++ show l
  modify (\s -> s {epPos = l})

getExtraDP :: (Monad m, Monoid w) => EP w m (Maybe Anchor)
getExtraDP = gets uExtraDP

setExtraDP :: (Monad m, Monoid w) => Maybe Anchor -> EP w m ()
setExtraDP md = do
  debugM $ "setExtraDP:" ++ show md
  modify (\s -> s {uExtraDP = md})

getPriorEndD :: (Monad m, Monoid w) => EP w m Pos
getPriorEndD = gets dPriorEndPosition

getAnchorU :: (Monad m, Monoid w) => EP w m RealSrcSpan
getAnchorU = gets uAnchorSpan

setPriorEndD :: (Monad m, Monoid w) => Pos -> EP w m ()
setPriorEndD pe = do
  -- setLayoutStartIfNeededD (snd pe)
  setPriorEndNoLayoutD pe

setPriorEndNoLayoutD :: (Monad m, Monoid w) => Pos -> EP w m ()
setPriorEndNoLayoutD pe = do
  debugM $ "setPriorEndNoLayout:pe=" ++ show pe
  modify (\s -> s { dPriorEndPosition = pe })

setPriorEndASTD :: (Monad m, Monoid w) => Bool -> RealSrcSpan -> EP w m ()
setPriorEndASTD layout pe = setPriorEndASTPD layout (rs2range pe)

setPriorEndASTPD :: (Monad m, Monoid w) => Bool -> (Pos,Pos) -> EP w m ()
setPriorEndASTPD layout pe@(fm,to) = do
  debugM $ "setPriorEndASTD:pe=" ++ show pe
  when layout $ setLayoutStartD (snd fm)
  modify (\s -> s { dPriorEndPosition = to } )

setLayoutStartD :: (Monad m, Monoid w) => Int -> EP w m ()
setLayoutStartD p = do
  EPState{dMarkLayout} <- get
  when dMarkLayout $ do
    debugM $ "setLayoutStartD: setting dLHS=" ++ show p
    modify (\s -> s { dMarkLayout = False
                    , dLHS = LayoutStartCol p})

setAnchorU :: (Monad m, Monoid w) => RealSrcSpan -> EP w m ()
setAnchorU rss = do
  debugM $ "setAnchorU:" ++ show (rs2range rss)
  modify (\s -> s { uAnchorSpan = rss })

getUnallocatedComments :: (Monad m, Monoid w) => EP w m [Comment]
getUnallocatedComments = gets epComments

putUnallocatedComments :: (Monad m, Monoid w) => [Comment] -> EP w m ()
putUnallocatedComments cs = modify (\s -> s { epComments = cs } )

-- | Push a fresh stack frame for the applied comments gatherer
pushAppliedComments  :: (Monad m, Monoid w) => EP w m ()
pushAppliedComments = modify (\s -> s { epCommentsApplied = []:(epCommentsApplied s) })

-- | Return the comments applied since the last call
-- takeAppliedComments, and clear them, not popping the stack
takeAppliedComments :: (Monad m, Monoid w) => EP w m [Comment]
takeAppliedComments = do
  ccs <- gets epCommentsApplied
  case ccs of
    [] -> do
      modify (\s -> s { epCommentsApplied = [] })
      return []
    h:t -> do
      modify (\s -> s { epCommentsApplied = []:t })
      return (reverse h)

-- | Return the comments applied since the last call
-- takeAppliedComments, and clear them, popping the stack
takeAppliedCommentsPop :: (Monad m, Monoid w) => EP w m [Comment]
takeAppliedCommentsPop = do
  ccs <- gets epCommentsApplied
  case ccs of
    [] -> do
      modify (\s -> s { epCommentsApplied = [] })
      return []
    h:t -> do
      modify (\s -> s { epCommentsApplied = t })
      return h

-- | Mark a comment as being applied.  This is used to update comments
-- when doing delta processing
applyComment :: (Monad m, Monoid w) => Comment -> EP w m ()
applyComment c = do
  ccs <- gets epCommentsApplied
  case ccs of
    []    -> modify (\s -> s { epCommentsApplied = [[c]] } )
    (h:t) -> modify (\s -> s { epCommentsApplied = (c:h):t } )

getLayoutOffsetP :: (Monad m, Monoid w) => EP w m LayoutStartCol
getLayoutOffsetP = gets pLHS

setLayoutOffsetP :: (Monad m, Monoid w) => LayoutStartCol -> EP w m ()
setLayoutOffsetP c = do
  debugM $ "setLayoutOffsetP:" ++ show c
  modify (\s -> s { pLHS = c })


-- ---------------------------------------------------------------------

advance :: (Monad m, Monoid w) => DeltaPos -> EP w m ()
advance dp = do
  p <- getPosP
  colOffset <- getLayoutOffsetP
  debugM $ "advance:(p,dp,colOffset,ws)=" ++ show (p,dp,colOffset,undelta p dp colOffset)
  printWhitespace (undelta p dp colOffset)

-- ---------------------------------------------------------------------

adjustDeltaForOffsetM :: (Monad m, Monoid w) => DeltaPos -> EP w m DeltaPos
adjustDeltaForOffsetM dp = do
  colOffset <- gets dLHS
  return (adjustDeltaForOffset colOffset dp)

-- ---------------------------------------------------------------------
-- Printing functions

printString :: (Monad m, Monoid w) => Bool -> String -> EP w m ()
printString layout str = do
  EPState{epPos = (_,c), pMarkLayout} <- get
  EPOptions{epTokenPrint, epWhitespacePrint} <- ask
  when (pMarkLayout && layout) $ do
    debugM $ "printString: setting pLHS to " ++ show c
    modify (\s -> s { pLHS = LayoutStartCol c, pMarkLayout = False } )

  -- Advance position, taking care of any newlines in the string
  let strDP = dpFromString str
      cr = getDeltaLine strDP
  p <- getPosP
  colOffset <- getLayoutOffsetP
  -- debugM $ "printString:(p,colOffset,strDP,cr)="  ++ show (p,colOffset,strDP,cr)
  if cr == 0
    then setPosP (undelta p strDP colOffset)
    else setPosP (undelta p strDP 1)

  -- Debug stuff
  -- pp <- getPosP
  -- debugM $ "printString: (p,pp,str)" ++ show (p,pp,str)
  -- Debug end

  --
  if not layout && c == 0
    then lift (epWhitespacePrint str) >>= \s -> tell EPWriter { output = s}
    else lift (epTokenPrint      str) >>= \s -> tell EPWriter { output = s}

--------------------------------------------------------

printStringAdvance :: (Monad m, Monoid w) => String -> EP w m ()
printStringAdvance str = do
  ss <- getAnchorU
  _ <- printStringAtRs ss str
  return ()

--------------------------------------------------------

newLine :: (Monad m, Monoid w) => EP w m ()
newLine = do
    (l,_) <- getPosP
    printString False "\n"
    setPosP (l+1,1)

padUntil :: (Monad m, Monoid w) => Pos -> EP w m ()
padUntil (l,c) = do
    (l1,c1) <- getPosP
    if | l1 == l && c1 <= c -> printString False $ replicate (c - c1) ' '
       | l1 < l             -> newLine >> padUntil (l,c)
       | otherwise          -> return ()

printWhitespace :: (Monad m, Monoid w) => Pos -> EP w m ()
printWhitespace = padUntil

printCommentAt :: (Monad m, Monoid w) => Pos -> String -> EP w m ()
printCommentAt p str = do
  debugM $ "printCommentAt: (pos,str)" ++ show (p,str)
  printWhitespace p >> printString False str

printStringAt :: (Monad m, Monoid w) => Pos -> String -> EP w m ()
printStringAt p str = printWhitespace p >> printString True str
