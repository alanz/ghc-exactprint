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
import Control.Monad.RWS
import Data.Data ( Data )
import Data.Dynamic
import Data.Foldable
import qualified Data.Set.Ordered as OSet
import Data.Typeable
import Data.List ( partition, sort, sortBy)
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
    -- runEP r (annotate ast) as
    undefined

makeDeltaAst :: ExactPrint ast => ast -> ast
makeDeltaAst ast = fst $ runIdentity (runEP deltaOptions (markAnnotated ast))

------------------------------------------------------

type EP w m a = RWST (EPOptions m w) (EPWriter w) EPState m a
type EPP a = EP String Identity a

runEP :: EPOptions Identity String
      -> Annotated a -> Identity (a, String)
runEP epReader action = do
  (ast, w) <- evalRWST action epReader defaultEPState
  return (ast, output w)


-- runEP :: (Monad m, Monoid a)
--       => PrintOptions m a
--       -> Annotated () -> Anns -> m a
-- runEP epReader action ans =
--   fmap (output . snd) .
--     (\next -> execRWST next epReader (defaultEPState ans))
--   . printInterpret $ action



-- -- | Unwrap an RWST computation as a function.
-- -- (The inverse of 'rwsT'.)
-- runRWST :: (Monoid w) => RWST r w s m a -> r -> s -> m (a, s, w)

-- -- | Evaluate a computation with the given initial state and environment,
-- -- returning the final value and output, discarding the final state.
-- evalRWST :: (Monad m, Monoid w)
--          => RWST r w s m a      -- ^computation to execute
--          -> r                   -- ^initial environment
--          -> s                   -- ^initial value
--          -> m (a, w)            -- ^computation yielding final value and output


-- -- | Evaluate a computation with the given initial state and environment,
-- -- returning the final state and output, discarding the final value.
-- execRWST :: (Monad m, Monoid w)
--          => RWST r w s m a      -- ^computation to execute
--          -> r                   -- ^initial environment
--          -> s                   -- ^initial value
--          -> m (s, w)            -- ^computation yielding final state and output



xx :: Annotated () -> EP String Identity ()
-- xx :: Annotated() -> RWST (EPOptions m w) (EPWriter w) EPState m ()
xx = id

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

-- | Options which can be used to simply update the AST to be in delta form, without generating output
deltaOptions :: EPOptions Identity String
deltaOptions = epOptions (\_ _ -> return "") (\_ -> return "") (\_ -> return "") NormalLayout True

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
type Annotated a = EP String Identity a

-- ---------------------------------------------------------------------

-- | Key entry point.  Switches to an independent AST element with its
-- own annotation, calculating new offsets, etc
markAnnotated :: ExactPrint a => a -> Annotated a
markAnnotated a = enterAnn (getAnnotationEntry a) a

-- | For HsModule, because we do not have a proper SrcSpan, we must
-- indicate to flush trailing comments when done.
data FlushComments = FlushComments
                   | NoFlushComments
                   deriving (Eq, Show)

-- | For GenLocated SrcSpan, we construct an entry location but cannot update it.
data CanUpdateAnchor = CanUpdateAnchor
                     | NoCanUpdateAnchor
                   deriving (Eq, Show)

data Entry = Entry Anchor EpAnnComments FlushComments CanUpdateAnchor
           | NoEntryVal


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

cua :: CanUpdateAnchor -> EPP [a] -> EPP [a]
cua CanUpdateAnchor f = f
cua NoCanUpdateAnchor _ = return []

-- | "Enter" an annotation, by using the associated 'anchor' field as
-- the new reference point for calculating all DeltaPos positions.
--
-- This is combination of the ghc=exactprint Delta.withAST and
-- Print.exactPC functions and effectively does the delta processing
-- immediately followed by the print processing.  JIT ghc-exactprint.
enterAnn :: (ExactPrint a) => Entry -> a -> Annotated a
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
  let edp' = adjustDeltaForOffset 0
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
            dp = adjustDeltaForOffset 0
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

  -- postCs <- takeAppliedComments
  let newAchor = anchor' { anchor_op = MovedAnchor edp }
  let r = case canUpdateAnchor of
            CanUpdateAnchor -> setAnnotationAnchor a' newAchor (mkEpaComments (noKWComments (priorCs++ postCs)) [])
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

addCommentsA :: [LEpaComment] -> EPP ()
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
addComments :: [Comment] -> EPP ()
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
flushComments :: [LEpaComment] -> EPP ()
flushComments trailing = do
  addCommentsA trailing
  cs <- getUnallocatedComments
  -- Must compare without span filenames, for CPP injected comments with fake filename
  let cmp (Comment _ l1 _) (Comment _ l2 _) = compare (ss2pos $ anchor l1) (ss2pos $ anchor l2)
  debugM $ "flushing comments starting"
  mapM_ printOneComment (sortBy cmp cs)
  debugM $ "flushing comments done"

-- ---------------------------------------------------------------------

-- |In order to interleave annotations into the stream, we turn them into
-- comments.
annotationsToComments :: [AddEpAnn] -> [AnnKeywordId] -> EPP ()
annotationsToComments ans kws = do
  let
    getSpans _ [] = []
    getSpans k1 (AddEpAnn k2 ss:as)
      | k1 == k2 = ss : getSpans k1 as
      | otherwise = getSpans k1 as
    doOne :: AnnKeywordId -> EPP [Comment]
    doOne kw = do
      let sps =getSpans kw ans
      return $ map (mkKWComment kw ) sps
    -- TODO:AZ make sure these are sorted/merged properly when the invariant for
    -- allocateComments is re-established.
  newComments <- mapM doOne kws
  addComments (concat newComments)

annotationsToCommentsA :: EpAnn [AddEpAnn] -> [AnnKeywordId] -> EPP ()
annotationsToCommentsA EpAnnNotUsed _ = return ()
annotationsToCommentsA an kws = annotationsToComments (anns an) kws

-- ---------------------------------------------------------------------

-- Temporary function to simply reproduce the "normal" pretty printer output
withPpr :: (Outputable a) => a -> Annotated a
withPpr a = do
  ss <- getAnchorU
  debugM $ "withPpr: ss=" ++ show ss
  printStringAtKw' ss (showPprUnsafe a)
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
  exact :: a -> Annotated a

-- ---------------------------------------------------------------------

-- | Bare Located elements are simply stripped off without further
-- processing.
instance (ExactPrint a) => ExactPrint (Located a) where
  getAnnotationEntry (L l _) = Entry (spanAsAnchor l) emptyComments NoFlushComments NoCanUpdateAnchor

  setAnnotationAnchor la _anc _cs = la

  exact (L l a) = L l <$> markAnnotated a

instance (ExactPrint a) => ExactPrint (LocatedA a) where
  getAnnotationEntry = entryFromLocatedA
  setAnnotationAnchor la cs anc = setAnchorAn la cs anc
  -- setAnnotationAnchor la anc = error $ "LocatedA:setAnnotationAnchor (la,anc)=" ++ showAst (getLoc la,anc)
  exact (L la a) = do
    debugM $ "LocatedA a:la loc=" ++ show (ss2range $ locA la)
    a' <- markAnnotated a
    markALocatedA (ann la)
    return (L la a')

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
  setAnnotationAnchor hsmod _ _ = hsmod
                   `debug` ("setAnnotationAnchor hsmod called")

  exact hsmod@(HsModule EpAnnNotUsed _ _ _ _ _ _ _) = withPpr hsmod >> return hsmod
  exact hsmod@(HsModule an _lo mmn mexports imports decls mdeprec mbDoc) = do

    markAnnotated mbDoc

    case mmn of
      Nothing -> return ()
      Just (L ln mn) -> do
        markEpAnn' an am_main AnnModule
        markAnnotated (L ln mn)

        -- forM_ mdeprec markLocated
        setLayoutTopLevelP $ markAnnotated mdeprec

        setLayoutTopLevelP $ markAnnotated mexports

        debugM $ "HsModule.AnnWhere coming"
        setLayoutTopLevelP $ markEpAnn' an am_main AnnWhere

    (decls', imports') <- markAnnList' False (am_decls $ anns an) $ do
      imports' <- markTopLevelList imports
      decls' <- markTopLevelList decls
      return (decls', imports')

    return hsmod { hsmodImports = imports', hsmodDecls = decls' }

-- ---------------------------------------------------------------------

-- TODO:AZ: do we *need* the following, or can we capture it in the AST?
-- | We can have a list with its own entry point defined. Create a
-- data structure to capture this, for defining an ExactPrint instance
-- data AnnotatedList a = AnnotatedList (Maybe Anchor) a
--                      deriving (Eq,Show)

-- instance (ExactPrint a) => ExactPrint (AnnotatedList a) where
--   getAnnotationEntry (AnnotatedList (Just anc) _) = Entry anc (EpaComments []) NoFlushComments
--   getAnnotationEntry (AnnotatedList Nothing    _) = NoEntryVal

--   setAnnotationAnchor (AnnotatedList _ a) anc cs = (AnnotatedList (Just anc) a)

--   exact a@(AnnotatedList an ls) = do
--     debugM $ "AnnotatedList:an=" ++ show an
--     markAnnotatedWithLayout ls
--     return a


-- ---------------------------------------------------------------------
-- Start of utility functions
-- ---------------------------------------------------------------------

printSourceText :: SourceText -> String -> EPP SourceText
printSourceText a@(NoSourceText) txt   =  printStringAdvance txt >> return a
printSourceText a@(SourceText   txt) _ =  printStringAdvance txt >> return a

-- ---------------------------------------------------------------------

printStringAtRs :: RealSrcSpan -> String -> EPP ()
printStringAtRs ss str = printStringAtKw' ss str

printStringAtSs :: SrcSpan -> String -> EPP ()
printStringAtSs ss str = printStringAtKw' (realSrcSpan ss) str

-- ---------------------------------------------------------------------

-- AZ:TODO get rid of this
printStringAtMkw :: Maybe EpaLocation -> String -> EPP ()
printStringAtMkw (Just aa) s = printStringAtAA aa s
printStringAtMkw Nothing s = printStringAtLsDelta (SameLine 1) s


printStringAtAA :: EpaLocation -> String -> EPP ()
printStringAtAA (EpaSpan r) s = printStringAtKw' r s
printStringAtAA (EpaDelta d) s = do
  pe <- getPriorEndD
  p1 <- getPosP
  printStringAtLsDelta d s
  p2 <- getPosP
  debugM $ "printStringAtAA:(pe,p1,p2)=" ++ show (pe,p1,p2)
  setPriorEndASTPD True (p1,p2)

-- Based on Delta.addAnnotationWorker
printStringAtKw' :: RealSrcSpan -> String -> EPP ()
printStringAtKw' pa str = do
  printComments pa
  pe <- getPriorEndD
  debugM $ "printStringAtKw':pe=" ++ show pe
  let p = ss2delta pe pa
  p' <- adjustDeltaForOffsetM p
  printStringAtLsDelta p' str
  setPriorEndASTD True pa

-- ---------------------------------------------------------------------

markExternalSourceText :: SrcSpan -> SourceText -> String -> EPP ()
markExternalSourceText l NoSourceText txt   = printStringAtKw' (realSrcSpan l) txt
markExternalSourceText l (SourceText txt) _ = printStringAtKw' (realSrcSpan l) txt

-- ---------------------------------------------------------------------

markAddEpAnn :: AddEpAnn -> EPP ()
markAddEpAnn a@(AddEpAnn kw _) = mark [a] kw

markLocatedMAA :: EpAnn a -> (a -> Maybe AddEpAnn) -> EPP ()
markLocatedMAA EpAnnNotUsed  _  = return ()
markLocatedMAA (EpAnn _ a _) f =
  case f a of
    Nothing -> return ()
    Just aa -> markAddEpAnn aa

markLocatedAA :: EpAnn a -> (a -> AddEpAnn) -> EPP ()
markLocatedAA EpAnnNotUsed  _  = return ()
markLocatedAA (EpAnn _ a _) f = markKw (f a)

markLocatedAAL :: EpAnn a -> (a -> [AddEpAnn]) -> AnnKeywordId -> EPP ()
markLocatedAAL EpAnnNotUsed  _ _ = return ()
markLocatedAAL (EpAnn _ a _) f kw = go (f a)
  where
    go [] = return ()
    go (aa@(AddEpAnn kw' _):as)
      | kw' == kw = mark [aa] kw
      | otherwise = go as

markLocatedAALS :: EpAnn a -> (a -> [AddEpAnn]) -> AnnKeywordId -> Maybe String -> EPP ()
markLocatedAALS an f kw Nothing = markLocatedAAL an f kw
markLocatedAALS EpAnnNotUsed  _ _ _ = return ()
markLocatedAALS (EpAnn _ a _) f kw (Just str) = go (f a)
  where
    go [] = return ()
    go (AddEpAnn kw' r:as)
      | kw' == kw = printStringAtAA r str
      | otherwise = go as

-- ---------------------------------------------------------------------

markArrow :: EpAnn TrailingAnn -> HsArrow GhcPs -> EPP (HsArrow GhcPs)
markArrow an arr = do
  case arr of
    HsUnrestrictedArrow _u ->
      return ()
    HsLinearArrow _u ma -> do
      mapM_ markAddEpAnn ma
    HsExplicitMult _u ma t  -> do
      mapM_ markAddEpAnn ma
      markAnnotated t
      return ()

  case an of
    EpAnnNotUsed -> pure ()
    _ -> markKwT (anns an)
  return arr

-- ---------------------------------------------------------------------

markAnnCloseP :: EpAnn AnnPragma -> EPP ()
markAnnCloseP an = markLocatedAALS an (pure . apr_close) AnnClose (Just "#-}")

markAnnOpenP :: EpAnn AnnPragma -> SourceText -> String -> EPP ()
markAnnOpenP an NoSourceText txt   = markLocatedAALS an (pure . apr_open) AnnOpen (Just txt)
markAnnOpenP an (SourceText txt) _ = markLocatedAALS an (pure . apr_open) AnnOpen (Just txt)

markAnnOpen :: EpAnn [AddEpAnn] -> SourceText -> String -> EPP ()
markAnnOpen an NoSourceText txt   = markLocatedAALS an id AnnOpen (Just txt)
markAnnOpen an (SourceText txt) _ = markLocatedAALS an id AnnOpen (Just txt)

markAnnOpen' :: Maybe EpaLocation -> SourceText -> String -> EPP ()
markAnnOpen' ms NoSourceText txt   = printStringAtMkw ms txt
markAnnOpen' ms (SourceText txt) _ = printStringAtMkw ms txt

-- ---------------------------------------------------------------------

markOpeningParen, markClosingParen :: EpAnn AnnParen -> EPP ()
markOpeningParen an = markParen an fst
markClosingParen an = markParen an snd

markParen :: EpAnn AnnParen -> (forall a. (a,a) -> a) -> EPP ()
markParen EpAnnNotUsed _ = return ()
markParen (EpAnn _ (AnnParen pt o c) _) f = markKwA (f $ kw pt) (f (o, c))
  where
    kw AnnParens       = (AnnOpenP,  AnnCloseP)
    kw AnnParensHash   = (AnnOpenPH, AnnClosePH)
    kw AnnParensSquare = (AnnOpenS, AnnCloseS)


markAnnKw :: EpAnn a -> (a -> EpaLocation) -> AnnKeywordId -> EPP ()
markAnnKw EpAnnNotUsed  _ _  = return ()
markAnnKw (EpAnn _ a _) f kw = markKwA kw (f a)

markAnnKwAll :: EpAnn a -> (a -> [EpaLocation]) -> AnnKeywordId -> EPP ()
markAnnKwAll EpAnnNotUsed  _ _  = return ()
markAnnKwAll (EpAnn _ a _) f kw = mapM_ (markKwA kw) (sort (f a))

markAnnKwM :: EpAnn a -> (a -> Maybe EpaLocation) -> AnnKeywordId -> EPP ()
markAnnKwM EpAnnNotUsed  _ _ = return ()
markAnnKwM (EpAnn _ a _) f kw = go (f a)
  where
    go Nothing = return ()
    go (Just s) = markKwA kw s

markALocatedA :: EpAnn AnnListItem -> EPP (EpAnn AnnListItem)
markALocatedA EpAnnNotUsed  = return EpAnnNotUsed
markALocatedA an@(EpAnn _ a _) = markTrailing (lann_trailing a) >> return an

markEpAnn :: EpAnn [AddEpAnn] -> AnnKeywordId -> EPP ()
markEpAnn EpAnnNotUsed _ = return ()
markEpAnn (EpAnn _ a _) kw = mark a kw

markEpAnn' :: EpAnn ann -> (ann -> [AddEpAnn]) -> AnnKeywordId -> EPP ()
markEpAnn' EpAnnNotUsed _ _ = return ()
markEpAnn' (EpAnn _ a _) f kw = mark (f a) kw

markEpAnnAll :: EpAnn ann -> (ann -> [AddEpAnn]) -> AnnKeywordId -> EPP ()
markEpAnnAll EpAnnNotUsed _ _ = return ()
markEpAnnAll (EpAnn _ a _) f kw = mapM_ markKw (sort anns)
  where
    anns = filter (\(AddEpAnn ka _) -> ka == kw) (f a)

markAnnAll :: [AddEpAnn] -> AnnKeywordId -> EPP ()
markAnnAll a kw = mapM_ markKw (sort anns)
  where
    anns = filter (\(AddEpAnn ka _) -> ka == kw) a

mark :: [AddEpAnn] -> AnnKeywordId -> EPP ()
mark anns kw = do
  case find (\(AddEpAnn k _) -> k == kw) anns of
    Just aa -> markKw aa
    Nothing -> case find (\(AddEpAnn k _) -> k == (unicodeAnn kw)) anns of
      Just aau -> markKw aau
      Nothing -> return ()

markKwT :: TrailingAnn -> EPP ()
markKwT (AddSemiAnn ss)    = markKwA AnnSemi ss
markKwT (AddCommaAnn ss)   = markKwA AnnComma ss
markKwT (AddVbarAnn ss)    = markKwA AnnVbar ss
markKwT (AddRarrowAnn ss)  = markKwA AnnRarrow ss
markKwT (AddRarrowAnnU ss) = markKwA AnnRarrowU ss
markKwT (AddLollyAnnU ss)  = markKwA AnnLollyU ss

markKw :: AddEpAnn -> EPP ()
markKw (AddEpAnn kw ss) = markKwA kw ss

-- | This should be the main driver of the process, managing comments
markKwA :: AnnKeywordId -> EpaLocation -> EPP ()
markKwA kw aa = printStringAtAA aa (keywordToString (G kw))

-- ---------------------------------------------------------------------

markAnnList :: Bool -> EpAnn AnnList -> EPP a -> EPP a
markAnnList _ EpAnnNotUsed action = action
markAnnList reallyTrail (EpAnn _ ann _) action = markAnnList' reallyTrail ann action

markAnnList' :: Bool -> AnnList -> EPP a -> EPP a
markAnnList' reallyTrail ann action = do
  p <- getPosP
  debugM $ "markAnnList : " ++ showPprUnsafe (p, ann)
  mapM_ markAddEpAnn (al_open ann)
  unless reallyTrail $ markTrailing (al_trailing ann) -- Only makes sense for HsModule.
  markAnnAll (sort $ al_rest ann) AnnSemi
  r <- action
  mapM_ markAddEpAnn (al_close ann)
  debugM $ "markAnnList: calling markTrailing with:" ++ showPprUnsafe (al_trailing ann)
  when reallyTrail $ markTrailing (al_trailing ann) -- normal case
  return r

-- ---------------------------------------------------------------------

printComments :: RealSrcSpan -> EPP ()
printComments ss = do
  cs <- commentAllocation ss
  debugM $ "printComments: (ss): " ++ showPprUnsafe (rs2range ss)
  -- debugM $ "printComments: (ss,comment locations): " ++ showPprUnsafe (rs2range ss,map commentAnchor cs)
  mapM_ printOneComment cs

-- ---------------------------------------------------------------------

printOneComment :: Comment -> EPP ()
printOneComment c@(Comment _str loc _mo) = do
  debugM $ "printOneComment:c=" ++ showGhc c
  dp <-case anchor_op loc of
    MovedAnchor dp -> return dp
    _ -> do
        pe <- getPriorEndD
        let dp = ss2delta pe (anchor loc)
        -- debugM $ "printOneComment:(dp,pe,anchor loc)=" ++ showGhc (dp,pe,ss2pos $ anchor loc)
        return dp
  dp'' <- adjustDeltaForOffsetM dp
  mep <- getExtraDP
  dp' <- case mep of
    Just (Anchor _ (MovedAnchor edp)) -> do
      -- debugM $ "printOneComment:edp=" ++ show edp
      return edp
    _ -> return dp''
  LayoutStartCol dOff <- gets dLHS
  -- debugM $ "printOneComment:(dp,dp',dp'',dOff)=" ++ showGhc (dp,dp',dp'',dOff)
  setPriorEndD (ss2posEnd (anchor loc))
  applyComment c
  printQueuedComment (anchor loc) c dp'

-- ---------------------------------------------------------------------

commentAllocation :: RealSrcSpan -> EPP [Comment]
commentAllocation ss = do
  cs <- getUnallocatedComments
  -- Note: The CPP comment injection may change the file name in the
  -- RealSrcSpan, which affects comparison, as the Ord instance for
  -- RealSrcSpan compares the file first. So we sort via ss2pos
  -- TODO: this is inefficient, use Pos all the way through
  let (earlier,later) = partition (\(Comment _str loc _mo) -> (ss2pos $ anchor loc) <= (ss2pos ss)) cs
  putUnallocatedComments later
  -- debugM $ "commentAllocation:(ss,earlier,later)" ++ show (rs2range ss,earlier,later)
  return earlier

-- ---------------------------------------------------------------------

markAnnotatedWithLayout :: ExactPrint ast => ast -> EPP ast
markAnnotatedWithLayout a = setLayoutBoth $ markAnnotated a

-- ---------------------------------------------------------------------

markTopLevelList :: ExactPrint ast => [ast] -> EPP [ast]
markTopLevelList ls = mapM (\a -> setLayoutTopLevelP $ markAnnotated a) ls

-- ---------------------------------------------------------------------

instance ExactPrint ModuleName where
  getAnnotationEntry _ = NoEntryVal
  setAnnotationAnchor n _ _ = n
  exact n = do
    debugM $ "ModuleName: " ++ showPprUnsafe n
    withPpr n
    return n

-- ---------------------------------------------------------------------

instance ExactPrint (LocatedP WarningTxt) where
  getAnnotationEntry = entryFromLocatedA
  setAnnotationAnchor = setAnchorAn

  exact a@(L (SrcSpanAnn an _) (WarningTxt (L _ src) ws)) = do
    markAnnOpenP an src "{-# WARNING"
    markLocatedAAL an apr_rest AnnOpenS
    markAnnotated ws
    markLocatedAAL an apr_rest AnnCloseS
    markAnnCloseP an
    return a

  exact a@(L (SrcSpanAnn an _) (DeprecatedTxt (L _ src) ws)) = do
    markAnnOpenP an src "{-# DEPRECATED"
    markLocatedAAL an apr_rest AnnOpenS
    markAnnotated ws
    markLocatedAAL an apr_rest AnnCloseS
    markAnnCloseP an
    return a

-- ---------------------------------------------------------------------

instance ExactPrint (ImportDecl GhcPs) where
  getAnnotationEntry idecl = fromAnn (ideclExt idecl)
  setAnnotationAnchor idecl anc cs = idecl { ideclExt = setAnchorEpa (ideclExt idecl) anc cs }
  exact x@(ImportDecl EpAnnNotUsed _ _ _ _ _ _ _ _ _) = withPpr x
  exact id@(ImportDecl ann@(EpAnn _ an _) msrc (L lm modname) mpkg _src safeflag qualFlag _impl mAs hiding) = do

    markAnnKw ann importDeclAnnImport AnnImport

    -- "{-# SOURCE" and "#-}"
    case msrc of
      SourceText _txt -> do
        debugM $ "ImportDecl sourcetext"
        let mo = fmap fst $ importDeclAnnPragma an
        let mc = fmap snd $ importDeclAnnPragma an
        markAnnOpen' mo msrc "{-# SOURCE"
        printStringAtMkw mc "#-}"
      NoSourceText -> return ()
    when safeflag (markAnnKwM ann importDeclAnnSafe AnnSafe)
    case qualFlag of
      QualifiedPre  -- 'qualified' appears in prepositive position.
        -> printStringAtMkw (importDeclAnnQualified an) "qualified"
      _ -> return ()
    case mpkg of
     Just (StringLiteral src v _) ->
       printStringAtMkw (importDeclAnnPackage an) (sourceTextToString src (show v))
     _ -> return ()

    printStringAtKw' (realSrcSpan lm) (moduleNameString modname)

    case qualFlag of
      QualifiedPost  -- 'qualified' appears in postpositive position.
        -> printStringAtMkw (importDeclAnnQualified an) "qualified"
      _ -> return ()

    case mAs of
      Nothing -> return ()
      Just (L l mn) -> do
        printStringAtMkw (importDeclAnnAs an) "as"
        printStringAtKw' (realSrcSpan l) (moduleNameString mn)

    case hiding of
      Nothing -> return ()
      Just (_isHiding,lie) -> markAnnotated lie >> return ()

    return id


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
    { dc_a :: EpAnn [AddEpAnn]
    , dc_f :: TopLevelFlag
    , dc_d :: DataFamInstDecl GhcPs
    }

instance ExactPrint DataFamInstDeclWithContext where
  getAnnotationEntry (DataFamInstDeclWithContext _ _ (DataFamInstDecl (FamEqn { feqn_ext = an})))
    = fromAnn an
  setAnnotationAnchor (DataFamInstDeclWithContext a c (DataFamInstDecl fe)) anc cs
    = (DataFamInstDeclWithContext a c (DataFamInstDecl (fe { feqn_ext = (setAnchorEpa (feqn_ext fe) anc cs)})))
  exact (DataFamInstDeclWithContext an c d) = do
    debugM $ "starting DataFamInstDeclWithContext:an=" ++ showAst an
    d' <- exactDataFamInstDecl an c d
    return (DataFamInstDeclWithContext an c d')

-- ---------------------------------------------------------------------

exactDataFamInstDecl :: EpAnn [AddEpAnn] -> TopLevelFlag -> DataFamInstDecl GhcPs
                     -> EPP (DataFamInstDecl GhcPs)
exactDataFamInstDecl an top_lvl
  (DataFamInstDecl (FamEqn { feqn_ext    = an2
                           , feqn_tycon  = tycon
                           , feqn_bndrs  = bndrs
                           , feqn_pats   = pats
                           , feqn_fixity = fixity
                           , feqn_rhs    = defn })) = do
    (tycon', bndrs', _,  mc, defn') <- exactDataDefn an2 pp_hdr defn
    return
      (DataFamInstDecl ( FamEqn { feqn_ext    = an2
                                , feqn_tycon  = tycon'
                                , feqn_bndrs  = bndrs'
                                , feqn_pats   = pats
                                , feqn_fixity = fixity
                                , feqn_rhs    = defn' }))
                    `debug` ("exactDataFamInstDecl: defn' derivs:" ++ showAst (dd_derivs defn'))
  where
    pp_hdr :: Maybe (LHsContext GhcPs) -> EPP (LocatedN RdrName, HsOuterTyVarBndrs () GhcPs, HsTyPats GhcPs, Maybe (LHsContext GhcPs))
    pp_hdr mctxt = do
      case top_lvl of
        TopLevel -> markEpAnn an AnnInstance -- TODO: maybe in toplevel
        NotTopLevel -> return ()
      exactHsFamInstLHS an tycon bndrs pats fixity mctxt

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
  exact dd@(DerivDecl an typ ms mov) = do
    markEpAnn an AnnDeriving
    mapM_ markAnnotated ms
    markEpAnn an AnnInstance
    mapM_ markAnnotated mov
    markAnnotated typ
    return dd

-- ---------------------------------------------------------------------

instance ExactPrint (ForeignDecl GhcPs) where
  getAnnotationEntry (ForeignImport an _ _  _) = fromAnn an
  getAnnotationEntry (ForeignExport an _ _  _) = fromAnn an

  setAnnotationAnchor (ForeignImport an a b c) anc cs = ForeignImport (setAnchorEpa an anc cs) a b c
  setAnnotationAnchor (ForeignExport an a b c) anc cs = ForeignExport (setAnchorEpa an anc cs) a b c

  exact f@(ForeignImport an n ty fimport) = do
    markEpAnn an AnnForeign
    markEpAnn an AnnImport

    markAnnotated fimport

    markAnnotated n
    markEpAnn an AnnDcolon
    markAnnotated ty
    return f

  exact f@(ForeignExport an n ty fexport) = do
    markEpAnn an AnnForeign
    markEpAnn an AnnExport
    markAnnotated fexport
    markAnnotated n
    markEpAnn an AnnDcolon
    markAnnotated ty
    return f

-- ---------------------------------------------------------------------

instance ExactPrint ForeignImport where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a
  exact i@(CImport cconv safety@(L ll _) _mh _imp (L ls src)) = do
    markAnnotated cconv
    unless (ll == noSrcSpan) $ markAnnotated safety >> return ()
    unless (ls == noSrcSpan) $ markExternalSourceText ls src "" >> return ()
    return i

-- ---------------------------------------------------------------------

instance ExactPrint ForeignExport where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a
  exact i@(CExport spec (L ls src)) = do
    debugM $ "CExport starting"
    markAnnotated spec
    unless (ls == noSrcSpan) $ markExternalSourceText ls src ""
    return i

-- ---------------------------------------------------------------------

instance ExactPrint CExportSpec where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a
  exact e@(CExportStatic _st _lbl cconv) = do
    debugM $ "CExportStatic starting"
    markAnnotated cconv
    return e

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

  exact w@(Warnings an src warns) = do
    markAnnOpen an src "{-# WARNING" -- Note: might be {-# DEPRECATED
    markAnnotated warns
    markLocatedAALS an id AnnClose (Just "#-}")
    return w

-- ---------------------------------------------------------------------

instance ExactPrint (WarnDecl GhcPs) where
  getAnnotationEntry (Warning an _ _) = fromAnn an
  setAnnotationAnchor (Warning an a b) anc cs = Warning (setAnchorEpa an anc cs) a b

  exact w@(Warning an lns txt) = do
    markAnnotated lns
    markEpAnn an AnnOpenS -- "["
    case txt of
      WarningTxt    _src ls -> markAnnotated ls
      DeprecatedTxt _src ls -> markAnnotated ls
    markEpAnn an AnnCloseS -- "]"
    return w

-- ---------------------------------------------------------------------

instance ExactPrint StringLiteral where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a

  exact l@(StringLiteral src fs mcomma) = do
    printSourceText src (show (unpackFS fs))
    mapM_ (\r -> printStringAtKw' r ",") mcomma
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
    case src of
      NoSourceText      -> markLocatedAALS an id AnnOpen  (Just "{-# RULES")
      SourceText srcTxt -> markLocatedAALS an id AnnOpen  (Just srcTxt)
    rules' <- markAnnotated rules
    markLocatedAALS an id AnnClose (Just "#-}")
    return (HsRules an src rules')

-- ---------------------------------------------------------------------

instance ExactPrint (RuleDecl GhcPs) where
  getAnnotationEntry (HsRule {rd_ext = an}) = fromAnn an
  setAnnotationAnchor r anc cs = r { rd_ext = setAnchorEpa (rd_ext r) anc cs}
  exact (HsRule an ln act mtybndrs termbndrs lhs rhs) = do
    debugM "HsRule entered"
    ln' <- markAnnotated ln
    debugM "HsRule after ln"
    markActivation an ra_rest act
    debugM "HsRule after act"
    mtybndrs' <- case mtybndrs of
      Nothing -> return Nothing
      Just bndrs -> do
        markLocatedMAA an (\a -> fmap fst (ra_tyanns a))  -- AnnForall
        bndrs' <- mapM markAnnotated bndrs
        markLocatedMAA an (\a -> fmap snd (ra_tyanns a))  -- AnnDot
        return (Just bndrs')

    markLocatedMAA an (\a -> fmap fst (ra_tmanns a))  -- AnnForall
    termbndrs' <- mapM markAnnotated termbndrs
    markLocatedMAA an (\a -> fmap snd (ra_tmanns a))  -- AnnDot

    lhs' <- markAnnotated lhs
    markEpAnn' an ra_rest AnnEqual
    rhs' <- markAnnotated rhs
    return (HsRule an ln' act mtybndrs' termbndrs' lhs' rhs')

markActivation :: EpAnn a -> (a -> [AddEpAnn]) -> Activation -> Annotated ()
markActivation an fn act = do
  case act of
    ActiveBefore src phase -> do
      markEpAnn' an fn AnnOpenS --  '['
      markEpAnn' an fn AnnTilde -- ~
      markLocatedAALS an fn AnnVal (Just (toSourceTextWithSuffix src (show phase) ""))
      markEpAnn' an fn AnnCloseS -- ']'
    ActiveAfter src phase -> do
      markEpAnn' an fn AnnOpenS --  '['
      markLocatedAALS an fn AnnVal (Just (toSourceTextWithSuffix src (show phase) ""))
      markEpAnn' an fn AnnCloseS -- ']'
    NeverActive -> do
      markEpAnn' an fn AnnOpenS --  '['
      markEpAnn' an fn AnnTilde -- ~
      markEpAnn' an fn AnnCloseS -- ']'
    _ -> return ()

-- ---------------------------------------------------------------------

instance ExactPrint (SpliceDecl GhcPs) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a

  exact s@(SpliceDecl _ splice _flag) = do
    markAnnotated splice
    return s

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
  exact r@(RoleAnnotDecl an ltycon roles) = do
    markEpAnn an AnnType
    markEpAnn an AnnRole
    markAnnotated ltycon
    let markRole (L l (Just r)) = markAnnotated (L l r) >> return ()
        markRole (L l Nothing) = printStringAtSs l "_"  >> return ()
    mapM_ markRole roles
    return r

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
    (tycon', bndrs', pats', _) <- exactHsFamInstLHS an tycon bndrs pats fixity Nothing
    markEpAnn an AnnEqual
    rhs' <- markAnnotated rhs
    return (FamEqn { feqn_ext = an
                   , feqn_tycon  = tycon'
                   , feqn_bndrs  = bndrs'
                   , feqn_pats   = pats'
                   , feqn_fixity = fixity
                   , feqn_rhs    = rhs' })

-- ---------------------------------------------------------------------

exactHsFamInstLHS ::
      EpAnn [AddEpAnn]
   -> LocatedN RdrName
   -> HsOuterTyVarBndrs () GhcPs
   -> HsTyPats GhcPs
   -> LexicalFixity
   -> Maybe (LHsContext GhcPs)
   -> EPP (LocatedN RdrName, HsOuterTyVarBndrs () GhcPs, HsTyPats GhcPs, Maybe (LHsContext GhcPs))
exactHsFamInstLHS an thing bndrs typats fixity mb_ctxt = do
  markEpAnn an AnnForall
  bndrs' <- markAnnotated bndrs
  markEpAnn an AnnDot
  mb_ctxt' <- mapM markAnnotated mb_ctxt
  (thing', typats') <- exact_pats typats
  return (thing', bndrs', typats', mb_ctxt')
  where
    exact_pats :: HsTyPats GhcPs -> EPP (LocatedN RdrName, HsTyPats GhcPs)
    exact_pats (patl:patr:pats)
      | Infix <- fixity
      = let exact_op_app = do
              markAnnAll (epAnnAnns an) AnnOpenP
              patl' <- markAnnotated patl
              thing' <- markAnnotated thing
              patr' <- markAnnotated patr
              markAnnAll (epAnnAnns an) AnnCloseP
              return (thing', [patl',patr'])
        in case pats of
             [] -> exact_op_app
             _  -> do
               (thing', p) <- exact_op_app
               pats' <- mapM markAnnotated pats
               return (thing', p++pats')

    exact_pats pats = do
      markAnnAll (epAnnAnns an) AnnOpenP
      thing' <- markAnnotated thing
      pats' <- markAnnotated pats
      markAnnAll (epAnnAnns an) AnnCloseP
      return (thing', pats')

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
    markAnnotated eqn
    return d

-- ---------------------------------------------------------------------

instance ExactPrint (LocatedP OverlapMode) where
  getAnnotationEntry = entryFromLocatedA
  setAnnotationAnchor = setAnchorAn

  -- NOTE: NoOverlap is only used in the typechecker
  exact l@(L (SrcSpanAnn an _) (NoOverlap src)) = do
    markAnnOpenP an src "{-# NO_OVERLAP"
    markAnnCloseP an
    return l

  exact l@(L (SrcSpanAnn an _) (Overlappable src)) = do
    markAnnOpenP an src "{-# OVERLAPPABLE"
    markAnnCloseP an
    return l

  exact l@(L (SrcSpanAnn an _) (Overlapping src)) = do
    markAnnOpenP an src "{-# OVERLAPPING"
    markAnnCloseP an
    return l

  exact l@(L (SrcSpanAnn an _) (Overlaps src)) = do
    markAnnOpenP an src "{-# OVERLAPS"
    markAnnCloseP an
    return l

  exact l@(L (SrcSpanAnn an _) (Incoherent src)) = do
    markAnnOpenP an src "{-# INCOHERENT"
    markAnnCloseP an
    return l

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

  exact p@(PSB{ psb_ext = an
              , psb_id = psyn, psb_args = details
              , psb_def = pat
              , psb_dir = dir }) = do
    markEpAnn an AnnPattern
    case details of
      InfixCon v1 v2 -> do
        markAnnotated v1
        markAnnotated psyn
        markAnnotated v2
        return ()
      PrefixCon tvs vs -> do
        markAnnotated psyn
        markAnnotated tvs
        markAnnotated vs
        return ()
      RecCon vs -> do
        markAnnotated psyn
        markEpAnn an AnnOpenC  -- '{'
        markAnnotated vs
        markEpAnn an AnnCloseC -- '}'
        return ()

    case dir of
      Unidirectional           -> do
        markEpAnn an AnnLarrow
        markAnnotated pat
        return ()
      ImplicitBidirectional    -> do
        markEpAnn an AnnEqual
        markAnnotated pat
        return ()
      ExplicitBidirectional mg -> do
        markEpAnn an AnnLarrow
        markAnnotated pat
        markEpAnn an AnnWhere
        markAnnotated mg
        return ()

    return p


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

exactMatch :: (ExactPrint (GRHSs GhcPs body)) => (Match GhcPs body) -> Annotated (Match GhcPs body)
exactMatch m@(Match an mctxt pats grhss) = do

  debugM $ "exact Match entered"

  (mctxt', pats') <-
    case mctxt of
      FunRhs fun fixity strictness -> do
        debugM $ "exact Match FunRhs:" ++ showPprUnsafe fun
        case strictness of
          SrcStrict -> markEpAnn an AnnBang
          _ -> pure ()
        case fixity of
          Prefix -> do
            annotationsToCommentsA an [AnnOpenP,AnnCloseP]
            fun' <- markAnnotated fun
            pats' <- markAnnotated pats
            return (FunRhs fun' fixity strictness, pats')
          Infix ->
            case pats of
              (p1:p2:rest)
                | null rest -> do
                    p1'  <- markAnnotated p1
                    fun' <- markAnnotated fun
                    p2'  <- markAnnotated p2
                    return (FunRhs fun' fixity strictness, [p1',p2'])
                | otherwise -> do
                    markEpAnn an AnnOpenP
                    p1'  <- markAnnotated p1
                    fun' <- markAnnotated fun
                    p2'  <- markAnnotated p2
                    markEpAnn an AnnCloseP
                    rest' <- mapM markAnnotated rest
                    return (FunRhs fun' fixity strictness, p1':p2':rest')
              _ -> panic "FunRhs"
      LambdaExpr -> do
        markEpAnn an AnnLam
        pats' <- markAnnotated pats
        return (LambdaExpr, pats')
      CaseAlt -> do
        pats' <- markAnnotated pats
        return (CaseAlt, pats')
      _ -> do
        mctxt' <- withPpr mctxt
        return (mctxt', pats)

  grhss' <- markAnnotated grhss

  return (Match an mctxt' pats' grhss')

-- ---------------------------------------------------------------------

instance ExactPrint (GRHSs GhcPs (LocatedA (HsExpr GhcPs))) where
  getAnnotationEntry (GRHSs _ _ _) = NoEntryVal
  setAnnotationAnchor a _ _ = a

  exact (GRHSs an grhss binds) = do
    grhss' <- markAnnotated grhss
    binds' <- markAnnotated binds
    return (GRHSs an grhss' binds')


instance ExactPrint (GRHSs GhcPs (LocatedA (HsCmd GhcPs))) where
  getAnnotationEntry (GRHSs _ _ _) = NoEntryVal
  setAnnotationAnchor a _ _ = a

  exact (GRHSs an grhss binds) = do
    grhss' <- markAnnotated grhss
    binds' <- markAnnotated binds
    return (GRHSs an grhss' binds')

-- ---------------------------------------------------------------------

-- Temporary until https://gitlab.haskell.org/ghc/ghc/-/issues/20247
-- is fixed
fixValbindsAnn :: EpAnn AnnList -> EpAnn AnnList
fixValbindsAnn EpAnnNotUsed = EpAnnNotUsed
fixValbindsAnn (EpAnn anchor (AnnList ma o c r t) cs)
  = (EpAnn (widenAnchor anchor (map toEpaAnn t)) (AnnList ma o c r t) cs)
  where
    toEpaAnn (AddSemiAnn ss)    = AddEpAnn AnnSemi ss
    toEpaAnn (AddCommaAnn ss)   = AddEpAnn AnnComma ss
    toEpaAnn (AddVbarAnn ss)    = AddEpAnn AnnVbar ss
    toEpaAnn (AddRarrowAnn ss)  = AddEpAnn AnnRarrow ss
    toEpaAnn (AddRarrowAnnU ss) = AddEpAnn AnnRarrowU ss
    toEpaAnn (AddLollyAnnU ss)  = AddEpAnn AnnLollyU ss

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
    markLocatedAAL an al_rest AnnWhere
    let manc = case an of
                 EpAnnNotUsed -> Nothing
                 _ -> al_anchor $ anns an

    case manc of
      Just anc -> do
        when (not $ isEmptyValBinds valbinds) $ setExtraDP (Just anc)
      _ -> return ()

    valbinds' <- markAnnList False an $ markAnnotatedWithLayout valbinds
    return (HsValBinds an' valbinds')

  exact (HsIPBinds an bs)
    = markAnnList True an (markLocatedAAL an al_rest AnnWhere
                           >> markAnnotated bs
                           >>= \bs' -> return (HsIPBinds an bs'))
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

  exact b@(IPBind an (Left lr) rhs) = do
    markAnnotated lr
    markEpAnn an AnnEqual
    markAnnotated rhs
    return b

  exact (IPBind _ (Right _) _) = error $ "ExactPrint IPBind: Right only after typechecker"

-- ---------------------------------------------------------------------

instance ExactPrint HsIPName where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a

  exact i@(HsIPName fs) = printStringAdvance ("?" ++ (unpackFS fs)) >> return i

-- ---------------------------------------------------------------------
-- Managing lists which have been separated, e.g. Sigs and Binds

prepareListAnnotationF ::
  EpAnn [AddEpAnn] -> [LDataFamInstDecl GhcPs] -> [(RealSrcSpan,EPP Dynamic)]
prepareListAnnotationF an ls = map (\b -> (realSrcSpan $ getLocA b, go b)) ls
  where
    go (L l a) = do
      -- d' <- markAnnotated (DataFamInstDeclWithContext an TopLevel a)
      d' <- markAnnotated (DataFamInstDeclWithContext an NotTopLevel a)
      return (toDyn (L l (dc_d d')))

prepareListAnnotationA :: (Typeable a, ExactPrint (LocatedAn an a))
  => [LocatedAn an a] -> [(RealSrcSpan,EPP Dynamic)]
prepareListAnnotationA ls = map (\b -> (realSrcSpan $ getLocA b,go b)) ls
  where
    go b = do
      b' <- markAnnotated b
      return (toDyn b')

withSortKey :: AnnSortKey -> [(RealSrcSpan, EPP Dynamic)] -> EPP [Dynamic]
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

  exact s@(TypeSig an vars ty)  = exactVarSig an vars ty >> return s

  exact s@(PatSynSig an lns typ) = do
    markLocatedAAL an asRest AnnPattern
    markAnnotated lns
    markLocatedAA an asDcolon
    markAnnotated typ
    return s

  exact s@(ClassOpSig an is_deflt vars ty)
    | is_deflt  = markLocatedAAL an asRest AnnDefault >> exactVarSig an vars ty >> return s
    | otherwise = exactVarSig an vars ty >> return s

  exact s@(FixSig an (FixitySig _ names (Fixity src v fdir))) = do
    let fixstr = case fdir of
         InfixL -> "infixl"
         InfixR -> "infixr"
         InfixN -> "infix"
    markLocatedAALS an id AnnInfix (Just fixstr)
    markLocatedAALS an id AnnVal (Just (sourceTextToString src (show v)))
    markAnnotated names
    return s


  exact s@(InlineSig an ln inl) = do
    markAnnOpen an (inl_src inl) "{-# INLINE"
    markActivation an id (inl_act inl)
    markAnnotated ln
    debugM $ "InlineSig:an=" ++ showAst an
    p <- getPosP
    debugM $ "InlineSig: p=" ++ show p
    markLocatedAALS an id AnnClose (Just "#-}")
    debugM $ "InlineSig:done"
    return s

  exact s@(SpecSig an ln typs inl) = do
    markAnnOpen an (inl_src inl) "{-# SPECIALISE" -- Note: may be {-# SPECIALISE_INLINE
    markActivation an id (inl_act inl)
    markAnnotated ln
    markEpAnn an AnnDcolon
    markAnnotated typs
    markLocatedAALS an id AnnClose (Just "#-}")
    return s

  exact s@(SpecInstSig an src typ) = do
    markAnnOpen an src "{-# SPECIALISE"
    markEpAnn an AnnInstance
    markAnnotated typ
    markLocatedAALS an id AnnClose (Just "#-}")
    return s


  exact s@(MinimalSig an src formula) = do
    markAnnOpen an src "{-# MINIMAL"
    markAnnotated formula
    markLocatedAALS an id AnnClose (Just "#-}")
    return s

  exact s@(SCCFunSig an src ln ml) = do
    markAnnOpen an src "{-# SCC"
    markAnnotated ln
    markAnnotated ml
    markLocatedAALS an id AnnClose (Just "#-}")
    return s

  exact s@(CompleteMatchSig an src cs mty) = do
    markAnnOpen an src "{-# COMPLETE"
    markAnnotated cs
    case mty of
      Nothing -> return ()
      Just ty -> do
        markEpAnn an AnnDcolon
        markAnnotated ty
        return ()
    markLocatedAALS an id AnnClose (Just "#-}")
    return s

  exact x = error $ "exact Sig for:" ++ showAst x

-- ---------------------------------------------------------------------

exactVarSig :: (ExactPrint a) => EpAnn AnnSig -> [LocatedN RdrName] -> a -> EPP ()
exactVarSig an vars ty = do
  mapM_ markAnnotated vars
  markLocatedAA an asDcolon
  markAnnotated ty
  return ()

-- ---------------------------------------------------------------------

instance ExactPrint (StandaloneKindSig GhcPs) where
  getAnnotationEntry (StandaloneKindSig an _ _) = fromAnn an
  setAnnotationAnchor (StandaloneKindSig an a b) anc cs = StandaloneKindSig (setAnchorEpa an anc cs) a b

  exact s@(StandaloneKindSig an vars sig) = do
    markEpAnn an AnnType
    markAnnotated vars
    markEpAnn an AnnDcolon
    markAnnotated sig
    return s

-- ---------------------------------------------------------------------

instance ExactPrint (DefaultDecl GhcPs) where
  getAnnotationEntry (DefaultDecl an _) = fromAnn an
  setAnnotationAnchor (DefaultDecl an a) anc cs = DefaultDecl (setAnchorEpa an anc cs) a

  exact d@(DefaultDecl an tys) = do
    markEpAnn an AnnDefault
    markEpAnn an AnnOpenP
    markAnnotated tys
    markEpAnn an AnnCloseP
    return d

-- ---------------------------------------------------------------------

instance ExactPrint (AnnDecl GhcPs) where
  getAnnotationEntry (HsAnnotation an _ _ _) = fromAnn an
  setAnnotationAnchor (HsAnnotation an a b c) anc cs = HsAnnotation (setAnchorEpa an anc cs) a b c

  exact a@(HsAnnotation an src prov e) = do
    markAnnOpenP an src "{-# ANN"
    case prov of
      (ValueAnnProvenance n) -> markAnnotated n >> return ()
      (TypeAnnProvenance n) -> do
        markLocatedAAL an apr_rest AnnType
        markAnnotated n
        return ()
      ModuleAnnProvenance -> markLocatedAAL an apr_rest AnnModule >> return ()

    markAnnotated e
    markAnnCloseP an
    return a

-- ---------------------------------------------------------------------

instance ExactPrint (BF.BooleanFormula (LocatedN RdrName)) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a

  exact f@(BF.Var x)  = do
    markAnnotated x
    return f
  exact f@(BF.Or ls)  = do
    markAnnotated ls
    return f
  exact f@(BF.And ls) = do
    markAnnotated ls
    return f
  exact f@(BF.Parens x)  = do
    markAnnotated x
    return f

-- ---------------------------------------------------------------------

instance (ExactPrint body) => ExactPrint (HsWildCardBndrs GhcPs body) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a
  exact w@(HsWC _ ty) = markAnnotated ty >> return w

-- ---------------------------------------------------------------------

instance ExactPrint (GRHS GhcPs (LocatedA (HsExpr GhcPs))) where
  getAnnotationEntry (GRHS an _ _) = fromAnn an
  setAnnotationAnchor (GRHS an a b) anc cs = GRHS (setAnchorEpa an anc cs) a b

  exact (GRHS an guards expr) = do
    debugM $ "GRHS comments:" ++ showGhc (comments an)
    markAnnKwM an ga_vbar AnnVbar
    guards' <- markAnnotated guards
    debugM $ "GRHS before matchSeparator"
    markLocatedAA an ga_sep -- Mark the matchSeparator for these GRHSs
    debugM $ "GRHS after matchSeparator"
    expr' <- markAnnotated expr
    return (GRHS an guards' expr')

instance ExactPrint (GRHS GhcPs (LocatedA (HsCmd GhcPs))) where
  getAnnotationEntry (GRHS ann _ _) = fromAnn ann
  setAnnotationAnchor (GRHS an a b) anc cs = GRHS (setAnchorEpa an anc cs) a b

  exact (GRHS an guards expr) = do
    markAnnKwM an ga_vbar AnnVbar
    guards' <- markAnnotated guards
    markLocatedAA an ga_sep -- Mark the matchSeparator for these GRHSs
    expr' <- markAnnotated expr
    return (GRHS an guards' expr')

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
  exact x@(HsUnboundVar an v) = do
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
  exact (HsLam _ _) = error $ "HsLam with other than one match"

  exact (HsLamCase an mg) = do
    markEpAnn an AnnLam
    markEpAnn an AnnCase
    mg' <- markAnnotated mg
    return (HsLamCase an mg')

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
  exact (OpApp _an e1 e2 e3) = do
    e1' <- markAnnotated e1
    e2' <- markAnnotated e2
    e3' <- markAnnotated e3
    return (OpApp _an e1' e2' e3')

  exact (NegApp an e s) = do
    markEpAnn an AnnMinus
    e' <- markAnnotated e
    return (NegApp an e' s)

  exact (HsPar an e) = do
    markOpeningParen an
    e' <- markAnnotated e
    debugM $ "HsPar closing paren"
    markClosingParen an
    debugM $ "HsPar done"
    return (HsPar an e')

  exact (SectionL an expr op) = do
    expr' <- markAnnotated expr
    op' <- markAnnotated op
    return (SectionL an expr' op')

  exact (SectionR an op expr) = do
    op' <- markAnnotated op
    expr' <- markAnnotated expr
    return (SectionR an op' expr')

  exact (ExplicitTuple an args b) = do
    if b == Boxed then markEpAnn an AnnOpenP
                  else markEpAnn an AnnOpenPH

    args' <- mapM markAnnotated args

    if b == Boxed then markEpAnn an AnnCloseP
                  else markEpAnn an AnnClosePH
    debugM $ "ExplicitTuple done"
    return (ExplicitTuple an args' b)

  exact (ExplicitSum an alt arity expr) = do
    markAnnKw an aesOpen AnnOpenPH
    markAnnKwAll an aesBarsBefore AnnVbar
    expr' <- markAnnotated expr
    markAnnKwAll an aesBarsAfter AnnVbar
    markAnnKw an aesClose AnnClosePH
    return (ExplicitSum an alt arity expr')

  exact (HsCase an e alts) = do
    markAnnKw an hsCaseAnnCase AnnCase
    e' <- markAnnotated e
    markAnnKw an hsCaseAnnOf AnnOf
    markEpAnn' an hsCaseAnnsRest AnnOpenC
    markEpAnnAll an hsCaseAnnsRest AnnSemi
    alts' <- setLayoutBoth $ markAnnotated alts
    markEpAnn' an hsCaseAnnsRest AnnCloseC
    return (HsCase an e' alts')

  exact (HsIf an e1 e2 e3) = do
    markAnnKw an aiIf AnnIf
    e1' <- markAnnotated e1
    markAnnKwM an aiThenSemi AnnSemi
    markAnnKw an aiThen AnnThen
    e2' <- markAnnotated e2
    markAnnKwM an aiElseSemi AnnSemi
    markAnnKw an aiElse AnnElse
    e3' <- markAnnotated e3
    return (HsIf an e1' e2' e3')

  exact (HsMultiIf an mg) = do
    markEpAnn an AnnIf
    markEpAnn an AnnOpenC -- optional
    mg' <- markAnnotated mg
    markEpAnn an AnnCloseC -- optional
    return (HsMultiIf an mg')

  exact (HsLet an binds e) = do
    setLayoutBoth $ do -- Make sure the 'in' gets indented too
      markAnnKw an alLet AnnLet
      debugM $ "HSlet:binds coming"
      binds' <- setLayoutBoth $ markAnnotated binds
      debugM $ "HSlet:binds done"
      markAnnKw an alIn AnnIn
      debugM $ "HSlet:expr coming"
      e' <- markAnnotated e
      return (HsLet an binds' e')

  exact (HsDo an do_or_list_comp stmts) = do
    debugM $ "HsDo"
    stmts' <- markAnnList True an $ exactDo an do_or_list_comp stmts
    return (HsDo an do_or_list_comp stmts')

  exact (ExplicitList an es) = do
    debugM $ "ExplicitList start"
    markLocatedMAA an al_open
    es' <- markAnnotated es
    markLocatedMAA an al_close
    debugM $ "ExplicitList end"
    return (ExplicitList an es')
  exact (RecordCon an con_id binds) = do
    con_id' <- markAnnotated con_id
    markEpAnn an AnnOpenC
    binds' <- markAnnotated binds
    markEpAnn an AnnCloseC
    return (RecordCon an con_id' binds')
  exact (RecordUpd an expr fields) = do
    expr' <- markAnnotated expr
    markEpAnn an AnnOpenC
    fields' <- markAnnotated fields
    markEpAnn an AnnCloseC
    return (RecordUpd an expr' fields')
  exact (HsGetField an expr field) = do
    expr' <- markAnnotated expr
    field' <- markAnnotated field
    return (HsGetField an expr' field')
  exact (HsProjection an flds) = do
    markAnnKw an apOpen AnnOpenP
    flds' <- markAnnotated flds
    markAnnKw an apClose AnnCloseP
    return (HsProjection an flds')
  exact (ExprWithTySig an expr sig) = do
    expr' <- markAnnotated expr
    markEpAnn an AnnDcolon
    sig' <- markAnnotated sig
    return (ExprWithTySig an expr' sig')
  exact (ArithSeq an s seqInfo) = do
    markEpAnn an AnnOpenS -- '['
    seqInfo' <- case seqInfo of
        From e -> do
          e' <- markAnnotated e
          markEpAnn an AnnDotdot
          return (From e')
        FromTo e1 e2 -> do
          e1' <- markAnnotated e1
          markEpAnn an AnnDotdot
          e2' <- markAnnotated e2
          return (FromTo e1' e2')
        FromThen e1 e2 -> do
          e1' <- markAnnotated e1
          markEpAnn an AnnComma
          e2' <- markAnnotated e2
          markEpAnn an AnnDotdot
          return (FromThen e1' e2')
        FromThenTo e1 e2 e3 -> do
          e1' <- markAnnotated e1
          markEpAnn an AnnComma
          e2' <- markAnnotated e2
          markEpAnn an AnnDotdot
          e3' <- markAnnotated e3
          return (FromThenTo e1' e2' e3')
    markEpAnn an AnnCloseS -- ']'
    return (ArithSeq an s seqInfo')


  exact (HsBracket an (ExpBr a e)) = do
    markEpAnn an AnnOpenEQ -- "[|"
    markEpAnn an AnnOpenE  -- "[e|" -- optional
    e' <- markAnnotated e
    markEpAnn an AnnCloseQ -- "|]"
    return (HsBracket an (ExpBr a e'))
  exact (HsBracket an (PatBr a e)) = do
    markLocatedAALS an id AnnOpen (Just "[p|")
    e' <- markAnnotated e
    markEpAnn an AnnCloseQ -- "|]"
    return (HsBracket an (PatBr a e'))
  exact (HsBracket an (DecBrL a e)) = do
    markLocatedAALS an id AnnOpen (Just "[d|")
    -- See https://gitlab.haskell.org/ghc/ghc/-/issues/20257, we need
    -- to mark braces here for the time being
    markEpAnn an AnnOpenC -- "{"
    e' <- markAnnotated e
    markEpAnn an AnnCloseC -- "}"
    markEpAnn an AnnCloseQ -- "|]"
    return (HsBracket an (DecBrL a e'))
  -- -- exact (HsBracket an (DecBrG _ _)) =
  -- --   traceM "warning: DecBrG introduced after renamer"
  exact (HsBracket an (TypBr a e)) = do
    markLocatedAALS an id AnnOpen (Just "[t|")
    e' <- markAnnotated e
    markEpAnn an AnnCloseQ -- "|]"
    return (HsBracket an (TypBr a e'))
  exact (HsBracket an (VarBr a b e)) = do
    e' <- if b
      then do
        markEpAnn an AnnSimpleQuote
        markAnnotated e
      else do
        markEpAnn an AnnThTyQuote
        markAnnotated e
    return (HsBracket an (VarBr a b e'))
  exact (HsBracket an (TExpBr a e)) = do
    markLocatedAALS an id AnnOpen (Just "[||")
    markLocatedAALS an id AnnOpenE (Just "[e||")
    e' <- markAnnotated e
    markLocatedAALS an id AnnClose (Just "||]")
    return (HsBracket an (TExpBr a e'))


  -- exact x@(HsRnBracketOut{})           = withPpr x
  -- exact x@(HsTcBracketOut{})           = withPpr x
  exact (HsSpliceE a sp) = do
    sp' <- markAnnotated sp
    return (HsSpliceE a sp')

  exact (HsProc an p c) = do
    debugM $ "HsProc start"
    markEpAnn an AnnProc
    p' <- markAnnotated p
    markEpAnn an AnnRarrow
    debugM $ "HsProc after AnnRarrow"
    c' <- markAnnotated c
    return (HsProc an p' c')

  exact (HsStatic an e) = do
    markEpAnn an AnnStatic
    e' <- markAnnotated e
    return (HsStatic an e')

  -- exact x@(HsTick {})                  = withPpr x
  -- exact x@(HsBinTick {})               = withPpr x
  exact (HsPragE a prag e) = do
    prag' <- markAnnotated prag
    e' <- markAnnotated e
    return (HsPragE a prag' e')
  exact x = error $ "exact HsExpr for:" ++ showAst x

-- ---------------------------------------------------------------------

exactDo :: (ExactPrint (LocatedAn an a))
        => EpAnn AnnList -> (HsStmtContext any) -> (LocatedAn an a) -> EPP (LocatedAn an a)
exactDo an (DoExpr m)    stmts = exactMdo an m AnnDo             >> markMaybeDodgyStmts stmts
exactDo an GhciStmtCtxt  stmts = markLocatedAAL an al_rest AnnDo >> markMaybeDodgyStmts stmts
exactDo an ArrowExpr     stmts = markLocatedAAL an al_rest AnnDo >> markMaybeDodgyStmts stmts
exactDo an (MDoExpr m)   stmts = exactMdo an m AnnMdo            >> markMaybeDodgyStmts stmts
exactDo _  ListComp      stmts = markMaybeDodgyStmts stmts
exactDo _  MonadComp     stmts = markMaybeDodgyStmts stmts
exactDo _  _             _     = panic "pprDo" -- PatGuard, ParStmtCxt

exactMdo :: EpAnn AnnList -> Maybe ModuleName -> AnnKeywordId -> EPP ()
exactMdo an Nothing            kw = markLocatedAAL  an al_rest kw
exactMdo an (Just module_name) kw = markLocatedAALS an al_rest kw (Just n)
    where
      n = (moduleNameString module_name) ++ "." ++ (keywordToString (G kw))

markMaybeDodgyStmts :: ExactPrint (LocatedAn an a)
  => LocatedAn an a -> EPP (LocatedAn an a)
markMaybeDodgyStmts stmts =
  if isGoodSrcSpan (getLocA stmts)
    then markAnnotatedWithLayout stmts
    else return stmts

-- ---------------------------------------------------------------------
instance ExactPrint (HsPragE GhcPs) where
  getAnnotationEntry HsPragSCC{}  = NoEntryVal
  setAnnotationAnchor a _ _ = a

  exact p@(HsPragSCC an st sl) = do
    markAnnOpenP an st "{-# SCC"
    let txt = sourceTextToString (sl_st sl) (unpackFS $ sl_fs sl)
    markLocatedAALS an apr_rest AnnVal    (Just txt) -- optional
    markLocatedAALS an apr_rest AnnValStr (Just txt) -- optional
    markAnnCloseP an
    return p


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

  exact s@(HsTypedSplice an DollarSplice _n e) = do
    markEpAnn an AnnDollarDollar
    markAnnotated e
    return s

  exact s@(HsUntypedSplice an decoration _n b) = do
    when (decoration == DollarSplice) $ markEpAnn an AnnDollar
    markAnnotated b
    return s

  exact s@(HsQuasiQuote _ _ q ss fs) = do
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
    return s

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
    if isPun then return ()
             else markEpAnn an AnnEqual
    arg' <- if ((locA $ getLoc arg) == noSrcSpan )
      then return arg
      else markAnnotated arg
    return (HsRecField an f' arg' isPun)

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

instance ExactPrint (HsFieldLabel GhcPs) where
  getAnnotationEntry (HsFieldLabel an _) = fromAnn an
  setAnnotationAnchor (HsFieldLabel an a) anc cs = HsFieldLabel (setAnchorEpa an anc cs) a

  exact (HsFieldLabel an fs) = do
    markAnnKwM an afDot  AnnDot
    fs' <- markAnnotated fs
    return (HsFieldLabel an fs')

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
        markKw (anns an)
        arg' <- markAnnotated arg
        return (HsCmdArrApp an arr' arg' o isRightToLeft)
      else do
        arg' <- markAnnotated arg
        markKw (anns an)
        arr' <- markAnnotated arr
        return (HsCmdArrApp an arr' arg' o isRightToLeft)

  exact (HsCmdArrForm an e fixity mf cs) = do
    markLocatedMAA an al_open
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
    markLocatedMAA an al_close
    return (HsCmdArrForm an e' fixity mf cs')

  exact (HsCmdApp an e1 e2) = do
    e1' <- markAnnotated e1
    e2' <- markAnnotated e2
    return (HsCmdApp an e1' e2')

  exact (HsCmdLam a match) = do
    match' <- markAnnotated match
    return (HsCmdLam a match')

  exact (HsCmdPar an e) = do
    markOpeningParen an
    e' <- markAnnotated e
    markClosingParen an
    return (HsCmdPar an e')

  exact (HsCmdCase an e alts) = do
    markAnnKw an hsCaseAnnCase AnnCase
    e' <- markAnnotated e
    markAnnKw an hsCaseAnnOf AnnOf
    markEpAnn' an hsCaseAnnsRest AnnOpenC
    markEpAnnAll an hsCaseAnnsRest AnnSemi
    alts' <- markAnnotated alts
    markEpAnn' an hsCaseAnnsRest AnnCloseC
    return (HsCmdCase an e' alts')

  exact (HsCmdLamCase an matches) = do
    markEpAnn an AnnLam
    markEpAnn an AnnCase
    matches' <- markAnnotated matches
    return (HsCmdLamCase an matches')

  exact (HsCmdIf an a e1 e2 e3) = do
    markAnnKw an aiIf AnnIf
    e1' <- markAnnotated e1
    markAnnKwM an aiThenSemi AnnSemi
    markAnnKw an aiThen AnnThen
    e2' <- markAnnotated e2
    markAnnKwM an aiElseSemi AnnSemi
    markAnnKw an aiElse AnnElse
    e3' <- markAnnotated e3
    return (HsCmdIf an a e1' e2' e3')

  exact (HsCmdLet an binds e) = do
    markAnnKw an alLet AnnLet
    binds' <- markAnnotated binds
    markAnnKw an alIn AnnIn
    e' <- markAnnotated e
    return (HsCmdLet an binds' e')

  exact (HsCmdDo an es) = do
    debugM $ "HsCmdDo"
    markEpAnn' an al_rest AnnDo
    es' <- markAnnotated es
    return (HsCmdDo an es')

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
    markEpAnn an AnnLarrow
    body' <- markAnnotated body
    return (BindStmt an pat' body')

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
    markEpAnn an AnnLet
    binds' <- markAnnotated binds
    return (LetStmt an binds')

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
    markLocatedAAL an al_rest AnnRec
    stmts' <- markAnnList True an (markAnnotated stmts)
    return (RecStmt an stmts' a b c d e)

  -- exact x = error $ "exact CmdLStmt for:" ++ showAst x
  -- exact x = error $ "exact CmdLStmt for:"


-- ---------------------------------------------------------------------

instance ExactPrint (ParStmtBlock GhcPs GhcPs) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a
  exact (ParStmtBlock a stmts b c) = do
    stmts' <- markAnnotated stmts
    return (ParStmtBlock a stmts' b c)

exactTransStmt :: EpAnn [AddEpAnn] -> Maybe (LHsExpr GhcPs) -> (LHsExpr GhcPs) -> TransForm
  -> EPP (Maybe (LHsExpr GhcPs), (LHsExpr GhcPs))
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
    annotationsToComments (epAnnAnns an) [AnnOpenP,AnnCloseP]
    markEpAnn an AnnType

    (ltycon', tyvars',_,_) <- exactVanillaDeclHead ltycon tyvars fixity Nothing
    markEpAnn an AnnEqual
    rhs' <- markAnnotated rhs
    return (SynDecl { tcdSExt = an
                    , tcdLName = ltycon', tcdTyVars = tyvars', tcdFixity = fixity
                    , tcdRhs = rhs' })

  -- TODO: add a workaround for https://gitlab.haskell.org/ghc/ghc/-/issues/20452
  exact (DataDecl { tcdDExt = an, tcdLName = ltycon, tcdTyVars = tyvars
                  , tcdFixity = fixity, tcdDataDefn = defn }) = do
    (ltycon', tyvars', _, mctxt', defn') <- exactDataDefn an (exactVanillaDeclHead ltycon tyvars fixity) defn
    return (DataDecl { tcdDExt = an, tcdLName = ltycon', tcdTyVars = tyvars'
                     , tcdFixity = fixity, tcdDataDefn = defn' })

  -- -----------------------------------

  exact d@(ClassDecl {tcdCExt = (an, sortKey, lo),
                      tcdCtxt = context, tcdLName = lclas, tcdTyVars = tyvars,
                      tcdFixity = fixity,
                      tcdFDs  = fds,
                      tcdSigs = sigs, tcdMeths = methods,
                      tcdATs = ats, tcdATDefs = at_defs,
                      tcdDocs = _docs})
      -- TODO: add a test that demonstrates tcdDocs
      | null sigs && isEmptyBag methods && null ats && null at_defs -- No "where" part
      = do
          (fds', lclas', tyvars',context') <- top_matter
          markEpAnn an AnnOpenC
          markEpAnn an AnnCloseC
          return (ClassDecl {tcdCExt = (an, sortKey, lo),
                             tcdCtxt = context', tcdLName = lclas', tcdTyVars = tyvars',
                             tcdFixity = fixity,
                             tcdFDs  = fds',
                             tcdSigs = sigs, tcdMeths = methods,
                             tcdATs = ats, tcdATDefs = at_defs,
                             tcdDocs = _docs})

      | otherwise       -- Laid out
      = do
          (fds', lclas', tyvars',context') <- top_matter
          markEpAnn an AnnOpenC
          markEpAnnAll an id AnnSemi
          ds <- withSortKey sortKey
                               (prepareListAnnotationA sigs
                             ++ prepareListAnnotationA (bagToList methods)
                             ++ prepareListAnnotationA ats
                             ++ prepareListAnnotationA at_defs
                             -- ++ prepareListAnnotation docs
                               )
          markEpAnn an AnnCloseC
          let
            sigs'    = undynamic ds
            methods' = listToBag $ undynamic ds
            ats'     = undynamic ds
            at_defs' = undynamic ds
          return (ClassDecl {tcdCExt = (an, sortKey, lo),
                             tcdCtxt = context', tcdLName = lclas', tcdTyVars = tyvars',
                             tcdFixity = fixity,
                             tcdFDs  = fds',
                             tcdSigs = sigs', tcdMeths = methods',
                             tcdATs = ats', tcdATDefs = at_defs',
                             tcdDocs = _docs})
      where
        top_matter = do
          annotationsToComments (epAnnAnns an)  [AnnOpenP, AnnCloseP]
          markEpAnn an AnnClass
          (lclas', tyvars',_,context') <-  exactVanillaDeclHead lclas tyvars fixity context
          fds' <- if (null fds)
            then return fds
            else do
              markEpAnn an AnnVbar
              markAnnotated fds
          markEpAnn an AnnWhere
          return (fds', lclas', tyvars',context')


-- ---------------------------------------------------------------------

instance ExactPrint (FunDep GhcPs) where
  getAnnotationEntry (FunDep an _ _) = fromAnn an
  setAnnotationAnchor (FunDep an a b) anc cs = FunDep (setAnchorEpa an anc cs) a b

  exact f@(FunDep an ls rs') = do
    markAnnotated ls
    markEpAnn an AnnRarrow
    markAnnotated rs'
    return f

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
    exactFlavour an info
    exact_top_level
    annotationsToCommentsA an [AnnOpenP,AnnCloseP]
    (ltycon', tyvars',_,_) <- exactVanillaDeclHead ltycon tyvars fixity Nothing
    result' <- exact_kind
    mb_inj' <-
      case mb_inj of
        Nothing -> return mb_inj
        Just inj -> do
          markEpAnn an AnnVbar
          inj' <- markAnnotated inj
          return (Just inj')
    info' <- case info of
               ClosedTypeFamily mb_eqns -> do
                 markEpAnn an AnnWhere
                 markEpAnn an AnnOpenC
                 mb_eqns' <-
                   case mb_eqns of
                     Nothing -> markEpAnn an AnnDotdot >> return mb_eqns
                     Just eqns -> do
                       eqns' <- markAnnotated eqns
                       return (Just eqns')
                 markEpAnn an AnnCloseC
                 return (ClosedTypeFamily mb_eqns')
               _ -> return info
    return (FamilyDecl { fdExt = an
                       , fdInfo = info'
                       , fdTopLevel = top_level
                       , fdLName = ltycon'
                       , fdTyVars = tyvars'
                       , fdFixity = fixity
                       , fdResultSig = L lr result'
                       , fdInjectivityAnn = mb_inj' })
    where
      exact_top_level = case top_level of
                          TopLevel    -> markEpAnn an AnnFamily
                          NotTopLevel -> do
                            -- It seems that in some kind of legacy
                            -- mode the 'family' keyword is still
                            -- accepted.
                            markEpAnn an AnnFamily
                            return ()

      exact_kind = case result of
                     NoSig    _         -> return result
                     KindSig  x kind    -> do
                       markEpAnn an AnnDcolon
                       kind' <- markAnnotated kind
                       return (KindSig  x kind')
                     TyVarSig x tv_bndr -> do
                       markEpAnn an AnnEqual
                       tv_bndr' <- markAnnotated tv_bndr
                       return (TyVarSig x tv_bndr')


exactFlavour :: EpAnn [AddEpAnn] -> FamilyInfo GhcPs -> EPP ()
exactFlavour an DataFamily            = markEpAnn an AnnData
exactFlavour an OpenTypeFamily        = markEpAnn an AnnType
exactFlavour an (ClosedTypeFamily {}) = markEpAnn an AnnType

-- ---------------------------------------------------------------------

exactDataDefn
  :: EpAnn [AddEpAnn]
  -> (Maybe (LHsContext GhcPs) -> EPP (LocatedN RdrName, a, b, Maybe (LHsContext GhcPs))) -- Printing the header
  -> HsDataDefn GhcPs
  -> EPP (LocatedN RdrName, a, b, Maybe (LHsContext GhcPs), HsDataDefn GhcPs)
exactDataDefn an exactHdr
                 (HsDataDefn { dd_ext = x, dd_ND = new_or_data, dd_ctxt = context
                             , dd_cType = mb_ct
                             , dd_kindSig = mb_sig
                             , dd_cons = condecls, dd_derivs = derivings }) = do
  annotationsToComments (epAnnAnns an) [AnnOpenP, AnnCloseP]
  if new_or_data == DataType
    then markEpAnn an AnnData
    else markEpAnn an AnnNewtype
  markEpAnn an AnnInstance -- optional
  mb_ct' <- mapM markAnnotated mb_ct
  (ln', tvs', b, mctxt') <- exactHdr context
  mb_sig' <- case mb_sig of
    Nothing -> return Nothing
    Just kind -> do
      markEpAnn an AnnDcolon
      kind' <- markAnnotated kind
      return (Just kind')
  when (isGadt condecls) $ markEpAnn an AnnWhere
  markEpAnn an AnnOpenC
  condecls' <- exact_condecls an condecls
  markEpAnn an AnnCloseC
  derivings' <- mapM markAnnotated derivings
  return (ln', tvs', b, mctxt',
                 (HsDataDefn { dd_ext = x, dd_ND = new_or_data, dd_ctxt = context
                             , dd_cType = mb_ct'
                             , dd_kindSig = mb_sig'
                             , dd_cons = condecls', dd_derivs = derivings' }))


exactVanillaDeclHead :: LocatedN RdrName
                     -> LHsQTyVars GhcPs
                     -> LexicalFixity
                     -> Maybe (LHsContext GhcPs)
                     -> EPP (LocatedN RdrName, LHsQTyVars GhcPs, (), Maybe (LHsContext GhcPs))
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
  return (thing', tvs { hsq_explicit = tyvars' }, (), context')

-- ---------------------------------------------------------------------

instance ExactPrint (InjectivityAnn GhcPs) where
  getAnnotationEntry (InjectivityAnn an _ _) = fromAnn an
  setAnnotationAnchor (InjectivityAnn an a b) anc cs = InjectivityAnn (setAnchorEpa an anc cs) a b
  exact a@(InjectivityAnn an lhs rhs) = do
    markEpAnn an AnnVbar
    markAnnotated lhs
    markEpAnn an AnnRarrow
    mapM_ markAnnotated rhs
    return a

-- ---------------------------------------------------------------------

class Typeable flag => ExactPrintTVFlag flag where
  exactTVDelimiters :: EpAnn [AddEpAnn] -> flag -> Annotated a -> Annotated a

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
    when (promoted == IsPromoted) $ markEpAnn an AnnSimpleQuote
    name' <- markAnnotated name
    return (HsTyVar an promoted name')
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
    mult' <- markArrow an mult
    ty2' <- markAnnotated ty2
    return (HsFunTy an mult' ty1' ty2')
  exact (HsListTy an tys) = do
    markOpeningParen an
    tys' <- markAnnotated tys
    markClosingParen an
    return (HsListTy an tys')
  exact (HsTupleTy an con tys) = do
    markOpeningParen an
    tys' <- markAnnotated tys
    markClosingParen an
    return (HsTupleTy an con tys')
  exact (HsSumTy an tys) = do
    markOpeningParen an
    tys' <- markAnnotated tys
    markClosingParen an
    return (HsSumTy an tys')
  exact (HsOpTy an t1 lo t2) = do
    t1' <- markAnnotated t1
    lo' <- markAnnotated lo
    t2' <- markAnnotated t2
    return (HsOpTy an t1' lo' t2')
  exact (HsParTy an ty) = do
    markOpeningParen an
    ty' <- markAnnotated ty
    markClosingParen an
    return (HsParTy an ty')
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
    case mt of
      NoSourceText -> return ()
      SourceText src -> do
        debugM $ "HsBangTy: src=" ++ showAst src
        markLocatedAALS an id AnnOpen  (Just src)
        markLocatedAALS an id AnnClose (Just "#-}")
        debugM $ "HsBangTy: done unpackedness"
    case str of
      SrcLazy     -> markEpAnn an AnnTilde
      SrcStrict   -> markEpAnn an AnnBang
      NoSrcStrict -> return ()
    ty' <- markAnnotated ty
    return (HsBangTy an (HsSrcBang mt up str) ty')
  -- exact x@(HsRecTy an _)            = withPpr x
  exact (HsExplicitListTy an prom tys) = do
    when (isPromoted prom) $ markEpAnn an AnnSimpleQuote
    markEpAnn an AnnOpenS
    tys' <- markAnnotated tys
    markEpAnn an AnnCloseS
    return (HsExplicitListTy an prom tys')
  exact (HsExplicitTupleTy an tys) = do
    markEpAnn an AnnSimpleQuote
    markEpAnn an AnnOpenP
    tys' <- markAnnotated tys
    markEpAnn an AnnCloseP
    return (HsExplicitTupleTy an tys')
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

  exact f@(HsForAllVis an bndrs)   = do
    markLocatedAA an fst -- AnnForall
    markAnnotated bndrs
    markLocatedAA an snd -- AnnRarrow
    return f

  exact f@(HsForAllInvis an bndrs) = do
    markLocatedAA an fst -- AnnForall
    markAnnotated bndrs
    markLocatedAA an snd -- AnnDot
    return f

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

  exact l@(L (SrcSpanAnn EpAnnNotUsed _) a) = markAnnotated a >> return l
  exact l@(L (SrcSpanAnn (EpAnn _ (AnnContext ma opens closes) _) _) a) = do
    mapM_ (markKwA AnnOpenP) (sort opens)
    markAnnotated a
    mapM_ (markKwA AnnCloseP) (sort closes)
    case ma of
      Just (UnicodeSyntax, r) -> markKwA AnnDarrowU r >> pure ()
      Just (NormalSyntax,  r) -> markKwA AnnDarrow  r >> pure ()
      Nothing -> pure ()
    return l

-- ---------------------------------------------------------------------

instance ExactPrint (DerivClauseTys GhcPs) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a

  exact d@(DctSingle _ ty) = do
    markAnnotated ty
    return d
  exact d@(DctMulti _ tys) = do
    markAnnotated tys
    return d

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
    printUnicode l n
    return x
  exact x@(L (SrcSpanAnn (EpAnn _anchor ann _cs) ll) n) = do
    case ann of
      NameAnn a o l c t -> do
        markName a o (Just (l,n)) c
        markTrailing t
      NameAnnCommas a o cs c t -> do
        let (kwo,kwc) = adornments a
        markKw (AddEpAnn kwo o)
        forM_ cs (\loc -> markKw (AddEpAnn AnnComma loc))
        markKw (AddEpAnn kwc c)
        markTrailing t
      NameAnnOnly a o c t -> do
        markName a o Nothing c
        markTrailing t
      NameAnnRArrow nl t -> do
        markKw (AddEpAnn AnnRarrow nl)
        markTrailing t
      NameAnnQuote q name t -> do
        debugM $ "NameAnnQuote"
        markKw (AddEpAnn AnnSimpleQuote q)
        markAnnotated (L name n)
        markTrailing t
      NameAnnTrailing t -> do
        printUnicode ll n
        markTrailing t
    return x

printUnicode :: SrcSpan -> RdrName -> EPP ()
printUnicode l n = do
  let str = case (showPprUnsafe n) of
            -- TODO: unicode support?
              "forall" -> if spanLength (realSrcSpan l) == 1 then "∀" else "forall"
              s -> s
  printStringAtSs l str

markName :: NameAdornment
         -> EpaLocation -> Maybe (EpaLocation,RdrName) -> EpaLocation -> EPP ()
markName adorn open mname close = do
  let (kwo,kwc) = adornments adorn
  markKw (AddEpAnn kwo open)
  case mname of
    Nothing -> return ()
    Just (name, a) -> printStringAtAA name (showPprUnsafe a)
  markKw (AddEpAnn kwc close)

adornments :: NameAdornment -> (AnnKeywordId, AnnKeywordId)
adornments NameParens     = (AnnOpenP, AnnCloseP)
adornments NameParensHash = (AnnOpenPH, AnnClosePH)
adornments NameBackquotes = (AnnBackquote, AnnBackquote)
adornments NameSquare     = (AnnOpenS, AnnCloseS)

markTrailing :: [TrailingAnn] -> EPP ()
markTrailing ts = do
  p <- getPosP
  debugM $ "markTrailing:" ++ showPprUnsafe (p,ts)
  mapM_ markKwT (sort ts)

-- ---------------------------------------------------------------------

-- based on pp_condecls in Decls.hs
exact_condecls :: EpAnn [AddEpAnn] -> [LConDecl GhcPs] -> EPP [LConDecl GhcPs]
exact_condecls an cs
  | gadt_syntax                  -- In GADT syntax
  = do
      mapM markAnnotated cs
  | otherwise                    -- In H98 syntax
  = do
      markEpAnn an AnnEqual
      mapM markAnnotated cs
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
  exact d@(ConDeclH98 { con_ext = an
                      , con_name = con
                      , con_forall = has_forall
                      , con_ex_tvs = ex_tvs
                      , con_mb_cxt = mcxt
                      , con_args = args
                      , con_doc = doc }) = do
    mapM_ markAnnotated doc
    when has_forall $ markEpAnn an AnnForall
    mapM_ markAnnotated ex_tvs
    when has_forall $ markEpAnn an AnnDot
    mapM_ markAnnotated mcxt
    when (isJust mcxt) $ markEpAnn an AnnDarrow

    exact_details args
    return d

    where
    --   -- In ppr_details: let's not print the multiplicities (they are always 1, by
    --   -- definition) as they do not appear in an actual declaration.
      exact_details (InfixCon t1 t2) = do
        markAnnotated t1
        markAnnotated con
        markAnnotated t2
        return ()
      exact_details (PrefixCon tyargs tys) = do
        markAnnotated con
        markAnnotated tyargs
        markAnnotated tys
        return ()
      exact_details (RecCon fields) = do
        markAnnotated con
        markAnnotated fields
        return ()

  -- -----------------------------------

  exact d@(ConDeclGADT { con_g_ext = an
                       , con_names = cons
                       , con_bndrs = bndrs
                       , con_mb_cxt = mcxt, con_g_args = args
                       , con_res_ty = res_ty, con_doc = doc }) = do
    mapM_ markAnnotated doc
    mapM_ markAnnotated cons
    markEpAnn an AnnDcolon
    annotationsToComments (epAnnAnns an)  [AnnOpenP, AnnCloseP]
    markAnnotated bndrs
    mapM_ markAnnotated mcxt
    when (isJust mcxt) $ markEpAnn an AnnDarrow
    case args of
        (PrefixConGADT args') -> mapM_ markScaled args' >> return ()
        (RecConGADT fields)   -> markAnnotated fields >> return ()
    markAnnotated res_ty
    return d

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

  exact f@(ConDeclField an names ftype mdoc) = do
    markAnnotated names
    markEpAnn an AnnDcolon
    markAnnotated ftype
    mapM_ markAnnotated mdoc
    return f

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

markScaled :: (HsScaled GhcPs (LBangType GhcPs)) -> Annotated ()
markScaled (HsScaled arr (L l c)) = do
  markAnnotated ((L l (HsScaled arr (L (noAnnSrcSpan $ locA l) c)))
                 :: LocatedA (HsScaled GhcPs (LBangType GhcPs)))
  return ()

instance (ExactPrint a) => ExactPrint (HsScaled GhcPs a) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a
  exact s@(HsScaled arr t) = do
    markAnnotated t
    markArrow EpAnnNotUsed arr
    return s

-- ---------------------------------------------------------------------

instance ExactPrint (LocatedP CType) where
  getAnnotationEntry = entryFromLocatedA
  setAnnotationAnchor = setAnchorAn

  exact x@(L (SrcSpanAnn EpAnnNotUsed _) ct) = withPpr ct >> return x
  exact x@(L (SrcSpanAnn an _ll)
         (CType stp mh (stct,ct))) = do
    markAnnOpenP an stp "{-# CTYPE"
    case mh of
      Nothing -> return ()
      Just (Header srcH _h) ->
         markLocatedAALS an apr_rest AnnHeader (Just (toSourceTextWithSuffix srcH "" ""))
    markLocatedAALS an apr_rest AnnVal (Just (toSourceTextWithSuffix stct (unpackFS ct) ""))
    markAnnCloseP an
    return x

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

  exact x@(L (SrcSpanAnn ann l) ies) = do
    debugM $ "LocatedL [LIE"
    markLocatedAAL ann al_rest AnnHiding
    p <- getPosP
    debugM $ "LocatedL [LIE:p=" ++ showPprUnsafe p
    ies' <- markAnnList True ann (markAnnotated ies)
    return (L (SrcSpanAnn ann l) ies')

instance (ExactPrint (Match GhcPs (LocatedA body)))
   => ExactPrint (LocatedL [LocatedA (Match GhcPs (LocatedA body))]) where
  getAnnotationEntry = entryFromLocatedA
  setAnnotationAnchor = setAnchorAn
  exact (L la a) = do
    debugM $ "LocatedL [LMatch"
    -- TODO: markAnnList?
    markEpAnnAll (ann la) al_rest AnnWhere
    markLocatedMAA (ann la) al_open
    markEpAnnAll (ann la) al_rest AnnSemi
    a' <- markAnnotated a
    markLocatedMAA (ann la) al_close
    return (L la a')

-- instance ExactPrint (LocatedL [ExprLStmt GhcPs]) where
instance ExactPrint (LocatedL [LocatedA (StmtLR GhcPs GhcPs (LocatedA (HsExpr GhcPs)))]) where
  getAnnotationEntry = entryFromLocatedAFixed
  setAnnotationAnchor = setAnchorAn
  exact (L (SrcSpanAnn an' l) stmts) = do
    let an = fixAnnListAnn an'
    debugM $ "LocatedL [ExprLStmt"
    stmts' <- markAnnList True an $ do
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
    return (L (SrcSpanAnn an' l) stmts')

-- instance ExactPrint (LocatedL [CmdLStmt GhcPs]) where
instance ExactPrint (LocatedL [LocatedA (StmtLR GhcPs GhcPs (LocatedA (HsCmd GhcPs)))]) where
  getAnnotationEntry = entryFromLocatedAFixed
  setAnnotationAnchor = setAnchorAn
  exact (L (SrcSpanAnn ann' l) es) = do
    let ann = fixAnnListAnn ann'
    debugM $ "LocatedL [CmdLStmt"
    markLocatedMAA ann al_open
    es' <- mapM markAnnotated es
    markLocatedMAA ann al_close
    return (L (SrcSpanAnn ann' l) es')

instance ExactPrint (LocatedL [LocatedA (ConDeclField GhcPs)]) where
  getAnnotationEntry = entryFromLocatedA
  setAnnotationAnchor = setAnchorAn
  exact (L (SrcSpanAnn an l) fs) = do
    debugM $ "LocatedL [LConDeclField"
    fs' <- markAnnList True an (markAnnotated fs)
    return (L (SrcSpanAnn an l) fs')

instance ExactPrint (LocatedL (BF.BooleanFormula (LocatedN RdrName))) where
  getAnnotationEntry = entryFromLocatedA
  setAnnotationAnchor = setAnchorAn
  exact (L (SrcSpanAnn an l) bf) = do
    debugM $ "LocatedL [LBooleanFormula"
    bf' <- markAnnList True an (markAnnotated bf)
    return (L (SrcSpanAnn an l) bf')

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

  exact i@(IEVar _ ln) = markAnnotated ln >> return i
  exact i@(IEThingAbs _ thing) = markAnnotated thing >> return i
  exact i@(IEThingAll an thing) = do
    markAnnotated thing
    markEpAnn an AnnOpenP
    markEpAnn an AnnDotdot
    markEpAnn an AnnCloseP
    return i

  exact i@(IEThingWith an thing wc withs) = do
    markAnnotated thing
    markEpAnn an AnnOpenP
    case wc of
      NoIEWildcard -> markAnnotated withs
      IEWildcard pos -> do
        let (bs, as) = splitAt pos withs
        markAnnotated bs
        markEpAnn an AnnDotdot
        markEpAnn an AnnComma
        markAnnotated as
    markEpAnn an AnnCloseP
    return i

  exact i@(IEModuleContents an (L lm mn)) = do
    markEpAnn an AnnModule
    printStringAtSs lm (moduleNameString mn)
    return i

  -- exact (IEGroup _ _ _)          = NoEntryVal
  -- exact (IEDoc _ _)              = NoEntryVal
  -- exact (IEDocNamed _ _)         = NoEntryVal
  exact x = error $ "missing match for IE:" ++ showAst x

-- ---------------------------------------------------------------------

instance ExactPrint (IEWrappedName RdrName) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a

  exact i@(IEName n) = markAnnotated n >> return i
  exact i@(IEPattern r n) = do
    printStringAtAA r "pattern"
    markAnnotated n
    return i
  exact i@(IEType r n) = do
    printStringAtAA r "type"
    markAnnotated n
    return i

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

  exact p@(WildPat _) = do
    anchor <- getAnchorU
    debugM $ "WildPat:anchor=" ++ show anchor
    printStringAtRs anchor "_"
    return p
  exact p@(VarPat _ n) = do
    -- The parser inserts a placeholder value for a record pun rhs. This must be
    -- filtered.
    let pun_RDR = "pun-right-hand-side"
    when (showPprUnsafe n /= pun_RDR) $ markAnnotated n >> return ()
    return p
  exact p@(LazyPat an pat) = do
    markEpAnn an AnnTilde
    markAnnotated pat
    return p
  exact p@(AsPat an n pat) = do
    markAnnotated n
    markEpAnn an AnnAt
    markAnnotated pat
    return p
  exact p@(ParPat an pat) = do
    markAnnKw an ap_open AnnOpenP
    markAnnotated pat
    markAnnKw an ap_close AnnCloseP
    return p

  exact p@(BangPat an pat) = do
    markEpAnn an AnnBang
    markAnnotated pat
    return p

  exact p@(ListPat an pats) = markAnnList True an (markAnnotated pats) >> return p

  exact p@(TuplePat an pats boxity) = do
    case boxity of
      Boxed   -> markEpAnn an AnnOpenP
      Unboxed -> markEpAnn an AnnOpenPH
    markAnnotated pats
    case boxity of
      Boxed   -> markEpAnn an AnnCloseP
      Unboxed -> markEpAnn an AnnClosePH
    return p

  exact p@(SumPat an pat _alt _arity) = do
    markLocatedAAL an sumPatParens AnnOpenPH
    markAnnKwAll an sumPatVbarsBefore AnnVbar
    markAnnotated pat
    markAnnKwAll an sumPatVbarsAfter AnnVbar
    markLocatedAAL an sumPatParens AnnClosePH
    return p

  -- | ConPat an con args)
  exact p@(ConPat an con details) = exactUserCon an con details >> return p
  exact p@(ViewPat an expr pat) = do
    markAnnotated expr
    markEpAnn an AnnRarrow
    markAnnotated pat
    return p
  exact p@(SplicePat _ splice) = markAnnotated splice >> return p
  exact p@(LitPat _ lit) = printStringAdvance (hsLit2String lit) >> return p
  exact p@(NPat an ol mn _) = do
    when (isJust mn) $ markEpAnn an AnnMinus
    markAnnotated ol
    return p

  -- | NPlusKPat an n lit1 lit2 _ _)
  exact p@(NPlusKPat an n k lit2 _ _) = do
    markAnnotated n
    -- We need a fix for
    -- https://gitlab.haskell.org/ghc/ghc/-/issues/20243 to complete
    -- this
    markAnnotated k
    return p

  exact p@(SigPat an pat sig) = do
    markAnnotated pat
    markEpAnn an AnnDcolon
    markAnnotated sig
    return p
  -- exact x = error $ "missing match for Pat:" ++ showAst x

-- ---------------------------------------------------------------------

instance ExactPrint (HsPatSigType GhcPs) where
  getAnnotationEntry = const NoEntryVal
  setAnnotationAnchor a _ _ = a

  exact s@(HsPS an ty) = do
    markAnnKw an id AnnAt
    markAnnotated ty
    return s

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

exactUserCon :: (ExactPrint con) => EpAnn [AddEpAnn] -> con -> HsConPatDetails GhcPs -> EPP ()
exactUserCon _  c (InfixCon p1 p2) = markAnnotated p1 >> markAnnotated c >> markAnnotated p2 >> return ()
exactUserCon an c details          = do
  markAnnotated c
  markEpAnn an AnnOpenC
  exactConArgs details
  markEpAnn an AnnCloseC
  return ()


exactConArgs ::HsConPatDetails GhcPs -> EPP ()
exactConArgs (PrefixCon tyargs pats) = markAnnotated tyargs >> markAnnotated pats >> return ()
exactConArgs (InfixCon p1 p2) = markAnnotated p1 >> markAnnotated p2 >> return ()
exactConArgs (RecCon rpats)   = markAnnotated rpats >> return ()

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
      return h

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

adjustDeltaForOffsetM :: DeltaPos -> EPP DeltaPos
adjustDeltaForOffsetM dp = do
  colOffset <- gets dLHS
  return (adjustDeltaForOffset 0 colOffset dp)

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

printStringAdvance :: String -> EPP String
printStringAdvance str = do
  ss <- getAnchorU
  printStringAtKw' ss str
  return str

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
