{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Language.Haskell.GHC.ExactPrint
-- Based on
-- --------------------------------------------------------------------------
-- Module      :  Language.Haskell.Exts.Annotated.ExactPrint
-- Copyright   :  (c) Niklas Broberg 2009
-- License     :  BSD-style (see the file LICENSE.txt)
--
-- Maintainer  :  Niklas Broberg, d00nibro@chalmers.se
-- Stability   :  stable
-- Portability :  portable
--
-- Exact-printer for Haskell abstract syntax. The input is a (semi-concrete)
-- abstract syntax tree, annotated with exact source information to enable
-- printing the tree exactly as it was parsed.
--
-----------------------------------------------------------------------------
module Language.Haskell.GHC.ExactPrint
        ( annotateAST
        , Anns
        , exactPrintAnnotated
        , exactPrintAnnotation

        , exactPrint
        , ExactP

        ) where

import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Utils

import Control.Monad (liftM, ap)
import Control.Applicative
import Control.Monad.State
import Control.Exception
import Data.Data
import Data.List
import Data.Monoid
import Control.Monad.Writer
-- import Data.List.Utils -- TODO: Reinstate when available

import qualified Bag           as GHC
import qualified BasicTypes    as GHC
import qualified Class         as GHC
import qualified CoAxiom       as GHC
import qualified FastString    as GHC
import qualified ForeignCall   as GHC
import qualified GHC           as GHC
import qualified SrcLoc        as GHC

import qualified Data.Map as Map

-- ---------------------------------------------------------------------

-- Compatibiity types, from HSE

-- | A portion of the source, extended with information on the position of entities within the span.
data SrcSpanInfo = SrcSpanInfo
    { srcInfoSpan    :: GHC.SrcSpan
    , srcInfoPoints  :: [GHC.SrcSpan]    -- Marks the location of specific entities inside the span
    }
  deriving (Eq,Ord,Show,Typeable,Data)


-- | A class to work over all kinds of source location information.
class SrcInfo si where
  toSrcInfo   :: GHC.SrcLoc -> [GHC.SrcSpan] -> GHC.SrcLoc -> si
  fromSrcInfo :: SrcSpanInfo -> si
  getPointLoc :: si -> GHC.SrcLoc
  fileName    :: si -> String
  startLine   :: si -> Int
  startColumn :: si -> Int

  getPointLoc si = GHC.mkSrcLoc (GHC.mkFastString $ fileName si) (startLine si) (startColumn si)


instance SrcInfo GHC.SrcSpan where
  toSrcInfo   = error "toSrcInfo GHC.SrcSpan undefined"
  fromSrcInfo = error "toSrcInfo GHC.SrcSpan undefined"

  getPointLoc = GHC.srcSpanStart

  fileName (GHC.RealSrcSpan s) = GHC.unpackFS $ GHC.srcSpanFile s
  fileName _                   = "bad file name for SrcSpan"

  startLine   = srcSpanStartLine
  startColumn = srcSpanStartColumn



class Annotated a where
  ann :: a -> GHC.SrcSpan

instance Annotated (GHC.Located a) where
  ann (GHC.L l _) = l

------------------------------------------------------
-- The EP monad and basic combinators

-- The (ColOffset,ColOffset) value carries the normal and current
-- column offset. The second one captures the difference between the
-- original col when the DP was captured and the current one.

data EPState = EPState
             { epPos :: Pos
             , stack :: [(ColOffset, ColDelta)]
             , srcSpans :: [GHC.SrcSpan]
             , comments :: [DComment]
             , eFunId   :: (Bool, String)
             , eFunIsInfix :: Bool
             , annKds   :: [AnnKds]
             , anns     :: Anns
             }

type EP a = StateT EPState (Writer (Endo String)) a

runEP :: EP () -> GHC.SrcSpan -> [DComment] -> Anns -> String
runEP f ss cs ans =
  flip appEndo "" . snd . runWriter
  . flip execStateT (defaultState ss cs ans) $ f

defaultState :: GHC.SrcSpan -> [DComment] -> Anns -> EPState
defaultState ss cs as = EPState
             { epPos = (1,1)
             , stack = [(0,0)]
             , srcSpans = [ss]
             , comments = cs
             , eFunId   = (False, "")
             , eFunIsInfix = False
             , annKds      = []
             , anns = as
             }

getPos :: EP Pos
getPos = gets epPos

setPos :: Pos -> EP ()
setPos l = modify (\s -> s {epPos = l})

-- ---------------------------------------------------------------------

-- Get the current column offset
getOffset :: EP ColOffset
getOffset = gets (fst . ghead "getOffset" . stack)

-- |Given a step offset to be applied, the original column when the
-- offset was calculated and the current column, determine an
-- equivalent offset
pushOffset :: ColOffset -> Col -> Pos -> EP ()
pushOffset dc sc (_or,oc) = do
  (r, c) <- gets epPos
  (co, cd) <- gets (ghead "pushOffset" . stack)
  let nd = sc - oc
      (co',cd') = if nd == cd
                    then (dc + co,cd)
                    else (dc + co + (cd - nd), nd)
  modify (\s -> s {stack = (co',cd'): stack s})

popOffset :: EP ()
popOffset = modify (\s -> s {stack = tail (stack s)})
--     `debug` ("popOffset:old co,new co=" ++ show (fst _o,fst $ head dp))
 --              )
-- ---------------------------------------------------------------------

pushSrcSpan :: GHC.SrcSpan -> EP ()
pushSrcSpan ss = modify (\s -> s {srcSpans = ss:(srcSpans s)})

popSrcSpan :: EP ()
popSrcSpan = modify (\s -> s {srcSpans = tail (srcSpans s)})

getAnnotation :: (Data a) => GHC.Located a -> EP (Maybe Annotation)
getAnnotation a  = gets (getAnnotationEP a . anns)

getAndRemoveAnnotation :: (Data a) => GHC.Located a -> EP (Maybe AnnValue)
getAndRemoveAnnotation a = do
  (r, an') <- gets (getAndRemoveAnnotationEP a . anns)
  modify (\s -> s { anns = an' })
  return r


pushKds :: AnnKds -> EP ()
pushKds kd = modify (\s -> s { annKds = kd : (annKds s)})

popKds :: EP ()
popKds = modify (\s -> s { annKds = tail (annKds s)})

-- | Get and remove the first item in the (k,v) list for which the k matches.
-- Return the value, together with any comments skipped over to get there.
destructiveGetFirst :: KeywordId -> ([(KeywordId,v)],[(KeywordId,v)])
                    -> ([(KeywordId,v)],[v],[(KeywordId,v)])
destructiveGetFirst _key (acc,[]) = ([],[],acc)
destructiveGetFirst  key (acc,((k,v):kvs))
  | k == key = let (cs,others) = commentsAndOthers acc in (cs,[v],others++kvs)
  | otherwise = destructiveGetFirst key (acc++[(k,v)],kvs)
  where
    commentsAndOthers kvs' = partition isComment kvs'
    isComment ((AnnComment _),_) = True
    isComment _              = False

-- |destructive get, hence use an annotation once only
getAnnFinal :: KeywordId -> EP [DeltaPos]
getAnnFinal kw = do
  kd <- gets annKds
  let (r, kd', dcs) = case kd of
                  []    -> ([],[], [])
                  (k:kds) -> (r',kk:kds, dcs')
                    where (cs', r',kk) = destructiveGetFirst kw ([],k)
                          dcs' = concatMap keywordIdToDComment cs'
  modify (\s -> s { annKds = kd' })
  modify (\s -> s { comments = dcs } )
  return r


keywordIdToDComment :: (KeywordId, DeltaPos) -> [DComment]
keywordIdToDComment (AnnComment comment,_dp) = [comment]
keywordIdToDComment _                   = []

-- |non-destructive get
peekAnnFinal :: KeywordId -> EP [DeltaPos]
peekAnnFinal kw = do
  (_, r, _) <- (\kd -> destructiveGetFirst kw ([], kd)) <$> gets (head . annKds)
  return r

getFunId :: EP (Bool,String)
getFunId = gets eFunId

setFunId :: (Bool,String) -> EP ()
setFunId st = modify (\s -> s{eFunId = st})

getFunIsInfix :: EP Bool
getFunIsInfix = gets eFunIsInfix

setFunIsInfix :: Bool -> EP ()
setFunIsInfix b = modify (\s -> s {eFunIsInfix = b})

-- ---------------------------------------------------------------------

printString :: String -> EP ()
printString str = do
  (l,c) <- gets epPos
  setPos (l, c + length str)
  tell (Endo $ showString str)

getComments :: EP [DComment]
getComments = gets comments

{-
dropComment :: EP ()
dropComment =
    modify (\s -> s { comments = newComments (comments s) } )
    where
      newComments cs =
        case cs of
            (_:csl) -> csl
            _       -> cs
mergeComments :: [DComment] -> EP ()
mergeComments dcs = do
    cs <- gets comments
    srcs <- gets srcSpans
    dps  <- gets stack
    let ll = ss2pos $ head srcs
        (co,_) = ghead "mergeComments" dps
        acs = map (undeltaComment ll co) dcs
        cs' = merge acs cs
    modify (\s -> s {comments = cs'}) -- `debug` ("mergeComments:(l,acs,dcs)=" ++ show (l,acs,dcs))
-}

newLine :: EP ()
newLine = do
    (l,_) <- getPos
    printString "\n"
    setPos (l+1,1)

padUntil :: Pos -> EP ()
padUntil (l,c) = do
    (l1,c1) <- getPos
    case  {- trace (show ((l,c), (l1,c1))) -} () of
     _ {-()-} | l1 >= l && c1 <= c -> printString $ replicate (c - c1) ' '
              | l1 < l             -> newLine >> padUntil (l,c)
              | otherwise          -> return ()

{-
mPrintComments :: Pos -> EP ()
mPrintComments p = do
  {-
    mc <- getComment
    case mc of
     Nothing -> return ()
     Just (DComment s str) ->
        (
        when (s < p) $ do
            dropComment
            padUntil s
            printComment str
            -- setPos e -- AZ:only seems to affect the end position of the file
            mPrintComments p
         ) -- `debug` ("mPrintComments:(s,p):" ++ show (s,p))
-}
  return ()
-}

{-
printComment :: String -> EP ()
printComment str = printString str
-}

printWhitespace :: Pos -> EP ()
printWhitespace p = do
  -- mPrintComments p >> padUntil p
  padUntil p

printStringAt :: Pos -> String -> EP ()
printStringAt p str = printWhitespace p >> printString str

-- ---------------------------------------------------------------------

-- |This should be the final point where things are mode concrete,
-- before output. Hence the point where comments can be inserted
printStringAtLsDelta :: [DeltaPos] -> String -> EP ()
printStringAtLsDelta mc s =
  case reverse mc of
    (cl:_) -> do
      p <- getPos
      colOffset <- getOffset
      if isGoodDeltaWithOffset cl colOffset
        then do
          cs <- getComments
          mapM_ printQueuedComment cs
          printStringAt (undelta p cl colOffset) s
            `debug` ("printStringAtLsDelta:(pos,s):" ++ show (undelta p cl colOffset,s))
        else return () `debug` ("printStringAtLsDelta:bad delta for (mc,s):" ++ show (mc,s))
    _ -> return ()


isGoodDeltaWithOffset :: DeltaPos -> Int -> Bool
isGoodDeltaWithOffset dp colOffset = isGoodDelta (DP (undelta (0,0) dp colOffset))

-- AZ:TODO: harvest the commonality between this and printStringAtLsDelta
printQueuedComment :: DComment -> EP ()
printQueuedComment (DComment (dp,de) s) = do
  p <- getPos
  colOffset <- getOffset
  if isGoodDeltaWithOffset dp colOffset
    then do
      printStringAt (undelta p dp colOffset) s
         `debug` ("printQueuedComment:(pos,s):" ++ show (undelta p dp colOffset,s))
      setPos (undelta p de colOffset)
    else return () `debug` ("printQueuedComment::bad delta for (dp,s):" ++ show (dp,s))

-- ---------------------------------------------------------------------

getPosForDelta :: DeltaPos -> EP Pos
getPosForDelta dp = do
  p <- getPos
  colOffset <- getOffset
  return (undelta p dp colOffset)

-- ---------------------------------------------------------------------

printStringAtMaybeAnn :: KeywordId -> String -> EP ()
printStringAtMaybeAnn an str = do
  ma <- getAnnFinal an
  printStringAtLsDelta ma str
    `debug` ("printStringAtMaybeAnn:(an,ma,str)=" ++ show (an,ma,str))

printStringAtMaybeAnnAll :: KeywordId -> String -> EP ()
printStringAtMaybeAnnAll an str = go
  where
    go = do
      ma <- getAnnFinal an
      case ma of
        [] -> return ()
        [d]  -> printStringAtLsDelta [d] str >> go

-- ---------------------------------------------------------------------

countAnns :: KeywordId -> EP Int
countAnns an = do
  ma <- peekAnnFinal an
  return (length ma)

------------------------------------------------------------------------------
-- Printing of source elements

-- | Print an AST exactly as specified by the annotations on the nodes in the tree.
-- exactPrint :: (ExactP ast) => ast -> [Comment] -> String
exactPrint :: (ExactP ast) => GHC.Located ast -> [DComment] -> String
exactPrint ast@(GHC.L l _) cs = runEP (exactPC ast) l cs Map.empty


exactPrintAnnotated ::
     GHC.Located (GHC.HsModule GHC.RdrName) -> GHC.ApiAnns -> String
exactPrintAnnotated ast@(GHC.L l _) ghcAnns = runEP (loadInitialComments >> exactPC ast) l [] an
  where
    an = annotateLHsModule ast ghcAnns

exactPrintAnnotation :: ExactP ast =>
  GHC.Located ast -> [DComment] -> Anns -> String
exactPrintAnnotation ast@(GHC.L l _) cs an = runEP (loadInitialComments >> exactPC ast) l cs an
  -- `debug` ("exactPrintAnnotation:an=" ++ (concatMap (\(l,a) -> show (ss2span l,a)) $ Map.toList an ))

annotateAST :: GHC.Located (GHC.HsModule GHC.RdrName) -> GHC.ApiAnns -> Anns
annotateAST ast ghcAnns = annotateLHsModule ast ghcAnns

loadInitialComments :: EP ()
loadInitialComments = do
  -- return () `debug` ("loadInitialComments entered")
  -- Just (Ann cs _ _) <- getAnnotation (GHC.L GHC.noSrcSpan ())
  -- mergeComments cs -- `debug` ("loadInitialComments cs=" ++ show cs)
  -- return () `debug` ("loadInitialComments exited")
  return ()

-- |First move to the given location, then call exactP
exactPC :: (ExactP ast) => GHC.Located ast -> EP ()
exactPC a@(GHC.L l ast) =
    do pushSrcSpan l `debug` ("exactPC entered for:" ++ showGhc l)
       ma <- getAndRemoveAnnotation a
       (offset,edp,kd) <- case ma of
         Nothing -> return (0,DP (0,0),[])
         Just ((Ann edp dp),kds) -> do
             return (dp,edp,kds)
       pushKds kd
       op <- getPosForDelta edp
       pushOffset offset (srcSpanStartColumn l) op
       do
         exactP ast
         printStringAtMaybeAnn (G GHC.AnnComma) ","
         printStringAtMaybeAnnAll AnnSemiSep ";"
       popOffset
       popKds
       popSrcSpan


printMerged :: (ExactP a, ExactP b) => [GHC.Located a] -> [GHC.Located b] -> EP ()
printMerged [] [] = return ()
printMerged [] bs = mapM_ exactPC bs
printMerged as [] = mapM_ exactPC as
printMerged (a@(GHC.L l1 _):as) (b@(GHC.L l2 _):bs) =
  if l1 < l2
    then exactPC a >> printMerged    as (b:bs)
    else exactPC b >> printMerged (a:as)   bs

-- ---------------------------------------------------------------------

prepareListPrint :: ExactP ast
                 => [GHC.GenLocated GHC.SrcSpan ast] -> [(GHC.SrcSpan, EP ())]
prepareListPrint ls = map (\b@(GHC.L l _) -> (l,exactPC b)) ls

applyListPrint :: (Monad m, Ord a) => [(a, m b)] -> m ()
applyListPrint ls = mapM_ (\(_,b) -> b) $ sortBy (\(a,_) (b,_) -> compare a b) ls

-- ---------------------------------------------------------------------
-- Exact printing for GHC

class (Data ast) => ExactP ast where
  -- | Print an AST fragment. The correct position in output is
  -- already established.
  exactP :: ast -> EP ()

instance ExactP (GHC.HsModule GHC.RdrName) where
  exactP (GHC.HsModule mmn mexp imps decls mdepr _haddock) = do

    case mmn of
      Just (GHC.L _ mn) -> do
        printStringAtMaybeAnn (G GHC.AnnModule) "module" -- `debug` ("exactP.HsModule:cs=" ++ show cs)
        printStringAtMaybeAnn (G GHC.AnnVal) (GHC.moduleNameString mn)
      Nothing -> return ()

    case mdepr of
      Nothing -> return ()
      Just depr -> exactPC depr

    case mexp of
      Just lexps -> do
        return () `debug` ("about to exactPC lexps")
        exactPC lexps
        return ()
      Nothing -> return ()

    printStringAtMaybeAnn (G GHC.AnnWhere) "where"
    printStringAtMaybeAnn (G GHC.AnnOpenC)  "{"
    printStringAtMaybeAnnAll (G GHC.AnnSemi) ";" -- possible leading semis
    exactP imps

    mapM_ exactPC decls

    printStringAtMaybeAnn (G GHC.AnnCloseC) "}"

    -- put the end of file whitespace in
    printStringAtMaybeAnn (G GHC.AnnEofPos) ""

-- ---------------------------------------------------------------------

instance ExactP GHC.WarningTxt where
  exactP (GHC.WarningTxt (GHC.L _ ls) lss) = do
    printStringAtMaybeAnn (G GHC.AnnOpen) ls
    printStringAtMaybeAnn (G GHC.AnnOpenS) "["
    mapM_ exactPC lss
    printStringAtMaybeAnn (G GHC.AnnCloseS) "]"
    printStringAtMaybeAnn (G GHC.AnnClose) "#-}"

  exactP (GHC.DeprecatedTxt (GHC.L _ ls) lss) = do
    printStringAtMaybeAnn (G GHC.AnnOpen) ls
    printStringAtMaybeAnn (G GHC.AnnOpenS) "["
    mapM_ exactPC lss
    printStringAtMaybeAnn (G GHC.AnnCloseS) "]"
    printStringAtMaybeAnn (G GHC.AnnClose) "#-}"

-- ---------------------------------------------------------------------

instance ExactP (GHC.ModuleName) where
  exactP mn = do
    printString (GHC.moduleNameString mn)

-- ---------------------------------------------------------------------

instance ExactP [GHC.LIE GHC.RdrName] where
  exactP ies = do
    printStringAtMaybeAnn (G GHC.AnnHiding) "hiding"
    printStringAtMaybeAnn (G GHC.AnnOpenP) "("
    mapM_ exactPC ies
    printStringAtMaybeAnn (G GHC.AnnCloseP) ")"

-- ---------------------------------------------------------------------

instance ExactP (GHC.IE GHC.RdrName) where
  exactP (GHC.IEVar ln) = do
    printStringAtMaybeAnn (G GHC.AnnPattern) "pattern"
    printStringAtMaybeAnn (G GHC.AnnType)    "type"
    exactPC ln

  exactP (GHC.IEThingAbs n) = do
    printStringAtMaybeAnn (G GHC.AnnType)    "type"
    exactPC n

  exactP (GHC.IEThingWith n ns) = do
    exactPC n
    printStringAtMaybeAnn (G GHC.AnnOpenP)    "("
    mapM_ exactPC ns
    printStringAtMaybeAnn (G GHC.AnnCloseP)   ")"

  exactP (GHC.IEThingAll n) = do
    exactPC n
    printStringAtMaybeAnn (G GHC.AnnOpenP)   "("
    printStringAtMaybeAnn (G GHC.AnnDotdot)  ".."
    printStringAtMaybeAnn (G GHC.AnnCloseP)  ")"

  exactP (GHC.IEModuleContents (GHC.L _ mn)) = do
    printStringAtMaybeAnn (G GHC.AnnModule)  "module"
    printStringAtMaybeAnn (G GHC.AnnVal)     (GHC.moduleNameString mn)

  exactP x = printString ("no exactP.IE for " ++ showGhc (x))

-- ---------------------------------------------------------------------

instance ExactP [GHC.LImportDecl GHC.RdrName] where
  exactP imps = mapM_ exactPC imps

-- ---------------------------------------------------------------------

instance ExactP (GHC.ImportDecl GHC.RdrName) where
  exactP imp = do
    printStringAtMaybeAnn (G GHC.AnnImport) "import"

    printStringAtMaybeAnn (G GHC.AnnOpen)  "{-# SOURCE"
    printStringAtMaybeAnn (G GHC.AnnClose)  "#-}"

    printStringAtMaybeAnn (G GHC.AnnSafe)      "safe"
    printStringAtMaybeAnn (G GHC.AnnQualified) "qualified"
    printStringAtMaybeAnn (G GHC.AnnVal) (GHC.moduleNameString $ GHC.unLoc $ GHC.ideclName imp)

    case GHC.ideclAs imp of
      Nothing -> return ()
      Just mn -> do
        printStringAtMaybeAnn (G GHC.AnnAs) "as"
        printStringAtMaybeAnn (G GHC.AnnVal) (GHC.moduleNameString mn)

    case GHC.ideclHiding imp of
      Nothing -> return ()
      Just (_,lie) -> do
        -- printStringAtMaybeAnn (G GHC.AnnHiding "hiding"
        exactPC lie

-- ---------------------------------------------------------------------

doMaybe :: (Monad m) => (a -> m ()) -> Maybe a -> m ()
doMaybe f ma = case ma of
                 Nothing -> return ()
                 Just a -> f a

instance ExactP (GHC.HsDecl GHC.RdrName) where
  exactP decl = case decl of
    GHC.TyClD d       -> exactP d
    GHC.InstD d       -> exactP d
    GHC.DerivD d      -> exactP d
    GHC.ValD d        -> exactP d
    GHC.SigD d        -> exactP d
    GHC.DefD d        -> exactP d
    GHC.ForD d        -> exactP d
    GHC.WarningD d    -> exactP d
    GHC.AnnD d        -> exactP d
    GHC.RuleD d       -> exactP d
    GHC.VectD d       -> exactP d
    GHC.SpliceD d     -> exactP d
    GHC.DocD d        -> exactP d
    GHC.QuasiQuoteD d -> exactP d
    GHC.RoleAnnotD d  -> exactP d

-- ---------------------------------------------------------------------

instance ExactP (GHC.RoleAnnotDecl GHC.RdrName) where
  exactP (GHC.RoleAnnotDecl ln mr) = do
    printStringAtMaybeAnn (G GHC.AnnType) "type"
    printStringAtMaybeAnn (G GHC.AnnRole) "role"
    exactPC ln
    mapM_ exactPC mr

instance ExactP (Maybe GHC.Role) where
  exactP Nothing  = printStringAtMaybeAnn (G GHC.AnnVal) "_"
  exactP (Just r) = printStringAtMaybeAnn (G GHC.AnnVal) (GHC.unpackFS $ GHC.fsFromRole r)

-- ---------------------------------------------------------------------

instance ExactP (GHC.HsQuasiQuote GHC.RdrName) where
  exactP = assert False undefined

-- ---------------------------------------------------------------------

instance ExactP (GHC.SpliceDecl GHC.RdrName) where
  exactP (GHC.SpliceDecl (GHC.L _ (GHC.HsSplice _n e)) flag) = do
    case flag of
      GHC.ExplicitSplice ->
        printStringAtMaybeAnn (G GHC.AnnOpen) "$("
      GHC.ImplicitSplice ->
        printStringAtMaybeAnn (G GHC.AnnOpen) "$$("
    exactPC e
    printStringAtMaybeAnn (G GHC.AnnClose) ")"

-- ---------------------------------------------------------------------

instance ExactP (GHC.VectDecl GHC.RdrName) where
  exactP (GHC.HsVect src ln e) = do
    printStringAtMaybeAnn (G GHC.AnnOpen) src -- "{-# VECTORISE"
    exactPC ln
    printStringAtMaybeAnn (G GHC.AnnEqual) "="
    exactPC e
    printStringAtMaybeAnn (G GHC.AnnClose) "#-}"

  exactP (GHC.HsNoVect src ln) = do
    printStringAtMaybeAnn (G GHC.AnnOpen) src -- "{-# NOVECTORISE"
    exactPC ln
    printStringAtMaybeAnn (G GHC.AnnClose) "#-}"

  exactP (GHC.HsVectTypeIn src _b ln mln) = do
    printStringAtMaybeAnn (G GHC.AnnOpen) src -- "{-# VECTORISE" or "{-# VECTORISE SCALAR"
    printStringAtMaybeAnn (G GHC.AnnType) "type"
    exactPC ln
    printStringAtMaybeAnn (G GHC.AnnEqual) "="
    case mln of
      Nothing -> return ()
      Just n -> exactPC n
    printStringAtMaybeAnn (G GHC.AnnClose) "#-}"

  exactP (GHC.HsVectTypeOut {}) = error $ "exactP.HsVectTypeOut: only valid after type checker"

  exactP (GHC.HsVectClassIn src ln) = do
    printStringAtMaybeAnn (G GHC.AnnOpen) src -- "{-# VECTORISE"
    printStringAtMaybeAnn (G GHC.AnnClass) "class"
    exactPC ln
    printStringAtMaybeAnn (G GHC.AnnClose) "#-}"

  exactP (GHC.HsVectClassOut {}) = error $ "exactP.HsVectClassOut: only valid after type checker"
  exactP (GHC.HsVectInstIn {})   = error $ "exactP.HsVectInstIn: not supported?"
  exactP (GHC.HsVectInstOut {})  = error $ "exactP.HsVectInstOut: not supported?"


-- ---------------------------------------------------------------------

instance ExactP (GHC.RuleDecls GHC.RdrName) where
  exactP (GHC.HsRules src rules) = do
    printStringAtMaybeAnn (G GHC.AnnOpen) src
    mapM_ exactPC rules
    printStringAtMaybeAnn (G GHC.AnnClose) "#-}"

-- ---------------------------------------------------------------------

instance ExactP (GHC.AnnDecl GHC.RdrName) where
  exactP (GHC.HsAnnotation src prov e) = do
    printStringAtMaybeAnn (G GHC.AnnOpen) src
    printStringAtMaybeAnn (G GHC.AnnType) "type"
    printStringAtMaybeAnn (G GHC.AnnModule) "module"
    case prov of
      (GHC.ValueAnnProvenance n) -> exactPC n
      (GHC.TypeAnnProvenance n) -> exactPC n
      (GHC.ModuleAnnProvenance) -> return ()
    exactPC e
    printStringAtMaybeAnn (G GHC.AnnClose) "#-}"

-- ---------------------------------------------------------------------

instance ExactP (GHC.RuleDecl GHC.RdrName) where
  exactP (GHC.HsRule ln act bndrs lhs _ rhs _) = do
    exactPC ln
    -- activation
    printStringAtMaybeAnn (G GHC.AnnOpenS) "["
    printStringAtMaybeAnn (G GHC.AnnTilde) "~"
    case act of
      GHC.ActiveBefore n -> printStringAtMaybeAnn (G GHC.AnnVal) (show n)
      GHC.ActiveAfter n  -> printStringAtMaybeAnn (G GHC.AnnVal) (show n)
      _                  -> return ()
    printStringAtMaybeAnn (G GHC.AnnCloseS) "]"

    printStringAtMaybeAnn (G GHC.AnnForall) "forall"
    mapM_ exactPC bndrs
    printStringAtMaybeAnn (G GHC.AnnDot) "."

    exactPC lhs
    printStringAtMaybeAnn (G GHC.AnnEqual) "="
    exactPC rhs

-- ---------------------------------------------------------------------

instance ExactP (GHC.RuleBndr GHC.RdrName) where
  exactP (GHC.RuleBndr ln) = exactPC ln
  exactP (GHC.RuleBndrSig ln (GHC.HsWB thing _ _ _)) = do
    printStringAtMaybeAnn (G GHC.AnnOpenP) "("
    exactPC ln
    printStringAtMaybeAnn (G GHC.AnnDcolon) "::"
    exactPC thing
    printStringAtMaybeAnn (G GHC.AnnCloseP) ")"

-- ---------------------------------------------------------------------

instance ExactP (GHC.WarnDecls GHC.RdrName) where
  exactP (GHC.Warnings src warns) = do
    printStringAtMaybeAnn (G GHC.AnnOpen) src
    mapM_ exactPC warns
    printStringAtMaybeAnn (G GHC.AnnClose) "#-}"

-- ---------------------------------------------------------------------

instance ExactP (GHC.WarnDecl GHC.RdrName) where
  exactP (GHC.Warning lns txt) = do
     mapM_ exactPC lns
     printStringAtMaybeAnn (G GHC.AnnOpenS) "["
     case txt of
       -- TODO: AZ: why are we ignoring src?
       GHC.WarningTxt    src ls -> mapM_ exactPC ls
       GHC.DeprecatedTxt src ls -> mapM_ exactPC ls
     printStringAtMaybeAnn (G GHC.AnnCloseS) "]"


instance ExactP GHC.FastString where
  exactP fs = printStringAtMaybeAnn (G GHC.AnnVal) (show (GHC.unpackFS fs))

-- ---------------------------------------------------------------------

instance ExactP (GHC.ForeignDecl GHC.RdrName) where
  exactP (GHC.ForeignImport ln typ _
               (GHC.CImport cconv safety@(GHC.L ll _) _mh _imp (GHC.L _ src))) = do
    printStringAtMaybeAnn (G GHC.AnnForeign) "foreign"
    printStringAtMaybeAnn (G GHC.AnnImport) "import"

    exactPC cconv

    if ll == GHC.noSrcSpan
      then return ()
      else exactPC safety

    printStringAtMaybeAnn (G GHC.AnnVal) ("\"" ++ src ++ "\"")
    exactPC ln
    printStringAtMaybeAnn (G GHC.AnnDcolon) "::"
    exactPC typ

  exactP (GHC.ForeignExport ln typ _ (GHC.CExport spec (GHC.L _ src))) = do
    printStringAtMaybeAnn (G GHC.AnnForeign) "foreign"
    printStringAtMaybeAnn (G GHC.AnnExport) "export"
    exactPC spec
    printStringAtMaybeAnn (G GHC.AnnVal) ("\"" ++ src ++ "\"")
    exactPC ln
    printStringAtMaybeAnn (G GHC.AnnDcolon) "::"
    exactPC typ

-- ---------------------------------------------------------------------

instance (ExactP GHC.CExportSpec) where
  exactP (GHC.CExportStatic _ cconv) = exactP cconv

-- ---------------------------------------------------------------------

instance ExactP GHC.CCallConv where
  exactP GHC.StdCallConv        = printStringAtMaybeAnn (G GHC.AnnVal) "stdcall"
  exactP GHC.CCallConv          = printStringAtMaybeAnn (G GHC.AnnVal) "ccall"
  exactP GHC.CApiConv           = printStringAtMaybeAnn (G GHC.AnnVal) "capi"
  exactP GHC.PrimCallConv       = printStringAtMaybeAnn (G GHC.AnnVal) "prim"
  exactP GHC.JavaScriptCallConv = printStringAtMaybeAnn (G GHC.AnnVal) "javascript"

-- ---------------------------------------------------------------------

instance ExactP GHC.Safety where
  exactP GHC.PlayRisky         = printStringAtMaybeAnn (G GHC.AnnVal) "unsafe"
  exactP GHC.PlaySafe          = printStringAtMaybeAnn (G GHC.AnnVal) "safe"
  exactP GHC.PlayInterruptible = printStringAtMaybeAnn (G GHC.AnnVal) "interruptible"


-- ---------------------------------------------------------------------

instance ExactP (GHC.DerivDecl GHC.RdrName) where
  exactP (GHC.DerivDecl typ mov) = do
    printStringAtMaybeAnn (G GHC.AnnDeriving) "deriving"
    printStringAtMaybeAnn (G GHC.AnnInstance) "instance"
    case mov of
      Nothing -> return ()
      Just ov -> exactPC ov
    exactPC typ

-- ---------------------------------------------------------------------

instance ExactP (GHC.DefaultDecl GHC.RdrName) where
  exactP (GHC.DefaultDecl typs) = do
    printStringAtMaybeAnn (G GHC.AnnDefault) "default"
    printStringAtMaybeAnn (G GHC.AnnOpenP) "("
    mapM_ exactPC typs
    printStringAtMaybeAnn (G GHC.AnnCloseP) ")"

-- ---------------------------------------------------------------------

instance ExactP (GHC.InstDecl GHC.RdrName) where
  exactP (GHC.ClsInstD      cid) = exactP  cid
  exactP (GHC.DataFamInstD dfid) = exactP dfid
  exactP (GHC.TyFamInstD   tfid) = exactP tfid

-- ---------------------------------------------------------------------

instance ExactP GHC.OverlapMode where
  exactP (GHC.NoOverlap src) = do
    printStringAtMaybeAnn (G GHC.AnnOpen) src
    printStringAtMaybeAnn (G GHC.AnnClose) "#-}"

  exactP (GHC.Overlappable src) = do
    printStringAtMaybeAnn (G GHC.AnnOpen) src
    printStringAtMaybeAnn (G GHC.AnnClose) "#-}"

  exactP (GHC.Overlapping src) = do
    printStringAtMaybeAnn (G GHC.AnnOpen) src
    printStringAtMaybeAnn (G GHC.AnnClose) "#-}"

  exactP (GHC.Overlaps src) = do
    printStringAtMaybeAnn (G GHC.AnnOpen) src
    printStringAtMaybeAnn (G GHC.AnnClose) "#-}"

  exactP (GHC.Incoherent src) = do
    printStringAtMaybeAnn (G GHC.AnnOpen) src
    printStringAtMaybeAnn (G GHC.AnnClose) "#-}"


-- ---------------------------------------------------------------------

instance ExactP (GHC.ClsInstDecl GHC.RdrName) where
  exactP (GHC.ClsInstDecl poly binds sigs tyfams datafams mov) = do
    printStringAtMaybeAnn (G GHC.AnnInstance) "instance"
    case mov of
      Nothing -> return ()
      Just ov -> exactPC ov
    exactPC poly
    printStringAtMaybeAnn (G GHC.AnnWhere) "where"
    printStringAtMaybeAnn (G GHC.AnnOpenC) "{"
    printStringAtMaybeAnnAll (G GHC.AnnSemi) ";"

    applyListPrint (prepareListPrint (GHC.bagToList binds)
                 ++ prepareListPrint sigs
                 ++ prepareListPrint tyfams
                 ++ prepareListPrint datafams
                    )
    printStringAtMaybeAnn (G GHC.AnnCloseC) "}"

-- ---------------------------------------------------------------------

instance ExactP (GHC.TyFamInstDecl GHC.RdrName) where
   exactP (GHC.TyFamInstDecl eqn _) = do
     printStringAtMaybeAnn (G GHC.AnnType)     "type"
     printStringAtMaybeAnn (G GHC.AnnInstance) "instance"
     exactPC eqn

-- ---------------------------------------------------------------------

instance ExactP (GHC.DataFamInstDecl GHC.RdrName) where
   exactP (GHC.DataFamInstDecl ln (GHC.HsWB pats _ _ _)
            (GHC.HsDataDefn _nOrD _ctx _mtyp mkind cons mderivs) _) = do
    printStringAtMaybeAnn (G GHC.AnnData)     "data"
    printStringAtMaybeAnn (G GHC.AnnNewtype)  "newtype"
    printStringAtMaybeAnn (G GHC.AnnInstance) "instance"
    exactPC ln
    mapM_ exactPC pats

    printStringAtMaybeAnn (G GHC.AnnDcolon) "::"
    doMaybe exactPC mkind

    printStringAtMaybeAnn (G GHC.AnnWhere) "where"
    printStringAtMaybeAnn (G GHC.AnnEqual) "="

    mapM_ exactPC cons
    doMaybe exactPC mderivs

-- ---------------------------------------------------------------------

instance ExactP (GHC.HsBind GHC.RdrName) where
  exactP (GHC.FunBind (GHC.L _ n) isInfix  (GHC.MG matches _ _ _) _ _ _) = do
    setFunId (isSymbolRdrName n,rdrName2String n)
    setFunIsInfix isInfix
    mapM_ exactPC matches

  exactP (GHC.PatBind lhs (GHC.GRHSs grhs lb) _ty _fvs _ticks) = do
    exactPC lhs
    printStringAtMaybeAnn (G GHC.AnnEqual) "="
    mapM_ exactPC grhs
    printStringAtMaybeAnn (G GHC.AnnWhere) "where"
    -- exactP lb
    exactPC (GHC.L (getLocalBindsSrcSpan lb) lb)


  exactP (GHC.VarBind _var_id _var_rhs _var_inline ) = printString "VarBind"
  exactP (GHC.AbsBinds _abs_tvs _abs_ev_vars _abs_exports _abs_ev_binds _abs_binds) = printString "AbsBinds"

  exactP (GHC.PatSynBind (GHC.PSB n _fvs args def dir)) = do
    printStringAtMaybeAnn (G GHC.AnnPattern) "pattern"
    exactPC n
    case args of
      GHC.InfixPatSyn na nb -> do
        exactPC na
        exactPC nb
      GHC.PrefixPatSyn ns -> do
        mapM_ exactPC ns

    printStringAtMaybeAnn (G GHC.AnnEqual)   "="
    printStringAtMaybeAnn (G GHC.AnnLarrow)  "<-"

    exactPC def
    case dir of
      GHC.Unidirectional           -> return ()
      GHC.ImplicitBidirectional    -> return ()
      GHC.ExplicitBidirectional mg -> exactPMatchGroup mg

    printStringAtMaybeAnn (G GHC.AnnWhere)   "where"
    printStringAtMaybeAnn (G GHC.AnnOpenC)    "{"
    printStringAtMaybeAnn (G GHC.AnnCloseC)   "}"

-- ---------------------------------------------------------------------

instance ExactP (GHC.IPBind GHC.RdrName) where
  exactP (GHC.IPBind en e) = do
    case en of
      Left n -> exactPC n
      Right _i -> error $ "annotateP.IPBind:should not happen"
    printStringAtMaybeAnn (G GHC.AnnEqual) "="
    exactPC e

-- ---------------------------------------------------------------------

instance (ExactP body) => ExactP (GHC.Match GHC.RdrName (GHC.Located body)) where
  exactP (GHC.Match mln pats typ (GHC.GRHSs grhs lb)) = do
    (isSym,funid) <- getFunId
    isInfix <- getFunIsInfix
    let
      get_infix Nothing = isInfix
      get_infix (Just (_,f)) = f
    case (get_infix mln,pats) of
      (True,[a,b]) -> do
        exactPC a
        case mln of
          Nothing -> do
            if isSym
              then printStringAtMaybeAnn (G GHC.AnnFunId) funid
              else printStringAtMaybeAnn (G GHC.AnnFunId) ("`"++ funid ++ "`")
          Just (n,_) -> exactPC n
        exactPC b
      _ -> do
        case mln of
          Nothing -> printStringAtMaybeAnn (G GHC.AnnFunId) funid
          Just (n,_)  -> exactPC n
        mapM_ exactPC pats
    printStringAtMaybeAnn (G GHC.AnnEqual)  "="
    printStringAtMaybeAnn (G GHC.AnnRarrow) "->" -- for HsLam
    mapM_ exactPC typ
    mapM_ exactPC grhs
    printStringAtMaybeAnn (G GHC.AnnWhere) "where"
    printStringAtMaybeAnn (G GHC.AnnOpenC) "{"
    printStringAtMaybeAnnAll (G GHC.AnnSemi) ";"
    -- exactP lb
    exactPC (GHC.L (getLocalBindsSrcSpan lb) lb)
    printStringAtMaybeAnn (G GHC.AnnCloseC) "}"

-- ---------------------------------------------------------------------

instance ExactP (GHC.Pat GHC.RdrName) where

  exactP (GHC.WildPat _) = printStringAtMaybeAnn (G GHC.AnnVal) "_"

  exactP (GHC.VarPat n) = printStringAtMaybeAnn (G GHC.AnnVal) (rdrName2String n)

  exactP (GHC.LazyPat p)    = do
    printStringAtMaybeAnn (G GHC.AnnTilde) "~"
    exactPC p

  exactP (GHC.AsPat n p) = do
    exactPC n
    printStringAtMaybeAnn (G GHC.AnnAt) "@"
    exactPC p

  exactP (GHC.ParPat p) = do
    return () `debug` ("in exactP.ParPat")
    printStringAtMaybeAnn (G GHC.AnnOpenP) "("
    exactPC p
    printStringAtMaybeAnn (G GHC.AnnCloseP) ")"

  exactP (GHC.BangPat p) = do
    printStringAtMaybeAnn (G GHC.AnnBang) "!"
    exactPC p

  exactP (GHC.ListPat ps _ _) = do
    printStringAtMaybeAnn (G GHC.AnnOpenS) "["
    mapM_ exactPC ps
    printStringAtMaybeAnn (G GHC.AnnCloseS) "]"

  exactP (GHC.TuplePat pats b _) = do
    if b == GHC.Boxed then printStringAtMaybeAnn (G GHC.AnnOpenP) "("
                      else printStringAtMaybeAnn (G GHC.AnnOpen) "(#"
    mapM_ exactPC pats
    if b == GHC.Boxed then printStringAtMaybeAnn (G GHC.AnnCloseP) ")"
                      else printStringAtMaybeAnn (G GHC.AnnClose) "#)"

  exactP (GHC.PArrPat ps _) = do
    printStringAtMaybeAnn (G GHC.AnnOpen) "[:"
    mapM_ exactPC ps
    printStringAtMaybeAnn (G GHC.AnnClose) ":]"

  exactP (GHC.ConPatIn n dets) = do
    case dets of
      GHC.PrefixCon args -> do
        exactPC n
        mapM_ exactPC args
      GHC.RecCon (GHC.HsRecFields fs _) -> do
        exactPC n
        printStringAtMaybeAnn (G GHC.AnnOpenC) "{"
        mapM_ exactPC fs
        printStringAtMaybeAnn (G GHC.AnnDotdot) ".."
        printStringAtMaybeAnn (G GHC.AnnCloseC) "}"
      GHC.InfixCon a1 a2 -> do
        exactPC a1
        exactPC n
        exactPC a2

  exactP (GHC.ConPatOut {}) = return ()

  exactP (GHC.ViewPat e pat _) = do
    exactPC e
    printStringAtMaybeAnn (G GHC.AnnRarrow) "->"
    exactPC pat

  exactP (GHC.SplicePat (GHC.HsSplice _ e)) = do
    printStringAtMaybeAnn (G GHC.AnnOpen) "$("
    exactPC e
    printStringAtMaybeAnn (G GHC.AnnClose) ")"

  exactP (GHC.QuasiQuotePat (GHC.HsQuasiQuote n _ q)) = do
    printStringAtMaybeAnn (G GHC.AnnVal)
                    ("[" ++ (rdrName2String n) ++ "|" ++ (GHC.unpackFS q) ++ "|]")

  exactP (GHC.LitPat lp) = do
    printStringAtMaybeAnn (G GHC.AnnVal) (hsLit2String lp)

  exactP (GHC.NPat ol _ _)  = do
    printStringAtMaybeAnn (G GHC.AnnMinus) "-"
    exactPC ol

  exactP (GHC.NPlusKPat ln ol _ _) = do
    exactPC ln
    printStringAtMaybeAnn (G GHC.AnnVal) "+"
    exactPC ol

  exactP (GHC.SigPatIn pat (GHC.HsWB ty _ _ _)) = do
    exactPC pat
    printStringAtMaybeAnn (G GHC.AnnDcolon) "::"
    exactPC ty

  exactP (GHC.SigPatOut {}) = return ()
  exactP (GHC.CoPat {}) = return ()

-- ---------------------------------------------------------------------

instance ExactP (GHC.HsType GHC.Name) where
  exactP typ = do
    return () `debug` ("exactP.HsType not implemented for " ++ showGhc (typ))
    printString "HsType.Name"
    -- Note: This should never appear, only the one for GHC.RdrName
    assert False undefined

-- ---------------------------------------------------------------------

instance ExactP (GHC.HsType GHC.RdrName) where
  exactP (GHC.HsForAllTy _f mwc (GHC.HsQTvs _kvs tvs) ctx@(GHC.L lc ctxs) typ) = do
    printStringAtMaybeAnn (G GHC.AnnOpenP)   "("
    printStringAtMaybeAnn (G GHC.AnnForall) "forall"
    mapM_ exactPC tvs
    printStringAtMaybeAnn (G GHC.AnnDot)    "."

    case mwc of
      Nothing -> exactPC ctx
      Just lwc  -> exactPC (GHC.L lc (GHC.sortLocated ((GHC.L lwc GHC.HsWildcardTy):ctxs)))

    printStringAtMaybeAnn (G GHC.AnnDarrow) "=>"
    exactPC typ
    printStringAtMaybeAnn (G GHC.AnnCloseP)  ")"

  exactP (GHC.HsTyVar n) = do
    printStringAtMaybeAnn (G GHC.AnnDcolon) "::" -- for HsKind, aliased to HsType
    exactP n

  exactP (GHC.HsAppTy t1 t2) = do
    printStringAtMaybeAnn (G GHC.AnnDcolon) "::" -- for HsKind, aliased to HsType
    exactPC t1
    exactPC t2

  exactP (GHC.HsFunTy t1 t2) = do
    printStringAtMaybeAnn (G GHC.AnnDcolon) "::" -- for HsKind, aliased to HsType
    exactPC t1
    printStringAtMaybeAnn (G GHC.AnnRarrow) "->"
    exactPC t2

  exactP (GHC.HsListTy t) = do
    printStringAtMaybeAnn (G GHC.AnnDcolon) "::" -- for HsKind, aliased to HsType
    printStringAtMaybeAnn (G GHC.AnnOpenS)  "["
    exactPC t
    printStringAtMaybeAnn (G GHC.AnnCloseS) "]"

  exactP (GHC.HsPArrTy t) = do
    printStringAtMaybeAnn (G GHC.AnnOpen)  "[:"
    exactPC t
    printStringAtMaybeAnn (G GHC.AnnClose) ":]"

  exactP (GHC.HsTupleTy _tsort ts) = do
    printStringAtMaybeAnn (G GHC.AnnDcolon) "::" -- for HsKind, aliased to HsType
    printStringAtMaybeAnn (G GHC.AnnOpenP) "("
    printStringAtMaybeAnn (G GHC.AnnOpen)  "(#"
    mapM_ exactPC ts
    printStringAtMaybeAnn (G GHC.AnnCloseP) ")"
    printStringAtMaybeAnn (G GHC.AnnClose)  "#)"

  exactP (GHC.HsOpTy t1 (_,op) t2) = do
    exactPC t1
    exactPC op
    exactPC t2

  exactP (GHC.HsParTy t1) = do
    printStringAtMaybeAnn (G GHC.AnnDcolon) "::" -- for HsKind, aliased to HsType
    printStringAtMaybeAnn (G GHC.AnnOpenP)  "("
    exactPC t1
    printStringAtMaybeAnn (G GHC.AnnCloseP) ")"

  exactP (GHC.HsIParamTy (GHC.HsIPName n) t) = do
    printStringAtMaybeAnn (G GHC.AnnVal) ("?" ++ (GHC.unpackFS n))
    printStringAtMaybeAnn (G GHC.AnnDcolon) "::"
    exactPC t

  exactP (GHC.HsEqTy t1 t2) = do
    exactPC t1
    printStringAtMaybeAnn (G GHC.AnnTilde) "~"
    exactPC t2

  exactP (GHC.HsKindSig t k) = do
    printStringAtMaybeAnn (G GHC.AnnOpenP)  "("
    exactPC t
    printStringAtMaybeAnn (G GHC.AnnDcolon) "::"
    exactPC k
    printStringAtMaybeAnn (G GHC.AnnCloseP) ")"

  exactP (GHC.HsQuasiQuoteTy (GHC.HsQuasiQuote n _ss q)) = do
    printStringAtMaybeAnn (G GHC.AnnVal)
                    ("[" ++ (rdrName2String n) ++ "|" ++ (GHC.unpackFS q) ++ "|]")

  exactP (GHC.HsSpliceTy (GHC.HsSplice _is e) _) = do
    printStringAtMaybeAnn (G GHC.AnnOpen)  "$("
    exactPC e
    printStringAtMaybeAnn (G GHC.AnnClose) ")"

  exactP (GHC.HsDocTy t d) = do
    exactPC t
    exactPC d

  exactP (GHC.HsBangTy b t) = do
    case b of
      (GHC.HsSrcBang ms (Just True) _) -> do
        printStringAtMaybeAnn (G GHC.AnnOpen)  (maybe "{-# UNPACK" id ms)
        printStringAtMaybeAnn (G GHC.AnnClose) "#-}"
        printStringAtMaybeAnn (G GHC.AnnBang)  "!"
      (GHC.HsSrcBang ms (Just False) _) -> do
        printStringAtMaybeAnn (G GHC.AnnOpen)  (maybe "{-# NOUNPACK" id ms)
        printStringAtMaybeAnn (G GHC.AnnClose) "#-}"
        printStringAtMaybeAnn (G GHC.AnnBang)  "!"
      _ -> do
        printStringAtMaybeAnn (G GHC.AnnBang)  "!"
    exactPC t

  exactP (GHC.HsRecTy cons) = do
    printStringAtMaybeAnn (G GHC.AnnOpenC)  "{"
    mapM_ exactPC cons
    printStringAtMaybeAnn (G GHC.AnnCloseC) "}"

  exactP (GHC.HsCoreTy _t) = return ()

  exactP (GHC.HsExplicitListTy _ ts) = do
    printStringAtMaybeAnn (G GHC.AnnDcolon) "::" -- for HsKind, aliased to HsType
    printStringAtMaybeAnn (G GHC.AnnOpen)  "'["
    mapM_ exactPC ts
    printStringAtMaybeAnn (G GHC.AnnCloseS) "]"

  exactP (GHC.HsExplicitTupleTy _ ts) = do
    printStringAtMaybeAnn (G GHC.AnnDcolon) "::" -- for HsKind, aliased to HsType
    printStringAtMaybeAnn (G GHC.AnnOpen)  "'("
    mapM_ exactPC ts
    printStringAtMaybeAnn (G GHC.AnnClose) ")"

  exactP (GHC.HsTyLit lit) = do
    printStringAtMaybeAnn (G GHC.AnnDcolon) "::" -- for HsKind, aliased to HsType
    case lit of
      (GHC.HsNumTy s _) -> printStringAtMaybeAnn (G GHC.AnnVal) s
      (GHC.HsStrTy s _) -> printStringAtMaybeAnn (G GHC.AnnVal) s

  exactP (GHC.HsWrapTy _ _) = return ()

  exactP GHC.HsWildcardTy = do
    printStringAtMaybeAnn (G GHC.AnnVal) "_"
    printStringAtMaybeAnn (G GHC.AnnDarrow) "=>" -- if only part of a partial type signature context

  exactP (GHC.HsNamedWildcardTy n) = do
    printStringAtMaybeAnn (G GHC.AnnVal) (rdrName2String n)

-- ---------------------------------------------------------------------

instance ExactP GHC.HsDocString where
  exactP (GHC.HsDocString s) = do
    printStringAtMaybeAnn (G GHC.AnnVal) (GHC.unpackFS s)

instance ExactP (GHC.ConDeclField GHC.RdrName) where
  exactP (GHC.ConDeclField ns ty mdoc) = do
    mapM_ exactPC ns
    printStringAtMaybeAnn (G GHC.AnnDcolon) "::"
    exactPC ty
    case mdoc of
      Just doc -> exactPC doc
      Nothing -> return ()

instance ExactP (GHC.HsContext GHC.RdrName) where
  exactP typs = do
    -- printStringAtMaybeAnn (G GHC.AnnUnit "()"

    printStringAtMaybeAnn (G GHC.AnnDeriving) "deriving"
    printStringAtMaybeAnn (G GHC.AnnOpenP) "("
    mapM_ exactPC typs
    -- printStringAtMaybeAnn (G GHC.AnnUnit "()"
    printStringAtMaybeAnn (G GHC.AnnCloseP) ")"
    printStringAtMaybeAnn (G GHC.AnnDarrow) "=>"

instance (ExactP body) => ExactP (GHC.GRHS GHC.RdrName (GHC.Located body)) where
  exactP (GHC.GRHS guards expr) = do
    printStringAtMaybeAnn (G GHC.AnnVbar) "|"
    mapM_ exactPC guards
    printStringAtMaybeAnn (G GHC.AnnEqual) "="
    printStringAtMaybeAnn (G GHC.AnnRarrow) "->" -- in a case
    exactPC expr

instance (ExactP body)
  => ExactP (GHC.Stmt GHC.RdrName (GHC.Located body)) where

  exactP (GHC.LastStmt body _) = exactPC body
    `debug` ("exactP.LastStmt")

  exactP (GHC.BindStmt pat body _ _) = do
    exactPC pat
    printStringAtMaybeAnn (G GHC.AnnLarrow) "<-"
    exactPC body
    printStringAtMaybeAnn (G GHC.AnnVbar)  "|" -- possible in list comprehension

  exactP (GHC.BodyStmt e _ _ _) = do
    exactPC e

  exactP (GHC.LetStmt lb) = do
    printStringAtMaybeAnn (G GHC.AnnLet) "let"
    printStringAtMaybeAnn (G GHC.AnnOpenC) "{"
    -- pushNestedOffset
    -- exactP lb
    exactPC (GHC.L (getLocalBindsSrcSpan lb) lb)
    -- popOffset
    -- popSrcSpan
    printStringAtMaybeAnn (G GHC.AnnCloseC) "}"

  exactP (GHC.ParStmt pbs _ _) = do
    mapM_ exactPParStmtBlock pbs

  exactP (GHC.TransStmt form stmts _b using by _ _ _) = do
    mapM_ exactPC stmts
    case form of
      GHC.ThenForm -> do
        printStringAtMaybeAnn (G GHC.AnnThen) "then"
        exactPC using
        printStringAtMaybeAnn (G GHC.AnnBy) "by"
        case by of
          Just b -> exactPC b
          Nothing -> return ()
      GHC.GroupForm -> do
        printStringAtMaybeAnn (G GHC.AnnThen)  "then"
        printStringAtMaybeAnn (G GHC.AnnGroup) "group"
        printStringAtMaybeAnn (G GHC.AnnBy)    "by"
        case by of
          Just b -> exactPC b
          Nothing -> return ()
        printStringAtMaybeAnn (G GHC.AnnUsing) "using"
        exactPC using

  exactP (GHC.RecStmt stmts _ _ _ _ _ _ _ _) = do
    printStringAtMaybeAnn (G GHC.AnnRec) "rec"
    printStringAtMaybeAnn (G GHC.AnnOpenC) "{"
    printStringAtMaybeAnnAll (G GHC.AnnSemi) ";"
    mapM_ exactPC stmts
    printStringAtMaybeAnn (G GHC.AnnCloseC) "}"

-- ---------------------------------------------------------------------

exactPParStmtBlock :: GHC.ParStmtBlock GHC.RdrName GHC.RdrName -> EP ()
exactPParStmtBlock (GHC.ParStmtBlock stmts _ns _) = do
  mapM_ exactPC stmts

-- ---------------------------------------------------------------------

instance ExactP (GHC.HsExpr GHC.RdrName) where
  exactP (GHC.HsVar v)  = exactP v
  exactP (GHC.HsIPVar (GHC.HsIPName v)) = do
    printStringAtMaybeAnn (G GHC.AnnVal) ("?" ++ GHC.unpackFS v)
  exactP (GHC.HsOverLit lit)     = exactP lit
  exactP (GHC.HsLit lit)         = exactP lit
  exactP (GHC.HsLam match)       = do
    printStringAtMaybeAnn (G GHC.AnnLam) "\\"
    exactPMatchGroup match
  exactP (GHC.HsLamCase _ match) = exactPMatchGroup match
  exactP (GHC.HsApp e1 e2)       = exactPC e1 >> exactPC e2
  exactP (GHC.OpApp e1 op _f e2) = exactPC e1 >> exactPC op >> exactPC e2

  exactP (GHC.NegApp e _)        = do
    printStringAtMaybeAnn (G GHC.AnnMinus) "-"
    exactPC e

  exactP (GHC.HsPar e) = do
    printStringAtMaybeAnn (G GHC.AnnOpenP)  "("
    exactPC e
    printStringAtMaybeAnn (G GHC.AnnCloseP) ")"

  exactP (GHC.SectionL e1 e2)    = exactPC e1 >> exactPC e2
  exactP (GHC.SectionR e1 e2)    = exactPC e1 >> exactPC e2

  exactP (GHC.ExplicitTuple args b) = do
    if b == GHC.Boxed then printStringAtMaybeAnn (G GHC.AnnOpenP) "("
                      else printStringAtMaybeAnn (G GHC.AnnOpen) "(#"

    mapM_ exactPC args `debug` ("exactP.ExplicitTuple")

    if b == GHC.Boxed then printStringAtMaybeAnn (G GHC.AnnCloseP) ")"
                      else printStringAtMaybeAnn (G GHC.AnnClose) "#)"

  exactP (GHC.HsCase e1 matches) = do
    printStringAtMaybeAnn (G GHC.AnnCase) "case"
    exactPC e1
    printStringAtMaybeAnn (G GHC.AnnOf) "of"
    printStringAtMaybeAnn (G GHC.AnnOpenC) "{"
    printStringAtMaybeAnnAll (G GHC.AnnSemi) ";"
    exactPMatchGroup matches
    printStringAtMaybeAnn (G GHC.AnnCloseC) "}"

  exactP (GHC.HsIf _ e1 e2 e3)   = do
    printStringAtMaybeAnn (G GHC.AnnIf) "if"
    exactPC e1
    printStringAtMaybeAnn (G GHC.AnnSemi) ";"
    printStringAtMaybeAnn (G GHC.AnnThen) "then"
    exactPC e2
    printStringAtMaybeAnn (G GHC.AnnSemi) ";"
    printStringAtMaybeAnn (G GHC.AnnElse) "else"
    exactPC e3

  exactP (GHC.HsMultiIf _ rhs)   = do
    printStringAtMaybeAnn (G GHC.AnnIf) "if"
    mapM_ exactPC rhs

  exactP (GHC.HsLet lb e)    = do
    return () `debug` ("exactP.HsLet started")
    printStringAtMaybeAnn (G GHC.AnnLet) "let"
    printStringAtMaybeAnn (G GHC.AnnOpenC) "{"
    printStringAtMaybeAnnAll (G GHC.AnnSemi) ";"
    -- pushNestedOffset
    exactPC (GHC.L (getLocalBindsSrcSpan lb) lb)
    return () `debug` ("exactP.HsLet lb done")
    -- popOffset
    -- popSrcSpan
    printStringAtMaybeAnn (G GHC.AnnCloseC) "}"
    printStringAtMaybeAnn (G GHC.AnnIn) "in"
    exactPC e
    return () `debug` ("exactP.HsLet ended")

  exactP (GHC.HsDo cts stmts _typ)    = do
    printStringAtMaybeAnn (G GHC.AnnDo) "do"
    let (ostr,cstr,isComp) =
          if isListComp cts
            then case cts of
                   GHC.PArrComp -> ("[:",":]",True)
                   _            -> ("[",  "]",True)
            else ("{","}",False)

    printStringAtMaybeAnn (G GHC.AnnOpenS) "["
    printStringAtMaybeAnn (G GHC.AnnOpen) ostr
    printStringAtMaybeAnn (G GHC.AnnOpenC) "{"
    printStringAtMaybeAnnAll (G GHC.AnnSemi) ";"
    if isComp
      then do
        exactPC(last stmts)
        printStringAtMaybeAnn (G GHC.AnnVbar) "|"
        mapM_ exactPC (init stmts)
      else do
        mapM_ exactPC stmts
    printStringAtMaybeAnn (G GHC.AnnCloseS) "]"
    printStringAtMaybeAnn (G GHC.AnnClose) cstr
    printStringAtMaybeAnn (G GHC.AnnCloseC) "}"

  exactP (GHC.ExplicitList _ _ es) = do
    printStringAtMaybeAnn (G GHC.AnnOpenS) "["
    mapM_ exactPC es
    printStringAtMaybeAnn (G GHC.AnnCloseS) "]"

  exactP (GHC.ExplicitPArr _ es)   = do
    printStringAtMaybeAnn (G GHC.AnnOpen) "[:"
    mapM_ exactPC es
    printStringAtMaybeAnn (G GHC.AnnClose) ":]"

  exactP (GHC.RecordCon n _ (GHC.HsRecFields fs _)) = do
    exactPC n
    printStringAtMaybeAnn (G GHC.AnnOpenC) "{"
    printStringAtMaybeAnn (G GHC.AnnDotdot) ".."
    mapM_ exactPC fs
    printStringAtMaybeAnn (G GHC.AnnCloseC) "}"

  -- TODO: AZ: cons are not processed
  exactP (GHC.RecordUpd e (GHC.HsRecFields fs _) cons _ _)  = do
    exactPC e
    printStringAtMaybeAnn (G GHC.AnnOpenC) "{"
    printStringAtMaybeAnn (G GHC.AnnDotdot) ".."
    mapM_ exactPC fs
    printStringAtMaybeAnn (G GHC.AnnCloseC) "}"

  exactP (GHC.ExprWithTySig e typ _) = do
    exactPC e
    printStringAtMaybeAnn (G GHC.AnnDcolon) "::"
    exactPC typ

  exactP (GHC.ExprWithTySigOut e typ) = do
    exactPC e
    printStringAtMaybeAnn (G GHC.AnnDcolon) "::"
    exactPC typ

  exactP (GHC.ArithSeq _ _ seqInfo) = do
    printStringAtMaybeAnn (G GHC.AnnOpenS) "["
    case seqInfo of
      GHC.From e1 -> exactPC e1 >> printStringAtMaybeAnn (G GHC.AnnDotdot) ".."
      GHC.FromTo e1 e2 -> do
        exactPC e1
        printStringAtMaybeAnn (G GHC.AnnDotdot) ".."
        exactPC e2
      GHC.FromThen e1 e2 -> do
        exactPC e1
        printStringAtMaybeAnn (G GHC.AnnComma) ","
        exactPC e2
        printStringAtMaybeAnn (G GHC.AnnDotdot) ".."
      GHC.FromThenTo e1 e2 e3 -> do
        exactPC e1
        printStringAtMaybeAnn (G GHC.AnnComma) ","
        exactPC e2
        printStringAtMaybeAnn (G GHC.AnnDotdot) ".."
        exactPC e3
    printStringAtMaybeAnn (G GHC.AnnCloseS) "]"

  exactP (GHC.PArrSeq _ seqInfo) = do
    printStringAtMaybeAnn (G GHC.AnnOpen) "[:"
    case seqInfo of
      GHC.From e1 -> exactPC e1 >> printStringAtMaybeAnn (G GHC.AnnDotdot) ".."
      GHC.FromTo e1 e2 -> do
        exactPC e1
        printStringAtMaybeAnn (G GHC.AnnDotdot) ".."
        exactPC e2
      GHC.FromThen e1 e2 -> do
        exactPC e1
        printStringAtMaybeAnn (G GHC.AnnComma) ","
        exactPC e2
        printStringAtMaybeAnn (G GHC.AnnDotdot) ".."
      GHC.FromThenTo e1 e2 e3 -> do
        exactPC e1
        printStringAtMaybeAnn (G GHC.AnnComma) ","
        exactPC e2
        printStringAtMaybeAnn (G GHC.AnnDotdot) ".."
        exactPC e3
    printStringAtMaybeAnn (G GHC.AnnClose) ":]"

  exactP (GHC.HsSCC src str e) = do
    printStringAtMaybeAnn (G GHC.AnnOpen) src -- "{-# SCC"
    printStringAtMaybeAnn (G GHC.AnnVal) (GHC.unpackFS str)
    printStringAtMaybeAnn (G GHC.AnnValStr) ("\"" ++ GHC.unpackFS str ++ "\"")
    printStringAtMaybeAnn (G GHC.AnnClose) "#-}"
    exactPC e

  exactP (GHC.HsCoreAnn src str e) = do
    printStringAtMaybeAnn (G GHC.AnnOpen) src -- "{-# CORE"
    printStringAtMaybeAnn (G GHC.AnnVal) (GHC.unpackFS str)
    printStringAtMaybeAnn (G GHC.AnnClose) "#-}"
    exactPC e

  exactP (GHC.HsBracket (GHC.VarBr single v)) = do
    if single then printStringAtMaybeAnn (G GHC.AnnVal) ("'"  ++ rdrName2String v)
              else printStringAtMaybeAnn (G GHC.AnnVal) ("''" ++ rdrName2String v)
  exactP (GHC.HsBracket (GHC.DecBrL ds)) = do
    cnt <- countAnns (G GHC.AnnOpen)
    case cnt of
      1 -> do
        printStringAtMaybeAnn (G GHC.AnnOpen)  "[d|"
        mapM_ exactPC ds
        printStringAtMaybeAnn (G GHC.AnnClose) "|]"
      _ -> do
        printStringAtMaybeAnn (G GHC.AnnOpen)  "[d|"
        printStringAtMaybeAnn (G GHC.AnnOpenC) "{"
        mapM_ exactPC ds
        printStringAtMaybeAnn (G GHC.AnnCloseC) "}"
        printStringAtMaybeAnn (G GHC.AnnClose) "|]"
  exactP (GHC.HsBracket (GHC.ExpBr e)) = do
        printStringAtMaybeAnn (G GHC.AnnOpen)  "[|"
        exactPC e
        printStringAtMaybeAnn (G GHC.AnnClose) "|]"
  exactP (GHC.HsBracket (GHC.TExpBr e)) = do
        printStringAtMaybeAnn (G GHC.AnnOpen)  "[||"
        exactPC e
        printStringAtMaybeAnn (G GHC.AnnClose) "||]"
  exactP (GHC.HsBracket (GHC.TypBr e)) = do
        printStringAtMaybeAnn (G GHC.AnnOpen)  "[t|"
        exactPC e
        printStringAtMaybeAnn (G GHC.AnnClose) "|]"
  exactP (GHC.HsBracket (GHC.PatBr e)) = do
        printStringAtMaybeAnn (G GHC.AnnOpen)  "[p|"
        exactPC e
        printStringAtMaybeAnn (G GHC.AnnClose) "|]"

  exactP (GHC.HsRnBracketOut _ _) = return ()
  exactP (GHC.HsTcBracketOut _ _) = return ()

  exactP (GHC.HsSpliceE False (GHC.HsSplice _ e)) = do
    printStringAtMaybeAnn (G GHC.AnnOpen) "$("
    exactPC e
    printStringAtMaybeAnn (G GHC.AnnClose) ")"
  exactP (GHC.HsSpliceE True (GHC.HsSplice _ e)) = do
    printStringAtMaybeAnn (G GHC.AnnOpen) "$$("
    exactPC e
    printStringAtMaybeAnn (G GHC.AnnClose) ")"

  exactP (GHC.HsQuasiQuoteE (GHC.HsQuasiQuote n _ str)) = do
    printStringAtMaybeAnn (G GHC.AnnVal)
          ("[" ++ (rdrName2String n) ++ "|" ++ (GHC.unpackFS str) ++ "|]")

  exactP (GHC.HsProc p c) = do
    printStringAtMaybeAnn (G GHC.AnnProc) "proc"
    exactPC p
    printStringAtMaybeAnn (G GHC.AnnRarrow) "->"
    exactPC c

  exactP (GHC.HsStatic e) = do
    printStringAtMaybeAnn (G GHC.AnnStatic) "static"
    exactPC e

  exactP (GHC.HsArrApp e1 e2 _ _ _) = do
    exactPC e1
    -- only one of the next 4 will be resent
    printStringAtMaybeAnn (G GHC.Annlarrowtail) "-<"
    printStringAtMaybeAnn (G GHC.Annrarrowtail) ">-"
    printStringAtMaybeAnn (G GHC.AnnLarrowtail) "-<<"
    printStringAtMaybeAnn (G GHC.AnnRarrowtail) ">>-"

    exactPC e2

  exactP (GHC.HsArrForm e _ cs) = do
    printStringAtMaybeAnn (G GHC.AnnOpen) "(|"
    exactPC e
    mapM_ exactPC cs
    printStringAtMaybeAnn (G GHC.AnnClose)  "|)"

  exactP (GHC.HsTick _ _) = return ()
  exactP (GHC.HsBinTick _ _ _) = return ()

  exactP (GHC.HsTickPragma src (str,(v1,v2),(v3,v4)) e) = do
    -- '{-# GENERATED' STRING INTEGER ':' INTEGER '-' INTEGER ':' INTEGER '#-}'
    printStringAtMaybeAnn (G GHC.AnnOpen)  src -- "{-# GENERATED"
    printStringAtMaybeAnn (G GHC.AnnVal)   (show $ GHC.unpackFS str)
    printStringAtMaybeAnn (G GHC.AnnVal)   (show v1)
    printStringAtMaybeAnn (G GHC.AnnColon) ":"
    printStringAtMaybeAnn (G GHC.AnnVal)   (show v2)
    printStringAtMaybeAnn (G GHC.AnnMinus) "-"
    printStringAtMaybeAnn (G GHC.AnnVal)   (show v3)
    printStringAtMaybeAnn (G GHC.AnnColon) ":"
    printStringAtMaybeAnn (G GHC.AnnVal)   (show v4)
    printStringAtMaybeAnn (G GHC.AnnClose) "#-}"
    exactPC e

  exactP (GHC.EWildPat) = printStringAtMaybeAnn (G GHC.AnnVal) "_"

  exactP (GHC.EAsPat n e) = do
    exactPC n
    printStringAtMaybeAnn (G GHC.AnnAt) "@"
    exactPC e

  exactP (GHC.EViewPat e1 e2) = do
    exactPC e1
    printStringAtMaybeAnn (G GHC.AnnRarrow) "->"
    exactPC e2

  exactP (GHC.ELazyPat e) = do
    printStringAtMaybeAnn (G GHC.AnnTilde) "~"
    exactPC e

  exactP (GHC.HsType ty) = exactPC ty
  exactP (GHC.HsWrap _ _) = return ()
  exactP (GHC.HsUnboundVar _) = return ()

  exactP e = printString "HsExpr"
    `debug` ("exactP.HsExpr:not processing " ++ (showGhc e) )

-- ---------------------------------------------------------------------

instance (ExactP arg) => ExactP (GHC.HsRecField GHC.RdrName (GHC.Located arg)) where
  exactP (GHC.HsRecField n e _) = do
    exactPC n
    printStringAtMaybeAnn (G GHC.AnnEqual) "="
    exactPC e

-- ---------------------------------------------------------------------

instance ExactP (GHC.HsCmdTop GHC.RdrName) where
  exactP (GHC.HsCmdTop cmd _ _ _) = exactPC cmd

-- ---------------------------------------------------------------------

instance ExactP (GHC.HsCmd GHC.RdrName) where
  exactP (GHC.HsCmdArrApp e1 e2 _ _ _) = do
    exactPC e1
    -- only one of the next 4 will be resent
    printStringAtMaybeAnn (G GHC.Annlarrowtail) "-<"
    printStringAtMaybeAnn (G GHC.Annrarrowtail) ">-"
    printStringAtMaybeAnn (G GHC.AnnLarrowtail) "-<<"
    printStringAtMaybeAnn (G GHC.AnnRarrowtail) ">>-"

    exactPC e2

  exactP (GHC.HsCmdArrForm e _ cs) = do
    printStringAtMaybeAnn (G GHC.AnnOpen) "(|"
    exactPC e
    mapM_ exactPC cs
    printStringAtMaybeAnn (G GHC.AnnClose)  "|)"

  exactP (GHC.HsCmdApp e1 e2) = exactPC e1 >> exactPC e2

  exactP (GHC.HsCmdLam match) = do
    printStringAtMaybeAnn (G GHC.AnnLam) "\\"
    exactPMatchGroup match

  exactP (GHC.HsCmdPar e) = do
    printStringAtMaybeAnn (G GHC.AnnOpenP)  "("
    exactPC e
    printStringAtMaybeAnn (G GHC.AnnCloseP) ")"

  exactP (GHC.HsCmdCase e1 matches) = do
    printStringAtMaybeAnn (G GHC.AnnCase) "case"
    exactPC e1
    printStringAtMaybeAnn (G GHC.AnnOf) "of"
    printStringAtMaybeAnn (G GHC.AnnOpenC) "{"
    exactPMatchGroup matches
    printStringAtMaybeAnn (G GHC.AnnCloseC) "}"

  exactP (GHC.HsCmdIf _ e1 e2 e3)   = do
    printStringAtMaybeAnn (G GHC.AnnIf) "if"
    exactPC e1
    printStringAtMaybeAnn (G GHC.AnnSemi) ";"
    printStringAtMaybeAnn (G GHC.AnnThen) "then"
    exactPC e2
    printStringAtMaybeAnn (G GHC.AnnSemi) ";"
    printStringAtMaybeAnn (G GHC.AnnElse) "else"
    exactPC e3

  exactP (GHC.HsCmdLet lb e)    = do
    printStringAtMaybeAnn (G GHC.AnnLet) "let"
    printStringAtMaybeAnn (G GHC.AnnOpenC) "{"
    -- exactP lb
    exactPC (GHC.L (getLocalBindsSrcSpan lb) lb)
    printStringAtMaybeAnn (G GHC.AnnCloseC) "}"
    printStringAtMaybeAnn (G GHC.AnnIn) "in"
    exactPC e

  exactP (GHC.HsCmdDo stmts _typ)    = do
    printStringAtMaybeAnn (G GHC.AnnDo) "do"
    printStringAtMaybeAnn (G GHC.AnnOpenC) "{"
    mapM_ exactPC stmts
    printStringAtMaybeAnn (G GHC.AnnCloseC) "}"

  exactP (GHC.HsCmdCast {}) = error $ "exactP.HsCmdCast: only valid after type checker"

-- ---------------------------------------------------------------------

instance ExactP GHC.RdrName where
  exactP n = do
    case rdrName2String n of
      "[]" -> do
        printStringAtMaybeAnn (G GHC.AnnOpenS) "["
        printStringAtMaybeAnn (G GHC.AnnCloseS) "]"
      "()" -> do
        printStringAtMaybeAnn (G GHC.AnnOpenP) "("
        printStringAtMaybeAnn (G GHC.AnnCloseP) ")"
      "(##)" -> do
        printStringAtMaybeAnn (G GHC.AnnOpen) "(#"
        printStringAtMaybeAnn (G GHC.AnnClose) "#)"
      "[::]" -> do
        printStringAtMaybeAnn (G GHC.AnnOpen) "[:"
        printStringAtMaybeAnn (G GHC.AnnClose) ":]"
      str ->  do
        printStringAtMaybeAnn (G GHC.AnnType)      "type"
        printStringAtMaybeAnn (G GHC.AnnOpenP)     "("
        printStringAtMaybeAnn (G GHC.AnnBackquote)  "`"
        printStringAtMaybeAnn (G GHC.AnnTildehsh)  "~#"
        printStringAtMaybeAnn (G GHC.AnnTilde)     "~"
        printStringAtMaybeAnn (G GHC.AnnRarrow)    "->"
        printStringAtMaybeAnn (G GHC.AnnVal)       str
        printStringAtMaybeAnn (G GHC.AnnBackquote) "`"
        printStringAtMaybeAnnAll (G GHC.AnnCommaTuple) "," -- For '(,,,)'
        printStringAtMaybeAnn (G GHC.AnnCloseP)    ")"
        return () `debug` ("exactP.RdrName:n=" ++ str)

instance ExactP GHC.HsIPName where
  exactP (GHC.HsIPName n) = do
    printStringAtMaybeAnn (G GHC.AnnVal) ("?" ++ GHC.unpackFS n)

-- ---------------------------------------------------------------------

exactPMatchGroup :: (ExactP body) => (GHC.MatchGroup GHC.RdrName (GHC.Located body))
                   -> EP ()
exactPMatchGroup (GHC.MG matches _ _ _)
  = mapM_ exactPC matches

-- ---------------------------------------------------------------------

instance ExactP (GHC.HsTupArg GHC.RdrName) where
  exactP (GHC.Missing _) = do
    printStringAtMaybeAnn (G GHC.AnnComma) ","
    return ()
  exactP (GHC.Present e) = do
    exactPC e
    printStringAtMaybeAnn (G GHC.AnnComma) ","

instance ExactP (GHC.HsLocalBinds GHC.RdrName) where
  exactP (GHC.HsValBinds (GHC.ValBindsIn binds sigs)) = do
    printMerged (GHC.bagToList binds) sigs
  exactP (GHC.HsValBinds (GHC.ValBindsOut _binds _sigs)) = printString "ValBindsOut"
  exactP (GHC.HsIPBinds (GHC.IPBinds binds _)) = mapM_ exactPC binds
  exactP (GHC.EmptyLocalBinds) = return ()

-- ---------------------------------------------------------------------

instance ExactP (GHC.Sig GHC.RdrName) where
  exactP (GHC.TypeSig lns typ _) = do
    mapM_ exactPC lns
    printStringAtMaybeAnn (G GHC.AnnDcolon) "::"
    exactPC typ

  exactP (GHC.PatSynSig n (_,GHC.HsQTvs _ns bndrs) ctx1 ctx2 typ) = do
    printStringAtMaybeAnn (G GHC.AnnPattern) "pattern"
    exactPC n
    printStringAtMaybeAnn (G GHC.AnnDcolon) "::"

    -- Note: The 'forall' bndrs '.' may occur multiple times
    printStringAtMaybeAnn (G GHC.AnnForall) "forall"
    mapM_ exactPC bndrs
    printStringAtMaybeAnn (G GHC.AnnDot) "."

    exactPC ctx1
    printStringAtMaybeAnn (G GHC.AnnDarrow) "=>"
    exactPC ctx2
    printStringAtMaybeAnn (G GHC.AnnDarrow) "=>"
    exactPC typ

  exactP (GHC.GenericSig ns typ) = do
    printStringAtMaybeAnn (G GHC.AnnDefault) "default"
    mapM_ exactPC ns
    printStringAtMaybeAnn (G GHC.AnnDcolon) "::"
    exactPC typ

  exactP (GHC.IdSig _) = return ()

  exactP (GHC.FixSig (GHC.FixitySig lns (GHC.Fixity v fdir))) = do
    let fixstr = case fdir of
         GHC.InfixL -> "infixl"
         GHC.InfixR -> "infixr"
         GHC.InfixN -> "infix"
    printStringAtMaybeAnn (G GHC.AnnInfix) fixstr
    printStringAtMaybeAnn (G GHC.AnnVal) (show v)
    mapM_ exactPC lns

  exactP (GHC.InlineSig n inl) = do
    let actStr = case GHC.inl_act inl of
          GHC.NeverActive -> ""
          GHC.AlwaysActive -> ""
          GHC.ActiveBefore np -> show np
          GHC.ActiveAfter  np -> show np
    printStringAtMaybeAnn (G GHC.AnnOpen) (GHC.inl_src inl) -- "{-# INLINE"
    printStringAtMaybeAnn (G GHC.AnnOpenS)  "["
    printStringAtMaybeAnn (G GHC.AnnTilde) "~"
    printStringAtMaybeAnn (G GHC.AnnVal)   actStr
    printStringAtMaybeAnn (G GHC.AnnCloseS) "]"
    exactPC n
    printStringAtMaybeAnn (G GHC.AnnClose) "#-}"


  exactP (GHC.SpecSig n typs inl) = do
    printStringAtMaybeAnn (G GHC.AnnOpen)  (GHC.inl_src inl) -- "{-# SPECIALISE"
    printStringAtMaybeAnn (G GHC.AnnOpenS)  "["
    printStringAtMaybeAnn (G GHC.AnnTilde)  "~"
    printStringAtMaybeAnn (G GHC.AnnVal)   "TODO:what here?" -- e.g. 34
    printStringAtMaybeAnn (G GHC.AnnCloseS) "]"
    exactPC n
    printStringAtMaybeAnn (G GHC.AnnDcolon) "::"
    mapM_ exactPC typs
    printStringAtMaybeAnn (G GHC.AnnClose) "#-}"


  exactP _ = printString "Sig"

-- ---------------------------------------------------------------------

instance ExactP (GHC.HsOverLit GHC.RdrName) where
  exactP ol = do
    case GHC.ol_val ol of
      GHC.HsIntegral src _ -> printStringAtMaybeAnn (G GHC.AnnVal) src
      GHC.HsFractional l   -> printStringAtMaybeAnn (G GHC.AnnVal) (GHC.fl_text l)
      GHC.HsIsString src _ -> printStringAtMaybeAnn (G GHC.AnnVal) src

-- ---------------------------------------------------------------------

instance ExactP GHC.HsLit where
  exactP lit = printStringAtMaybeAnn (G GHC.AnnVal) (hsLit2String lit)

hsLit2String :: GHC.HsLit -> GHC.SourceText
hsLit2String lit =
  case lit of
    GHC.HsChar       src _   -> src
    GHC.HsCharPrim   src _   -> src
    GHC.HsString     src _   -> src
    GHC.HsStringPrim src _   -> src
    GHC.HsInt        src _   -> src
    GHC.HsIntPrim    src _   -> src
    GHC.HsWordPrim   src _   -> src
    GHC.HsInt64Prim  src _   -> src
    GHC.HsWord64Prim src _   -> src
    GHC.HsInteger    src _ _ -> src
    GHC.HsRat        (GHC.FL src _) _ -> src
    GHC.HsFloatPrim  (GHC.FL src _)   -> src
    GHC.HsDoublePrim (GHC.FL src _)   -> src

-- ---------------------------------------------------------------------


instance ExactP (GHC.TyClDecl GHC.RdrName) where
  exactP (GHC.FamDecl famdecl) = exactP famdecl

  exactP (GHC.SynDecl ln (GHC.HsQTvs _ tyvars) typ _) = do
    printStringAtMaybeAnn (G GHC.AnnType) "type"
    exactPC ln
    mapM_ exactPC tyvars
    printStringAtMaybeAnn (G GHC.AnnEqual) "="
    exactPC typ

  exactP (GHC.DataDecl ln (GHC.HsQTvs _ns tyVars)
           (GHC.HsDataDefn _nOrD ctx mctyp mkind cons mderivs) _) = do
    printStringAtMaybeAnn (G GHC.AnnData)    "data"
    printStringAtMaybeAnn (G GHC.AnnNewtype) "newtype"
    doMaybe exactPC mctyp
    exactPC ctx
    printStringAtMaybeAnn (G GHC.AnnDarrow) "=>"
    printTyClass ln tyVars
    printStringAtMaybeAnn (G GHC.AnnDcolon) "::"
    doMaybe exactPC mkind
    printStringAtMaybeAnn (G GHC.AnnEqual) "="
    printStringAtMaybeAnn (G GHC.AnnWhere) "where"
    mapM_ exactPC cons
    doMaybe exactPC mderivs

  exactP (GHC.ClassDecl ctx ln (GHC.HsQTvs _ns tyVars) fds
                          sigs meths ats atdefs docs _) = do
    printStringAtMaybeAnn (G GHC.AnnClass) "class"
    exactPC ctx
    printTyClass ln tyVars
    printStringAtMaybeAnn (G GHC.AnnVbar) "|"
    mapM_ exactPC fds
    printStringAtMaybeAnn (G GHC.AnnWhere) "where"
    printStringAtMaybeAnn (G GHC.AnnOpenC)  "{"
    printStringAtMaybeAnnAll (G GHC.AnnSemi) ";"

    applyListPrint (prepareListPrint sigs
                 ++ prepareListPrint (GHC.bagToList meths)
                 ++ prepareListPrint ats
                 ++ prepareListPrint atdefs
                 ++ prepareListPrint docs
                    )

    printStringAtMaybeAnn (G GHC.AnnCloseC) "}"

-- ---------------------------------------------------------------------

printTyClass :: (ExactP ast, ExactP ast1)
             => GHC.Located ast -> [GHC.Located ast1] -> EP ()
printTyClass ln tyVars = do
    printStringAtMaybeAnnAll (G GHC.AnnOpenP) "("
    applyListPrint (prepareListPrint [ln]
               ++ prepareListPrint (take 2 tyVars))
    -- exactPC ln
    printStringAtMaybeAnnAll (G GHC.AnnCloseP) ")"
    mapM_ exactPC (drop 2 tyVars)

-- ---------------------------------------------------------------------

instance ExactP (GHC.FamilyDecl GHC.RdrName) where
  exactP (GHC.FamilyDecl info ln (GHC.HsQTvs _ tyvars) mkind) = do
    printStringAtMaybeAnn (G GHC.AnnType)   "type"
    printStringAtMaybeAnn (G GHC.AnnData)   "data"
    printStringAtMaybeAnn (G GHC.AnnFamily) "family"
    exactPC ln
    mapM_ exactPC tyvars
    case mkind of
      Nothing -> return ()
      Just k -> exactPC k
    printStringAtMaybeAnn (G GHC.AnnWhere) "where"
    printStringAtMaybeAnn (G GHC.AnnOpenC) "{"
    case info of
      GHC.ClosedTypeFamily eqns -> mapM_ exactPC eqns
      _ -> return ()
    printStringAtMaybeAnn (G GHC.AnnCloseC) "}"

-- ---------------------------------------------------------------------

instance ExactP (GHC.TyFamDefltEqn GHC.RdrName) where
   exactP = assert False undefined

-- ---------------------------------------------------------------------

instance ExactP (GHC.TyFamInstEqn GHC.RdrName) where
  exactP (GHC.TyFamEqn ln (GHC.HsWB pats _ _ _) typ) = do
    exactPC ln
    mapM_ exactPC pats
    printStringAtMaybeAnn (G GHC.AnnEqual) "="
    exactPC typ

-- ---------------------------------------------------------------------

instance ExactP GHC.DocDecl where
  exactP (GHC.DocCommentNext (GHC.HsDocString fs))
    = printStringAtMaybeAnn (G GHC.AnnVal) (GHC.unpackFS fs)
  exactP (GHC.DocCommentPrev (GHC.HsDocString fs))
    = printStringAtMaybeAnn (G GHC.AnnVal) (GHC.unpackFS fs)
  exactP (GHC.DocCommentNamed _s (GHC.HsDocString fs))
    = printStringAtMaybeAnn (G GHC.AnnVal) (GHC.unpackFS fs)
  exactP (GHC.DocGroup _i (GHC.HsDocString fs))
    = printStringAtMaybeAnn (G GHC.AnnVal) (GHC.unpackFS fs)

-- ---------------------------------------------------------------------

instance ExactP (GHC.FunDep (GHC.Located GHC.RdrName)) where
  exactP (ls,rs) = do
    mapM_ exactPC ls
    printStringAtMaybeAnn (G GHC.AnnRarrow) "->"
    mapM_ exactPC rs

-- ---------------------------------------------------------------------

instance ExactP (GHC.HsTyVarBndr GHC.RdrName) where
  exactP (GHC.UserTyVar n) = printStringAtMaybeAnn (G GHC.AnnVal) (rdrName2String n)
  exactP (GHC.KindedTyVar n ty) = do
    printStringAtMaybeAnn (G GHC.AnnOpenP) "("
    exactPC n
    printStringAtMaybeAnn (G GHC.AnnDcolon) "::"
    exactPC ty
    printStringAtMaybeAnn (G GHC.AnnCloseP) ")"

-- ---------------------------------------------------------------------

instance ExactP [GHC.LConDecl GHC.RdrName] where
  exactP cons = mapM_ exactPC cons

-- ---------------------------------------------------------------------

instance ExactP (GHC.ConDecl GHC.RdrName) where
  exactP (GHC.ConDecl lns _exp (GHC.HsQTvs _ns bndrs) ctx dets res _ _) = do
    case res of
      GHC.ResTyH98 -> do
        printStringAtMaybeAnn (G GHC.AnnForall) "forall"
        mapM_ exactPC bndrs
        printStringAtMaybeAnn (G GHC.AnnDot) "."

        exactPC ctx
        printStringAtMaybeAnn (G GHC.AnnDarrow) "=>"

        -- only do names if not infix
        case dets of
          GHC.InfixCon _ _ -> return ()
          _ -> mapM_ exactPC lns

        case dets of
          GHC.PrefixCon args -> mapM_ exactPC args
          GHC.RecCon fs -> do
            printStringAtMaybeAnn (G GHC.AnnOpenC) "{"
            exactPC fs
            printStringAtMaybeAnn (G GHC.AnnCloseC) "}"
          GHC.InfixCon a1 a2 -> do
            exactPC a1
            mapM_ exactPC lns
            exactPC a2


      GHC.ResTyGADT ls ty -> do
        -- only do names if not infix
        case dets of
          GHC.InfixCon _ _ -> return ()
          _ -> mapM_ exactPC lns

        case dets of
          GHC.PrefixCon args -> mapM_ exactPC args
          GHC.RecCon fs -> do
            printStringAtMaybeAnn (G GHC.AnnOpenC) "{"
            exactPC fs
            printStringAtMaybeAnn (G GHC.AnnCloseC) "}"
          GHC.InfixCon a1 a2 -> do
            exactPC a1
            mapM_ exactPC lns
            exactPC a2

        printStringAtMaybeAnn (G GHC.AnnDcolon) "::"

        exactPC (GHC.L ls (ResTyGADTHook bndrs))

        exactPC ctx
        printStringAtMaybeAnn (G GHC.AnnDarrow) "=>"
        exactPC ty

    printStringAtMaybeAnn (G GHC.AnnVbar) "|"

-- ---------------------------------------------------------------------

instance ExactP (ResTyGADTHook GHC.RdrName) where
  exactP (ResTyGADTHook bndrs) = do
    printStringAtMaybeAnn (G GHC.AnnForall) "forall"
    mapM_ exactPC bndrs
    printStringAtMaybeAnn (G GHC.AnnDot) "."

-- ---------------------------------------------------------------------

instance ExactP [GHC.LConDeclField GHC.RdrName] where
  exactP fs = do
    printStringAtMaybeAnn (G GHC.AnnOpenC) "{"
    mapM_ exactPC fs
    printStringAtMaybeAnn (G GHC.AnnDotdot) ".."
    printStringAtMaybeAnn (G GHC.AnnCloseC) "}"

-- ---------------------------------------------------------------------

instance ExactP (GHC.CType) where
  exactP (GHC.CType src mh f) = do
    printStringAtMaybeAnn (G GHC.AnnOpen) src
    case mh of
      Nothing -> return ()
      Just (GHC.Header h) ->
         printStringAtMaybeAnn (G GHC.AnnHeader) ("\"" ++ GHC.unpackFS h ++ "\"")
    printStringAtMaybeAnn (G GHC.AnnVal) ("\"" ++ GHC.unpackFS f ++ "\"")
    printStringAtMaybeAnn (G GHC.AnnClose) "#-}"

-- ---------------------------------------------------------------------


