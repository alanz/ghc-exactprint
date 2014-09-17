{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Language.Haskell.GHC.ExactPrint.Utils
  (
    annotateLHsModule

  , ghcIsWhere
  , ghcIsLet
  , ghcIsComment
  , ghcIsMultiLine

  , srcSpanStartLine
  , srcSpanEndLine
  , srcSpanStartColumn
  , srcSpanEndColumn

  , ss2span
  , ss2pos
  , ss2posEnd
  , undelta
  , undeltaComment
  , rdrName2String
  , isSymbolRdrName
  ) where

import Control.Applicative (Applicative(..))
import Control.Monad (when, liftM, ap)
import Control.Exception
import Data.List.Utils
import Data.Maybe

import Language.Haskell.GHC.ExactPrint.Types

import qualified Bag           as GHC
import qualified DynFlags      as GHC
import qualified FastString    as GHC
import qualified ForeignCall   as GHC
import qualified GHC           as GHC
import qualified GHC.Paths     as GHC
import qualified Lexer         as GHC
import qualified Name          as GHC
import qualified NameSet       as GHC
import qualified Outputable    as GHC
import qualified RdrName       as GHC
import qualified SrcLoc        as GHC
import qualified StringBuffer  as GHC
import qualified UniqSet       as GHC
import qualified Unique        as GHC
import qualified Var           as GHC

import qualified Data.Map as Map

import Debug.Trace

debug :: c -> String -> c
debug = flip trace

-- ---------------------------------------------------------------------

newtype AP x = AP (Pos -> [Comment] -> [PosToken]
        -> (x, Pos, [Comment], [PosToken], [(GHC.SrcSpan,[Annotation])]))

instance Functor AP where
  fmap = liftM

instance Applicative AP where
  pure = return
  (<*>) = ap

instance Monad AP where
  return x = AP $ \l cs toks -> (x, l, cs, toks, [])

  AP m >>= k = AP $ \l0 c0 toks0 -> let
        (a, l1, c1, toks1, s1) = m l0 c0 toks0
        AP f = k a
        (b, l2, c2, toks2, s2) = f l1 c1 toks1
    in (b, l2, c2, toks2, s1 ++ s2)

runAP :: AP () -> [Comment] -> [PosToken] -> Anns
runAP (AP f) cs toks = let (_,_,_,_,s) = f (1,1) cs toks in Map.fromListWith (++) s


getComments :: AP [Comment]
getComments = AP (\l cs toks -> (cs,l,cs,toks,[]))

setComments :: [Comment] -> AP ()
setComments cs = AP (\l _ toks -> ((),l,cs,toks,[]))


getToks :: AP [PosToken]
getToks = AP (\l cs toks -> (toks,l,cs,toks,[]))

setToks :: [PosToken] -> AP ()
setToks toks = AP (\l cs _ -> ((),l,cs,toks,[]))

-- ---------------------------------------------------------------------

annotateLHsModule :: GHC.Located (GHC.HsModule GHC.RdrName)
  -> [Comment] -> [PosToken]
  -> [(GHC.SrcSpan,[Annotation])]
annotateLHsModule modu cs toks = annotate modu cs toks

-- ---------------------------------------------------------------------

-- TODO: mirror the EP monad structure
class Annotate a where

  -- |Generate an annotation for @a@ and all its subelements
  annotate :: a
    -> [Comment] -> [PosToken]
    -> [(GHC.SrcSpan,[Annotation])]

instance Annotate (GHC.Located (GHC.HsModule GHC.RdrName)) where
  annotate (GHC.L lm (GHC.HsModule mmn mexp imps decs _depr _haddock)) cs toks = r
            `debug` ("annotateLHsModule:(lm,lpo,lastTok,secondLastTok)="
               ++ show (lm,lpo,lastTok,secondLastTok,take 5 $ reverse toks))
    where
      r = [(lm,[Ann [] (DP (0,0)) (AnnHsModule lpo)])]
          ++ headerAnn ++ impsAnn ++ declsAnn
      pos = ss2pos lm  -- start of the syntax fragment
      infiniteSpan = ((1,1),(99999999,0)) -- lm is a single char span
      moduleTok = head $ filter ghcIsModule toks
      whereTok  = head $ filter ghcIsWhere  toks
      headerAnn = case mmn of
        Nothing -> []
        Just (GHC.L l _mn) -> (l,[Ann lcs (DP (0,0)) annSpecific]):aexps
          where
            lcs = localComments infiniteSpan cs (expSpan ++ impSpan ++ decsSpan)
            expSpan = case mexp of
              Nothing -> []
              Just _ -> [(undelta pos opPos,snd cpSpan)]

            impSpan  = getListSpans imps
            decsSpan = getListSpan decs

            annSpecific = AnnModuleName mPos mnPos opPos cpPos wherePos
            mPos  = ss2delta pos $ tokenSpan moduleTok
            mnPos = ss2delta pos l
            wherePos = ss2delta pos $ tokenSpan whereTok
            (opPos,cpPos,aexps,cpSpan) = case mexp of
              Nothing -> (DP (0,0), DP (0,0),[], ((0,0),(0,0)) )
              Just exps -> (opPos',cpPos',aexps',cpSpan')
                where
                  opTok = head $ filter ghcIsOParen toks
                  (toksE,cpRel) = case exps of
                    [] -> (toks,pos)
                    _ -> let (_,etoks,_) = splitToks ((tokenSpan opTok),
                                                      (tokenSpan cpTok)) toks
                         in (etoks ++ [cpTok],ss2posEnd $ GHC.getLoc (last exps))
                  cpTok   = head $ filter ghcIsCParen toks
                  opPos'  = ss2delta pos   $ tokenSpan opTok
                  cpPos'  = ss2delta cpRel $ tokenSpan cpTok
                  cpSpan' = ss2span $ tokenSpan cpTok
                  aexps'  = annotate exps cs toksE

      impsAnn  = annotate imps cs toks
      declsAnn = annotate decs cs toks

      lpo = ss2delta (ss2posEnd $ tokenSpan secondLastTok) (tokenSpan lastTok)
      secondLastTok = head $ dropWhile ghcIsComment $ tail $ reverse toks
      lastTok       = last toks

instance (Annotate a) => Annotate [a] where
  annotate asts cs toks = concatMap (\ast -> annotate ast cs toks) asts

-- ---------------------------------------------------------------------

-- TODO: turn this into a class.
-- TODO: distribute comments as per hindent
{-
annotateLHsModule :: GHC.Located (GHC.HsModule GHC.RdrName)
  -> [Comment] -> [PosToken]
  -> [(GHC.SrcSpan,[Annotation])]
annotateLHsModule (GHC.L lm (GHC.HsModule mmn mexp imps decs depr haddock)) cs toks = r
          `debug` ("annotateLHsModule:(lm,lpo,lastTok,secondLastTok)=" ++ show (lm,lpo,lastTok,secondLastTok,take 5 $ reverse toks))
  where
    r = [(lm,[Ann [] (DP (0,0)) (AnnHsModule lpo)])] ++ headerAnn ++ impsAnn ++ declsAnn
    pos = ss2pos lm  -- start of the syntax fragment
    infiniteSpan = ((1,1),(99999999,0)) -- lm is a single char span
    moduleTok = head $ filter ghcIsModule toks
    whereTok  = head $ filter ghcIsWhere  toks
    headerAnn = case mmn of
      Nothing -> []
      Just (GHC.L l _mn) -> (l,[Ann lcs (DP (0,0)) annSpecific]):aexps
        where
          lcs = localComments infiniteSpan cs (expSpan ++ impSpan ++ decsSpan)
          expSpan = case mexp of
            Nothing -> []
            Just _ -> [(undelta pos opPos,snd cpSpan)]

          impSpan  = getListSpans imps
          decsSpan = getListSpan decs

          annSpecific = AnnModuleName mPos mnPos opPos cpPos wherePos
          mPos  = ss2delta pos $ tokenSpan moduleTok
          mnPos = ss2delta pos l
          wherePos = ss2delta pos $ tokenSpan whereTok
          (opPos,cpPos,aexps,cpSpan) = case mexp of
            Nothing -> (DP (0,0), DP (0,0),[], ((0,0),(0,0)) )
            Just exps -> (opPos',cpPos',aexps',cpSpan')
              where
                opTok = head $ filter ghcIsOParen toks
                (toksE,cpRel) = case exps of
                  [] -> (toks,pos)
                  -- _ -> let (_,etoks,ts) = splitToks (GHC.getLoc (head exps),
                  --                                    GHC.getLoc (last exps)) toks
                  _ -> let (_,etoks,_) = splitToks ((tokenSpan opTok),
                                                    (tokenSpan cpTok)) toks
                       in (etoks ++ [cpTok],ss2posEnd $ GHC.getLoc (last exps))
                cpTok   = head $ filter ghcIsCParen toks
                opPos'  = ss2delta pos $ tokenSpan opTok
                cpPos'  = ss2delta cpRel $ tokenSpan cpTok
                cpSpan' = ss2span $ tokenSpan cpTok
                aexps'  = annotateLIEs exps cs toksE
                        -- `debug` ("annotateLHsModule:toksE=" ++ show toksE)

    impsAnn  = annotateLImportDecls imps cs toks
    declsAnn = annotateLHsDecls     decs cs toks

    lpo = ss2delta (ss2posEnd $ tokenSpan secondLastTok) (tokenSpan lastTok)
    secondLastTok = head $ dropWhile ghcIsComment $ tail $ reverse toks
    lastTok       = last toks
-}

-- ---------------------------------------------------------------------
{-
annotateLImportDecls :: [(GHC.LImportDecl GHC.RdrName)]
  -> [Comment] -> [PosToken] -> [(GHC.SrcSpan,[Annotation])]
annotateLImportDecls [] _ _ = []
annotateLImportDecls (x:xs) cs toks =
   annotateLImportDecl x cs toks ++ annotateLImportDecls xs cs toks
-}

instance Annotate (GHC.LImportDecl GHC.RdrName) where
  annotate (GHC.L l (GHC.ImportDecl (GHC.L ln _) _pkg _src _safe qual _impl as hiding)) cs toksIn = r
    where
      r = [(l,[Ann lcs impPos annSpecific])] ++ aimps
      annSpecific = AnnImportDecl impPos Nothing Nothing mqual mas maspos mhiding opPos cpPos
      p = ss2pos l
      (_,toks,_) = splitToksForSpan l toksIn
      impPos = findPrecedingDelta ghcIsImport ln toks p

      mqual = if qual
                then Just (findPrecedingDelta ghcIsQualified ln toks p)
                else Nothing

      (mas,maspos) = case as of
        Nothing -> (Nothing,Nothing)
        Just _  -> (Just (findDelta ghcIsAs l toks p),asp)
          where
             (_,middle,_) = splitToksForSpan l toks
             asp = case filter ghcIsAnyConid (reverse middle) of
               [] -> Nothing
               (t:_) -> Just (ss2delta (ss2pos l) $ tokenSpan t)

      mhiding = case hiding of
        Nothing -> Nothing
        Just (True, _)  -> Just (findDelta ghcIsHiding l toks p)
        Just (False,_)  -> Nothing

      (aimps,opPos,cpPos) = case hiding of
        Nothing -> ([],Nothing,Nothing)
        Just (_,ies) -> (annotate ies cs toksI,opPos',cpPos')
          where
            opTok = head $ filter ghcIsOParen toks
            cpTok = head $ filter ghcIsCParen toks
            opPos' = Just $ ss2delta p     $ tokenSpan opTok
            cpPos' = Just $ ss2delta cpRel $ tokenSpan cpTok
            (toksI,toksRest,cpRel) = case ies of
              [] -> (toks,toks,ss2posEnd $ tokenSpan opTok)
              _ -> let (_,etoks,ts) = splitToks (GHC.getLoc (head ies),
                                                 GHC.getLoc (last ies)) toks
                   in (etoks,ts,ss2posEnd $ GHC.getLoc (last ies))


      subs = []
      lcs = localComments (ss2span l) cs subs

{-
ideclName :: Located ModuleName
    Module name.

ideclPkgQual :: Maybe FastString
    Package qualifier.

ideclSource :: Bool
    True = {--} import

ideclSafe :: Bool
    True => safe import

ideclQualified :: Bool
    True => qualified

ideclImplicit :: Bool
    True => implicit import (of Prelude)

ideclAs :: Maybe ModuleName
    as Module

ideclHiding :: Maybe (Bool, [LIE name])
    (True => hiding, names)

-}

-- ---------------------------------------------------------------------
{-
annotateLIEs :: [GHC.LIE GHC.RdrName]
  -> [Comment] -> [PosToken]
  -> [(GHC.SrcSpan, [Annotation])]
annotateLIEs [ ]    _  _    = []
annotateLIEs (x:xs) cs toks = annotateLIE   x cs toks
                           ++ annotateLIEs xs cs toks
-}
-- ---------------------------------------------------------------------

instance Annotate (GHC.LIE GHC.RdrName) where
  -- This receives the toks for the entire exports section.
  -- So it can scan for the separating comma if required
  annotate (GHC.L l (GHC.IEVar _))     cs toks = [(l,[Ann lcs p (AnnIEVar mc)])]
    where (mc, p, lcs) = getListAnnInfo l ghcIsComma ghcIsCParen cs toks

  annotate (GHC.L l (GHC.IEThingAbs _)) cs toks = [(l,[Ann lcs p (AnnIEThingAbs mc)])]
    where (mc, p, lcs) = getListAnnInfo l ghcIsComma ghcIsCParen cs toks

  annotate (GHC.L l (_)) cs toks = assert False undefined

-- ---------------------------------------------------------------------

getListAnnInfo :: GHC.SrcSpan
  -> (PosToken -> Bool) -> (PosToken -> Bool)
  -> [Comment] -> [PosToken]
  -> (Maybe DeltaPos, DeltaPos, [DComment])
getListAnnInfo l isSeparator isTerminator cs toks = (mc,p,lcs) `debug` ("getListAnnInfo:lcs=" ++ show lcs)
  where (mc,p,sp) = calcListOffsets isSeparator isTerminator l toks
        lcs = localComments sp cs [] `debug` ("getListAnnInfo:sp=" ++ show sp )

isCommaOrCParen :: PosToken -> Bool
isCommaOrCParen t = ghcIsComma t || ghcIsCParen t

-- ---------------------------------------------------------------------

calcListOffsets :: (PosToken -> Bool) -> (PosToken -> Bool)
  -> GHC.SrcSpan
  -> [PosToken]
  -> (Maybe DeltaPos, DeltaPos, Span)
calcListOffsets isSeparator isTerminator l toks = (mc,p,sp) `debug` ("calcListOffsets:(l,mc,p,sp)=" ++ show (ss2span l,mc,p,sp))
  where
    (endPos,mc) = case findTrailing isToken l toks of
      Nothing -> (ss2posEnd l,Nothing) `debug` ("calcListOffsets:no next pos")
      Just t  -> ((tokenPos t),mc') `debug` ("calcListOffsets:next=" ++ show (tokenPos t))
        where mc' = if isSeparator t
                      then Just (ss2delta (ss2posEnd l) (tokenSpan t))
                      else Nothing
    p = DP (0,0)
    sp = (ss2pos l,endPos)

    isToken t = isSeparator t || isTerminator t

-- ---------------------------------------------------------------------

calcListOffsetsPreceding ::(PosToken -> Bool) -> GHC.SrcSpan
  -> [PosToken]
  -> Maybe GHC.SrcSpan -- Span for previous item, where there is one
  -> GHC.SrcSpan       -- opening parenthesis of list
  -> (Maybe DeltaPos, DeltaPos, Span)
calcListOffsetsPreceding isToken l toks pl pr = (mc,p,sp) `debug` ("calcListOffsets:(l,mc,p,sp,pr)=" ++ show (ss2span l,mc,p,sp,ss2span pr))
  where
    endPos = case findTrailingSrcSpan isToken l toks of
      Nothing -> ss2posEnd l `debug` ("calcListOffsets:no next pos")
      Just ss -> ss2pos ss   `debug` ("calcListOffsets:next=" ++ show (ss2span ss))
    (mc,p,sp) = case findPreceding isToken l toks of
      -- Nothing -> (Nothing, ss2delta (ss2posEnd pr) l, (ss2posEnd pr,ss2posEnd l))
      Nothing -> (Nothing, DP (0,0),                  (ss2posEnd pr,endPos))
      Just ss -> (Just lo, ss2delta (ss2posEnd ss) l, (ss2posEnd ss,endPos))
                 where lp = maybe l id pl
                       lo = (ss2delta (ss2posEnd lp) ss)

-- ---------------------------------------------------------------------

{-
annotateLHsDecls :: [(GHC.LHsDecl GHC.RdrName)]
  -> [Comment] -> [PosToken]
  -> [(GHC.SrcSpan,[Annotation])]
annotateLHsDecls [] _ _ = []
annotateLHsDecls (x:xs) cs toks = annotateLHsDecl x cs toks ++ annotateLHsDecls xs cs toks
-}

instance Annotate (GHC.LHsDecl GHC.RdrName) where
  annotate (GHC.L l decl) cs toksIn =
   case decl of
      GHC.TyClD d -> annotateLTyClDecl (GHC.L l d) cs toksIn
      GHC.InstD d -> error $ "annotateLHsDecl:unimplemented " ++ "InstD"
      GHC.DerivD d -> error $ "annotateLHsDecl:unimplemented " ++ "DerivD"
      GHC.ValD d -> annotate (GHC.L l d) cs toksIn
      GHC.SigD d -> error $ "annotateLHsDecl:unimplemented " ++ "SigD"
      GHC.DefD d -> error $ "annotateLHsDecl:unimplemented " ++ "DefD"
      GHC.ForD d -> error $ "annotateLHsDecl:unimplemented " ++ "ForD"
      GHC.WarningD d -> error $ "annotateLHsDecl:unimplemented " ++ "WarningD"
      GHC.AnnD d -> error $ "annotateLHsDecl:unimplemented " ++ "AnnD"
      GHC.RuleD d -> error $ "annotateLHsDecl:unimplemented " ++ "RuleD"
      GHC.VectD d -> error $ "annotateLHsDecl:unimplemented " ++ "VectD"
      GHC.SpliceD d -> error $ "annotateLHsDecl:unimplemented " ++ "SpliceD"
      GHC.DocD d -> error $ "annotateLHsDecl:unimplemented " ++ "DocD"
      GHC.QuasiQuoteD d -> error $ "annotateLHsDecl:unimplemented " ++ "QuasiQuoteD"
      GHC.RoleAnnotD d -> error $ "annotateLHsDecl:unimplemented " ++ "RoleAnnotD"

    where
      r = assert False undefined

-- ---------------------------------------------------------------------

instance Annotate (GHC.LHsBindLR GHC.RdrName GHC.RdrName) where
  annotate (GHC.L l (GHC.FunBind (GHC.L _ n) isInfix (GHC.MG matches _ _ _) _ _ _)) cs toksIn = r
    where
      r = [(l,[Ann lcs p AnnFunBind])] ++ matchesAnn
      lcs = []
      p = DP (0,0)
      matchesAnn = concatMap (\m -> annotateLMatch m n isInfix cs toksIn) matches

-- ---------------------------------------------------------------------

annotateLMatch :: (GHC.LMatch GHC.RdrName (GHC.LHsExpr GHC.RdrName))
  -> GHC.RdrName -> Bool
  -> [Comment] -> [PosToken]
  -> [(GHC.SrcSpan,[Annotation])]
annotateLMatch (GHC.L l (GHC.Match pats _typ (GHC.GRHSs grhs lb))) n isInfix cs toksIn = r -- `debug` ("annotateLMatch:" ++ show (r,length grhs))
  where
    r = matchAnn ++ patsAnn ++ typAnn ++ rhsAnn ++ lbAnn
    matchAnn = [(l,[Ann lcs (DP (0,0)) (AnnMatch nPos n isInfix eqPos)])]
    lcs = [] -- `debug` ("annotateLMatch:" ++ show matchToks)
     -- `debug` ("annotateLMatch:n" ++ show (GHC.isSymOcc $ GHC.rdrNameOcc n))

    (_,matchToks,_) = splitToksForSpan l toksIn
    nPos = if isInfix
             then fromJust $ findTokenWrtPrior ghcIsFunName ln matchToks
             else findDelta ghcIsFunName l matchToks (ss2pos l)

    ln = GHC.mkSrcSpan (GHC.srcSpanEnd (GHC.getLoc (head pats)))
                       (GHC.srcSpanEnd l)

    eqPos = case grhs of
             [GHC.L _ (GHC.GRHS [] _)] -> findTokenWrtPrior ghcIsEqual l toksIn -- unguarded
             _                         -> Nothing

    patsAnn = concatMap (\pat -> annotateLPat pat cs toksIn) pats
    typAnn  = []
    rhsAnn  = concatMap (\rhs -> annotateLGRHS rhs cs toksIn) grhs
    lbAnn   = annotateHsLocalBinds lb cs toksIn

-- ---------------------------------------------------------------------
{-
rhs     :: { Located (GRHSs RdrName) }
        : '=' exp wherebinds    { sL (comb3 $1 $2 $3) $ GRHSs (unguardedRHS $2) (unLoc $3) }
        | gdrhs wherebinds      { LL $ GRHSs (reverse (unLoc $1)) (unLoc $2) }

gdrhs :: { Located [LGRHS RdrName] }
        : gdrhs gdrh            { LL ($2 : unLoc $1) }
        | gdrh                  { L1 [$1] }

gdrh :: { LGRHS RdrName }
        : '|' guardquals '=' exp        { sL (comb2 $1 $>) $ GRHS (unLoc $2) $4 }

-}
annotateLGRHS :: GHC.LGRHS GHC.RdrName (GHC.LHsExpr GHC.RdrName)
  -> [Comment] -> [PosToken]
  -> [(GHC.SrcSpan,[Annotation])]
annotateLGRHS (GHC.L l (GHC.GRHS guards expr@(GHC.L le _))) cs toksIn = r -- `debug` ("annotateLGRHS :l=" ++ show (ss2span l))
  where
    r = [(l,[Ann lcs (DP (0,0)) (AnnGRHS guardPos eqPos)])] ++ guardsAnn ++ exprAnn
    lcs = [] -- `debug` ("annotateLGRHS:(guardPos,eqPos)=" ++ show (guardPos,eqPos))
    guardsAnn = concatMap (\g -> annotateLStmt g cs toksIn) guards

    (guardPos,eqPos) = case guards of
             [] -> (Nothing,Nothing)
             _  -> (Just $ findDelta ghcIsVbar l toksIn (ss2pos l)
                   , findTokenWrtPrior ghcIsEqual l toksIn)

    exprAnn = annotateLHsExpr expr cs toksIn

-- ---------------------------------------------------------------------

findTokenWrtPrior :: (PosToken -> Bool) -> GHC.SrcSpan -> [PosToken] -> Maybe DeltaPos
findTokenWrtPrior isToken le toksIn = eqPos -- `debug` ("findTokenWrtPrior:" ++ show (ss2span le))
  where
    eqPos = case findTokenSrcSpan isToken le toksIn of
      Just eqSpan -> Just $ ss2delta pe eqSpan
        where
          (before,_,_) = splitToksForSpan eqSpan toksIn
          prior = head $ dropWhile ghcIsComment $ reverse before
          pe = tokenPosEnd prior
      Nothing -> Nothing

-- ---------------------------------------------------------------------

annotateLPat :: GHC.LPat GHC.RdrName
  -> [Comment] -> [PosToken]
  -> [(GHC.SrcSpan,[Annotation])]
annotateLPat (GHC.L l (GHC.NPat ol _ _)) cs toksIn = r
  where
    r = annotateLHsExpr (GHC.L l (GHC.HsOverLit ol)) cs toksIn

annotateLPat (GHC.L l _) cs toksIn = r
  where
    r = [(l,[Ann lcs (DP (0,0)) (AnnNone)])]
    lcs = []

-- ---------------------------------------------------------------------

annotateLStmt :: GHC.LStmt GHC.RdrName (GHC.LHsExpr GHC.RdrName)
  -> [Comment] -> [PosToken]
  -> [(GHC.SrcSpan,[Annotation])]
annotateLStmt (GHC.L l (GHC.BodyStmt body _ _ _)) cs toksIn = r
  where
    r = lAnn ++ bodyAnn
    lAnn = [(l,[Ann lcs (DP (0,0)) (AnnStmtLR)])]
    lcs = []

    bodyAnn = annotateLHsExpr body cs toksIn

-- ---------------------------------------------------------------------

annotateHsLocalBinds :: (GHC.HsLocalBinds GHC.RdrName)
  -> [Comment] -> [PosToken]
  -> [(GHC.SrcSpan,[Annotation])]
annotateHsLocalBinds (GHC.HsValBinds (GHC.ValBindsIn binds sigs)) cs toksIn = r -- `debug` ("annotateHsLocalBinds")
  where
    r = bindsAnn ++ sigsAnn
    -- bindsAnn = concatMap (\b -> annotate b cs toksIn) $ GHC.bagToList binds
    bindsAnn = annotate (GHC.bagToList binds) cs toksIn
    sigsAnn = []
annotateHsLocalBinds (GHC.HsValBinds _) _ _ = assert False undefined
annotateHsLocalBinds (GHC.HsIPBinds vb) _ _ = assert False undefined
annotateHsLocalBinds (GHC.EmptyLocalBinds) _ _ = []

-- ---------------------------------------------------------------------

annotateLHsExpr :: GHC.LHsExpr GHC.RdrName
  -> [Comment] -> [PosToken]
  -> [(GHC.SrcSpan,[Annotation])]
annotateLHsExpr (GHC.L l (GHC.HsOverLit ov)) cs toksIn = r -- `debug` ("annotateLHsExpr.HsOverLit:" ++ show r)
  where
    r = [(l,[Ann [] (DP (0,0)) (AnnOverLit str)])]
    Just tokLit = findToken ghcIsOverLit l toksIn
    str = tokenString tokLit

annotateLHsExpr (GHC.L l (GHC.OpApp e1 op _f e2)) cs toksIn = r
  where
    r = e1Ann ++ opAnn ++ e2Ann
    e1Ann = annotateLHsExpr e1 cs toksIn
    opAnn = annotateLHsExpr op cs toksIn
    e2Ann = annotateLHsExpr e2 cs toksIn

annotateLHsExpr (GHC.L l (GHC.HsLet lb expr)) cs toksIn = r `debug` ("annotateLHsExpr.HsLet:l=" ++ show (ss2span l))
  where
    r = (l,[Ann lcs (DP (0,0)) annSpecific]) : lbAnn ++ exprAnn
    lcs = []
    p = ss2pos l

    annSpecific = AnnHsLet letPos inPos

    Just letp = findTokenSrcSpan ghcIsLet l toksIn
    Just inp  = findTokenSrcSpan ghcIsIn l toksIn
    letPos = Just $ ss2delta p letp
    inPos  = Just $ ss2delta p inp

    -- lbAnn = annotateHsLocalBinds lb cs toksIn
    lbAnn = annotateHsLocalBinds lb cs toksIn
    exprAnn = annotateLHsExpr expr cs toksIn

annotateLHsExpr _ cs toksIn = [] -- assert False undefined

-- ---------------------------------------------------------------------

annotateLTyClDecl :: GHC.LTyClDecl GHC.RdrName
  -> [Comment] -> [PosToken]
  -> [(GHC.SrcSpan,[Annotation])]
annotateLTyClDecl (GHC.L l (GHC.DataDecl ln (GHC.HsQTvs ns tyVars) defn _)) cs toksIn = r
  where
    r = [(l,[Ann lcs p (AnnDataDecl eqPos)])] ++ declAnn
    lcs = []
    p = DP (0,0)
    Just eqPos = findTokenWrtPrior ghcIsEqual l toksIn

    declAnn = annotateHsDataDefn defn cs toksIn

-- ---------------------------------------------------------------------

annotateHsDataDefn :: (GHC.HsDataDefn GHC.RdrName)
  -> [Comment] -> [PosToken]
  -> [(GHC.SrcSpan,[Annotation])]
annotateHsDataDefn (GHC.HsDataDefn nOrD ctx mtyp mkind cons mderivs) cs toksIn = r
  where
    r = concatMap (\c -> annotateLConDecl c cs toksIn) cons

-- ---------------------------------------------------------------------

annotateLConDecl :: (GHC.LConDecl GHC.RdrName)
  -> [Comment] -> [PosToken]
  -> [(GHC.SrcSpan,[Annotation])]
annotateLConDecl (GHC.L l (GHC.ConDecl ln exp qvars ctx dets res _ _)) cs toksIn = r
  where
    r = [(l,[Ann lcs p (AnnConDecl mc)])] `debug` ("annotateLConDecl:(mc,toks)=" ++ show (mc,toks))
    (_,_,toks) = splitToksForSpan l toksIn
    (mc, p, lcs) = getListAnnInfo l ghcIsVbar (const False) cs toksIn

-- ---------------------------------------------------------------------

getListSpan :: [GHC.Located e] -> [Span]
getListSpan [] = []
getListSpan xs = [ss2span $ GHC.mkSrcSpan (GHC.srcSpanStart (GHC.getLoc (head xs)))
                                          (GHC.srcSpanEnd   (GHC.getLoc (last xs)))
                 ]

getListSpans :: [GHC.Located e] -> [Span]
getListSpans xs = map (ss2span . GHC.getLoc) xs


commentPos :: Comment -> (Pos,Pos)
commentPos (Comment _ p _) = p

dcommentPos :: DComment -> (DeltaPos,DeltaPos)
dcommentPos (DComment _ p _) = p


-- ---------------------------------------------------------------------

-- | Given an enclosing Span @ss@, and a list of sub SrcSpans @ds@,
-- identify all comments that are in @ss@ but not in @ds@, and convert
-- them to be DComments relative to @ss@
localComments :: Span -> [Comment] -> [Span] -> [DComment]
localComments (p,e) cs ds = r -- `debug` ("localComments:(p,ds,r):" ++ show (p,ds,map commentPos matches,map dcommentPos r))
  where
    r = map (\c -> deltaComment p c) matches

    matches = filter notSub cs'
    cs' = filter (\(Comment _ com _) -> isSubPos com (p,e)) cs

    notSub :: Comment -> Bool
    notSub (Comment _ com _) = not $ any (\sub -> isSubPos com sub) ds

    isSubPos (subs,sube) (parents,parente)
      = parents <= subs && parente >= sube

-- ---------------------------------------------------------------------

findTokenSrcSpan :: (PosToken -> Bool) -> GHC.SrcSpan -> [PosToken] -> Maybe GHC.SrcSpan
findTokenSrcSpan isToken ss toks =
  case findToken isToken ss toks of
      Nothing -> Nothing
      Just t  -> Just (tokenSpan t)

-- ---------------------------------------------------------------------

findToken :: (PosToken -> Bool) -> GHC.SrcSpan -> [PosToken] -> Maybe PosToken
findToken isToken ss toks = r
  where
    (_,middle,_) = splitToksForSpan ss toks
    r = case filter isToken middle of
      [] -> Nothing
      (t:_) -> Just t


-- ---------------------------------------------------------------------

findPreceding :: (PosToken -> Bool) -> GHC.SrcSpan -> [PosToken] -> Maybe GHC.SrcSpan
findPreceding isToken ss toks = r
  where
    (toksBefore,_,_) = splitToksForSpan ss toks
    r = case filter isToken (reverse toksBefore) of
      [] -> Nothing
      (t:_) -> Just (tokenSpan t)

-- ---------------------------------------------------------------------

findPrecedingDelta :: (PosToken -> Bool) -> GHC.SrcSpan -> [PosToken]
 -> Pos -> DeltaPos
findPrecedingDelta isToken ln toks p =
  case findPreceding isToken ln toks of
    Nothing -> error $ "findPrecedingDelta: No matching token preceding :" ++ show (ss2span ln)
    Just ss -> ss2delta p ss

-- ---------------------------------------------------------------------

findDelta :: (PosToken -> Bool) -> GHC.SrcSpan -> [PosToken]
 -> Pos -> DeltaPos
findDelta isToken ln toks p =
  case findTokenSrcSpan isToken ln toks of
    Nothing -> error $ "findPrecedingDelta: No matching token preceding :" ++ show (ss2span ln)
    Just ss -> ss2delta p ss

-- ---------------------------------------------------------------------

findTrailingComma :: GHC.SrcSpan -> [PosToken] -> Maybe DeltaPos
findTrailingComma ss toks = r
  where
    (_,_,toksAfter) = splitToksForSpan ss toks
    r = case filter ghcIsComma toksAfter of
      [] -> Nothing
      (t:_) -> Just (ss2delta (ss2pos ss) $ tokenSpan t)


-- ---------------------------------------------------------------------

findTrailingSrcSpan :: (PosToken -> Bool) -> GHC.SrcSpan -> [PosToken] -> Maybe GHC.SrcSpan
findTrailingSrcSpan isToken ss toks = r
  where
    (_,_,toksAfter) = splitToksForSpan ss toks
    r = case filter isToken toksAfter of
      [] -> Nothing
      (t:_) -> Just (tokenSpan t)

-- ---------------------------------------------------------------------

findTrailing :: (PosToken -> Bool) -> GHC.SrcSpan -> [PosToken] -> Maybe PosToken
findTrailing isToken ss toks = r
  where
    (_,_,toksAfter) = splitToksForSpan ss toks
    r = case filter isToken toksAfter of
      [] -> Nothing
      (t:_) -> Just t


-- ---------------------------------------------------------------------

undeltaComment :: Pos -> DComment -> Comment
undeltaComment l (DComment b (dps,dpe) s) = Comment b ((undelta l dps),(undelta l dpe)) s

deltaComment :: Pos -> Comment -> DComment
deltaComment l (Comment b (s,e) str)
  = DComment b ((ss2deltaP l s),(ss2deltaP l e)) str

-- ---------------------------------------------------------------------

deriving instance Eq GHC.Token

ghcIsTok :: PosToken -> GHC.Token -> Bool
ghcIsTok ((GHC.L _ t),_s) tp = t == tp

ghcIsModule :: PosToken -> Bool
ghcIsModule t = ghcIsTok t GHC.ITmodule

ghcIsWhere :: PosToken -> Bool
ghcIsWhere t = ghcIsTok t GHC.ITwhere

ghcIsLet :: PosToken -> Bool
ghcIsLet t = ghcIsTok t GHC.ITlet

ghcIsElse :: PosToken -> Bool
ghcIsElse t = ghcIsTok t GHC.ITelse

ghcIsThen :: PosToken -> Bool
ghcIsThen t = ghcIsTok t GHC.ITthen

ghcIsOf :: PosToken -> Bool
ghcIsOf t = ghcIsTok t GHC.ITof

ghcIsDo :: PosToken -> Bool
ghcIsDo t = ghcIsTok t GHC.ITdo

ghcIsIn :: PosToken -> Bool
ghcIsIn t = ghcIsTok t GHC.ITin

ghcIsOParen :: PosToken -> Bool
ghcIsOParen t = ghcIsTok t GHC.IToparen

ghcIsCParen :: PosToken -> Bool
ghcIsCParen t = ghcIsTok t GHC.ITcparen

ghcIsComma :: PosToken -> Bool
ghcIsComma t = ghcIsTok t GHC.ITcomma

ghcIsImport :: PosToken -> Bool
ghcIsImport t = ghcIsTok t GHC.ITimport

ghcIsQualified :: PosToken -> Bool
ghcIsQualified t = ghcIsTok t GHC.ITqualified

ghcIsAs :: PosToken -> Bool
ghcIsAs t = ghcIsTok t GHC.ITas

ghcIsConid :: PosToken -> Bool
ghcIsConid ((GHC.L _ t),_) = case t of
       GHC.ITconid _ -> True
       _             -> False

ghcIsQConid :: PosToken -> Bool
ghcIsQConid((GHC.L _ t),_) = case t of
       GHC.ITqconid _ -> True
       _              -> False

ghcIsVarid :: PosToken -> Bool
ghcIsVarid ((GHC.L _ t),_) = case t of
       GHC.ITvarid _ -> True
       _             -> False

ghcIsVarsym :: PosToken -> Bool
ghcIsVarsym ((GHC.L _ t),_) = case t of
       GHC.ITvarsym _ -> True
       _              -> False

ghcIsBackquote :: PosToken -> Bool
ghcIsBackquote t = ghcIsTok t GHC.ITbackquote

ghcIsFunName :: PosToken -> Bool
ghcIsFunName t = ghcIsVarid t || ghcIsVarsym t || ghcIsBackquote t

ghcIsAnyConid :: PosToken -> Bool
ghcIsAnyConid t = ghcIsConid t || ghcIsQConid t


ghcIsHiding :: PosToken -> Bool
ghcIsHiding t = ghcIsTok t GHC.IThiding

ghcIsEqual :: PosToken -> Bool
ghcIsEqual t = ghcIsTok t GHC.ITequal

ghcIsVbar :: PosToken -> Bool
ghcIsVbar t = ghcIsTok t GHC.ITvbar


ghcIsInteger :: PosToken -> Bool
ghcIsInteger ((GHC.L _ t),_)  = case t of
      GHC.ITinteger _ -> True
      _               -> False

ghcIsRational :: PosToken -> Bool
ghcIsRational ((GHC.L _ t),_) = case t of
      GHC.ITrational _ -> True
      _                -> False

ghcIsOverLit :: PosToken -> Bool
ghcIsOverLit t = ghcIsInteger t || ghcIsRational t


ghcIsComment :: PosToken -> Bool
ghcIsComment ((GHC.L _ (GHC.ITdocCommentNext _)),_s)  = True
ghcIsComment ((GHC.L _ (GHC.ITdocCommentPrev _)),_s)  = True
ghcIsComment ((GHC.L _ (GHC.ITdocCommentNamed _)),_s) = True
ghcIsComment ((GHC.L _ (GHC.ITdocSection _ _)),_s)    = True
ghcIsComment ((GHC.L _ (GHC.ITdocOptions _)),_s)      = True
ghcIsComment ((GHC.L _ (GHC.ITdocOptionsOld _)),_s)   = True
ghcIsComment ((GHC.L _ (GHC.ITlineComment _)),_s)     = True
ghcIsComment ((GHC.L _ (GHC.ITblockComment _)),_s)    = True
ghcIsComment ((GHC.L _ _),_s)                         = False


ghcIsMultiLine :: PosToken -> Bool
ghcIsMultiLine ((GHC.L _ (GHC.ITdocCommentNext _)),_s)  = False
ghcIsMultiLine ((GHC.L _ (GHC.ITdocCommentPrev _)),_s)  = False
ghcIsMultiLine ((GHC.L _ (GHC.ITdocCommentNamed _)),_s) = False
ghcIsMultiLine ((GHC.L _ (GHC.ITdocSection _ _)),_s)    = False
ghcIsMultiLine ((GHC.L _ (GHC.ITdocOptions _)),_s)      = False
ghcIsMultiLine ((GHC.L _ (GHC.ITdocOptionsOld _)),_s)   = False
ghcIsMultiLine ((GHC.L _ (GHC.ITlineComment _)),_s)     = False
ghcIsMultiLine ((GHC.L _ (GHC.ITblockComment _)),_s)    = True
ghcIsMultiLine ((GHC.L _ _),_s)                         = False

-- ---------------------------------------------------------------------

ss2delta :: Pos -> GHC.SrcSpan -> DeltaPos
ss2delta ref ss = ss2deltaP ref (ss2pos ss)

-- | Convert the start of the second @Pos@ to be an offset from the
-- first. The assumption is the reference starts before the second @Pos@
ss2deltaP :: Pos -> Pos -> DeltaPos
ss2deltaP (refl,refc) (l,c) = DP (lo,co)
  where
    lo = l - refl
    co = if lo == 0 then c - refc
                    else c

undelta :: Pos -> DeltaPos -> Pos
undelta (l,c) (DP (dl,dc)) = (fl,fc)
  where
    fl = l + dl
    fc = if dl == 0 then c + dc else dc

-- prop_delta :: TODO

ss2pos :: GHC.SrcSpan -> Pos
ss2pos ss = (srcSpanStartLine ss,srcSpanStartColumn ss)

ss2posEnd :: GHC.SrcSpan -> Pos
ss2posEnd ss = (srcSpanEndLine ss,srcSpanEndColumn ss)

ss2span :: GHC.SrcSpan -> Span
ss2span ss = (ss2pos ss,ss2posEnd ss)

srcSpanStart :: GHC.SrcSpan -> Pos
srcSpanStart ss = (srcSpanStartLine ss,srcSpanStartColumn ss)

srcSpanEnd :: GHC.SrcSpan -> Pos
srcSpanEnd ss = (srcSpanEndLine ss,srcSpanEndColumn ss)


srcSpanEndColumn :: GHC.SrcSpan -> Int
srcSpanEndColumn (GHC.RealSrcSpan s) = GHC.srcSpanEndCol s
srcSpanEndColumn _ = 0

srcSpanStartColumn :: GHC.SrcSpan -> Int
srcSpanStartColumn (GHC.RealSrcSpan s) = GHC.srcSpanStartCol s
srcSpanStartColumn _ = 0

srcSpanEndLine :: GHC.SrcSpan -> Int
srcSpanEndLine (GHC.RealSrcSpan s) = GHC.srcSpanEndLine s
srcSpanEndLine _ = 0

srcSpanStartLine :: GHC.SrcSpan -> Int
srcSpanStartLine (GHC.RealSrcSpan s) = GHC.srcSpanStartLine s
srcSpanStartLine _ = 0

-- ---------------------------------------------------------------------

tokenSpan :: PosToken -> GHC.SrcSpan
tokenSpan ((GHC.L l _),_s) = l

tokenPos :: PosToken -> Pos
tokenPos ((GHC.L l _),_s) = srcSpanStart l

tokenPosEnd :: PosToken -> Pos
tokenPosEnd ((GHC.L l _),_s) = srcSpanEnd l

tokenString :: PosToken -> String
tokenString (_,s) = s

-- ---------------------------------------------------------------------

splitToks:: (GHC.SrcSpan,GHC.SrcSpan) -> [PosToken]->([PosToken],[PosToken],[PosToken])
splitToks (startPos, endPos) toks =
  let (toks1,toks2)   = break (\t -> tokenSpan t >= startPos) toks
      (toks21,toks22) = break (\t -> tokenSpan t >=   endPos) toks2
  in
    (toks1,toks21,toks22)

-- ---------------------------------------------------------------------

splitToksForSpan:: GHC.SrcSpan -> [PosToken] -> ([PosToken],[PosToken],[PosToken])
splitToksForSpan ss toks =
  let (toks1,toks2)   = break (\t -> tokenPos t >= srcSpanStart ss) toks
      (toks21,toks22) = break (\t -> tokenPos t >= srcSpanEnd   ss) toks2
  in
    (toks1,toks21,toks22)

-- ---------------------------------------------------------------------

isSymbolRdrName :: GHC.RdrName -> Bool
isSymbolRdrName n = GHC.isSymOcc $ GHC.rdrNameOcc n

rdrName2String :: GHC.RdrName -> String
rdrName2String r =
  case GHC.isExact_maybe r of
    Just n  -> name2String n
    Nothing ->  GHC.occNameString $ GHC.rdrNameOcc r

name2String :: GHC.Name -> String
name2String name = showGhc name

-- |Show a GHC API structure
showGhc :: (GHC.Outputable a) => a -> String
#if __GLASGOW_HASKELL__ > 706
showGhc x = GHC.showPpr GHC.unsafeGlobalDynFlags x
#else
#if __GLASGOW_HASKELL__ > 704
showGhc x = GHC.showSDoc GHC.tracingDynFlags $ GHC.ppr x
#else
showGhc x = GHC.showSDoc                     $ GHC.ppr x
#endif
#endif

-- ---------------------------------------------------------------------

instance Show (GHC.GenLocated GHC.SrcSpan GHC.Token) where
  show t@(GHC.L l tok) = show ((srcSpanStart l, srcSpanEnd l),tok)
