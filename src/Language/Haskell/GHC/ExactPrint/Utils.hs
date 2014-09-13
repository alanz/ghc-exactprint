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
  ) where

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

-- TODO: turn this into a class.
-- TODO: distribute comments as per hindent
annotateLHsModule :: GHC.Located (GHC.HsModule GHC.RdrName)
  -> [Comment] -> [PosToken]
  -> [(GHC.SrcSpan,[Annotation])]
annotateLHsModule (GHC.L lm (GHC.HsModule mmn mexp imps decs depr haddock)) cs toks = r
  where
    r = headerAnn ++ impsAnn ++ declsAnn
    pos = ss2pos lm  -- start of the syntax fragment
    infiniteSpan = ((1,0),(99999999,0)) -- lm is a single char span
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
                (toksE,toksRest,cpRel) = case exps of
                  [] -> (toks,toks,pos)
                  _ -> let (_,etoks,ts) = splitToks (GHC.getLoc (head exps),
                                                     GHC.getLoc (last exps)) toks
                       in (etoks,ts,ss2posEnd $ GHC.getLoc (last exps))
                cpTok = head $ filter ghcIsCParen toksRest
                opPos' = ss2delta pos $ tokenSpan opTok
                cpPos' = ss2delta cpRel $ tokenSpan cpTok
                cpSpan' = ss2span $ tokenSpan cpTok
                aexps' = annotateLIEs exps cs toksE Nothing (tokenSpan opTok)

    impsAnn  = annotateLImportDecls imps cs toks Nothing
    declsAnn = annotateLHsDecls     decs cs toks Nothing

-- ---------------------------------------------------------------------

annotateLImportDecls :: [(GHC.LImportDecl GHC.RdrName)]
  -> [Comment] -> [PosToken] -> Maybe GHC.SrcSpan -> [(GHC.SrcSpan,[Annotation])]
annotateLImportDecls [] _ _ _ = []
annotateLImportDecls [x] cs toks pl = annotateLImportDecl x cs toks pl
annotateLImportDecls (x1@(GHC.L l1 _):x2:xs) cs toks pl = annotateLImportDecl x1 cs toks pl ++ annotateLImportDecls (x2:xs) cs toks (Just l1)

annotateLImportDecl :: (GHC.LImportDecl GHC.RdrName) -> [Comment] -> [PosToken]
  -> Maybe GHC.SrcSpan -> [(GHC.SrcSpan,[Annotation])]
annotateLImportDecl (GHC.L l (GHC.ImportDecl (GHC.L ln _) _pkg src safe qual impl as hiding)) cs toksIn pl = r
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
      Just (_,ies) -> (annotateLIEs ies cs toksI Nothing (tokenSpan opTok),opPos',cpPos')
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

annotateLIEs :: [GHC.LIE GHC.RdrName] -> [Comment] -> [PosToken] -> Maybe GHC.SrcSpan
  -> GHC.SrcSpan -> [(GHC.SrcSpan, [Annotation])]
annotateLIEs [ ]    _  _ _  _ = []
annotateLIEs [x] cs toks pl pr                     = annotateLIE x  cs toks pl pr
annotateLIEs (x1@(GHC.L l1 _):x2:xs) cs toks pl pr = annotateLIE x1 cs toks pl pr ++ annotateLIEs (x2:xs) cs toks (Just l1) pr

-- This receives the toks for the entire exports section.
-- So it can scan for the separating comma if required
annotateLIE :: GHC.LIE GHC.RdrName -> [Comment] -> [PosToken] -> Maybe GHC.SrcSpan
  -> GHC.SrcSpan -> [(GHC.SrcSpan,[Annotation])]
annotateLIE (GHC.L l (GHC.IEVar _))      cs toks pl pr = [(l,[Ann lcs p (AnnIEVar mc)])]
  where (mc, p, lcs) = getListAnnInfo l cs toks pl pr

annotateLIE (GHC.L l (GHC.IEThingAbs _)) cs toks pl pr = [(l,[Ann lcs p (AnnIEThingAbs mc)])]
  where (mc, p, lcs) = getListAnnInfo l cs toks pl pr

annotateLIE (GHC.L l (_)) cs toks pl pr = [] -- assert False undefined

-- ---------------------------------------------------------------------

annotateLHsDecls :: [(GHC.LHsDecl GHC.RdrName)]
  -> [Comment] -> [PosToken] -> Maybe GHC.SrcSpan -> [(GHC.SrcSpan,[Annotation])]
annotateLHsDecls [] _ _ _ = []
annotateLHsDecls [x] cs toks pl = annotateLHsDecl x cs toks pl
annotateLHsDecls (x1@(GHC.L l1 _):x2:xs) cs toks pl = annotateLHsDecl x1 cs toks pl ++ annotateLHsDecls (x2:xs) cs toks (Just l1)

annotateLHsDecl :: (GHC.LHsDecl GHC.RdrName) -> [Comment] -> [PosToken]
  -> Maybe GHC.SrcSpan -> [(GHC.SrcSpan,[Annotation])]
annotateLHsDecl (GHC.L l decl) cs toksIn pl =
 case decl of
    GHC.TyClD d -> annotateLTyClDecl (GHC.L l d) cs toksIn pl
    GHC.InstD d -> error $ "annotateLHsDecl:unimplemented " ++ "InstD"
    GHC.DerivD d -> error $ "annotateLHsDecl:unimplemented " ++ "DerivD"
    GHC.ValD d -> annotateLHsBind (GHC.L l d) cs toksIn
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

annotateLHsBind :: GHC.LHsBindLR GHC.RdrName GHC.RdrName
  -> [Comment] -> [PosToken]
  -> [(GHC.SrcSpan,[Annotation])]
annotateLHsBind (GHC.L l (GHC.FunBind ln _ (GHC.MG matches _ _ _) _ _ _)) cs toksIn = r
  where
    r = [(l,[Ann lcs p AnnFunBind])] ++ matchesAnn
    lcs = []
    p = DP (0,0)
    matchesAnn = concatMap (\m -> annotateLMatch m cs toksIn) matches

-- ---------------------------------------------------------------------

annotateLMatch :: (GHC.LMatch GHC.RdrName (GHC.LHsExpr GHC.RdrName))
  -> [Comment] -> [PosToken]
  -> [(GHC.SrcSpan,[Annotation])]
annotateLMatch (GHC.L l (GHC.Match pats typ (GHC.GRHSs grhs lb))) cs toksIn = r -- `debug` ("annotateLMatch:" ++ show (r,length grhs))
  where
    r = matchAnn ++ patsAnn ++ typAnn ++ rhsAnn ++ lbAnn
    matchAnn = [(l,[Ann lcs (DP (0,0)) (AnnMatch eqPos)])]
    lcs = []
    eqPos = case findTokenSrcSpan ghcIsEqual le toksIn of
      Just eqSpan -> Just $ ss2delta pe eqSpan `debug` ("annotateLMatch:'=' found at:" ++ show (ss2span eqSpan,pe))
        where
          (before,_,_) = splitToksForSpan eqSpan toksIn
          prior = head $ dropWhile ghcIsComment $ reverse before
          pe = tokenPosEnd prior
      Nothing -> Nothing `debug` ("annotateLMatch:no '=' found in:" ++ show (ss2span l))
    le = l

    patsAnn = []
    typAnn  = []
    rhsAnn  = concatMap (\rhs -> annotateLGRHS rhs cs toksIn) grhs
    lbAnn   = annotateHsLocalBinds lb cs toksIn

-- ---------------------------------------------------------------------

annotateLGRHS :: GHC.LGRHS GHC.RdrName (GHC.LHsExpr GHC.RdrName)
  -> [Comment] -> [PosToken]
  -> [(GHC.SrcSpan,[Annotation])]
annotateLGRHS (GHC.L l (GHC.GRHS guards expr@(GHC.L le _))) cs toksIn = r  `debug` ("annotateLGRHS :l=" ++ show (ss2span l))
  where
    r = [(l,[Ann lcs (DP (0,0)) (AnnGRHS eqPos)])] ++ guardsAnn ++ exprAnn
    lcs = []
    guardsAnn = []
    -- eqPos = case findTokenSrcSpan ghcIsEqual l toksIn of
    eqPos = case findPreceding ghcIsEqual le toksIn of
      Just eqSpan -> Just $ ss2delta (ss2pos le) eqSpan `debug` ("annotateLGRHS:'=' found at:" ++ show (ss2span eqSpan))
      Nothing -> Nothing `debug` ("annotateLGRHS:no '=' found in:" ++ show (ss2span l))
    exprAnn = annotateLHsExpr expr cs toksIn

-- ---------------------------------------------------------------------

annotateHsLocalBinds :: (GHC.HsLocalBinds GHC.RdrName)
  -> [Comment] -> [PosToken]
  -> [(GHC.SrcSpan,[Annotation])]
annotateHsLocalBinds (GHC.HsValBinds (GHC.ValBindsIn binds sigs)) cs toksIn = r -- `debug` ("annotateHsLocalBinds")
  where
    r = bindsAnn ++ sigsAnn
    bindsAnn = concatMap (\b -> annotateLHsBind b cs toksIn) $ GHC.bagToList binds
    sigsAnn = []
annotateHsLocalBinds (GHC.HsValBinds _) _ _ = assert False undefined
annotateHsLocalBinds (GHC.HsIPBinds vb) _ _ = assert False undefined
annotateHsLocalBinds (GHC.EmptyLocalBinds) _ _ = []

-- ---------------------------------------------------------------------

annotateLHsExpr :: GHC.LHsExpr GHC.RdrName
  -> [Comment] -> [PosToken]
  -> [(GHC.SrcSpan,[Annotation])]
annotateLHsExpr (GHC.L l (GHC.HsOverLit ov)) cs toksIn = r -- `debug` ("annotateLHsExpr:" ++ show r)
  where
    r = [(l,[Ann [] (DP (0,0)) (AnnOverLit str)])]
    Just tokLit = findToken ghcIsOverLit l toksIn
    str = tokenString tokLit
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

    lbAnn = annotateHsLocalBinds lb cs toksIn
    exprAnn = annotateLHsExpr expr cs toksIn

annotateLHsExpr _ cs toksIn = [] -- assert False undefined

-- ---------------------------------------------------------------------

annotateLTyClDecl :: GHC.LTyClDecl GHC.RdrName
  -> [Comment] -> [PosToken]
  -> Maybe GHC.SrcSpan -> [(GHC.SrcSpan,[Annotation])]
annotateLTyClDecl (GHC.L l (GHC.DataDecl _ _ _ _)) cs toksIn pl = r
  where
    r = [(l,[Ann lcs p AnnDataDecl])] ++ matchesAnn
    lcs = []
    p = DP (0,0)
    matchesAnn = []

-- ---------------------------------------------------------------------

getListAnnInfo :: GHC.SrcSpan -> [Comment] -> [PosToken]
  -> Maybe GHC.SrcSpan -> GHC.SrcSpan
  -> (Maybe DeltaPos, DeltaPos, [DComment])
getListAnnInfo l cs toks pl pr = (mc,p,lcs)
  where (mc,p, sp) = calcListOffsets ghcIsComma l toks pl pr
        lcs = localComments sp cs [] -- `debug` ("annotateLIE:sp=" ++ show sp )

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

calcListOffsets ::(PosToken -> Bool) -> GHC.SrcSpan
  -> [PosToken] -> Maybe GHC.SrcSpan -> GHC.SrcSpan
  -> (Maybe DeltaPos, DeltaPos, Span)
calcListOffsets isToken l toks pl pr = (mc,p,sp) -- `debug` ("calcListOffsets:(l,mc,p,sp,pr)=" ++ show (ss2span l,mc,p,sp,ss2span pr))
  where
    (mc,p,sp) = case findPreceding isToken l toks of
      Nothing -> (Nothing, ss2delta (ss2posEnd pr) l, (ss2posEnd pr,ss2posEnd l))
      Just ss -> (Just lo, ss2delta (ss2posEnd ss) l, (ss2posEnd lp,ss2posEnd l))
                 where lp = maybe l id pl
                       lo = (ss2delta (ss2posEnd lp) ss)

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


ghcIsAnyConid :: PosToken -> Bool
ghcIsAnyConid t = ghcIsConid t || ghcIsQConid t


ghcIsHiding :: PosToken -> Bool
ghcIsHiding t = ghcIsTok t GHC.IThiding

ghcIsEqual :: PosToken -> Bool
ghcIsEqual t = ghcIsTok t GHC.ITequal


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
