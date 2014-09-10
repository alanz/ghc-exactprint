{-# LANGUAGE FlexibleInstances #-}
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
  -> [Comment] -> [PosToken] -> [(GHC.SrcSpan,Annotation)]
annotateLHsModule (GHC.L lm (GHC.HsModule mmn mexp imps decs depr haddock)) cs toks = r ++ impsAnn
  where
    pos = ss2pos lm  -- start of the syntax fragment
    infiniteSpan = ((1,0),(99999999,0)) -- lm is a single char span
    moduleTok = head $ filter ghcIsModule toks
    whereTok  = head $ filter ghcIsWhere  toks
    r = case mmn of
      Nothing -> []
      Just (GHC.L l _mn) -> (l,Ann lcs (DP (0,0)) annSpecific):aexps
        where
          lcs = localComments infiniteSpan cs (expSpan ++ impSpan ++ decsSpan)
          expSpan = case mexp of
            Nothing -> []
            Just _ -> [(undelta pos opPos,snd cpSpan)]

          impSpan  = getListSpan imps
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

    impsAnn = annotateLImportDecls imps cs toks Nothing

-- ---------------------------------------------------------------------

annotateLImportDecls :: [(GHC.LImportDecl GHC.RdrName)]
  -> [Comment] -> [PosToken] -> Maybe GHC.SrcSpan -> [(GHC.SrcSpan,Annotation)]
annotateLImportDecls [] _ _ _ = []
annotateLImportDecls [x] cs toks pl = annotateLImportDecl x cs toks pl
annotateLImportDecls (x1@(GHC.L l1 _):x2:xs) cs toks pl = annotateLImportDecl x1 cs toks pl ++ annotateLImportDecls (x2:xs) cs toks (Just l1)

annotateLImportDecl :: (GHC.LImportDecl GHC.RdrName) -> [Comment] -> [PosToken]
  -> Maybe GHC.SrcSpan -> [(GHC.SrcSpan,Annotation)]
annotateLImportDecl (GHC.L l (GHC.ImportDecl (GHC.L ln _) _pkg src safe qual impl as hiding)) cs toks pl = r
  where
    r = [(l,Ann lcs impPos annSpecific)]
    annSpecific = AnnImportDecl impPos Nothing Nothing Nothing Nothing
    impPos = case findPreceding ghcIsImport ln toks of
      Nothing -> error $ "annotateLImportDecl: No import token preceding :" ++ show (ss2span ln)
      Just ss -> ss2delta (ss2pos l) ss

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

annotateLIEs :: [GHC.LIE GHC.RdrName] -> [Comment] -> [PosToken] -> Maybe GHC.SrcSpan -> GHC.SrcSpan -> [(GHC.SrcSpan, Annotation)]
annotateLIEs [ ]    _  _ _  _ = []
annotateLIEs [x] cs toks pl pr                     = annotateLIE x  cs toks pl pr
annotateLIEs (x1@(GHC.L l1 _):x2:xs) cs toks pl pr = annotateLIE x1 cs toks pl pr ++ annotateLIEs (x2:xs) cs toks (Just l1) pr

-- This receives the toks for the entire exports section.
-- So it can scan for the separating comma if required
annotateLIE :: GHC.LIE GHC.RdrName -> [Comment] -> [PosToken] -> Maybe GHC.SrcSpan -> GHC.SrcSpan -> [(GHC.SrcSpan,Annotation)]
annotateLIE (GHC.L l (GHC.IEVar _))      cs toks pl pr = [(l,Ann lcs p (AnnIEVar mc))]
  where (mc, p, lcs) = getListAnnInfo l cs toks pl pr

annotateLIE (GHC.L l (GHC.IEThingAbs _)) cs toks pl pr = [(l,Ann lcs p (AnnIEThingAbs mc))]
  where (mc, p, lcs) = getListAnnInfo l cs toks pl pr

annotateLIE (GHC.L l (_)) cs toks pl pr = [] -- assert False undefined

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

commentPos :: Comment -> (Pos,Pos)
commentPos (Comment _ p _) = p

dcommentPos :: DComment -> (DeltaPos,DeltaPos)
dcommentPos (DComment _ p _) = p


-- ---------------------------------------------------------------------

-- | Given an enclosing Span @ss@, and a list of sub SrcSpans @ds@,
-- identify all comments that are in @ss@ but not in @ds@, and convert
-- them to be DComments relative to @ss@
localComments :: Span -> [Comment] -> [Span] -> [DComment]
localComments (p,e) cs ds = r `debug` ("localComments:(p,ds,r):" ++ show (p,ds,map commentPos matches,map dcommentPos r))
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
calcListOffsets isToken l toks pl pr = (mc,p,sp) `debug` ("calcListOffsets:(l,mc,p,sp,pr)=" ++ show (ss2span l,mc,p,sp,ss2span pr))
  where
    (mc,p,sp) = case findPreceding isToken l toks of
      Nothing -> (Nothing, ss2delta (ss2posEnd pr) l, (ss2posEnd pr,ss2posEnd l))
      Just ss -> (Just lo, ss2delta (ss2posEnd ss) l, (ss2posEnd lp,ss2posEnd l))
                 where lp = maybe l id pl
                       lo = (ss2delta (ss2posEnd lp) ss)

-- ---------------------------------------------------------------------

findPreceding :: (PosToken -> Bool) -> GHC.SrcSpan -> [PosToken] -> Maybe GHC.SrcSpan
findPreceding isToken ss toks = r
  where
    (toksBefore,_,_) = splitToksForSpan ss toks
    r = case filter isToken (reverse toksBefore) of
      [] -> Nothing
      (t:_) -> Just (tokenSpan t)

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
-- This section is horrible because there is no Eq instance for
-- GHC.Token

ghcIsModule :: PosToken -> Bool
ghcIsModule ((GHC.L _ t),_s) =  case t of
                       GHC.ITmodule -> True
                       _            -> False
ghcIsWhere :: PosToken -> Bool
ghcIsWhere ((GHC.L _ t),_s) =  case t of
                       GHC.ITwhere -> True
                       _           -> False
ghcIsLet :: PosToken -> Bool
ghcIsLet   ((GHC.L _ t),_s) =  case t of
                       GHC.ITlet -> True
                       _         -> False

ghcIsElse :: PosToken -> Bool
ghcIsElse   ((GHC.L _ t),_s) =  case t of
                       GHC.ITelse -> True
                       _         -> False

ghcIsThen :: PosToken -> Bool
ghcIsThen   ((GHC.L _ t),_s) =  case t of
                       GHC.ITthen -> True
                       _         -> False

ghcIsOf :: PosToken -> Bool
ghcIsOf   ((GHC.L _ t),_s) =  case t of
                       GHC.ITof -> True
                       _        -> False

ghcIsDo :: PosToken -> Bool
ghcIsDo   ((GHC.L _ t),_s) =  case t of
                       GHC.ITdo -> True
                       _        -> False

ghcIsIn :: PosToken -> Bool
ghcIsIn    ((GHC.L _ t),_s) = case t of
                      GHC.ITin -> True
                      _        -> False

ghcIsOParen :: PosToken -> Bool
ghcIsOParen ((GHC.L _ t),_s) = case t of
                      GHC.IToparen -> True
                      _            -> False

ghcIsCParen :: PosToken -> Bool
ghcIsCParen ((GHC.L _ t),_s) = case t of
                      GHC.ITcparen -> True
                      _            -> False

ghcIsComma :: PosToken -> Bool
ghcIsComma ((GHC.L _ t),_s) = case t of
                      GHC.ITcomma -> True
                      _           -> False

ghcIsImport :: PosToken -> Bool
ghcIsImport ((GHC.L _ t),_s) = case t of
                      GHC.ITimport -> True
                      _            -> False



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
rdrName2String = GHC.occNameString . GHC.rdrNameOcc

-- ---------------------------------------------------------------------

instance Show (GHC.GenLocated GHC.SrcSpan GHC.Token) where
  show t@(GHC.L l tok) = show ((srcSpanStart l, srcSpanEnd l),tok)
