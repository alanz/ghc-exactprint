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
  , rdrName2String
  ) where

import Control.Exception

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
annotateLHsModule :: GHC.Located (GHC.HsModule GHC.RdrName) -> [PosToken] -> [(GHC.SrcSpan,Annotation)]
annotateLHsModule (GHC.L _ (GHC.HsModule mmn mexp imps decs depr haddock)) toks = r
  where
    moduleTok = head $ filter ghcIsModule toks
    whereTok  = head $ filter ghcIsWhere toks
    r = case mmn of
      Nothing -> [(undefined,AnnNone)]
      Just (GHC.L l mn) -> (l,AnnModuleName mPos mnPos opPos cpPos wherePos):aexps
        where
          mPos  = ss2delta $ tokenSpan moduleTok
          mnPos = ss2delta l
          wherePos = ss2delta $ tokenSpan whereTok
          (opPos,cpPos,aexps) = case mexp of
            Nothing -> ((0,0),(0,0),[])
            Just exps -> (opPos',cpPos',aexps')
              where
                opTok = head $ filter ghcIsOParen toks
                (toksE,toksRest) = case exps of
                  [] -> (toks,toks)
                  _ -> let (_,etoks,ts) = splitToks (GHC.getLoc (head exps),
                                                     GHC.getLoc (last exps)) toks
                       in (etoks,ts)
                cpTok = head $ filter ghcIsCParen toksRest
                opPos' = ss2delta $ tokenSpan opTok
                cpPos' = ss2delta $ tokenSpan cpTok
                aexps' = concatMap (\e -> annotateLIE e toksE) exps


-- This receives the toks for the entire exports section.
-- So it can scan for the separating comma if required
annotateLIE :: GHC.LIE GHC.RdrName -> [PosToken] -> [(GHC.SrcSpan,Annotation)]
annotateLIE (GHC.L l (GHC.IEVar _)) toks = [(l,AnnIEVar (findTrailingComma l toks))]
annotateLIE (GHC.L l (GHC.IEThingAbs _)) toks = [(l,AnnIEThingAbs (findTrailingComma l toks))]
annotateLIE (GHC.L l (_)) toks = [] -- assert False undefined

-- ---------------------------------------------------------------------

findTrailingComma :: GHC.SrcSpan -> [PosToken] -> Maybe DeltaPos
findTrailingComma ss toks = r -- `debug` ("findTrailingComma:toksAfter=" ++ show (ss,toks,toksAfter))
  where
    (_,_,toksAfter) = splitToksForSpan ss toks
    r = case filter ghcIsComma toksAfter of
      [] -> Nothing
      (t:_) -> Just (ss2delta $ tokenSpan t)

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

ss2delta :: GHC.SrcSpan -> DeltaPos
ss2delta ss = (srcSpanStartLine ss,srcSpanStartColumn ss)

ss2pos :: GHC.SrcSpan -> Pos
ss2pos ss = (srcSpanStartLine ss,srcSpanStartColumn ss)

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
