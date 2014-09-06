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

-- ---------------------------------------------------------------------

-- TODO: turn this into a class
annotateLHsModule :: GHC.Located (GHC.HsModule GHC.RdrName) -> [PosToken] -> (GHC.SrcSpan,Annotation)
annotateLHsModule (GHC.L _ (GHC.HsModule mmn mexp imps decs depr haddock)) toks = r
  where
    moduleTok = head $ filter ghcIsModule toks
    whereTok  = head $ filter ghcIsWhere toks
    r = case mmn of
      Nothing -> (undefined,AnnNone)
      Just (GHC.L l mn) -> (l,AnnModuleName mPos mnPos wherePos)
        where
          mPos = ss2delta $ tokenPos moduleTok
          mnPos = ss2delta l
          wherePos = ss2delta $ tokenPos whereTok


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

tokenPos :: PosToken -> GHC.SrcSpan
tokenPos ((GHC.L l _),_s) = l


