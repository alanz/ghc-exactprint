{-# LANGUAGE CPP #-}
-- functions from GHC copied here until they can be exported in the next version.

module Language.Haskell.GHC.ExactPrint.GhcInterim where

import ApiAnnotation
import Lexer
import SrcLoc

-- ---------------------------------------------------------------------
#if __GLASGOW_HASKELL__ > 800
#else
-- From Lexer.x
commentToAnnotation :: Located Token -> Located AnnotationComment
commentToAnnotation (L l (ITdocCommentNext s))  = L l (AnnDocCommentNext s)
commentToAnnotation (L l (ITdocCommentPrev s))  = L l (AnnDocCommentPrev s)
commentToAnnotation (L l (ITdocCommentNamed s)) = L l (AnnDocCommentNamed s)
commentToAnnotation (L l (ITdocSection n s))    = L l (AnnDocSection n s)
commentToAnnotation (L l (ITdocOptions s))      = L l (AnnDocOptions s)
#if __GLASGOW_HASKELL__ < 801
commentToAnnotation (L l (ITdocOptionsOld s))   = L l (AnnDocOptionsOld s)
#endif
commentToAnnotation (L l (ITlineComment s))     = L l (AnnLineComment s)
commentToAnnotation (L l (ITblockComment s))    = L l (AnnBlockComment s)
commentToAnnotation _ = error $ "commentToAnnotation called for non-comment:" -- ++ show x
#endif
