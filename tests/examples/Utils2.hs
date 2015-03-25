{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Utils2 where


import Control.Applicative (Applicative(..))
import Control.Monad (when, liftM, ap)
import Control.Exception
import Data.Data
import Data.List
import Data.Maybe
import Data.Monoid

-- import Language.Haskell.GHC.ExactPrint.Utils
import Language.Haskell.GHC.ExactPrint.Types hiding (showGhc)

import qualified Bag            as GHC
import qualified BasicTypes     as GHC
import qualified BooleanFormula as GHC
import qualified Class          as GHC
import qualified CoAxiom        as GHC
import qualified DynFlags       as GHC
import qualified FastString     as GHC
import qualified ForeignCall    as GHC
import qualified GHC            as GHC
import qualified GHC.Paths      as GHC
import qualified Lexer          as GHC
import qualified Name           as GHC
import qualified NameSet        as GHC
import qualified Outputable     as GHC
import qualified RdrName        as GHC
import qualified SrcLoc         as GHC
import qualified StringBuffer   as GHC
import qualified UniqSet        as GHC
import qualified Unique         as GHC
import qualified Var            as GHC

import qualified Data.Map as Map

-- ---------------------------------------------------------------------

instance AnnotateP GHC.RdrName where
  annotateP l n = do
    case rdrName2String n of
      "[]" -> do
        addDeltaAnnotation GHC.AnnOpenS -- '[' nonBUG
        addDeltaAnnotation GHC.AnnCloseS -- ']' BUG
      "()" -> do
        addDeltaAnnotation GHC.AnnOpenP -- '('
        addDeltaAnnotation GHC.AnnCloseP -- ')'
      "(##)" -> do
        addDeltaAnnotation GHC.AnnOpen -- '(#'
        addDeltaAnnotation GHC.AnnClose -- '#)'
      "[::]" -> do
        addDeltaAnnotation GHC.AnnOpen -- '[:'
        addDeltaAnnotation GHC.AnnClose -- ':]'
      _ ->  do
        addDeltaAnnotation GHC.AnnType
        addDeltaAnnotation GHC.AnnOpenP -- '('
        addDeltaAnnotationLs GHC.AnnBackquote 0
        addDeltaAnnotations GHC.AnnCommaTuple -- For '(,,,)'
        cnt <- countAnnsAP GHC.AnnVal
        cntT <- countAnnsAP GHC.AnnCommaTuple
        cntR <- countAnnsAP GHC.AnnRarrow
        case cnt of
          0 -> if cntT >0 || cntR >0 then return () else addDeltaAnnotationExt l GHC.AnnVal
          1 -> addDeltaAnnotation GHC.AnnVal
          x -> error $ "annotateP.RdrName: too many AnnVal :" ++ showGhc (l,x)
        addDeltaAnnotation GHC.AnnTildehsh
        addDeltaAnnotation GHC.AnnTilde
        addDeltaAnnotation GHC.AnnRarrow
        addDeltaAnnotationLs GHC.AnnBackquote 1
        addDeltaAnnotation GHC.AnnCloseP -- ')'

  -- temporary, for test
class (Typeable ast) => AnnotateP ast where
  annotateP :: GHC.SrcSpan -> ast -> IO ()

addDeltaAnnotation = undefined
addDeltaAnnotations = undefined
addDeltaAnnotationLs = undefined
addDeltaAnnotationExt = undefined
countAnnsAP = undefined
showGhc = undefined
rdrName2String = undefined

