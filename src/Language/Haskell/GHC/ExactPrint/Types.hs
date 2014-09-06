module Language.Haskell.GHC.ExactPrint.Types
  (
    PosToken
  , DeltaPos
  , Annotation(..)
  , Anns(..)
  ) where

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

type PosToken = (GHC.Located GHC.Token, String)

type DeltaPos = (Int,Int)

data Annotation = AnnModuleName
  { mn_module :: !DeltaPos -- module
  , mn_name   :: !DeltaPos -- Language.Haskell.GHC.Types
  , mn_where  :: !DeltaPos -- where
  }
  | AnnNone

type Anns = Map.Map GHC.SrcSpan Annotation

