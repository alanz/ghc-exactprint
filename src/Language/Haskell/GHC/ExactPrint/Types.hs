module Language.Haskell.GHC.ExactPrint.Types
  (
    Pos
  , PosToken
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

type Pos = (Int,Int)

type DeltaPos = (Int,Int)

data Annotation =
  AnnModuleName
    { mn_module :: !DeltaPos -- module
    , mn_name   :: !DeltaPos -- Language.Haskell.GHC.Types
    , mn_op     :: !DeltaPos -- '('
    , mn_cp     :: !DeltaPos -- ')'
    , mn_where  :: !DeltaPos -- where
    }

  -- IE variants
  | AnnIEVar      { ie_comma :: !(Maybe DeltaPos) }
  | AnnIEThingAbs { ie_comma :: !(Maybe DeltaPos) }
  | AnnIEThingAll
  | AnnIEThingWith
  | AnnIEModuleContents
  | AnnIEGroup
  | AnnIEDoc
  | AnnIEDocNamed

{-
IEVar name
IEThingAbs name
IEThingAll name
IEThingWith name [name]
IEModuleContents ModuleName
IEGroup Int HsDocString
IEDoc HsDocString
IEDocNamed String

-}
  | AnnNone

type Anns = Map.Map GHC.SrcSpan Annotation

