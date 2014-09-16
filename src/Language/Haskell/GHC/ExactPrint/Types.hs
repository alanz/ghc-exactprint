{-# LANGUAGE DeriveDataTypeable #-}
module Language.Haskell.GHC.ExactPrint.Types
  (
    Comment(..)
  , DComment(..)
  , Pos
  , Span
  , PosToken
  , DeltaPos(..)
  , Annotation(..)
  , AnnSpecific(..)
  , annNone
  , Anns(..)
  ) where

import Data.Data

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

-- | A Haskell comment. The 'Bool' is 'True' if the comment is multi-line, i.e. @{- -}@.
data Comment = Comment Bool (Pos,Pos) String
  deriving (Eq,Show,Typeable,Data)

data DComment = DComment Bool (DeltaPos,DeltaPos) String
  deriving (Eq,Show,Typeable,Data)

instance Ord Comment where
  compare (Comment _ p1 _) (Comment _ p2 _) = compare p1 p2

type PosToken = (GHC.Located GHC.Token, String)

type Pos = (Int,Int)
type Span = (Pos,Pos)

newtype DeltaPos = DP (Int,Int) deriving (Show,Eq,Ord,Typeable,Data)

annNone :: Annotation
annNone = Ann [] (DP (0,0)) AnnNone

data Annotation = Ann
  { ann_comments :: ![DComment]
  , ann_loc      :: !DeltaPos
  , ann_specific :: !AnnSpecific
  } deriving (Show)

data AnnSpecific =
  AnnModuleName
    { mn_module :: !DeltaPos -- module
    , mn_name   :: !DeltaPos -- Language.Haskell.GHC.Types
    , mn_op     :: !DeltaPos -- '('
    , mn_cp     :: !DeltaPos -- ')'
    , mn_where  :: !DeltaPos -- where
    }

  -- IE variants, *trailing* comma
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

  | AnnImportDecl
     { id_import    :: !DeltaPos
     , id_source    :: !(Maybe DeltaPos)
     , id_safe      :: !(Maybe DeltaPos)
     , id_qualified :: !(Maybe DeltaPos)
     , id_as        :: !(Maybe DeltaPos)
     , id_as_pos    :: !(Maybe DeltaPos)
     , id_hiding    :: !(Maybe DeltaPos)
     , id_op        :: !(Maybe DeltaPos)
     , id_cp        :: !(Maybe DeltaPos)
     }

  -- HsBindLR
  | AnnFunBind {}


  | AnnGRHS
    { grhs_guard :: !(Maybe DeltaPos) --  track the '|'
    , grhs_eq    :: !(Maybe DeltaPos)
    }

  | AnnMatch
     { match_npos  :: !DeltaPos -- location of the function name
     , match_n     :: !GHC.RdrName
     , match_infix :: !Bool  -- if the function is infix
     , match_eq    :: !(Maybe DeltaPos)
     }

  -- TyClDecl
  | AnnDataDecl {}


  -- HsOverLit, must keep the exact original string used
  | AnnOverLit { ol_str :: !String }


  -- HsExpr
  | AnnHsLet
      { hsl_let :: !(Maybe DeltaPos)
      , hsl_in  :: !(Maybe DeltaPos)
      }

  -- StmtLR
  | AnnStmtLR {}

  | AnnNone
  deriving (Show)

instance Show GHC.RdrName where
  show n = "(a RdrName)"

type Anns = Map.Map GHC.SrcSpan [Annotation]


