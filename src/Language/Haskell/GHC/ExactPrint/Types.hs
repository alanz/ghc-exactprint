{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
module Language.Haskell.GHC.ExactPrint.Types
  (
    Comment(..)
  , DComment(..)
  , Pos
  , Span
  , PosToken
  , DeltaPos(..)
  , Annotation(..)
  , annNone
  , Anns(..)
  , AnnsEP(..)
  , AnnsUser(..)

  , Value(..)
  , AnnKey
  , newValue
  , typeValue
  , fromValue
  , mkAnnKeyEP
  , mkAnnKeyV
  , getAnnotationValue
  , putAnnotationValue

  -- * Specific annotation
  , AnnHsModule(..)
  , AnnIe(..)
  , AnnImportDecl(..)
  , AnnTypeSig(..)
  , AnnHsBind(..)
  , AnnGRHS(..)
  , AnnMatch(..)
  , AnnOverLit(..)
  , AnnHsExpr(..)
  , AnnStmt(..)
  , AnnTyClDecl(..)
  , AnnHsType(..)
  , AnnPat(..)
  , AnnListItem(..)
  , AnnNone(..)
  ) where

import Data.Data
import Data.Maybe

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
data Comment = Comment Bool Span String
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
annNone = Ann [] (DP (0,0))

data Annotation = Ann
  { ann_comments :: ![DComment]
  , ann_delta    :: !DeltaPos -- Do we need this? Yes indeed.
  -- , ann_specific :: !Value
  } deriving (Show,Typeable)

-- Specific annotation types

data AnnHsModule = AnnHsModule
    { m_module :: !(Maybe DeltaPos) -- module
    , m_n      :: !(Maybe DeltaPos) -- name
    , m_op     :: !(Maybe DeltaPos) -- '('
    , m_cp     :: !(Maybe DeltaPos) -- ')'
    , m_where  :: !(Maybe DeltaPos) -- where
    , m_fileEnd :: !DeltaPos
    }
  deriving (Show,Typeable,Eq)

data AnnIe =
  -- IE variants, *trailing* comma
    AnnIEVar      { ie_comma :: !(Maybe DeltaPos) }
  | AnnIEThingAbs { ie_comma :: !(Maybe DeltaPos) }
  | AnnIEThingAll
  | AnnIEThingWith
  | AnnIEModuleContents
  | AnnIEGroup
  | AnnIEDoc
  | AnnIEDocNamed
  deriving (Show,Typeable,Eq)

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

data AnnImportDecl =
  AnnImportDecl
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
  deriving (Show,Typeable,Eq)


  -- Sig
data AnnTypeSig =
    AnnTypeSig { st_dcolon :: !DeltaPos }
  deriving (Show,Typeable,Eq)

data AnnHsBind
  -- HsBindLR
  = AnnFunBind {}
  | AnnPatBind
     { pb_equal :: !(Maybe DeltaPos)
     , pb_where :: !(Maybe DeltaPos) }

  -- AnnVarBind {} not needed, only introduced by type checker
  -- AnnAbsBind {} only used after renamer
  | AnnPatSynBind {}
  deriving (Show,Typeable,Eq)

data AnnGRHS
  = AnnGRHS
    { grhs_guard :: !(Maybe DeltaPos) --  track the '|'
    , grhs_eq    :: !(Maybe DeltaPos)
    }
  deriving (Show,Typeable,Eq)

data AnnMatch
  = AnnMatch
     { match_npos  :: !DeltaPos -- location of the function name
     , match_n     :: !GHC.RdrName
     , match_infix :: !Bool  -- if the function is infix
     , match_eq    :: !(Maybe DeltaPos)
     , match_where :: !(Maybe DeltaPos)
     }
  deriving (Show,Typeable,Eq)

  -- HsOverLit, must keep the exact original string used
data AnnOverLit
  = AnnOverLit { ol_str :: !String }
  deriving (Show,Typeable,Eq)


  -- HsExpr
data AnnHsExpr
  = AnnHsLet
      { hsl_let :: !(Maybe DeltaPos)
      , hsl_in  :: !(Maybe DeltaPos)
      }
  | AnnHsDo
      { hsd_do :: !(Maybe DeltaPos)
      }
  | AnnExplicitTuple
       { et_opos :: !DeltaPos,  et_cpos :: !DeltaPos }
  deriving (Show,Typeable,Eq)

  -- StmtLR
data AnnStmt
  = AnnStmtLR {}
  | AnnLetStmt
      { ls_let :: !(Maybe DeltaPos)
      , ls_in  :: !(Maybe DeltaPos)
      }
  | AnnArithSeq
      { as_ob     :: !DeltaPos
      , as_comma  :: !(Maybe DeltaPos)
      , as_dotdot :: !DeltaPos
      , as_cb     :: !DeltaPos
      }
  deriving (Show,Typeable,Eq)

  -- TyClDecl
  --  Data declarations
data AnnTyClDecl
  = AnnDataDecl { dd_equal :: !DeltaPos }
  | AnnConDecl  { cs_mvbar :: !(Maybe DeltaPos) }
  deriving (Show,Typeable,Eq)

  -- HsType
data AnnHsType
  = AnnHsForAllTy
      { fa_oparen :: !(Maybe DeltaPos)
      , fa_darrow :: !(Maybe DeltaPos)
      , fa_cparen :: !(Maybe DeltaPos)
      }
  | AnnHsFunTy { ft_rarrow :: !DeltaPos }
  | AnnHsParTy { pt_opos :: !DeltaPos,  pt_cpos :: !DeltaPos }
  | AnnHsTupleTy { t_opos :: !DeltaPos,  t_cpos :: !DeltaPos }
  | AnnHsIParamTy { ipt_dcolon :: !DeltaPos }
  | AnnHsEqTy { ipt_tilde :: !DeltaPos }
  | AnnHsKindSig
      { ks_op :: !DeltaPos, ks_dcolon :: !DeltaPos, ks_cp :: !DeltaPos }
  | AnnHsBangTy { b_bang :: !DeltaPos }
  | AnnHsExplicitListTy
       { el_opos :: !DeltaPos,  el_cpos :: !DeltaPos }
  | AnnHsExplicitTupleTy
       { ett_opos :: !DeltaPos,  ett_cpos :: !DeltaPos }
  deriving (Show,Typeable,Eq)

  -- Pat
data AnnPat
  = AnnAsPat { ap_as :: !DeltaPos }
  | AnnTuplePat { tp_opPos :: !DeltaPos, tp_cpPos :: !DeltaPos }
  deriving (Show,Typeable,Eq)

  -- Basics
data AnnListItem
  = AnnListItem { li_comma :: !(Maybe DeltaPos) }
  deriving (Show,Typeable,Eq)

data  AnnNone = AnnNone
  deriving (Show,Typeable,Eq)

instance Show GHC.RdrName where
  show n = "(a RdrName)"


type Anns = (AnnsEP,AnnsUser)

-- | For every @Located a@, use the @SrcSpan@ and TypeRep of a as the
-- key, to store the standard annotation.
type AnnsEP = Map.Map (GHC.SrcSpan,TypeRep) Annotation

-- | For every SrcSpan, store an annotation as a value where the
-- TypeRep is of the item wrapped in the Value
type AnnsUser = Map.Map (GHC.SrcSpan,TypeRep) Value

-- ---------------------------------------------------------------------
{-

Rationale.

We need an offset for every SrcSpan, as well as the comments
associated with it.

We also need optional other annotations for keywords etc.

So perhaps one AnnKey based on the typeRep of a in (Located a), and
another for the user annotation.


-}


type AnnKey = (GHC.SrcSpan, TypeRep)

mkAnnKeyEP :: (Typeable a) => GHC.Located a -> AnnKey
mkAnnKeyEP (GHC.L l a) = (l,typeOf a)

mkAnnKeyV :: GHC.SrcSpan -> Value -> AnnKey
mkAnnKeyV l a = (l,typeOf (Just (typeValue a)))

data Value = forall a . (Eq a, Show a, Typeable a) => Value a

instance Show (Value) where
  show _ = "Value (..)"

newValue :: (Eq a, Show a, Typeable a) => a -> Value
newValue = Value

typeValue :: Value -> TypeRep
typeValue (Value x) = typeOf x

fromValue :: Typeable a => Value -> a
fromValue (Value x) = fromMaybe (error errMsg) $ res
  where
    res = cast x
    errMsg = "fromValue, bad cast from " ++ show (typeOf x)
                ++ " to " ++ show (typeOf res)

getAnnotationEP :: (Typeable a) => AnnsEP -> GHC.Located a -> Maybe Annotation
getAnnotationEP anns (GHC.L span a) = Map.lookup (span, (typeOf a)) anns

-- | Retrieve an annotation based on the SrcSpan of the annotated AST
-- element, and the known type of the annotation.
getAnnotationValue :: (Typeable a) => AnnsUser -> GHC.SrcSpan -> Maybe a
getAnnotationValue anns span = res
  where res = case  Map.lookup (span, (typeOf res)) anns of
                Nothing -> Nothing
                Just d -> Just (fromValue d)

putAnnotationValue :: AnnsUser -> GHC.SrcSpan -> Value -> AnnsUser
putAnnotationValue anns span v
  = Map.insert (mkAnnKeyV span v) v anns
