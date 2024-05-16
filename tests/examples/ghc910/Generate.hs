{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass #-} -- For NFData instances
{-# LANGUAGE DeriveGeneric #-} -- For NFData instances
{-# LANGUAGE StandaloneDeriving #-} -- For Show (ParsingError inp)
{-# LANGUAGE ConstraintKinds #-} -- For Dict
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-} -- For nextInput
{-# LANGUAGE UndecidableInstances #-} -- For Show (ParsingError inp)
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Symantic.Parser.Machine.Generate where

import Control.DeepSeq (NFData(..))
import Control.Monad (Monad(..))
import Control.Monad.ST (ST, runST)
import Data.Bool (Bool(..), otherwise)
import Data.Char (Char)
import Data.Either (Either(..))
import Data.Eq (Eq(..))
import Data.Foldable (foldr, toList, null)
import Data.Function (($), (.), on)
import Data.Functor ((<$>))
import Data.Int (Int)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Map (Map)
import Data.Maybe (Maybe(..))
import Data.Ord (Ord(..), Ordering(..))
import Data.Proxy (Proxy(..))
import Data.Semigroup (Semigroup(..))
import Data.Set (Set)
import Data.String (String)
import Data.Traversable (Traversable(..))
import Data.Tuple (snd)
import Data.Typeable (Typeable)
import Data.Word (Word8)
import GHC.Generics (Generic)
import GHC.Show (showCommaSpace)
import Language.Haskell.TH (CodeQ)
import Prelude ((+), (-), error)
import Text.Show (Show(..), showParen, showString)
import qualified Data.HashMap.Strict as HM
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Internal as Map_
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Set.Internal as Set_
import qualified Data.STRef as ST
import qualified Language.Haskell.TH as TH
import qualified Language.Haskell.TH.Syntax as TH

import qualified Symantic.Data as Sym
import Symantic.Derive
import Symantic.ObserveSharing
import qualified Symantic.Parser.Grammar as Gram
import Symantic.Parser.Grammar.Combinators
  ( UnscopedRegister(..)
  , Exception(..)
  , Failure(..)
  , SomeFailure(..)
  , unSomeFailure
  , inputTokenProxy
  )
import Symantic.Parser.Machine.Input
import Symantic.Parser.Machine.Instructions
import qualified Language.Haskell.TH.HideName as TH
import qualified Symantic.Lang as Prod
import qualified Symantic.Optimize as Prod

--import Debug.Trace

-- | Convenient utility to generate some final 'TH.CodeQ'.
genCode :: Splice a -> CodeQ a
genCode = derive . Prod.normalOrderReduction

-- * Type 'Gen'
-- | Generate the 'CodeQ' parsing the input.
data Gen inp vs a = Gen
  { genAnalysisByLet :: OpenRecs TH.Name GenAnalysis
    -- ^ 'genAnalysis' for each 'defLet' and 'defJoin' of the 'Machine'.
  , genAnalysis :: OpenRec TH.Name GenAnalysis
    -- ^ Synthetized (bottom-up) static genAnalysis of the 'Machine'.
  , unGen :: forall st.
      GenCtx st inp vs a ->
      CodeQ (ST st (Either (ParsingError inp) a))
  }

{-# INLINE returnST #-}
returnST :: forall s a. a -> ST s a
returnST = return @(ST s)

-- | @('generateCode' input mach)@ generates @TemplateHaskell@ code
-- parsing the given 'input' according to the given 'Machine'.
generateCode ::
  -- Not really used constraints,
  -- just to please 'checkHorizon'.
  Ord (InputToken inp) =>
  Show (InputToken inp) =>
  TH.Lift (InputToken inp) =>
  NFData (InputToken inp) =>
  Typeable (InputToken inp) =>
  Inputable inp =>
  Show (Cursor inp) =>
  Gen inp '[] a ->
  CodeQ (inp -> Either (ParsingError inp) a)
generateCode gen =
    let Gen{unGen=k, ..} = checkHorizon gen in
    [|| \(input :: inp) ->
    -- Pattern bindings containing unlifted types
    -- should use an outermost bang pattern.
    let !(# init, readMore, readNext #) = $$(cursorOf [||input||])
        finalRet = \_farInp _farExp v _inp -> returnST $ Right v
        finalRaise :: forall st b. (OnException st inp b)
          = \ !exn _failInp !farInp !farExp ->
          returnST $ Left ParsingError
          { parsingErrorOffset = offset farInp
          , parsingErrorException = exn
          , parsingErrorUnexpected =
              if readMore farInp
              then Just (let (# c, _ #) = readNext farInp in c)
              else Nothing
          , parsingErrorExpecting =
              let (minHoriz, res) =
                    Set.foldr (\f (minH, acc) ->
                      case unSomeFailure f of
                        Just (FailureHorizon h :: Failure (Gram.CombSatisfiable (InputToken inp)))
                          | Just old <- minH -> (Just (min old h), acc)
                          | otherwise -> (Just h, acc)
                        _ -> (minH, f:acc)
                      ) (Nothing, []) farExp in
              Set.fromList $ case minHoriz of
                Just h -> SomeFailure (FailureHorizon @(InputToken inp) h) : res
                Nothing -> res
          }
    in runST $$(
      let
        -- | Defines 'inputTokenProxy' so that the TemplateHaskell code
        -- can refer to @(InputToken inp)@ through it.
        defInputTokenProxy :: TH.CodeQ a -> TH.CodeQ a
        defInputTokenProxy exprCode =
          TH.unsafeCodeCoerce [|
            let $(return (TH.VarP inputTokenProxy)) = Proxy :: Proxy (InputToken inp) in
            $(TH.unTypeQ (TH.examineCode exprCode))
          |]
      in
      defInputTokenProxy $
      k GenCtx
        { valueStack = ValueStackEmpty
        , onExceptionStackByLabel = Map.empty :: Map Exception (NonEmpty (TH.CodeQ (OnException s inp a)))
        , defaultCatch = [||finalRaise||]
        , onReturn = [||finalRet||] :: CodeQ (OnReturn s inp a a)
        , input = [||init||]
        , nextInput = [||readNext||]
        , moreInput = [||readMore||]
        -- , farthestError = [||Nothing||]
        , farthestInput = [||init||]
        , farthestExpecting = [||Set.empty||]
        , checkedHorizon = 0
        , analysisByLet = mutualFix genAnalysisByLet
        }
      )
    ||]

-- ** Type 'ParsingError'
data ParsingError inp
  =  ParsingError
  {  parsingErrorOffset :: Offset
  ,  parsingErrorException :: Exception
     -- | Note: if a 'FailureHorizon' greater than 1
     -- is amongst the 'parsingErrorExpecting'
     -- then 'parsingErrorUnexpected' is only the 'InputToken'
     -- at the begining of the expected 'Horizon'.
  ,  parsingErrorUnexpected :: Maybe (InputToken inp)
  ,  parsingErrorExpecting :: Set SomeFailure
  } deriving (Generic)
deriving instance NFData (InputToken inp) => NFData (ParsingError inp)
--deriving instance Show (InputToken inp) => Show (ParsingError inp)
instance Show (InputToken inp) => Show (ParsingError inp) where
  showsPrec p ParsingError{..} =
    showParen (p >= 11) $
      showString "ParsingErrorStandard {" .
      showString "parsingErrorOffset = " .
      showsPrec 0 parsingErrorOffset .
      showCommaSpace .
      showString "parsingErrorException = " .
      showsPrec 0 parsingErrorException .
      showCommaSpace .
      showString "parsingErrorUnexpected = " .
      showsPrec 0 parsingErrorUnexpected .
      showCommaSpace .
      showString "parsingErrorExpecting = fromList " .
      showsPrec 0 (
        -- Sort on the string representation
        -- because the 'Ord' of the 'SomeFailure'
        -- is based upon hashes ('typeRepFingerprint')
        -- depending on packages' ABI and whether
        -- cabal-install's setup is --inplace or not,
        -- and that would be too unstable for golden tests.
        List.sortBy (compare `on` show) $
        Set.toList parsingErrorExpecting
      ) .
      showString "}"

-- ** Type 'ErrorLabel'
type ErrorLabel = String

-- * Type 'GenAnalysis'
data GenAnalysis = GenAnalysis
  { minReads :: Horizon
    -- ^ The minimun number of input tokens to read
    -- on the current 'input' to reach a success.
  , mayRaise :: Map Exception ()
    -- ^ The 'Exception's that may be raised depending on the 'input'.
  , alwaysRaise :: Set Exception
    -- ^ The 'Exception's raised whatever is or happen to the 'input'.
  , freeRegs :: Set TH.Name
    -- ^ The free registers that are used.
  } deriving (Show)

-- ** Type 'Offset'
type Offset = Int
-- ** Type 'Horizon'
-- | Minimal input length required for a successful parsing.
type Horizon = Offset

-- | Merge given 'GenAnalysis' as sequences.
seqGenAnalysis :: NonEmpty GenAnalysis -> GenAnalysis
seqGenAnalysis aas@(a:|as) = GenAnalysis
  { minReads = List.foldl' (\acc -> (acc +) . minReads) (minReads a) as
  , mayRaise = sconcat (mayRaise <$> aas)
  , alwaysRaise = sconcat (alwaysRaise <$> aas)
  , freeRegs = sconcat (freeRegs <$> aas)
  }
-- | Merge given 'GenAnalysis' as alternatives.
altGenAnalysis :: NonEmpty GenAnalysis -> GenAnalysis
altGenAnalysis aas = GenAnalysis
  { minReads =
      case
        (`NE.filter` aas) $ \a ->
          -- If an alternative 'alwaysRaise's 'ExceptionFailure' whatever its 'input' is,
          -- it __should__ remain semantically the same (up to the exact 'Failure's)
          -- to raise an 'ExceptionFailure' even before knowing
          -- whether that alternative branch will be taken or not,
          -- hence an upstream 'checkHorizon' is allowed to raise an 'ExceptionFailure'
          -- based only upon the 'minReads' of such alternatives:
          Set.toList (alwaysRaise a) /= [ExceptionFailure]
      of
      [] -> 0
      a:as -> List.foldl' (\acc -> min acc . minReads) (minReads a) as
  , mayRaise = sconcat (mayRaise <$> aas)
  , alwaysRaise = foldr Set.intersection Set.empty (alwaysRaise <$> aas)
  , freeRegs = sconcat (freeRegs <$> aas)
  }



{-
-- *** Type 'FarthestError'
data FarthestError inp = FarthestError
  { farthestInput :: Cursor inp
  , farthestExpecting :: [Failure (InputToken inp)]
  }
-}

-- ** Type 'GenCtx'
-- | This is an inherited (top-down) context
-- only present at compile-time, to build TemplateHaskell splices.
data GenCtx st inp vs a =
  ( Cursorable (Cursor inp)
  -- For checkHorizon
  , TH.Lift (InputToken inp)
  , Show (InputToken inp)
  , Eq (InputToken inp)
  , Ord (InputToken inp)
  , Typeable (InputToken inp)
  , NFData (InputToken inp)
  ) => GenCtx
  { valueStack :: ValueStack vs
  , onExceptionStackByLabel :: Map Exception (NonEmpty (CodeQ (OnException st inp a)))
    -- | Default 'OnException' defined at the begining of the generated 'CodeQ',
    -- hence a constant within the 'Gen'eration.
  , defaultCatch :: forall b. CodeQ (OnException st inp b)
  , onReturn :: CodeQ (OnReturn st inp a a)
  , input :: CodeQ (Cursor inp)
  , moreInput :: CodeQ (Cursor inp -> Bool)
  , nextInput :: CodeQ (Cursor inp -> (# InputToken inp, Cursor inp #))
  , farthestInput :: CodeQ (Cursor inp)
  , farthestExpecting :: CodeQ (Set SomeFailure)
    -- | Remaining horizon already checked.
    -- Use to factorize 'input' length checks,
    -- instead of checking the 'input' length
    -- one 'InputToken' at a time at each 'read'.
    -- Updated by 'checkHorizon'
    -- and reset elsewhere when needed.
  , checkedHorizon :: Horizon
  -- | Output of 'mutualFix'.
  , analysisByLet :: LetRecs TH.Name GenAnalysis
  }

-- ** Type 'ValueStack'
data ValueStack vs where
  ValueStackEmpty :: ValueStack '[]
  ValueStackCons ::
    { valueStackHead :: Splice v
    , valueStackTail :: ValueStack vs
    } -> ValueStack (v ': vs)

instance InstrComment Gen where
  comment msg k = k
    { unGen = \ctx -> {-trace "unGen.comment" $-}
      [||
        let _ = $$(liftTypedString $ "comment: "<>msg) in
        $$(unGen k ctx)
      ||]
    }
instance InstrValuable Gen where
  pushValue x k = k
    { unGen = \ctx -> {-trace "unGen.pushValue" $-}
      [||
      let _ = "pushValue" in
      $$(unGen k ctx
        { valueStack = ValueStackCons x (valueStack ctx) })
      ||]
    }
  popValue k = k
    { unGen = \ctx -> {-trace "unGen.popValue" $-}
      [||
      let _ = "popValue" in
      $$(unGen k ctx
        { valueStack = valueStackTail (valueStack ctx) })
      ||]
    }
  lift2Value f k = k
    { unGen = \ctx -> {-trace "unGen.lift2Value" $-}
      [||
      let _ = $$(liftTypedString ("lift2Value checkedHorizon="<>show (checkedHorizon ctx))) in
      $$(unGen k ctx
        { valueStack =
          let ValueStackCons y (ValueStackCons x vs) = valueStack ctx in
          ValueStackCons (f Prod..@ x Prod..@ y) vs
        })
      ||]
    }
  swapValue k = k
    { unGen = \ctx -> {-trace "unGen.swapValue" $-} unGen k ctx
      { valueStack =
          let ValueStackCons y (ValueStackCons x vs) = valueStack ctx in
          ValueStackCons x (ValueStackCons y vs)
      }
    }
instance InstrBranchable Gen where
  caseBranch kx ky = Gen
    { genAnalysisByLet = genAnalysisByLet kx <> genAnalysisByLet ky
    , genAnalysis = \final -> altGenAnalysis $
        genAnalysis kx final :|
        [genAnalysis ky final]
    , unGen = \ctx -> {-trace "unGen.caseBranch" $-}
      let ValueStackCons v vs = valueStack ctx in
      [||
        case $$(genCode v) of
          Left  x -> $$(unGen kx ctx{ valueStack = ValueStackCons (splice [||x||]) vs })
          Right y -> $$(unGen ky ctx{ valueStack = ValueStackCons (splice [||y||]) vs })
      ||]
    }
  choicesBranch bs default_ = Gen
    { genAnalysisByLet = sconcat $ genAnalysisByLet default_ :| (genAnalysisByLet . snd <$> bs)
    , genAnalysis = \final -> altGenAnalysis $
        (\k -> genAnalysis k final)
        <$> (default_:|(snd <$> bs))
    , unGen = \ctx0 -> {-trace "unGen.choicesBranch" $-}
      let ValueStackCons v vs = valueStack ctx0 in
      let ctx = ctx0{valueStack = vs} in
      let
        go x ((p,b):bs') = [||
          if $$(genCode (p Prod..@ x))
          then
            let _ = $$(liftTypedString ("choicesBranch checkedHorizon="<>show (checkedHorizon ctx))) in
            $$({-trace "unGen.choicesBranch.b" $-} unGen b ctx)
          else
            let _ = "choicesBranch.else" in
            $$(go x bs')
          ||]
        go _ _ = unGen default_ ctx
      in go v bs
    }
instance InstrExceptionable Gen where
  raise exn = Gen
    { genAnalysisByLet = HM.empty
    , genAnalysis = \_final -> GenAnalysis
        { minReads = 0
        , mayRaise = Map.singleton (ExceptionLabel exn) ()
        , alwaysRaise = Set.singleton (ExceptionLabel exn)
        , freeRegs = Set.empty
        }
    , unGen = \ctx@GenCtx{} -> {-trace ("unGen.raise: "<>show exn) $-} [||
        $$(raiseException ctx (ExceptionLabel exn))
          (ExceptionLabel $$(TH.liftTyped exn))
          {-failInp-}$$(input ctx)
          {-farInp-}$$(input ctx)
          $$(farthestExpecting ctx)
      ||]
    }
  fail fs = Gen
    { genAnalysisByLet = HM.empty
    , genAnalysis = \_final -> GenAnalysis
        { minReads = 0
        , mayRaise = Map.singleton ExceptionFailure ()
        , alwaysRaise = Set.singleton ExceptionFailure
        , freeRegs = Set.empty
        }
    , unGen = \ctx@GenCtx{} -> {-trace ("unGen.fail: "<>show exn) $-}
      if null fs
      then [|| -- Raise without updating the farthest error.
          $$(raiseException ctx ExceptionFailure)
            ExceptionFailure
            {-failInp-}$$(input ctx)
            $$(farthestInput ctx)
            $$(farthestExpecting ctx)
        ||]
      else raiseFailure ctx [||fs||]
    }
  commit exn k = k
    { unGen = \ctx -> {-trace ("unGen.commit: "<>show exn) $-}
      [||
      let _ = "commit" in
      $$(unGen k ctx{onExceptionStackByLabel =
        Map.update (\case
            _r0:|(r1:rs) -> Just (r1:|rs)
            _ -> Nothing
          )
        exn (onExceptionStackByLabel ctx)
      })
      ||]
    }
  catch exn k onExn = Gen
    { genAnalysisByLet = genAnalysisByLet k <> genAnalysisByLet onExn
    , genAnalysis = \final ->
        let kAnalysis = genAnalysis k final in
        let onExnAnalysis = genAnalysis onExn final in
        altGenAnalysis $
          kAnalysis
            { mayRaise = Map.delete exn (mayRaise kAnalysis)
            , alwaysRaise = Set.delete exn (alwaysRaise kAnalysis)
            } :|
          [ onExnAnalysis ]
    , unGen = \ctx@GenCtx{} -> {-trace ("unGen.catch: "<>show exn) $-} [||
        let _ = $$(liftTypedString ("catch "<>show exn<>" checkedHorizon="<>show (checkedHorizon ctx))) in
        let onException = $$(onExceptionCode (input ctx) (checkedHorizon ctx) onExn ctx) in
        $$(unGen k ctx
        { onExceptionStackByLabel =
            Map.insertWith (<>) exn
              (NE.singleton [||onException||])
              (onExceptionStackByLabel ctx)
        }
      ) ||]
    }
-- ** Class 'SpliceInputable'
-- | Record an 'input' and a 'checkedHorizon' together
-- to be able to put both of them on the 'valueStack',
-- and having them moved together by operations
-- on the 'valueStack' (eg. 'lift2Value').
-- Used by 'saveInput' and 'loadInput'.
class SpliceInputable repr where
  inputSave :: CodeQ inp -> Horizon -> repr inp
data instance Sym.Data SpliceInputable repr a where
  InputSave :: CodeQ inp -> Horizon -> Sym.Data SpliceInputable repr inp
instance SpliceInputable (Sym.Data SpliceInputable repr) where
  inputSave = InputSave
instance SpliceInputable repr => SpliceInputable (Sym.SomeData repr) where
  inputSave inp = Sym.SomeData . InputSave inp
instance SpliceInputable TH.CodeQ where
  inputSave inp _hor = inp
instance SpliceInputable repr => Derivable (Sym.Data SpliceInputable repr) where
  derive = \case
    InputSave inp hor -> inputSave inp hor
instance InstrInputable Gen where
  saveInput k = k
    { unGen = \ctx ->
        {-trace "unGen.saveInput" $-}
        [||
        let _ = $$(liftTypedString $ "saveInput checkedHorizon="<>show (checkedHorizon ctx)) in
        $$(unGen k ctx
          { valueStack = inputSave (input ctx) (checkedHorizon ctx) `ValueStackCons` valueStack ctx
          })
        ||]
    }
  loadInput k = k
    { unGen = \ctx@GenCtx{} ->
        {-trace "unGen.loadInput" $-}
        let ValueStackCons v vs = valueStack ctx in
        let (input, checkedHorizon) = case v of
              Sym.Data (InputSave i h) -> (i, h)
              -- This case should never happen if 'saveInput' is used.
              i -> (genCode i, 0) in
        [||
        let _ = $$(liftTypedString $ "loadInput checkedHorizon="<>show checkedHorizon) in
        $$(unGen (checkHorizon k) ctx
          { valueStack = vs
          , input
          , checkedHorizon
          })
        ||]
    , genAnalysis = \final ->
        let analysis = genAnalysis k final in
        -- The input is reset and thus any previous 'checkHorizon'
        -- cannot check after this 'loadInput'.
        analysis{minReads = 0}
    }
instance InstrCallable Gen where
  defLet defs k = k
    { unGen = \ctx@GenCtx{} ->
        {-trace ("unGen.defLet: defs="<>show (HM.keys defs)) $-}
        TH.unsafeCodeCoerce $ do
          decls <- traverse (makeDecl ctx) (HM.toList defs)
          body <- TH.unTypeQ $ TH.examineCode $
            {-trace "unGen.defLet.body" $-}
            unGen k ctx
          return $ TH.LetE (
            -- | Use 'List.sortBy' to output more deterministic code
            -- to be able to golden test it, at the cost of more computations
            -- (at compile-time only though).
            List.sortBy (compare `on` TH.hideName) $
            toList decls
            ) body
    , genAnalysisByLet =
        HM.unions
          $ genAnalysisByLet k
          : ((\(SomeLet sub) -> genAnalysis sub) <$> defs)
          : ((\(SomeLet sub) -> genAnalysisByLet sub) <$> HM.elems defs)
    }
    where
    makeDecl ctx (subName, SomeLet sub) = do
      let subAnalysis = analysisByLet ctx HM.! subName
      body <- takeFreeRegs (freeRegs subAnalysis) $
        TH.unTypeQ $ TH.examineCode $ [|| -- buildRec in Parsley
        -- Called by 'call' or 'jump'.
        \ !callerOnReturn{-from onReturnCode-}
          !callerInput
          !callerOnExceptionStackByLabel{- 'onExceptionStackByLabel' from the 'call'-site -} ->
          $$({-trace ("unGen.defLet.sub: "<>show subName) $-} unGen sub ctx
            { valueStack = ValueStackEmpty
            -- Build a 'onExceptionStackByLabel' for the 'mayRaise' of the subroutine,
            -- where each 'OnException' calls the one passed by the 'call'-site (in 'callerOnExceptionStackByLabel').
            -- Note that currently the 'call'-site can supply in 'callerOnExceptionStackByLabel'
            -- a subset of the 'mayRaise' needed by this subroutine,
            -- because 'Map.findWithDefault' is used instead of 'Map.!'.
            , onExceptionStackByLabel = Map.mapWithKey
                (\lbl () -> NE.singleton [||Map.findWithDefault $$(defaultCatch ctx) lbl callerOnExceptionStackByLabel||])
                ({-trace ("mayRaise: "<>show subName) $-}
                  mayRaise subAnalysis)
            , input = [||callerInput||]
            , onReturn = {-trace ("unGen.defLet.sub.onReturn: "<>show subName) $-} [||callerOnReturn||]

            -- These are passed by the caller via 'callerOnReturn' or 'ko'
            -- , farthestInput =
            -- , farthestExpecting =

            -- Some callers can call this declaration
            -- with zero 'checkedHorizon', hence use this minimum.
            -- TODO: maybe it could be improved a bit
            -- by taking the minimum of the checked horizons
            -- before all the 'call's and 'jump's to this declaration.
            , checkedHorizon = 0
            })
        ||]
      let decl = TH.FunD subName [TH.Clause [] (TH.NormalB body) []]
      return decl
  jump isRec (LetName subName) = Gen
    { genAnalysisByLet = HM.empty
    , genAnalysis = \final ->
        if isRec
        then GenAnalysis
          { minReads = 0
          , mayRaise = Map.empty
          , alwaysRaise = Set.empty
          , freeRegs = Set.empty
          }
        else final HM.! subName
    , unGen = \ctx -> {-trace ("unGen.jump: "<>show subName) $-}
      let subAnalysis = analysisByLet ctx HM.! subName in
      [||
      let _ = "jump" in
      $$(TH.unsafeCodeCoerce $
        giveFreeRegs (freeRegs subAnalysis) $
        return (TH.VarE subName))
        {-ok-}$$(onReturn ctx)
        $$(input ctx)
        $$(liftTypedRaiseByLabel $
          onExceptionStackByLabel ctx
          -- Pass only the labels raised by the 'defLet'.
          `Map.intersection`
          (mayRaise subAnalysis)
        )
      ||]
    }
  call isRec (LetName subName) k = k
    { genAnalysis = \final ->
        if isRec
        then GenAnalysis
          { minReads = 0
          , mayRaise = Map.empty
          , alwaysRaise = Set.empty
          , freeRegs = Set.empty
          }
        else seqGenAnalysis $ (final HM.! subName) :| [ genAnalysis k final ]
    , unGen = {-trace ("unGen.call: "<>show subName) $-} \ctx ->
      -- let ks = (Map.keys (onExceptionStackByLabel ctx)) in
      let subAnalysis = analysisByLet ctx HM.! subName in
      [||
      -- let _ = $$(liftTypedString $ "call exceptByLet("<>show subName<>")="<>show (Map.keys (Map.findWithDefault Map.empty subName (exceptByLet ctx))) <> " onExceptionStackByLabel(ctx)="<> show ks) in
      $$(TH.unsafeCodeCoerce $
        giveFreeRegs (freeRegs subAnalysis) $
        return (TH.VarE subName))
        {-ok-}$$(onReturnCode k ctx)
        $$(input ctx)
        $$(liftTypedRaiseByLabel $
          -- FIXME: maybe it should rather pass all the 'mayRaise' of 'subName'
          -- and 'defaultCatch' be removed from 'makeDecl''s 'onExceptionStackByLabel'.
          onExceptionStackByLabel ctx
          -- Pass only the labels raised by the 'defLet'.
          `Map.intersection`
          (mayRaise subAnalysis)
        )
      ||]
    }
  ret = Gen
    { genAnalysisByLet = HM.empty
    , genAnalysis = \_final -> GenAnalysis
        { minReads = 0
        , mayRaise = Map.empty
        , alwaysRaise = Set.empty
        , freeRegs = Set.empty
        }
    , unGen = \ctx -> {-trace "unGen.ret" $-}
      {-trace "unGen.ret.returnCode" $-}
      returnCode ({-trace "unGen.ret.onReturn" $-} onReturn ctx) ctx
    }

takeFreeRegs :: TH.Quote m => Set TH.Name -> m TH.Exp -> m TH.Exp
takeFreeRegs frs k = go (Set.toList frs)
  where
  go [] = k
  go (r:rs) = [| \ $(return (TH.VarP r)) -> $(go rs) |]

giveFreeRegs :: TH.Quote m => Set TH.Name -> m TH.Exp -> m TH.Exp
giveFreeRegs frs k = go (Set.toList frs)
  where
  go [] = k
  go (r:rs) = [| $(go rs) $(return (TH.VarE r)) |]

-- | Like 'TH.liftString' but on 'TH.Code'.
-- Useful to get a 'TH.StringL' instead of a 'TH.ListE'.
liftTypedString :: String -> TH.Code TH.Q a
liftTypedString = TH.unsafeCodeCoerce . TH.liftString

-- | Like 'TH.liftTyped' but adjusted to work on 'onExceptionStackByLabel'
-- which already contains 'CodeQ' terms.
-- Moreover, only the 'OnException' at the top of the stack
-- is needed and thus generated in the resulting 'CodeQ'.
--
-- TODO: Use an 'Array' instead of a 'Map'?
liftTypedRaiseByLabel :: TH.Lift k => Map k (NonEmpty (CodeQ a)) -> CodeQ (Map k a)
liftTypedRaiseByLabel Map_.Tip = [|| Map_.Tip ||]
liftTypedRaiseByLabel (Map_.Bin s k (h:|_hs) l r) =
  [|| Map_.Bin s k $$h $$(liftTypedRaiseByLabel l) $$(liftTypedRaiseByLabel r) ||]

instance TH.Lift a => TH.Lift (Set a) where
  liftTyped Set_.Tip = [|| Set_.Tip ||]
  liftTyped (Set_.Bin s a l r) = [|| Set_.Bin $$(TH.liftTyped s) $$(TH.liftTyped a) $$(TH.liftTyped l) $$(TH.liftTyped r) ||]

-- ** Type 'OnReturn'
-- | A continuation generated by 'onReturnCode' and later called by 'returnCode'.
type OnReturn st inp v a =
  {-farthestInput-}Cursor inp ->
  {-farthestExpecting-}Set SomeFailure ->
  v ->
  Cursor inp ->
  ST st (Either (ParsingError inp) a)

-- | Generate an 'OnReturn' continuation to be called with 'returnCode'.
-- Used when 'call' 'ret'urns.
-- The return 'v'alue is 'pushValue'-ed on the 'valueStack'.
onReturnCode ::
  {-k-}Gen inp (v ': vs) a ->
  GenCtx st inp vs a ->
  CodeQ (OnReturn st inp v a)
onReturnCode k ctx = [||
  let _ = $$(liftTypedString $ "onReturn") in
  \farInp farExp v !inp ->
    $$({-trace "unGen.onReturnCode" $-} unGen k ctx
      { valueStack = ValueStackCons ({-trace "unGen.onReturnCode.value" $-} splice [||v||]) (valueStack ctx)
      , input = [||inp||]
      , farthestInput = [||farInp||]
      , farthestExpecting = [||farExp||]
      , checkedHorizon = 0
      }
    )
  ||]

-- | Generate a call to the 'onReturnCode' continuation.
-- Used when 'call' 'ret'urns.
returnCode ::
  CodeQ (OnReturn st inp v a) ->
  GenCtx st inp (v ': vs) a ->
  CodeQ (ST st (Either (ParsingError inp) a))
returnCode k = \ctx -> {-trace "returnCode" $-} [||
  let _ = "resume" in
  $$k
    $$(farthestInput ctx)
    $$(farthestExpecting ctx)
    (let _ = "resume.genCode" in $$({-trace "returnCode.genCode" $-}
      genCode $ valueStackHead $ valueStack ctx))
    $$(input ctx)
  ||]

-- ** Type 'OnException'
-- | A continuation generated by 'catch' and later called by 'raise' or 'fail'.
type OnException st inp a =
  Exception ->
  {-failInp-}Cursor inp ->
  {-farInp-}Cursor inp ->
  {-farExp-}Set SomeFailure ->
  ST st (Either (ParsingError inp) a)

-- TODO: some static infos should be attached to 'OnException'
-- to avoid comparing inputs when they're the same
-- and to improve 'checkedHorizon'.
onExceptionCode ::
  CodeQ (Cursor inp) -> Horizon ->
  Gen inp (Cursor inp : vs) a ->
  GenCtx st inp vs a -> TH.CodeQ (OnException st inp a)
onExceptionCode resetInput resetCheckedHorizon k ctx = [||
  let _ = $$(liftTypedString $ "onException") in
  \ !_exn !failInp !farInp !farExp ->
    $$(unGen k ctx
      -- Push 'input' and 'checkedHorizon'
      -- as they were when entering the 'catch' or 'iter',
      -- they will be available to 'loadInput', if any.
      { valueStack = inputSave resetInput resetCheckedHorizon
                     `ValueStackCons` valueStack ctx
      -- Note that 'onExceptionStackByLabel' is reset.
      -- Move the input to the failing position.
      , input = [||failInp||]
      -- The 'checkedHorizon' at the 'raise's are not known here.
      -- Nor whether 'failInp' is after 'checkedHorizon' or not.
      -- Hence fallback to a safe value.
      , checkedHorizon = 0
      -- Set those to the farthest error computed in 'raiseFailure'.
      , farthestInput = [||farInp||]
      , farthestExpecting = [||farExp||]
      })
  ||]

instance InstrJoinable Gen where
  defJoin (LetName n) sub k = k
    { unGen = \ctx ->
        {-trace ("unGen.defJoin: "<>show n) $-}
        TH.unsafeCodeCoerce [|
          let $(return (TH.VarP n)) = $(TH.unTypeQ $ TH.examineCode [||
                -- Called by 'returnCode'.
                \farInp farExp v !inp ->
                  $$({-trace ("unGen.defJoin.next: "<>show n) $-} unGen sub ctx
                    { valueStack = ValueStackCons (splice [||v||]) (valueStack ctx)
                    , input = [||inp||]
                    , farthestInput = [||farInp||]
                    , farthestExpecting = [||farExp||]
                    , checkedHorizon = 0
                    {- FIXME:
                    , onExceptionStackByLabel = Map.mapWithKey
                        (\lbl () -> NE.singleton [||koByLabel Map.! lbl||])
                        (mayRaise sub raiseLabelsByLetButSub)
                    -}
                    })
                ||])
          in $(TH.unTypeQ $ TH.examineCode $
            {-trace ("unGen.defJoin.expr: "<>show n) $-}
            unGen k ctx)
        |]
    , genAnalysisByLet =
        (genAnalysisByLet sub <>) $
        HM.insert n (genAnalysis sub) $
        genAnalysisByLet k
    }
  refJoin (LetName n) = Gen
    { unGen = \ctx ->
        {-trace ("unGen.refJoin: "<>show n) $-}
        returnCode
          (TH.unsafeCodeCoerce (return (TH.VarE n))) ctx
    , genAnalysisByLet = HM.empty
    , genAnalysis = \final ->
        HM.findWithDefault
          (error (show (n,HM.keys final)))
          n final
    }
instance InstrReadable Char Gen where
  read fs p = checkHorizon . checkToken fs p
instance InstrReadable Word8 Gen where
  read fs p = checkHorizon . checkToken fs p
instance InstrIterable Gen where
  iter (LetName loopJump) loop done = Gen
    { genAnalysisByLet = HM.unions
        [ -- No need to give 'freeRegs' when 'call'ing 'loopJump'
          -- because they're passed when 'call'ing 'iter'.
          -- This avoids to passing those registers around.
          HM.singleton loopJump (\final -> (genAnalysis loop final){freeRegs = Set.empty})
        , genAnalysisByLet loop
        , genAnalysisByLet done
        ]
    , genAnalysis = \final ->
      let loopAnalysis = genAnalysis loop final in
      let doneAnalysis = genAnalysis done final in
      GenAnalysis
        { minReads = minReads doneAnalysis
        , mayRaise =
            Map.delete ExceptionFailure (mayRaise loopAnalysis) <>
            mayRaise doneAnalysis
        , alwaysRaise =
            Set.delete ExceptionFailure (alwaysRaise loopAnalysis) <>
            alwaysRaise doneAnalysis
        , freeRegs = freeRegs loopAnalysis <> freeRegs doneAnalysis
        }
    , unGen = \ctx -> TH.unsafeCodeCoerce [|
        let _ = "iter" in
        let
          onException loopInput = $(TH.unTypeCode $ onExceptionCode
            (TH.unsafeCodeCoerce [|loopInput|]) 0 done ctx)
          $(return $ TH.VarP loopJump) = \_callerOnReturn callerInput callerOnExceptionStackByLabel ->
            $(TH.unTypeCode $ unGen loop ctx
              { valueStack = ValueStackEmpty
              , onExceptionStackByLabel =
                  Map.insertWith (<>) ExceptionFailure
                    (NE.singleton $ TH.unsafeCodeCoerce [|onException callerInput|])
                    (onExceptionStackByLabel ctx)
              , input = TH.unsafeCodeCoerce [|callerInput|]
              -- FIXME: promote to compile time error?
              , onReturn = TH.unsafeCodeCoerce [|error "invalid onReturn"|]
              , checkedHorizon = 0
              })
        in $(TH.unTypeCode $ unGen (jump True (LetName loopJump)) ctx{valueStack=ValueStackEmpty})
       |]
    }
instance InstrRegisterable Gen where
  newRegister (UnscopedRegister r) k = k
    { genAnalysis = \final ->
      let analysis = genAnalysis k final in
      analysis{freeRegs = Set.delete r $ freeRegs analysis}
    , unGen = \ctx ->
      let ValueStackCons v vs = valueStack ctx in
      TH.unsafeCodeCoerce [|
      do
        let dupv = $(TH.unTypeCode $ genCode v)
        $(return (TH.VarP r)) <- ST.newSTRef dupv
        $(TH.unTypeCode $ unGen k ctx{valueStack=vs})
      |]
    }
  readRegister (UnscopedRegister r) k = k
    { genAnalysis = \final ->
      let analysis = genAnalysis k final in
      analysis{freeRegs = Set.insert r $ freeRegs analysis}
    , unGen = \ctx -> [|| do
        sr <- ST.readSTRef $$(TH.unsafeCodeCoerce (return (TH.VarE r)))
        $$(unGen k ctx{valueStack=ValueStackCons (splice [||sr||]) (valueStack ctx)})
      ||]
    }
  writeRegister (UnscopedRegister r) k = k
    { genAnalysis = \final ->
      let analysis = genAnalysis k final in
      analysis{freeRegs = Set.insert r $ freeRegs analysis}
    , unGen = \ctx ->
      let ValueStackCons v vs = valueStack ctx in
      [|| do
        let dupv = $$(genCode v)
        ST.writeSTRef $$(TH.unsafeCodeCoerce (return (TH.VarE r))) dupv
        $$(unGen k ctx{valueStack=vs})
      ||]
    }

checkHorizon ::
  forall inp vs a.
  -- Those constraints are not used anyway
  -- because (TH.Lift SomeFailure) uses 'inputTokenProxy'.
  Ord (InputToken inp) =>
  Show (InputToken inp) =>
  TH.Lift (InputToken inp) =>
  NFData (InputToken inp) =>
  Typeable (InputToken inp) =>
  {-ok-}Gen inp vs a ->
  Gen inp vs a
checkHorizon ok = ok
  { genAnalysis = \final -> seqGenAnalysis $
      GenAnalysis { minReads = 0
                  , mayRaise = Map.singleton ExceptionFailure ()
                  , alwaysRaise = Set.empty
                  , freeRegs = Set.empty
                  } :|
      [ genAnalysis ok final ]
  , unGen = \ctx0@GenCtx{} ->
    if checkedHorizon ctx0 >= 1
    then
      [||
        let _ = $$(liftTypedString $ "checkHorizon.oldCheck: checkedHorizon="<>show (checkedHorizon ctx0)) in
        $$(unGen ok ctx0{checkedHorizon = checkedHorizon ctx0 - 1})
      ||]
    else
      let minHoriz = minReads $ genAnalysis ok $ analysisByLet ctx0 in
      if minHoriz == 0
      then
        [||
          let _ = "checkHorizon.noCheck" in
          $$(unGen ok ctx0)
        ||]
      else
        [||
          let inp = $$(input ctx0) in
          --let partialCont inp =
                -- Factorize generated code for raising the "fail".
                let readFail = $$(raiseException ctx0{input=[||inp||]} ExceptionFailure) in
                $$(
                  let ctx = ctx0
                        { onExceptionStackByLabel =
                            Map.adjust (\(_r:|rs) -> [||readFail||] :| rs)
                              ExceptionFailure (onExceptionStackByLabel ctx0)
                        , input = [||inp||]
                        } in
                  [||
                    let _ = $$(liftTypedString $ "checkHorizon.newCheck: checkedHorizon="<>show (checkedHorizon ctx)<>" minHoriz="<>show minHoriz) in
                    if $$(moreInput ctx)
                         $$(if minHoriz > 1
                           then [||$$shiftRight $$(TH.liftTyped (minHoriz - 1)) inp||]
                           else [||inp||])
                    then $$(unGen ok ctx{checkedHorizon = minHoriz})
                    else
                      let _ = $$(liftTypedString $ "checkHorizon.newCheck.fail") in
                      -- TODO: return a resuming continuation (like attoparsec's Partial)
                      -- This could be done with a Buffer for efficient backtracking:
                      -- http://www.serpentine.com/blog/2014/05/31/attoparsec/
                      $$(unGen (fail (Set.singleton $ SomeFailure $ FailureHorizon @(InputToken inp) minHoriz)) ctx)
                  ||]
                )
          --in partialCont $$(input ctx0)
        ||]
  }

-- | @('raiseFailure' ctx fs)@ raises 'ExceptionFailure'
-- with farthest parameters set to or updated with @(fs)@
-- according to the relative position of 'input' wrt. 'farthestInput'.
raiseFailure ::
  Cursorable (Cursor inp) =>
  GenCtx st inp cs a ->
  TH.CodeQ (Set SomeFailure) ->
  TH.CodeQ (ST st (Either (ParsingError inp) a))
raiseFailure ctx fs = [||
  let failExp = $$fs in
  let (# farInp, farExp #) =
        case $$compareOffset $$(farthestInput ctx) $$(input ctx) of
          LT -> (# $$(input ctx), failExp #)
          EQ -> (# $$(farthestInput ctx), failExp <> $$(farthestExpecting ctx) #)
          GT -> (# $$(farthestInput ctx), $$(farthestExpecting ctx) #)
  in $$(raiseException ctx ExceptionFailure)
    ExceptionFailure
    {-failInp-}$$(input ctx) farInp farExp
  ||]
-- | @('raiseException' ctx exn)@ raises exception @(exn)@
-- using any entry in 'onExceptionStackByLabel', or 'defaultCatch' if none.
raiseException ::
  GenCtx st inp vs a -> Exception ->
  CodeQ (Exception -> Cursor inp -> Cursor inp -> Set SomeFailure -> ST st (Either (ParsingError inp) a))
raiseException ctx exn =
  NE.head $ Map.findWithDefault
    (NE.singleton (defaultCatch ctx))
    exn (onExceptionStackByLabel ctx)

checkToken ::
  Set SomeFailure ->
  {-predicate-}Splice (InputToken inp -> Bool) ->
  {-ok-}Gen inp (InputToken inp ': vs) a ->
  Gen inp vs a
checkToken fs p ok = ok
  { genAnalysis = \final -> seqGenAnalysis $
      GenAnalysis { minReads = 1
                  , mayRaise = Map.singleton ExceptionFailure ()
                  , alwaysRaise = Set.empty
                  , freeRegs = Set.empty
                  } :|
      [ genAnalysis ok final ]
  , unGen = \ctx -> {-trace "unGen.read" $-} [||
    let _ = "checkToken" in
    let !(# c, cs #) = $$(nextInput ctx) $$(input ctx) in
    $$(genCode $
      Prod.ifThenElse
        (p Prod..@ splice [||c||])
        (splice $ unGen ok ctx
          { valueStack = ValueStackCons (splice [||c||]) (valueStack ctx)
          , input = [||cs||]
          })
        (splice [||
          let _ = "checkToken.fail" in
          $$(unGen (fail fs) ctx)
        ||])
    )||]
  }

