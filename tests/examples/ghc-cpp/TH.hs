{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

{-|
Module:      Data.Aeson.TypeScript.TH
Copyright:   (c) 2022 Tom McLaughlin
License:     BSD3
Stability:   experimental
Portability: portable

This library provides a way to generate TypeScript @.d.ts@ files that match your existing Aeson 'A.ToJSON' instances.
If you already use Aeson's Template Haskell support to derive your instances, then deriving TypeScript is as simple as

@
$('deriveTypeScript' myAesonOptions ''MyType)
@

For example,

@
data D a = Nullary
         | Unary Int
         | Product String Char a
         | Record { testOne   :: Double
                  , testTwo   :: Bool
                  , testThree :: D a
                  } deriving Eq
@

Next we derive the necessary instances.

@
$('deriveTypeScript' ('defaultOptions' {'fieldLabelModifier' = 'drop' 4, 'constructorTagModifier' = map toLower}) ''D)
@

Now we can use the newly created instances.

@
>>> putStrLn $ 'formatTSDeclarations' $ 'getTypeScriptDeclarations' (Proxy :: Proxy (D T))

type D\<T\> = INullary\<T\> | IUnary\<T\> | IProduct\<T\> | IRecord\<T\>;

interface INullary\<T\> {
  tag: "nullary";
}

interface IUnary\<T\> {
  tag: "unary";
  contents: number;
}

interface IProduct\<T\> {
  tag: "product";
  contents: [string, string, T];
}

interface IRecord\<T\> {
  tag: "record";
  One: number;
  Two: boolean;
  Three: D\<T\>;
}
@

It's important to make sure your JSON and TypeScript are being derived with the same options. For this reason, we
include the convenience 'HasJSONOptions' typeclass, which lets you write the options only once, like this:

@
instance HasJSONOptions MyType where getJSONOptions _ = ('defaultOptions' {'fieldLabelModifier' = 'drop' 4})

$('deriveJSON' ('getJSONOptions' (Proxy :: Proxy MyType)) ''MyType)
$('deriveTypeScript' ('getJSONOptions' (Proxy :: Proxy MyType)) ''MyType)
@

Or, if you want to be even more concise and don't mind defining the instances in the same file,

@
myOptions = 'defaultOptions' {'fieldLabelModifier' = 'drop' 4}

$('deriveJSONAndTypeScript' myOptions ''MyType)
@

Remembering that the Template Haskell 'Q' monad is an ordinary monad, you can derive instances for several types at once like this:

@
$('mconcat' \<$\> 'traverse' ('deriveJSONAndTypeScript' myOptions) [''MyType1, ''MyType2, ''MyType3])
@

Once you've defined all necessary instances, you can write a main function to dump them out into a @.d.ts@ file. For example:

@
main = putStrLn $ 'formatTSDeclarations' (
  ('getTypeScriptDeclarations' (Proxy :: Proxy MyType1)) <>
  ('getTypeScriptDeclarations' (Proxy :: Proxy MyType2)) <>
  ...
)
@

-}

module Data.Aeson.TypeScript.TH (
  deriveTypeScript
  , deriveTypeScript'
  , deriveTypeScriptLookupType

  -- * The main typeclass
  , TypeScript(..)
  , TSType(..)

  , TSDeclaration(TSRawDeclaration)

  -- * Formatting declarations
  , formatTSDeclarations
  , formatTSDeclarations'
  , formatTSDeclaration
  , FormattingOptions(..)
  , defaultFormattingOptions
  , defaultNameFormatter
  , SumTypeFormat(..)
  , ExportMode(..)

  -- * Advanced options
  , defaultExtraTypeScriptOptions
  , keyType
  , typeFamiliesToMapToTypeScript
  , ExtraTypeScriptOptions

  -- * Convenience tools
  , HasJSONOptions(..)
  , deriveJSONAndTypeScript
  , deriveJSONAndTypeScript'

  , T(..)
  , T1(..)
  , T2(..)
  , T3(..)
  , T4(..)
  , T5(..)
  , T6(..)
  , T7(..)
  , T8(..)
  , T9(..)
  , T10(..)

  , module Data.Aeson.TypeScript.Instances
  ) where

import Control.Monad
import Control.Monad.Writer
import Data.Aeson as A
import Data.Aeson.TH as A
import Data.Aeson.TypeScript.Formatting
import Data.Aeson.TypeScript.Instances ()
import Data.Aeson.TypeScript.Lookup
import Data.Aeson.TypeScript.Transform
import Data.Aeson.TypeScript.TypeManipulation
import Data.Aeson.TypeScript.Types
import Data.Aeson.TypeScript.Util
import qualified Data.List as L
import Data.Maybe
import Data.Proxy
import Data.String.Interpolate
import Language.Haskell.TH hiding (stringE)
import Language.Haskell.TH.Datatype
import qualified Language.Haskell.TH.Lib as TH

#if !MIN_VERSION_base(4,11,0)
import Data.Monoid
#endif

-- | Generates a 'TypeScript' instance declaration for the given data type.
deriveTypeScript' :: Options
                  -- ^ Encoding options.
                  -> Name
                  -- ^ Name of the type for which to generate a 'TypeScript' instance declaration.
                  -> ExtraTypeScriptOptions
                  -- ^ Extra options to control advanced features.
                  -> Q [Dec]
deriveTypeScript' options name extraOptions = do
  datatypeInfo' <- reifyDatatype name
  assertExtensionsTurnedOn datatypeInfo'

  -- Figure out what the generic variables are
  let eligibleGenericVars = catMaybes $ flip fmap (getDataTypeVars datatypeInfo') $ \case
        SigT (VarT n) StarT -> Just n
        _ -> Nothing
  let varsAndTVars = case eligibleGenericVars of
        [] -> []
        [x] -> [(x, "T")]
        xs -> zip xs allStarConstructors''
  genericVariablesAndSuffixes <- forM varsAndTVars $ \(var, tvar) -> do
    (_, genericInfos) <- runWriterT $ forM_ (datatypeCons datatypeInfo') $ \ci ->
      forM_ (namesAndTypes options [] ci) $ \(_, _, typ) -> do
        searchForConstraints extraOptions typ var
    return (var, (unifyGenericVariable genericInfos, tvar))

  -- Plug in generic variables and de-family-ify
  ((\x -> (datatypeInfo' { datatypeCons = x })) -> dti, extraDeclsOrGenericInfosInitial) <- runWriterT $ forM (datatypeCons datatypeInfo') $ \ci ->
    ((\x -> ci { constructorFields = x }) <$>) $ forM (constructorFields ci) $
      transformTypeFamilies extraOptions . mapType genericVariablesAndSuffixes

  -- Build constraints: a TypeScript constraint for every constructor type and one for every type variable.
  -- Probably overkill/not exactly right, but it's a start.
  let constructorPreds :: [Pred] = [AppT (ConT ''TypeScript) x | x <- mconcat $ fmap constructorFields (datatypeCons dti)
                                                               , hasFreeTypeVariable x
                                                               , not $ coveredByDataTypeVars (getDataTypeVars dti) x
                                                               ]
  let constructorPreds' :: [Pred] = [AppT (ConT ''TypeScript) x | x <- mconcat $ fmap constructorFields (datatypeCons datatypeInfo')
                                                                , hasFreeTypeVariable x
                                                                , not $ coveredByDataTypeVars (getDataTypeVars dti) x
                                                                ]
  let typeVariablePreds :: [Pred] = [AppT (ConT ''TypeScript) x | x <- getDataTypeVars dti]

  -- Build the declarations
  (types, (extraDeclsOrGenericInfosInitial <>) -> extraDeclsOrGenericInfos) <- runWriterT $ mapM (handleConstructor extraOptions options dti genericVariablesAndSuffixes) (datatypeCons dti)
  typeDeclaration <- [|TSTypeAlternatives $(TH.stringE $ getTypeName (datatypeName dti))
                                          $(genericVariablesListExpr True genericVariablesAndSuffixes)
                                          $(listE $ fmap return types)
                                          $(tryGetDoc (haddockModifier extraOptions) (datatypeName dti))|]

  declarationsFunctionBody <- [| $(return typeDeclaration) : $(listE (fmap return [x | ExtraDecl x <- extraDeclsOrGenericInfos])) |]

  -- Couldn't figure out how to put the constraints for "instance TypeScript..." in the quasiquote above without
  -- introducing () when the constraints are empty, which causes "illegal tuple constraint" unless the user enables ConstraintKinds.
  -- So, just use our mkInstance function
  getTypeScriptTypeExp <- [|$(TH.stringE $ getTypeName (datatypeName dti)) <> $(getBracketsExpressionAllTypesNoSuffix genericVariablesAndSuffixes)|]
  getParentTypesExp <- listE [ [|TSType (Proxy :: Proxy $(return t))|]
                             | t <- (mconcat $ fmap constructorFields (datatypeCons datatypeInfo')) <> [x | ExtraParentType x <- extraDeclsOrGenericInfos]]
  let predicates = L.nub (constructorPreds <> constructorPreds' <> typeVariablePreds <> [x | ExtraConstraint x <- extraDeclsOrGenericInfos])
  keyTypeDecl <- case keyType extraOptions of
    Nothing -> return []
    Just kt -> do
      keyTypeExp <- [|$(TH.stringE kt)|]
      return $ [FunD 'getTypeScriptKeyType [Clause [WildP] (NormalB keyTypeExp) []]]
  let inst = [mkInstance predicates (AppT (ConT ''TypeScript) (foldl AppT (ConT name) (getDataTypeVars dti))) ([
                 FunD 'getTypeScriptType [Clause [WildP] (NormalB getTypeScriptTypeExp) []]
                 , FunD 'getTypeScriptDeclarations [Clause [WildP] (NormalB declarationsFunctionBody) []]
                 , FunD 'getParentTypes [Clause [WildP] (NormalB getParentTypesExp) []]
                 ] <> keyTypeDecl)]
  return (mconcat [x | ExtraTopLevelDecs x <- extraDeclsOrGenericInfos] <> inst)

-- | Return a string to go in the top-level type declaration, plus an optional expression containing a declaration
handleConstructor :: ExtraTypeScriptOptions -> Options -> DatatypeInfo -> [(Name, (Suffix, Var))] -> ConstructorInfo -> WriterT [ExtraDeclOrGenericInfo] Q Exp
handleConstructor (ExtraTypeScriptOptions {..}) options (DatatypeInfo {..}) genericVariables ci = do
  if | (length datatypeCons == 1) && not (getTagSingleConstructors options) -> do
         writeSingleConstructorEncoding
         brackets <- lift $ getBracketsExpression False genericVariables
         lift [|$(TH.stringE interfaceName) <> $(return brackets)|]
     | allConstructorsAreNullary datatypeCons && allNullaryToStringTag options -> stringEncoding

     -- With UntaggedValue, nullary constructors are encoded as strings
     | (isUntaggedValue $ sumEncoding options) && isConstructorNullary ci -> stringEncoding

     -- Treat as a sum
     | isObjectWithSingleField $ sumEncoding options -> do
         writeSingleConstructorEncoding
         brackets <- lift $ getBracketsExpression False genericVariables
         lift [|"{" <> $(TH.stringE $ show $ constructorNameToUse options ci) <> ": " <> $(TH.stringE interfaceName) <> $(return brackets) <> "}"|]
     | isTwoElemArray $ sumEncoding options -> do
         writeSingleConstructorEncoding
         brackets <- lift $ getBracketsExpression False genericVariables
         lift [|"[" <> $(TH.stringE $ show $ constructorNameToUse options ci) <> ", " <> $(TH.stringE interfaceName) <> $(return brackets) <> "]"|]
     | isUntaggedValue $ sumEncoding options -> do
         writeSingleConstructorEncoding
         brackets <- lift $ getBracketsExpression False genericVariables
         lift [|$(TH.stringE interfaceName) <> $(return brackets)|]
     | otherwise -> do
         tagField :: [Exp] <- lift $ case sumEncoding options of
           TaggedObject tagFieldName _ -> (: []) <$> [|TSField False $(TH.stringE tagFieldName) $(TH.stringE [i|"#{constructorNameToUse options ci}"|]) Nothing|]
           _ -> return []

         tsFields <- getTSFields
         decl <- lift $ assembleInterfaceDeclaration (ListE (tagField ++ tsFields))
         tell [ExtraDecl decl]
         brackets <- lift $ getBracketsExpression False genericVariables
         lift [|$(TH.stringE interfaceName) <> $(return brackets)|]

  where
    stringEncoding = lift $ TH.stringE [i|"#{(constructorTagModifier options) $ getTypeName (constructorName ci)}"|]

    writeSingleConstructorEncoding = if
      | constructorVariant ci == NormalConstructor -> do
          encoding <- tupleEncoding
          tell [ExtraDecl encoding]

#if MIN_VERSION_aeson(0,10,0)
      | unwrapUnaryRecords options && (isSingleRecordConstructor ci) -> do
          let [typ] = constructorFields ci
          stringExp <- lift $ case typ of
            (AppT (ConT name) t) | name == ''Maybe && not (omitNothingFields options) -> [|$(getTypeAsStringExp t) <> " | null"|]
            _ -> getTypeAsStringExp typ
          alternatives <- lift [|TSTypeAlternatives $(TH.stringE interfaceName)
                                                    $(genericVariablesListExpr True genericVariables)
                                                    [$(return stringExp)]
                                                    $(tryGetDoc haddockModifier (constructorName ci))|]
          tell [ExtraDecl alternatives]
#endif

      | otherwise -> do
          tsFields <- getTSFields
          decl <- lift $ assembleInterfaceDeclaration (ListE tsFields)
          tell [ExtraDecl decl]

    -- * Type declaration to use
    interfaceName = "I" <> (lastNameComponent' $ constructorName ci)

    tupleEncoding =
      lift [|TSTypeAlternatives $(TH.stringE interfaceName)
                                $(genericVariablesListExpr True genericVariables)
                                [getTypeScriptType (Proxy :: Proxy $(return (contentsTupleTypeSubstituted genericVariables ci)))]
                                $(tryGetDoc haddockModifier (constructorName ci))|]

    assembleInterfaceDeclaration members = [|TSInterfaceDeclaration $(TH.stringE interfaceName)
                                                                    $(genericVariablesListExpr True genericVariables)
                                                                    $(return members)
                                                                    $(tryGetDoc haddockModifier (constructorName ci))|]

    getTSFields :: WriterT [ExtraDeclOrGenericInfo] Q [Exp]
    getTSFields = forM (namesAndTypes options genericVariables ci) $ \(name, nameString, typ) -> do
      (fieldTyp, optAsBool) <- lift $ case typ of
        (AppT (ConT name) t) | name == ''Maybe && not (omitNothingFields options) ->
          ( , ) <$> [|$(getTypeAsStringExp t) <> " | null"|] <*> getOptionalAsBoolExp t
        _ -> ( , ) <$> getTypeAsStringExp typ <*> getOptionalAsBoolExp typ

      lift [| TSField $(return optAsBool) $(TH.stringE nameString) $(return fieldTyp) $(tryGetDoc haddockModifier name) |]

    isSingleRecordConstructor (constructorVariant -> RecordConstructor [_]) = True
    isSingleRecordConstructor _ = False

-- * Convenience functions

-- | Convenience function to generate 'A.ToJSON', 'A.FromJSON', and 'TypeScript' instances simultaneously, so the instances are guaranteed to be in sync.
--
-- This function is given mainly as an illustration.
-- If you want some other permutation of instances, such as 'A.ToJSON' and 'A.TypeScript' only, just take a look at the source and write your own version.
--
-- @since 0.1.0.4
deriveJSONAndTypeScript :: Options
                        -- ^ Encoding options.
                        -> Name
                        -- ^ Name of the type for which to generate 'A.ToJSON', 'A.FromJSON', and 'TypeScript' instance declarations.
                        -> Q [Dec]
deriveJSONAndTypeScript options name = (<>) <$> (deriveTypeScript options name) <*> (A.deriveJSON options name)

deriveJSONAndTypeScript' :: Options
                         -- ^ Encoding options.
                         -> Name
                         -- ^ Name of the type for which to generate 'A.ToJSON', 'A.FromJSON', and 'TypeScript' instance declarations.
                         -> ExtraTypeScriptOptions
                         -- ^ Extra options to control advanced features.
                         -> Q [Dec]
deriveJSONAndTypeScript' options name extraOptions = (<>) <$> (deriveTypeScript' options name extraOptions) <*> (A.deriveJSON options name)

-- | Generates a 'TypeScript' instance declaration for the given data type.
deriveTypeScript :: Options
                 -- ^ Encoding options.
                 -> Name
                 -- ^ Name of the type for which to generate a 'TypeScript' instance declaration.
                 -> Q [Dec]
deriveTypeScript options name = deriveTypeScript' options name defaultExtraTypeScriptOptions

