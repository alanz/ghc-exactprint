{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Sum where

-- base
import GHC.Generics
import Data.Typeable
-- aeson
import Data.Aeson (FromJSON, ToJSON)

-- containers
import qualified Data.Map as Map

-- hspec
import Test.Hspec

-- QuickCheck
import Test.QuickCheck hiding (Result, Success)

-- quickcheck-arbitrary-adt
import Test.QuickCheck.Arbitrary.ADT
import Test.Aeson.Internal.ADT.GoldenSpecs

-- servant
import Servant.API

-- ocaml-export
import OCaml.Export hiding (mkGoldenFiles)
import Util


type SumPackage
  = OCamlPackage "sum" NoDependency :>
       (OCamlModule '["OnOrOff"] :> OnOrOff
  :<|> OCamlModule '["NameOrIdNumber"] :> NameOrIdNumber
  :<|> OCamlModule '["SumVariant"] :> SumVariant
  :<|> OCamlModule '["WithTuple"] :> WithTuple
  :<|> OCamlModule '["SumWithRecord"] :> SumWithRecord -- :> SumWithRecordMixed
  :<|> OCamlModule '["Result"] :> Result TypeParameterRef0 TypeParameterRef1 -- :> ComplexResult TypeParameterRef0 TypeParameterRef1 TypeParameterRef2
  :<|> OCamlModule '["NewType"] :> NewType)

compareInterfaceFiles :: FilePath -> SpecWith ()
compareInterfaceFiles = compareFiles "test/interface" "sum" True

mkGolden :: forall a. (ToADTArbitrary a, ToJSON a) => Proxy a -> IO ()
mkGolden Proxy = mkGoldenFileForType 10 (Proxy :: Proxy a) "test/interface/golden/golden/sum"

mkGoldenFiles :: IO ()
mkGoldenFiles = do
  mkGolden (Proxy :: Proxy OnOrOff)
  mkGolden (Proxy :: Proxy NameOrIdNumber)
  mkGolden (Proxy :: Proxy SumVariant)
  mkGolden (Proxy :: Proxy WithTuple)
  mkGolden (Proxy :: Proxy SumWithRecord)
  mkGolden (Proxy :: Proxy (Result TypeParameterRef0 TypeParameterRef1))
  mkGolden (Proxy :: Proxy NewType)

spec :: Spec
spec = do
  runIO mkGoldenFiles
  runGoldenSpec (Proxy :: Proxy SumPackage) 10 "test/interface/golden/golden/sum"

  let dir = "test/interface/temp"

  -- create spec to be tested against servant
  runIO $
    mkPackage
     (Proxy :: Proxy SumPackage)
     (PackageOptions dir "sum" Map.empty True $
        Just $ SpecOptions
          "__tests__/sum-servant"
          "golden/sum"
          (Just "http://localhost:8082"))

  -- create spec to be tested against files only
  runIO $
    mkPackage
     (Proxy :: Proxy SumPackage)
     (PackageOptions dir "sum" Map.empty True $
        Just $ SpecOptions
          "__tests__/sum"
          "golden/sum"
          Nothing)

  describe "OCaml Declaration with Interface: Sum Types" $ do
    compareInterfaceFiles "OnOrOff"
    compareInterfaceFiles "NameOrIdNumber"
    compareInterfaceFiles "SumVariant"
    compareInterfaceFiles "WithTuple"
    compareInterfaceFiles "SumWithRecord"
    compareInterfaceFiles "Result"
    compareInterfaceFiles "NewType"

data OnOrOff = On | Off
  deriving (Show,Eq,Generic,OCamlType,ToJSON,FromJSON)

instance Arbitrary OnOrOff where
  arbitrary = elements [On, Off]

instance ToADTArbitrary OnOrOff

data NameOrIdNumber = Name String | IdNumber Int
  deriving (Show, Eq, Generic, OCamlType,ToJSON,FromJSON)

instance Arbitrary NameOrIdNumber where
  arbitrary = oneof [Name <$> arbitrary, IdNumber <$> arbitrary]

instance ToADTArbitrary NameOrIdNumber

data Result a b
  = Success a
  | Error b
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance Arbitrary (Result TypeParameterRef0 TypeParameterRef1) where
  arbitrary = oneof [Success <$> arbitrary, Error <$> arbitrary]

instance ToADTArbitrary (Result TypeParameterRef0 TypeParameterRef1)

instance (Typeable a, OCamlType a, Typeable b, OCamlType b) => (OCamlType (Result a b))

data ComplexResult a b c
  = CR0 a
  | CR1 a b
  | CR2 b (c,a)
  | CR3 String b Int a
  | CR4 { cr4b :: b, cr4ac :: (a,c) }
  | CR5
  deriving (Show, Eq, Generic, ToJSON, FromJSON)

instance Arbitrary (ComplexResult TypeParameterRef0 TypeParameterRef1 TypeParameterRef2) where
  arbitrary =
    oneof
      [ CR0 <$> arbitrary
      , CR1 <$> arbitrary <*> arbitrary
      , CR2 <$> arbitrary <*> arbitrary
      , CR3 <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
      , CR4 <$> arbitrary <*> arbitrary
      , pure CR5
      ]

instance ToADTArbitrary (ComplexResult TypeParameterRef0 TypeParameterRef1 TypeParameterRef2)

instance (Typeable a, OCamlType a, Typeable b, OCamlType b, Typeable c, OCamlType c) => (OCamlType (ComplexResult a b c))

data SumVariant
  = HasNothing
  | HasSingleInt Int
  | HasSingleTuple (Int,Int)
  | HasMultipleInts Int Int
  | HasMultipleTuples (Int,Int) (Int,Int)
  | HasMixed Int String Double
  | HasNameOrIdNumber NameOrIdNumber Int
  deriving (Show,Eq,Generic, OCamlType, ToJSON, FromJSON)

instance Arbitrary SumVariant where
  arbitrary =
    oneof
      [ pure HasNothing
      , HasSingleInt <$> arbitrary
      , HasSingleTuple <$> arbitrary
      , HasMultipleInts <$> arbitrary <*> arbitrary
      , HasMultipleTuples <$> arbitrary <*> arbitrary
      , HasMixed <$> arbitrary <*> arbitrary <*> arbitrary
      , HasNameOrIdNumber <$> arbitrary <*> arbitrary
      ]

instance ToADTArbitrary SumVariant


type Tuple
  = (Int,Int)

data WithTuple = WithTuple Tuple
  deriving (Show,Eq,Generic, OCamlType, ToJSON, FromJSON)

instance Arbitrary WithTuple where
  arbitrary = WithTuple <$> arbitrary

instance ToADTArbitrary WithTuple

data SumWithRecord
  = A1 {a1 :: Int}
  | B2 {b2 :: String, b3 :: Int}
  deriving (Show,Eq,Generic, OCamlType, ToJSON, FromJSON)

instance Arbitrary SumWithRecord where
  arbitrary =
    oneof
      [ A1 <$> arbitrary
      , B2 <$> arbitrary <*> arbitrary
      ]

instance ToADTArbitrary SumWithRecord

data SumWithRecordMixed
  = SRM1 {srm1 :: Int}
  | SRM2
  | SRM3 {srm2 :: String, srm3 :: Float}
  | SRM4 Int (String, Double)
  deriving (Show,Eq,Generic, OCamlType, ToJSON, FromJSON)

instance Arbitrary SumWithRecordMixed where
  arbitrary =
    oneof
      [ SRM1 <$> arbitrary
      , pure SRM2
      , SRM3 <$> arbitrary <*> arbitrary
      , SRM4 <$> arbitrary <*> arbitrary
      ]

instance ToADTArbitrary SumWithRecordMixed


newtype NewType
  = NewType Int
  deriving (Show,Eq,Generic,OCamlType, ToJSON, FromJSON)

instance Arbitrary NewType where
  arbitrary = NewType <$> arbitrary

instance ToADTArbitrary NewType


{-
introduce Enumerator
extra type is made
but anything coming after the enumerator is broken




λ> toOCamlType (Proxy :: Proxy (ComplexResult TypeParameterRef0 TypeParameterRef1 TypeParameterRef2))

OCamlDatatype (HaskellTypeMetaData "ComplexResult" "Sum" "main") "ComplexResult" (OCamlValueConstructor (MultipleConstructors [MultipleConstructors [NamedConstructor "CR0" (OCamlTypeParameterRef "a0"),MultipleConstructors [NamedConstructor "CR1" (Values (OCamlTypeParameterRef "a0") (OCamlTypeParameterRef "a1")),NamedConstructor "CR2" (Values (OCamlTypeParameterRef "a1") (OCamlPrimitiveRef (OTuple2 (OCamlDatatype (HaskellTypeMetaData "a2" "OCaml.BuckleScript.Types" "ocaml-export") "a2" (OCamlValueConstructor (NamedConstructor "a2" (OCamlTypeParameterRef "a2")))) (OCamlDatatype (HaskellTypeMetaData "a0" "OCaml.BuckleScript.Types" "ocaml-export") "a0" (OCamlValueConstructor (NamedConstructor "a0" (OCamlTypeParameterRef "a0")))))))]],MultipleConstructors [NamedConstructor "CR3" (Values (Values (OCamlPrimitiveRef (OList (OCamlPrimitive OChar))) (OCamlTypeParameterRef "a1")) (Values (OCamlPrimitiveRef OInt) (OCamlTypeParameterRef "a0"))),MultipleConstructors [RecordConstructor "CR4" (Values (OCamlField "cr4b" (OCamlTypeParameterRef "a1")) (OCamlField "cr4ac" (OCamlPrimitiveRef (OTuple2 (OCamlDatatype (HaskellTypeMetaData "a0" "OCaml.BuckleScript.Types" "ocaml-export") "a0" (OCamlValueConstructor (NamedConstructor "a0" (OCamlTypeParameterRef "a0")))) (OCamlDatatype (HaskellTypeMetaData "a2" "OCaml.BuckleScript.Types" "ocaml-export") "a2" (OCamlValueConstructor (NamedConstructor "a2" (OCamlTypeParameterRef "a2")))))))),NamedConstructor "CR5" OCamlEmpty]]]))

λ> toOCamlType (Proxy :: Proxy SumWithRecord)

OCamlDatatype (HaskellTypeMetaData "SumWithRecord" "Sum" "main") "SumWithRecord" (OCamlSumOfRecordConstructor "SumWithRecord" (MultipleConstructors [RecordConstructor "A1" (OCamlField "a1" (OCamlPrimitiveRef OInt)),RecordConstructor "B2" (Values (OCamlField "b2" (OCamlPrimitiveRef (OList (OCamlPrimitive OChar)))) (OCamlField "b3" (OCamlPrimitiveRef OInt)))]))

-}

