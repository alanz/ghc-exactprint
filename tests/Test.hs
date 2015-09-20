{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
-- | Use "runhaskell Setup.hs test" or "cabal test" to run these tests.
module Main where

import Language.Haskell.GHC.ExactPrint.Utils (showGhc)

import qualified FastString     as GHC
import qualified GHC            as GHC

-- import qualified Data.Generics as SYB
-- import qualified GHC.SYB.Utils as SYB

import Control.Monad
import System.Directory
import System.FilePath
import System.IO
import System.Exit

import Data.List

import System.IO.Silently

import Test.Common
import Test.Transform

import Test.HUnit


-- import Debug.Trace

-- ---------------------------------------------------------------------

main :: IO ()
main = hSilence [stderr] $ do
  cnts <- fst <$> runTestText (putTextToHandle stdout True) tests
  putStrLn $ show cnts
  if errors cnts > 0 || failures cnts > 0
     then exitFailure
     else return () -- exitSuccess

transform :: IO ()
transform = hSilence [stderr] $ do
  cnts <- fst <$> runTestText (putTextToHandle stdout True) (TestList transformTests)
  putStrLn $ show cnts
  if errors cnts > 0 || failures cnts > 0
     then exitFailure
     else return () -- exitSuccess

-- ---------------------------------------------------------------------

tests :: Test
tests = TestList $
  [
    mkTestMod "AddAndOr3.hs"             "AddAndOr3"
  , mkTestMod "AltsSemis.hs"             "Main"
  , mkTestMod "Ann01.hs"                 "Ann01"
  , mkTestMod "Annotations.hs"           "Annotations"
  , mkTestMod "Arrow.hs"                 "Arrow"
  , mkParserTest "Arrows.hs"
  , mkTestMod "Associated.hs"            "Main"
  , mkTestMod "B.hs"                     "Main"
  , mkTestMod "C.hs"                     "C"
  , mkTestMod "BCase.hs"                 "Main"
  , mkTestMod "BangPatterns.hs"          "Main"
  , mkTestMod "Cg008.hs"                 "Cg008"
  , mkTestMod "DataDecl.hs"              "Main"
  , mkTestMod "DataFamilies.hs"          "DataFamilies"
  , mkTestMod "Dead1.hs"                 "Dead1"
  , mkTestMod "Default.hs"               "Main"
  , mkTestMod "Deriving.hs"              "Main"
  , mkParserTest "DerivingOC.hs"
  , mkTestMod "DocDecls.hs"              "DocDecls"
  , mkTestMod "DocDecls.hs"              "DocDecls"
  , mkTestMod "EmptyMostly.hs"           "EmptyMostly"
  , mkTestMod "EmptyMostlyInst.hs"       "EmptyMostlyInst"
  , mkTestMod "EmptyMostlyNoSemis.hs"    "EmptyMostlyNoSemis"
  , mkTestMod "Existential.hs"           "Main"
  , mkTestMod "ExprPragmas.hs"           "ExprPragmas"
  , mkTestMod "ExtraConstraints1.hs"     "ExtraConstraints1"
  , mkTestMod "ForAll.hs"                "ForAll"
  , mkTestMod "ForeignDecl.hs"           "ForeignDecl"
  , mkTestMod "FromUtils.hs"             "Main"
  , mkTestMod "FunDeps.hs"               "Main"
  , mkTestMod "FunctionalDeps.hs"        "Main"
  , mkTestMod "GenericDeriving.hs"       "Main"
  , mkTestMod "Guards.hs"                "Main"
  , mkTestMod "HsDo.hs"                  "HsDo"
  , mkTestMod "IfThenElse1.hs"           "Main"
  , mkTestMod "IfThenElse2.hs"           "Main"
  , mkTestMod "IfThenElse3.hs"           "Main"
  , mkTestMod "ImplicitParams.hs"        "Main"
  , mkTestMod "ImportsSemi.hs"           "ImportsSemi"
  , mkTestMod "Infix.hs"                 "Main"
  , mkTestMod "LayoutIn1.hs"             "LayoutIn1"
  , mkTestMod "LayoutIn3.hs"             "LayoutIn3"
  , mkTestMod "LayoutIn3a.hs"            "LayoutIn3a"
  , mkTestMod "LayoutIn3b.hs"            "LayoutIn3b"
  , mkTestMod "LayoutIn4.hs"             "LayoutIn4"
  , mkTestMod "LayoutLet.hs"             "Main"
  , mkTestMod "LayoutLet2.hs"            "LayoutLet2"
  , mkTestMod "LayoutLet3.hs"            "LayoutLet3"
  , mkTestMod "LayoutLet4.hs"            "LayoutLet4"
  , mkTestMod "LayoutWhere.hs"           "Main"
  , mkTestMod "LetExpr.hs"               "LetExpr"
  , mkTestMod "LetExprSemi.hs"           "LetExprSemi"
  , mkTestMod "LetIn1.hs"                "LetIn1"
  , mkTestMod "LetStmt.hs"               "Layout.LetStmt"
  , mkTestMod "ListComprehensions.hs"    "Main"
  , mkTestMod "LocToName.hs"             "LocToName"
  , mkTestMod "Mixed.hs"                 "Main"
  , mkTestMod "MonadComprehensions.hs"   "Main"
  , mkTestMod "Move1.hs"                 "Move1"
  , mkTestMod "MultiParamTypeClasses.hs" "Main"
  , mkTestMod "NullaryTypeClasses.hs"    "Main"
  , mkTestMod "OverloadedStrings.hs"     "Main"
  , mkTestMod "PArr.hs"                  "PArr"
  , mkTestMod "PatSynBind.hs"            "Main"
  , mkTestMod "ParensAroundContext.hs"   "ParensAroundContext"
  , mkTestMod "RankNTypes.hs"            "Main"
  , mkTestMod "RdrNames.hs"              "RdrNames"
  , mkTestMod "RebindableSyntax.hs"      "Main"
  , mkTestMod "RecordUpdate.hs"          "Main"
  , mkParserTest "RecursiveDo.hs"
  , mkTestMod "Roles.hs"                 "Roles"
  , mkTestMod "Rules.hs"                 "Rules"
  , mkTestMod "ScopedTypeVariables.hs"   "Main"
  , mkTestMod "Sigs.hs"                  "Sigs"
  , mkTestMod "Simple.hs"                "Main"
  , mkTestMod "Splice.hs"                "Splice"
  , mkTestMod "StaticPointers.hs"        "Main"
  , mkTestMod "Stmts.hs"                 "Stmts"
  , mkTestMod "Stream.hs"                "Stream"
  , mkTestMod "StrictLet.hs"             "Main"
  , mkTestMod "T2388.hs"                 "T2388"
  , mkTestMod "T3132.hs"                 "T3132"
  , mkTestMod "TH.hs"                    "Main"
  , mkTestMod "Trit.hs"                  "Trit"
  , mkTestMod "TransformListComp.hs"     "Main"
  , mkTestMod "Tuple.hs"                 "Main"
  , mkTestMod "TypeFamilies.hs"          "Main"
  , mkTestMod "TypeOperators.hs"         "Main"
  , mkTestMod "Utils2.hs"                "Utils2"
  , mkTestMod "Vect.hs"                  "Vect"
  , mkTestMod "ViewPatterns.hs"          "Main"
  , mkTestMod "Warning.hs"               "Warning"
  , mkTestMod "WhereIn4.hs"              "WhereIn4"
  , mkTestMod "Zipper.hs"                "Zipper"
  , mkTestMod "QuasiQuote.hs"            "QuasiQuote"
  , mkTestMod "Pseudonym.hs"             "Main"
  , mkTestMod "Obscure.hs"               "Main"
  , mkTestMod "Remorse.hs"               "Main"
  , mkTestMod "Jon.hs"                   "Main"
  , mkTestMod "RSA.hs"                   "Main"
  , mkTestMod "WhereIn3.hs"              "WhereIn3"
  , mkTestMod "Backquote.hs"             "Main"
  , mkTestMod "PatternGuards.hs"         "Main"
  , mkParserTest "Minimal.hs"
  , mkParserTest "Undefined2.hs"
  , mkParserTest "Undefined3.hs"
  , mkParserTest "Undefined4.hs"
  , mkParserTest "Undefined5.hs"
  , mkParserTest "Undefined6.hs"
  , mkParserTest "Undefined7.hs"
  , mkParserTest "Undefined8.hs"
  , mkParserTest "Undefined9.hs"
  , mkParserTest "Undefined10.hs"
  , mkParserTest "Undefined11.hs"
  , mkParserTest "Undefined13.hs"
  , mkParserTest "TypeSynOperator.hs"
  , mkParserTest "TemplateHaskell.hs"
  , mkParserTest "TypeBrackets.hs"
  , mkParserTest "SlidingDoClause.hs"
  , mkParserTest "SlidingListComp.hs"
  , mkParserTest "LiftedConstructors.hs"
  , mkParserTest "LambdaCase.hs"
  , mkParserTest "PuncFunctions.hs"
  , mkParserTest "TupleSections.hs"
  , mkParserTest "TypeSynParens.hs"
  , mkParserTest "SlidingRecordSetter.hs"
  , mkParserTest "MultiLineCommentWithPragmas.hs"
  , mkParserTest "GHCOrig.hs"
  , mkParserTest "DoubleForall.hs"
  , mkParserTest "AnnPackageName.hs"
  , mkParserTest "NestedLambda.hs"
  , mkParserTest "DefaultTypeInstance.hs"
  , mkParserTest "RecordWildcard.hs"
  , mkParserTest "MagicHash.hs"
  , mkParserTest "GADTRecords.hs"
  , mkParserTest "MangledSemiLet.hs"
  , mkParserTest "MultiImplicitParams.hs"
  , mkParserTest "UnicodeSyntaxFailure.hs"

  , mkParserTest "HangingRecord.hs"
  , mkParserTest "InfixPatternSynonyms.hs"
  , mkParserTest "LiftedInfixConstructor.hs"
  , mkParserTest "MultiWayIf.hs"
  , mkParserTest "OptSig.hs"
  , mkParserTest "StrangeTypeClass.hs"
  , mkParserTest "TypeSignatureParens.hs"
  , mkParserTest "Cpp.hs"

  , mkParserTest "Shebang.hs"
  , mkParserTest "PatSigBind.hs"
  , mkParserTest "ProcNotation.hs"
  , mkParserTest "DroppedDoSpace.hs"
  , mkParserTest "IndentedDo.hs"
  , mkParserTest "BracesSemiDataDecl.hs"
  , mkParserTest "SpacesSplice.hs"
  , mkParserTest "SemiWorkout.hs"
  , mkParserTest "ShiftingLambda.hs"
  , mkParserTest "NestedDoLambda.hs"
  , mkParserTest "DoPatBind.hs"

  , mkParserTest "LinePragma.hs"
  , mkParserTest "Hang.hs"

  , mkParserTest "HashQQ.hs"
  , mkParserTest "TypeBrackets2.hs"
  , mkParserTest "ExplicitNamespaces.hs"
  , mkParserTest "CorePragma.hs"
  , mkParserTest "GADTContext.hs"
  , mkParserTest "THMonadInstance.hs"
--  , mkParserTest "TypeBrackets3.hs" --  I think this test is junk but it parses?
  , mkParserTest "TypeBrackets4.hs"
  , mkParserTest "SlidingTypeSyn.hs"
  , mkParserTest "RecordSemi.hs"
  , mkParserTest "SlidingLambda.hs"
  , mkParserTest "DroppedComma.hs"
  , mkParserTest "TypeInstance.hs"
  , mkParserTest "ImplicitTypeSyn.hs"
  , mkParserTest "OveridingPrimitives.hs"
  , mkParserTest "SlidingDataClassDecl.hs"
  , mkParserTest "SemiInstance.hs"
  , mkParserTest "ImplicitSemi.hs"
  , mkParserTest "RulesSemi.hs"
  , mkParserTest "InlineSemi.hs"
  , mkParserTest "SpliceSemi.hs"
  , mkParserTest "Imports.hs"
  , mkParserTest "Internals.hs"
  , mkParserTest "Control.hs"
  , mkParserTest "T10196.hs"
  , mkParserTest "StringGap.hs"
  , mkParserTest "RedundantDo.hs"
  , mkParserTest "TypeSignature.hs"
  ]

  ++ transformTests

  ++ failingTests


-- Tests that will fail until https://phabricator.haskell.org/D907 lands in a
-- future GHC
failingTests :: [Test]
failingTests =
  [
  -- Require current master #10313 / Phab:D907
    mkTestModBad "Deprecation.hs"            "Deprecation"
  , mkTestModBad "MultiLineWarningPragma.hs" "Main"
  , mkTestModBad "UnicodeRules.hs"           "Main"

  -- Tests requiring future GHC modifications
  , mkTestModBad "UnicodeSyntax.hs"          "Tutorial"
  , mkTestModBad "InfixOperator.hs"          "Main"
  ]


mkParserTest :: FilePath -> Test
mkParserTest fp =
  let basename       = "tests" </> "examples" </> fp
      writeFailure   = writeFile (basename <.> "out")
      writeHsPP      = writeFile (basename <.> "hspp")
      writeIncons s  = writeFile (basename <.> "incons") (showGhc s)
  in
    TestCase (do r <- either (\(ParseFailure _ s) -> error s) id
                        <$> roundTripTest ("tests" </> "examples" </> fp)
                 writeFailure (debugTxt r)
                 forM_ (inconsistent r) writeIncons
                 forM_ (cppStatus r) writeHsPP
                 assertBool fp (status r == Success))



mkTestMod :: FilePath -> String -> Test
mkTestMod fileName _modName
  =  mkParserTest fileName


-- ---------------------------------------------------------------------

formatTT :: ([([Char], Bool)], [([Char], Bool)]) -> IO ()
formatTT (ts, fs) = do
  when (not . null $ tail ts) (do
    putStrLn "Pass"
    mapM_ (putStrLn . fst) (tail ts)
    )
  when (not . null $ fs) (do
    putStrLn "Fail"
    mapM_ (putStrLn . fst) fs)

tt' :: IO ()
tt' = formatTT =<< partition snd <$> sequence [ return ("", True)
    -- , manipulateAstTestWFname "ExprPragmas.hs"           "ExprPragmas"
    -- , manipulateAstTestWFname "MonadComprehensions.hs"   "Main"
    -- , manipulateAstTestWFname "RecursiveDo.hs"           "Main"
    -- , manipulateAstTestWFname "MultiParamTypeClasses.hs" "Main"
    -- , manipulateAstTestWFname "DataFamilies.hs"          "DataFamilies"
    -- , manipulateAstTestWFname "Deriving.hs"              "Main"
    -- , manipulateAstTestWFname "Default.hs"               "Main"
    -- , manipulateAstTestWFname "ForeignDecl.hs"           "ForeignDecl"
    -- , manipulateAstTestWFname "Warning.hs"               "Warning"
    -- , manipulateAstTestWFname "Annotations.hs"           "Annotations"
    -- -- , manipulateAstTestWFnameTH "QuasiQuote.hs"          "QuasiQuote"
    -- , manipulateAstTestWFname "Roles.hs"                 "Roles"
    -- , manipulateAstTestWFname "ImportsSemi.hs"           "ImportsSemi"
    -- , manipulateAstTestWFname "Stmts.hs"                 "Stmts"
    -- , manipulateAstTestWFname "Mixed.hs"                 "Main"
    -- , manipulateAstTestWFname "Arrow.hs"                 "Arrow"
    -- , manipulateAstTestWFname "PatSynBind.hs"            "Main"
    -- , manipulateAstTestWFname "HsDo.hs"                  "HsDo"
    -- , manipulateAstTestWFname "ForAll.hs"                "ForAll"
    -- , manipulateAstTestWFname "BangPatterns.hs"          "Main"
    -- , manipulateAstTestWFname "Move1.hs"                 "Move1"
    -- , manipulateAstTestWFname "TypeOperators.hs"         "Main"
    -- , manipulateAstTestWFname "NullaryTypeClasses.hs"    "Main"
    -- , manipulateAstTestWFname "FunctionalDeps.hs"        "Main"
    -- , manipulateAstTestWFname "DerivingOC.hs"            "Main"
    -- , manipulateAstTestWFname "GenericDeriving.hs"       "Main"
    -- , manipulateAstTestWFname "OverloadedStrings.hs"     "Main"
    -- , manipulateAstTestWFname "RankNTypes.hs"            "Main"
    -- , manipulateAstTestWFname "Arrows.hs"                "Main"
    -- , manipulateAstTestWFname "TH.hs"                    "Main"
    -- , manipulateAstTestWFname "StaticPointers.hs"        "Main"
    -- , manipulateAstTestWFname "Guards.hs"                "Main"
    -- , manipulateAstTestWFname "Vect.hs"                  "Vect"
    -- , manipulateAstTestWFname "Tuple.hs"                 "Main"
    -- , manipulateAstTestWFname "ExtraConstraints1.hs"     "ExtraConstraints1"
    -- , manipulateAstTestWFname "AddAndOr3.hs"             "AddAndOr3"
    -- , manipulateAstTestWFname "Ann01.hs"                 "Ann01"
    -- , manipulateAstTestWFname "StrictLet.hs"             "Main"
    -- , manipulateAstTestWFname "Cg008.hs"                 "Cg008"
    -- , manipulateAstTestWFname "T2388.hs"                 "T2388"
    -- , manipulateAstTestWFname "T3132.hs"                 "T3132"
    -- , manipulateAstTestWFname "Stream.hs"                "Stream"
    -- , manipulateAstTestWFname "Trit.hs"                  "Trit"
    -- , manipulateAstTestWFname "Zipper.hs"                "Zipper"
    -- , manipulateAstTestWFname "Sigs.hs"                  "Sigs"
    -- , manipulateAstTestWFname "Utils2.hs"                "Utils2"
    -- , manipulateAstTestWFname "EmptyMostlyInst.hs"       "EmptyMostlyInst"
    -- , manipulateAstTestWFname "EmptyMostlyNoSemis.hs"    "EmptyMostlyNoSemis"
    -- , manipulateAstTestWFname "EmptyMostly.hs"           "EmptyMostly"
    -- , manipulateAstTestWFname "FromUtils.hs"             "Main"
    -- , manipulateAstTestWFname "DocDecls.hs"              "DocDecls"
    -- , manipulateAstTestWFname "RecordUpdate.hs"          "Main"
    -- -- manipulateAstTestWFname "Unicode.hs"               "Main"
    -- , manipulateAstTestWFname "B.hs"                     "Main"
    -- , manipulateAstTestWFname "LayoutWhere.hs"           "Main"
    -- , manipulateAstTestWFname "Deprecation.hs"           "Deprecation"
    -- , manipulateAstTestWFname "UnicodeRules.hs"               "Main"
    -- , manipulateAstTestWFname "Infix.hs"                 "Main"
    -- , manipulateAstTestWFname "BCase.hs"                 "Main"
    -- , manipulateAstTestWFname "LetExprSemi.hs"           "LetExprSemi"
    -- , manipulateAstTestWFname "LetExpr2.hs"              "Main"
    -- , manipulateAstTestWFname "LetStmt.hs"               "Layout.LetStmt"
    -- , manipulateAstTestWFname "RebindableSyntax.hs"      "Main"
    -- -- , manipulateAstTestWithMod changeLayoutLet3 "LayoutLet4.hs" "LayoutLet4"
    -- -- , manipulateAstTestWithMod changeLayoutLet5 "LayoutLet5.hs" "LayoutLet5"
    -- , manipulateAstTestWFname "EmptyMostly2.hs"          "EmptyMostly2"
    -- , manipulateAstTestWFname "Dead1.hs"                 "Dead1"
    -- , manipulateAstTestWFname "DocDecls.hs"              "DocDecls"
    -- , manipulateAstTestWFname "ViewPatterns.hs"          "Main"
    -- , manipulateAstTestWFname "FooExpected.hs"          "Main"
    -- -- , manipulateAstTestWithMod changeLayoutLet2 "LayoutLet2.hs" "LayoutLet2"
    -- , manipulateAstTestWFname "LayoutIn1.hs"                 "LayoutIn1"
    -- -- , manipulateAstTestWithMod changeLayoutIn1  "LayoutIn1.hs" "LayoutIn1"
    -- , manipulateAstTestWFname "LocToName.hs"                 "LocToName"
    -- -- , manipulateAstTestWithMod changeLayoutIn4  "LayoutIn4.hs" "LayoutIn4"
    -- -- , manipulateAstTestWithMod changeLocToName  "LocToName.hs" "LocToName"
    -- -- , manipulateAstTestWithMod changeLayoutLet3 "LayoutLet3.hs" "LayoutLet3"
    -- -- , manipulateAstTestWithMod changeRename1    "Rename1.hs"  "Main"
    -- , manipulateAstTestWFname    "Rename1.hs"  "Main"
    -- , manipulateAstTestWFname "AltsSemis.hs"             "Main"
    -- , manipulateAstTestWFname "LetExpr.hs"               "LetExpr"
    -- , manipulateAstTestWFname "Rules.hs"                 "Rules"
    -- , manipulateAstTestWFname "LayoutLet2.hs"             "LayoutLet2"
    -- , manipulateAstTestWFname "LayoutIn3.hs"             "LayoutIn3"
    -- , manipulateAstTestWFname "LayoutIn3a.hs"             "LayoutIn3a"
    -- -- , manipulateAstTestWFnameMod changeLayoutIn3  "LayoutIn3a.hs" "LayoutIn3a"
    -- , manipulateAstTestWFname "LetIn1.hs"             "LetIn1"
    -- -- , manipulateAstTestWFnameMod changeLetIn1  "LetIn1.hs" "LetIn1"
    -- -- , manipulateAstTestWFnameMod changeLayoutIn3  "LayoutIn3b.hs" "LayoutIn3b"
    -- -- , manipulateAstTestWFnameMod changeLayoutIn3  "LayoutIn3.hs" "LayoutIn3"
    -- , manipulateAstTestWFname "LayoutLet2.hs"             "LayoutLet2"
    -- , manipulateAstTestWFname "LayoutLet.hs"             "Main"
    -- , manipulateAstTestWFname "Simple.hs"             "Main"
    -- , manipulateAstTestWFname "FunDeps.hs"               "Main"
    -- , manipulateAstTestWFname "IfThenElse3.hs"              "Main"
    -- , manipulateAstTestWFname "ImplicitParams.hs"        "Main"
    -- , manipulateAstTestWFname "ListComprehensions.hs"    "Main"
    -- , manipulateAstTestWFname "TransformListComp.hs"     "Main"
    -- , manipulateAstTestWFname "PArr.hs"                  "PArr"
    -- , manipulateAstTestWFname "DataDecl.hs"              "Main"
    -- , manipulateAstTestWFname "WhereIn4.hs"              "WhereIn4"
    -- , manipulateAstTestWFname "Pseudonym.hs"             "Main"
    -- , manipulateAstTestWFname "Obscure.hs"             "Main"
    -- , manipulateAstTestWFname "Remorse.hs"             "Main"
    -- , manipulateAstTestWFname "Jon.hs"             "Main"
    -- , manipulateAstTestWFname "RSA.hs"             "Main"
    -- , manipulateAstTestWFname "CExpected.hs"                "CExpected"
    -- , manipulateAstTestWFname "C.hs"                        "C"
    -- -- , manipulateAstTestWFnameMod changeCifToCase  "C.hs"    "C"
    -- -- , manipulateAstTestWFnameMod changeWhereIn3 "WhereIn3.hs"    "WhereIn3"
    -- , manipulateAstTestWFname "DoParens.hs"   "Main"
    -- , manipulateAstTestWFname "SimpleComplexTuple.hs" "Main"
    -- , manipulateAstTestWFname "Backquote.hs" "Main"
    -- , manipulateAstTestWFname "HangingRecord.hs" "Main"
    -- , manipulateAstTestWFname "PatternGuards.hs"              "Main"
    -- -- , manipulateAstTestWFnameMod (changeWhereIn3 2) "WhereIn3.hs"    "WhereIn3"
    -- -- , manipulateAstTestWFnameMod (changeWhereIn3 2) "WhereIn3.hs"    "WhereIn3"
    -- , manipulateAstTestWFname "DoParens.hs"   "Main"

    -- -- Future tests to pass, after appropriate dev is done
    -- -- , manipulateAstTestWFname "MultipleInferredContexts.hs"   "Main"
    -- -- , manipulateAstTestWFname "ArgPuncParens.hs"   "Main"
    -- -- , manipulateAstTestWFname "SimpleComplexTuple.hs" "Main"
    -- -- , manipulateAstTestWFname "DoPatBind.hs" "Main"
    -- , manipulateAstTestWFname "DroppedDoSpace.hs" "Main"
    -- , manipulateAstTestWFname "DroppedDoSpace2.hs" "Main"
    -- , manipulateAstTestWFname "GHCOrig.hs" "GHC.Tuple"

    -- , manipulateAstTestWFname "Cpp.hs"                   "Main"
    -- , manipulateAstTestWFname "MangledSemiLet.hs"        "Main"
    -- , manipulateAstTestWFname "ListComprehensions.hs"    "Main"
    -- , manipulateAstTestWFname "ParensAroundContext.hs"   "ParensAroundContext"
    -- , manipulateAstTestWFname "TypeFamilies.hs"          "Main"
    -- , manipulateAstTestWFname "Associated.hs"            "Main"
    -- , manipulateAstTestWFname "RdrNames.hs"              "RdrNames"
    -- , manipulateAstTestWFname "StrangeTypeClass.hs"      "Main"
    -- , manipulateAstTestWFname "TypeSignatureParens.hs"   "Main"
    -- , manipulateAstTestWFname "DoubleForall.hs"          "Main"
    -- , manipulateAstTestWFname "GADTRecords.hs"           "Main"
    -- , manipulateAstTestWFname "Existential.hs"           "Main"
    -- , manipulateAstTestWFname "ScopedTypeVariables.hs"   "Main"
    -- , manipulateAstTestWFname "T5951.hs"   "T5951"
    -- , manipulateAstTestWFname "Zipper2.hs"               "Zipper2"
    -- , manipulateAstTestWFname "RdrNames2.hs"             "RdrNames2"
    -- , manipulateAstTestWFname "Unicode.hs"               "Unicode"
    -- , manipulateAstTestWFname "OptSig2.hs"               "Main"
    -- , manipulateAstTestWFname "Minimal.hs"               "Main"
    -- , manipulateAstTestWFname "DroppedComma.hs"          "Main"
    -- , manipulateAstTestWFname "SlidingTypeSyn.hs"        "Main"
    -- , manipulateAstTestWFname "TupleSections.hs"         "Main"
    -- , manipulateAstTestWFname "CorePragma.hs"            "Main"
    -- , manipulateAstTestWFname "Splice.hs"                "Splice"
    -- , manipulateAstTestWFname "TemplateHaskell.hs"       "Main"
    -- , manipulateAstTestWFname "GADTContext.hs"           "Main"
    -- , manipulateAstTestWFnameBad "UnicodeSyntax.hs"      "Tutorial"
    -- , manipulateAstTestWFname "DataDecl.hs"              "Main"

    -- , manipulateAstTestWFname "TypeBrackets.hs"         "Main"
    -- , manipulateAstTestWFname "TypeBrackets2.hs"        "Main"
    -- , manipulateAstTestWFname "TypeBrackets4.hs"        "Main"
    -- , manipulateAstTestWFname "NestedLambda.hs"         "Main"
    -- , manipulateAstTestWFname "ShiftingLambda.hs"       "Main"
    -- , manipulateAstTestWFname "SlidingLambda.hs"        "Main"
--    , manipulateAstTestWFnameMod changeAddDecl "AddDecl.hs" "AddDecl"
    -- , manipulateAstTestWFnameMod changeLocalDecls "LocalDecls.hs" "LocalDecls"
    -- , manipulateAstTestWFname "LocalDecls2Expected.hs"        "LocalDecls2Expected"
    -- , manipulateAstTestWFname "LocalDecls2.hs"        "LocalDecls2"
    -- , manipulateAstTestWFnameMod changeLocalDecls2 "LocalDecls2.hs" "LocalDecls2"
    -- , manipulateAstTestWFname "WhereIn3.hs"                 "WhereIn3"
    -- , manipulateAstTestWFnameMod changeWhereIn3a "WhereIn3a.hs" "WhereIn3a"
    -- , manipulateAstTestWFname "Imports.hs"              "Imports"
    -- , manipulateAstTestWFname "T10196.hs"               "T10196"
    -- , manipulateAstTestWFnameMod addLocaLDecl1 "AddLocalDecl1.hs" "AddLocaLDecl1"
    -- , manipulateAstTestWFnameMod addLocaLDecl2 "AddLocalDecl2.hs" "AddLocaLDecl2"
    -- , manipulateAstTestWFnameMod addLocaLDecl3 "AddLocalDecl3.hs" "AddLocaLDecl3"
    -- , manipulateAstTestWFnameMod addLocaLDecl4 "AddLocalDecl4.hs" "AddLocaLDecl4"
    -- , manipulateAstTestWFnameMod rmDecl1       "RmDecl1.hs"       "RmDecl1"
    -- , manipulateAstTestWFname "RmDecl2.hs"                        "RmDecl2"
    -- , manipulateAstTestWFnameMod rmDecl2       "RmDecl2.hs"       "RmDecl2"
    -- , manipulateAstTestWFnameMod rmDecl3       "RmDecl3.hs"       "RmDecl3"
    -- , manipulateAstTestWFnameMod rmDecl4       "RmDecl4.hs"       "RmDecl4"
    -- , manipulateAstTestWFnameMod rmDecl5       "RmDecl5.hs"       "RmDecl5"
    -- , manipulateAstTestWFname "RmDecl5.hs"                        "RmDecl5"
    -- , manipulateAstTestWFnameMod rmDecl6       "RmDecl6.hs"       "RmDecl6"
    -- , manipulateAstTestWFnameMod rmDecl7       "RmDecl7.hs"       "RmDecl7"
    -- , manipulateAstTestWFname "TypeSignature.hs"                  "TypeSignature"
    -- , manipulateAstTestWFnameMod rmTypeSig1    "RmTypeSig1.hs"    "RmTypeSig1"
    -- , manipulateAstTestWFnameMod rmTypeSig2    "RmTypeSig2.hs"    "RmTypeSig2"
    -- , manipulateAstTestWFname "StringGap.hs"                      "StringGap"
    -- , manipulateAstTestWFnameMod addHiding1    "AddHiding1.hs"    "AddHiding1"
    -- , manipulateAstTestWFnameMod addHiding2    "AddHiding2.hs"    "AddHiding2"
    -- , manipulateAstTestWFnameMod cloneDecl1    "CloneDecl1.hs"    "CloneDecl1"
    -- , manipulateAstTestWFname "SimpleDo.hs"                      "Main"
    -- , manipulateAstTestWFnameMod addLocaLDecl5 "AddLocalDecl5.hs" "AddLocaLDecl5"
    , manipulateAstTestWFnameMod changeRename2    "Rename2.hs"  "Main"
    {-
    , manipulateAstTestWFname "Lhs.lhs"                  "Main"
    , manipulateAstTestWFname "Foo.hs"                   "Main"
    -}
    ]

testsTT :: Test
testsTT = TestList
  [
    mkParserTest "Cpp.hs"
  , mkParserTest "DroppedDoSpace.hs"
  ]

tt :: IO ()
-- tt = hSilence [stderr] $ do
tt = do
  cnts <- fst <$> runTestText (putTextToHandle stdout True) testsTT
  putStrLn $ show cnts
  if errors cnts > 0 || failures cnts > 0
     then exitFailure
     else return () -- exitSuccess


-- | Where all the tests are to be found
examplesDir :: FilePath
examplesDir = "tests" </> "examples"

examplesDir2 :: FilePath
examplesDir2 = "examples"



-- ---------------------------------------------------------------------

pwd :: IO FilePath
pwd = getCurrentDirectory

cd :: FilePath -> IO ()
cd = setCurrentDirectory

-- ---------------------------------------------------------------------

mkSs :: (Int,Int) -> (Int,Int) -> GHC.SrcSpan
mkSs (sr,sc) (er,ec)
  = GHC.mkSrcSpan (GHC.mkSrcLoc (GHC.mkFastString "examples/PatBind.hs") sr sc)
                  (GHC.mkSrcLoc (GHC.mkFastString "examples/PatBind.hs") er ec)

-- ---------------------------------------------------------------------
