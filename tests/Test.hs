{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
-- | Use "runhaskell Setup.hs test" or "cabal test" to run these tests.
module Main where

import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Preprocess
import Language.Haskell.GHC.ExactPrint.Transform
import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Utils

import GHC.Paths ( libdir )

import qualified Bag            as GHC
import qualified BasicTypes     as GHC
import qualified DynFlags       as GHC
import qualified FastString     as GHC
import qualified GHC            as GHC
-- import qualified MonadUtils     as GHC
import qualified OccName        as GHC
import qualified RdrName        as GHC

import qualified Data.Generics as SYB
import qualified GHC.SYB.Utils as SYB

import Control.Monad
import System.Directory
import System.FilePath
import System.IO
import System.Exit
import qualified Data.Map as Map

import Test.HUnit

import Data.List

import System.IO.Silently

import Common

-- import Debug.Trace

-- ---------------------------------------------------------------------

main :: IO ()
main = hSilence [stderr] $ do
  cnts <- fst <$> runTestText (putTextToHandle stdout True) tests
  putStrLn $ show cnts
  if errors cnts > 0 || failures cnts > 0
     then exitFailure
     else return () -- exitSuccess

-- tests = TestCase (do r <- manipulateAstTest "examples/LetStmt.hs" "Layout.LetStmt"
--                      assertBool "test" r )

tests :: Test
tests = TestList
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
  , mkTestMod "Deprecation.hs"           "Deprecation"
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
  , mkTestModTH "QuasiQuote.hs"          "QuasiQuote"
  , mkTestMod "Pseudonym.hs"             "Main"
  , mkTestMod "Obscure.hs"               "Main"
  , mkTestMod "Remorse.hs"               "Main"
  , mkTestMod "Jon.hs"                   "Main"
  , mkTestMod "RSA.hs"                   "Main"
  , mkTestMod "WhereIn3.hs"              "WhereIn3"
  , mkTestMod "Backquote.hs"              "Main"
  , mkTestMod "PatternGuards.hs"              "Main"
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
  , mkParserTest "MultiLineWarningPragma.hs"
  , mkParserTest "MultiWayIf.hs"
  , mkParserTest "OptSig.hs"
  , mkParserTest "StrangeTypeClass.hs"
  , mkParserTest "TypeSignatureParens.hs"
  , mkParserTest "UnicodeSyntax.hs"
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
  , mkParserTest "UnicodeRules.hs"
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

  , mkTestModChange changeLayoutLet2 "LayoutLet2.hs" "LayoutLet2"
  , mkTestModChange changeLayoutLet3 "LayoutLet3.hs" "LayoutLet3"
  , mkTestModChange changeLayoutLet3 "LayoutLet4.hs" "LayoutLet4"
  , mkTestModChange changeRename1    "Rename1.hs"    "Main"
  , mkTestModChange changeLayoutIn1  "LayoutIn1.hs"  "LayoutIn1"
  , mkTestModChange changeLayoutIn3  "LayoutIn3.hs"  "LayoutIn3"
  , mkTestModChange changeLayoutIn3  "LayoutIn3a.hs" "LayoutIn3a"
  , mkTestModChange changeLayoutIn3  "LayoutIn3b.hs" "LayoutIn3b"
  , mkTestModChange changeLayoutIn4  "LayoutIn4.hs"  "LayoutIn4"
  , mkTestModChange changeLocToName  "LocToName.hs"  "LocToName"
  , mkTestModChange changeLetIn1     "LetIn1.hs"     "LetIn1"
  , mkTestModChange changeWhereIn4   "WhereIn4.hs"   "WhereIn4"
--  , mkTestModChange changeCifToCase  "C.hs"          "C"

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



mkTestMain :: FilePath -> Test
mkTestMain fileName = TestCase (do r <- manipulateAstTest fileName "Main"
                                   assertBool fileName r )

mkTestMod :: FilePath -> String -> Test
mkTestMod fileName _modName
  =  mkParserTest fileName

mkTestModChange :: (Anns -> GHC.ParsedSource -> (Anns,GHC.ParsedSource)) -> FilePath -> String -> Test
mkTestModChange change fileName modName
  = TestCase (do r <- manipulateAstTestWithMod change fileName modName
                 assertBool fileName r )

mkTestModTH :: FilePath -> String -> Test
mkTestModTH fileName modName
  = TestCase (do r <- manipulateAstTestTH fileName modName
                 assertBool fileName r )
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
    -- , manipulateAstTestWFname "DataDecl.hs"              "Main"
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
    , manipulateAstTestWFname "Deprecation.hs"           "Deprecation"
    , manipulateAstTestWFname "UnicodeRules.hs"               "Main"
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
    -- , manipulateAstTestWFname "WhereIn3.hs"                 "WhereIn3"
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
    -- , manipulateAstTestWFname "TypeSignatureParens.hs"  "Main"
    -- , manipulateAstTestWFname "DoubleForall.hs"         "Main"
    -- , manipulateAstTestWFname "GADTRecords.hs"          "Main"
    -- , manipulateAstTestWFname "Existential.hs"           "Main"
    -- , manipulateAstTestWFname "ScopedTypeVariables.hs"   "Main"
    -- , manipulateAstTestWFname "T5951.hs"   "T5951"
    -- , manipulateAstTestWFname "Zipper2.hs"               "Zipper2"
    -- , manipulateAstTestWFname "RdrNames2.hs"             "RdrNames2"
    -- , manipulateAstTestWFname "Unicode.hs"                  "Unicode"
    -- , manipulateAstTestWFname "OptSig2.hs"                  "Main"
    -- , manipulateAstTestWFname "Minimal.hs"                  "Main"
    -- , manipulateAstTestWFname "DroppedComma.hs"                "Main"
    -- , manipulateAstTestWFname "SlidingTypeSyn.hs"                "Main"
    -- , manipulateAstTestWFname "TupleSections.hs"                "Main"
    -- , manipulateAstTestWFname "CorePragma.hs"                "Main"
    -- , manipulateAstTestWFname "Splice.hs"                "Splice"
    , manipulateAstTestWFname "TemplateHaskell.hs"         "Main"
    -- , manipulateAstTestWFname "GADTContext.hs" "Main"

    -- , manipulateAstTestWFname "TypeBrackets.hs"         "Main"
    -- , manipulateAstTestWFname "TypeBrackets2.hs"         "Main"
    -- , manipulateAstTestWFname "TypeBrackets4.hs"         "Main"
    , manipulateAstTestWFname "NestedLambda.hs"      "Main"
    , manipulateAstTestWFname "ShiftingLambda.hs"      "Main"
    , manipulateAstTestWFname "SlidingLambda.hs"      "Main"

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

-- ---------------------------------------------------------------------

-- |Remove a decl with a trailing comment, and remove the trailing comment too
changeWhereIn3 :: Int -> Anns -> GHC.ParsedSource -> (Anns,GHC.ParsedSource)
changeWhereIn3 declIndex ans p = (ans',p')
  where
    (p',(ans',_),_) = runTransform ans doTransform
    doTransform = doRmDecl p

    doRmDecl (GHC.L l (GHC.HsModule mmn mexp imps decls mdepr haddock)) = do
      let
        -- declIndex = 2 -- zero based
        decls1 = take declIndex decls
        decls2 = drop (declIndex + 1) decls
        decls' = decls1 ++ decls2
      return (GHC.L l (GHC.HsModule mmn mexp imps decls' mdepr haddock))
      -- error $ "doRmDecl:decls2=" ++ showGhc (length decls,decls1,decls2)

-- ---------------------------------------------------------------------

-- |Convert the if statement in C.hs to a case, adjusting layout appropriately.
changeCifToCase :: Anns -> GHC.ParsedSource -> (Anns,GHC.ParsedSource)
changeCifToCase ans p = (ans',p')
  where
    (p',(ans',_),_) = runTransform ans doTransform
    doTransform = SYB.everywhereM (SYB.mkM ifToCaseTransform) p

    ifToCaseTransform :: GHC.Located (GHC.HsExpr GHC.RdrName)
                      -> Transform (GHC.Located (GHC.HsExpr GHC.RdrName))
    ifToCaseTransform li@(GHC.L l (GHC.HsIf _se e1 e2 e3)) = do
      caseLoc        <- uniqueSrcSpan -- HaRe:-1:1
      trueMatchLoc   <- uniqueSrcSpan -- HaRe:-1:2
      trueLoc1       <- uniqueSrcSpan -- HaRe:-1:3
      trueLoc        <- uniqueSrcSpan -- HaRe:-1:4
      trueRhsLoc     <- uniqueSrcSpan -- HaRe:-1:5
      falseLoc1      <- uniqueSrcSpan -- HaRe:-1:6
      falseLoc       <- uniqueSrcSpan -- HaRe:-1:7
      falseMatchLoc  <- uniqueSrcSpan -- HaRe:-1:8
      falseRhsLoc    <- uniqueSrcSpan -- HaRe:-1:9
      caseVirtualLoc <- uniqueSrcSpan -- HaRe:-1:10
      let trueName  = mkRdrName "True"
      let falseName = mkRdrName "False"
      let ret = GHC.L caseLoc (GHC.HsCase e1
                 (GHC.MG
                  [
                    (GHC.L trueMatchLoc $ GHC.Match
                     Nothing
                     [
                       GHC.L trueLoc1 $ GHC.ConPatIn (GHC.L trueLoc trueName) (GHC.PrefixCon [])
                     ]
                     Nothing
                     (GHC.GRHSs
                       [
                         GHC.L trueRhsLoc $ GHC.GRHS [] e2
                       ] GHC.EmptyLocalBinds)
                    )
                  , (GHC.L falseMatchLoc $ GHC.Match
                     Nothing
                     [
                       GHC.L falseLoc1 $ GHC.ConPatIn (GHC.L falseLoc falseName) (GHC.PrefixCon [])
                     ]
                     Nothing
                     (GHC.GRHSs
                       [
                         GHC.L falseRhsLoc $ GHC.GRHS [] e3
                       ] GHC.EmptyLocalBinds)
                    )
                  ] [] GHC.placeHolderType GHC.FromSource))

      oldAnns <- getAnnsT
      let annIf   = gfromJust "Case.annIf"   $ getAnnotationEP li NotNeeded oldAnns
      let annCond = gfromJust "Case.annCond" $ getAnnotationEP e1 NotNeeded oldAnns
      let annThen = gfromJust "Case.annThen" $ getAnnotationEP e2 NotNeeded oldAnns
      let annElse = gfromJust "Case.annElse" $ getAnnotationEP e3 NotNeeded oldAnns
      logTr $ "Case:annIf="   ++ show annIf
      logTr $ "Case:annThen=" ++ show annThen
      logTr $ "Case:annElse=" ++ show annElse

      -- let ((_ifr,    ifc),  ifDP) = getOriginalPos oldAnns li (G GHC.AnnIf)
      -- let ((_thenr,thenc),thenDP) = getOriginalPos oldAnns li (G GHC.AnnThen)
      -- let ((_elser,elsec),elseDP) = getOriginalPos oldAnns li (G GHC.AnnElse)
      -- let newCol = ifc + 2
      let newCol = 6

      -- AZ:TODO: under some circumstances the GRHS annotations need LineSame, in others LineChanged.
      let ifDelta     = gfromJust "Case.ifDelta"     $ lookup (G GHC.AnnIf) (annsDP annIf)
      -- let ifSpanEntry = gfromJust "Case.ifSpanEntry" $ lookup AnnSpanEntry (annsDP annIf)
      let ifSpanEntry = annEntryDelta annIf
      let anne2' =
            [ ( AnnKey caseLoc       (CN "HsCase") NotNeeded,   annIf { annsDP = [ (AnnSpanEntry,ifSpanEntry),(G GHC.AnnCase, ifDelta)
                                                                     , (G GHC.AnnOf,     DP (0,1))
                                                                     ,(AnnList caseVirtualLoc NotNeeded,DP (0,0))] } )
            , ( AnnKey caseVirtualLoc (CN "(:)") NotNeeded,     Ann (DP (1,newCol)) (ColDelta newCol) (DP (1,newCol)) [] [(AnnSpanEntry,DP (1,0))])
            , ( AnnKey trueMatchLoc  (CN "Match") NotNeeded,    Ann (DP (0,0)) 0 (DP (0,0)) [] [] )
            , ( AnnKey trueLoc1      (CN "ConPatIn") NotNeeded, Ann (DP (0,0)) 0 (DP (0,0)) [] [] )
            , ( AnnKey trueLoc       (CN "Unqual") NotNeeded,   Ann (DP (0,0)) 0 (DP (0,0)) [] [(G GHC.AnnVal, DP (0,0))] )
            , ( AnnKey trueRhsLoc    (CN "GRHS") NotNeeded,     Ann (DP (0,2)) 6 (DP (0,0)) [] [(AnnSpanEntry,DP (0,2)),(G GHC.AnnRarrow, DP (0,0))] )

            , ( AnnKey falseMatchLoc (CN "Match") NotNeeded,    Ann (DP (1,0)) 0 (DP (0,0)) []  [(AnnSpanEntry,DP (1,0))] )
            , ( AnnKey falseLoc1     (CN "ConPatIn") NotNeeded, Ann (DP (0,0)) 0 (DP (0,0)) []  [] )
            , ( AnnKey falseLoc      (CN "Unqual") NotNeeded,   Ann (DP (0,0)) 0 (DP (0,0)) []  [ (G GHC.AnnVal, DP (0,0))] )
            , ( AnnKey falseRhsLoc   (CN "GRHS") NotNeeded,     Ann (DP (0,1)) 6 (DP (0,0)) []  [(AnnSpanEntry,DP (0,1)),(G GHC.AnnRarrow, DP (0,0))] )
            ]

      let annThen' = adjustAnnOffset (ColDelta 6) annThen
      let anne1 = Map.delete (AnnKey l (CN "HsIf") NotNeeded) oldAnns
          final = mergeAnns anne1 (Map.fromList anne2')
          anne3 = setLocatedAnns final
                    [ (e1, annCond)
                    , (e2, annThen')
                    , (e3, annElse)
                    ]
      putAnnsT anne3
      return ret
    ifToCaseTransform x = return x

    mkRdrName :: String -> GHC.RdrName
    mkRdrName s = GHC.mkVarUnqual (GHC.mkFastString s)

-- ---------------------------------------------------------------------

changeLayoutLet2 :: Anns -> GHC.ParsedSource -> (Anns, GHC.ParsedSource)
changeLayoutLet2 ans parsed = (ans,rename "xxxlonger" [((7,5),(7,8)),((8,24),(8,27))] parsed)

changeLocToName :: Anns -> GHC.ParsedSource -> (Anns, GHC.ParsedSource)
changeLocToName ans parsed = (ans,rename "LocToName.newPoint" [((20,1),(20,11)),((20,28),(20,38)),((24,1),(24,11))] parsed)

changeLayoutIn3 :: Anns -> GHC.ParsedSource -> (Anns, GHC.ParsedSource)
changeLayoutIn3 ans parsed = (ans,rename "anotherX" [((7,13),(7,14)),((7,37),(7,38)),((8,37),(8,38))] parsed)
-- changeLayoutIn3 parsed = rename "anotherX" [((7,13),(7,14)),((7,37),(7,38))] parsed

changeLayoutIn4 :: Anns -> GHC.ParsedSource -> (Anns, GHC.ParsedSource)
changeLayoutIn4 ans parsed = (ans,rename "io" [((7,8),(7,13)),((7,28),(7,33))] parsed)

changeLayoutIn1 :: Anns -> GHC.ParsedSource -> (Anns, GHC.ParsedSource)
changeLayoutIn1 ans parsed = (ans,rename "square" [((7,17),(7,19)),((7,24),(7,26))] parsed)

changeRename1 :: Anns -> GHC.ParsedSource -> (Anns, GHC.ParsedSource)
changeRename1 ans parsed = (ans,rename "bar2" [((3,1),(3,4))] parsed)

changeLayoutLet3 :: Anns -> GHC.ParsedSource -> (Anns, GHC.ParsedSource)
changeLayoutLet3 ans parsed = (ans,rename "xxxlonger" [((7,5),(7,8)),((9,14),(9,17))] parsed)

changeLayoutLet5 :: Anns -> GHC.ParsedSource -> (Anns, GHC.ParsedSource)
changeLayoutLet5 ans parsed = (ans,rename "x" [((7,5),(7,8)),((9,14),(9,17))] parsed)

rename :: (SYB.Data a) => String -> [Span] -> a -> a
rename newNameStr spans a
  = SYB.everywhere ( SYB.mkT   replaceRdr
                    `SYB.extT` replaceHsVar
                    `SYB.extT` replacePat
                   ) a
  where
    newName = GHC.mkRdrUnqual (GHC.mkVarOcc newNameStr)

    cond :: GHC.SrcSpan -> Bool
    cond ln = any (\ss -> ss2span ln == ss) spans

    replaceRdr :: GHC.Located GHC.RdrName -> GHC.Located GHC.RdrName
    replaceRdr (GHC.L ln _)
        | cond ln = GHC.L ln newName
    replaceRdr x = x

    replaceHsVar :: GHC.LHsExpr GHC.RdrName -> GHC.LHsExpr GHC.RdrName
    replaceHsVar (GHC.L ln (GHC.HsVar _))
        | cond ln = GHC.L ln (GHC.HsVar newName)
    replaceHsVar x = x

    replacePat (GHC.L ln (GHC.VarPat _))
        | cond ln = GHC.L ln (GHC.VarPat newName)
    replacePat x = x



-- ---------------------------------------------------------------------

changeWhereIn4 :: Anns -> GHC.ParsedSource -> (Anns,GHC.ParsedSource)
changeWhereIn4 ans parsed
  = (ans,SYB.everywhere (SYB.mkT replace) parsed)
  where
    replace :: GHC.Located GHC.RdrName -> GHC.Located GHC.RdrName
    replace (GHC.L ln _n)
      | ss2span ln == ((12,16),(12,17)) = GHC.L ln (GHC.mkRdrUnqual (GHC.mkVarOcc "p_2"))
    replace x = x

-- ---------------------------------------------------------------------

changeLetIn1 :: Anns -> GHC.ParsedSource -> (Anns,GHC.ParsedSource)
changeLetIn1 ans parsed
  = (ans,SYB.everywhere (SYB.mkT replace) parsed)
  where
    replace :: GHC.HsExpr GHC.RdrName -> GHC.HsExpr GHC.RdrName
    replace (GHC.HsLet localDecls expr@(GHC.L _ _))
      =
         let (GHC.HsValBinds (GHC.ValBindsIn bagDecls sigs)) = localDecls
             bagDecls' = GHC.listToBag $ init $ GHC.bagToList bagDecls
         in (GHC.HsLet (GHC.HsValBinds (GHC.ValBindsIn bagDecls' sigs)) expr)

    replace x = x

-- ---------------------------------------------------------------------

-- | Where all the tests are to be found
examplesDir :: FilePath
examplesDir = "tests" </> "examples"

examplesDir2 :: FilePath
examplesDir2 = "examples"

manipulateAstTestWithMod :: (Anns -> GHC.ParsedSource -> (Anns,GHC.ParsedSource)) -> FilePath -> String -> IO Bool
manipulateAstTestWithMod change file modname = manipulateAstTest' (Just change) False file modname

manipulateAstTestWFnameMod :: (Anns -> GHC.ParsedSource -> (Anns,GHC.ParsedSource)) -> FilePath -> String -> IO (FilePath,Bool)
manipulateAstTestWFnameMod change fileName modname
  = do r <- manipulateAstTestWithMod change fileName modname
       return (fileName,r)

manipulateAstTest :: FilePath -> String -> IO Bool
manipulateAstTest file modname = manipulateAstTest' Nothing False file modname

manipulateAstTestWFname :: FilePath -> String -> IO (FilePath, Bool)
manipulateAstTestWFname file modname = (file,) <$> manipulateAstTest file modname

manipulateAstTestTH :: FilePath -> String -> IO Bool
manipulateAstTestTH file modname = manipulateAstTest' Nothing True file modname




manipulateAstTest' :: Maybe (Anns -> GHC.ParsedSource -> (Anns,GHC.ParsedSource)) -> Bool -> FilePath -> String -> IO Bool
manipulateAstTest' mchange useTH file' modname = do
  let testpath = "./tests/examples/"
      file     = testpath </> file'
      out      = file <.> "out"
      expected = file <.> "expected"

  contents <- case mchange of
                   Nothing -> readUTF8File file
                   Just _  -> readUTF8File expected
  (ghcAnns',p,cppComments) <- hSilence [stderr] $  parsedFileGhc file modname useTH
  -- (ghcAnns',p,cppComments) <- parsedFileGhc file modname useTH
  let
    parsedOrig = GHC.pm_parsed_source $ p
    -- (ghcAnns,parsed) = fixBugsInAst ghcAnns' parsedOrig
    (ghcAnns,parsed) = (ghcAnns', parsedOrig)
    parsedAST = SYB.showData SYB.Parser 0 parsed
    -- cppComments = map (tokComment . commentToAnnotation . fst) cppCommentToks
    -- parsedAST = showGhc parsed
       -- `debug` ("getAnn:=" ++ (show (getAnnotationValue (snd ann) (GHC.getLoc parsed) :: Maybe AnnHsModule)))
    -- try to pretty-print; summarize the test result
    ann = relativiseApiAnnsWithComments cppComments parsedOrig ghcAnns'
      `debug` ("ghcAnns:" ++ showGhc ghcAnns)

    (ann',parsed') = case mchange of
                   Nothing     -> (ann,parsed)
                   Just change -> change ann parsed
    printed = exactPrintWithAnns parsed' ann' -- `debug` ("ann=" ++ (show $ map (\(s,a) -> (ss2span s, a)) $ Map.toList ann))
    outcome = if printed == contents
                then "Match\n"
                else "Fail\n"
    result = printed ++ "\n==============\n"
             ++ outcome ++ "\n==============\n"
             ++ "lengths:" ++ show (length printed,length contents) ++ "\n"
             ++ showAnnData ann 0 parsed'
             ++ "\n========================\n"
             ++ showGhc ann'
             ++ "\n========================\n"
             ++ showGhc ghcAnns
             ++ "\n========================\n"
             ++ parsedAST
             ++ "\n========================\n"
             ++ showGhc ann
  writeFile out $ result
  -- putStrLn $ "Test:parsed=" ++ parsedAST
  -- putStrLn $ "Test:showdata:parsedOrig" ++ SYB.showData SYB.Parser 0 parsedOrig
  -- putStrLn $ "Test:ann :" ++ showGhc ann
  -- putStrLn $ "Test:ghcAnns :" ++ showGhc ghcAnns
  -- putStrLn $ "Test:ghcAnns' :" ++ showGhc ghcAnns'
  -- putStrLn $ "Test:showdata:" ++ showAnnData ann 0 parsed
  -- putStrLn $ "Test:showdata:parsed'" ++ SYB.showData SYB.Parser 0 parsed'
  -- putStrLn $ "Test:showdata:parsed'" ++ showAnnData ann 0 parsed'
  return (printed == contents)


-- ---------------------------------------------------------------------
-- |Result of parsing a Haskell source file. It is simply the
-- TypeCheckedModule produced by GHC.
type ParseResult = GHC.ParsedModule

parsedFileGhc :: String -> String -> Bool -> IO (GHC.ApiAnns,ParseResult,[Comment])
parsedFileGhc fileName _modname useTH = do
    -- putStrLn $ "parsedFileGhc:" ++ show fileName
    GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut $ do
      GHC.runGhc (Just libdir) $ do
        dflags <- GHC.getSessionDynFlags
        let dflags2 = dflags { GHC.importPaths = ["./tests/examples/","../tests/examples/",
                                                  "./src/","../src/"] }
            tgt = if useTH then GHC.HscInterpreted
                           else GHC.HscNothing -- allows FFI
            dflags3 = dflags2 { GHC.hscTarget = tgt
                              , GHC.ghcLink =  GHC.LinkInMemory
                              }

            dflags4 = GHC.gopt_set dflags3 GHC.Opt_KeepRawTokenStream

        (dflags5,_args,_warns) <- GHC.parseDynamicFlagsCmdLine dflags4 [GHC.noLoc "-package ghc"]
        -- GHC.liftIO $ putStrLn $ "dflags set:(args,warns)" ++ show (map GHC.unLoc _args,map GHC.unLoc _warns)
        void $ GHC.setSessionDynFlags dflags5
        -- GHC.liftIO $ putStrLn $ "dflags set"

        -- hsc_env <- GHC.getSession
        -- (dflags6,fn_pp) <- GHC.liftIO $ GHC.preprocess hsc_env (fileName,Nothing)
        -- GHC.liftIO $ putStrLn $ "preprocess got:" ++ show fn_pp


        target <- GHC.guessTarget fileName Nothing
        GHC.setTargets [target]
        -- GHC.liftIO $ putStrLn $ "target set:" ++ showGhc (GHC.targetId target)
        void $ GHC.load GHC.LoadAllTargets -- Loads and compiles, much as calling make
        -- GHC.liftIO $ putStrLn $ "targets loaded"
        -- g <- GHC.getModuleGraph
        -- let showStuff ms = show (GHC.moduleNameString $ GHC.moduleName $ GHC.ms_mod ms,GHC.ms_location ms)
        -- GHC.liftIO $ putStrLn $ "module graph:" ++ (intercalate "," (map showStuff g))

        -- modSum <- GHC.getModSummary $ GHC.mkModuleName modname
        Just modSum <- getModSummaryForFile fileName
        -- GHC.liftIO $ putStrLn $ "got modSum"
        -- let modSum = head g
        cppComments <-  if (GHC.xopt GHC.Opt_Cpp dflags5)
                        then getCppTokensAsComments fileName
                        else return []
        -- let cppComments = [] :: [(GHC.Located GHC.Token, String)]
--        GHC.liftIO $ putStrLn $ "\ncppTokensAsComments for:"  ++ fileName ++ "=========\n"
--                              ++ showGhc cppComments ++ "\n================\n"
{-
        (sourceFile, source, flags) <- getModuleSourceAndFlags (GHC.ms_mod modSum)
        strSrcBuf <- getPreprocessedSrc sourceFile
        GHC.liftIO $ putStrLn $ "preprocessedSrc====\n" ++ strSrcBuf ++ "\n================\n"
-}
        p <- GHC.parseModule modSum
        -- GHC.liftIO $ putStrLn $ "got parsedModule"
--        t <- GHC.typecheckModule p
        -- GHC.liftIO $ putStrLn $ "typechecked"
        -- toks <- GHC.getRichTokenStream (GHC.ms_mod modSum)
        -- GHC.liftIO $ putStrLn $ "toks" ++ show toks
        let anns = GHC.pm_annotations p
        -- GHC.liftIO $ putStrLn $ "anns"
        return (anns,p,cppComments)

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

readUTF8File :: FilePath -> IO String
readUTF8File fp = openFile fp ReadMode >>= \h -> do
        hSetEncoding h utf8
        hGetContents h
