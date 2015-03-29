{-# LANGUAGE TupleSections #-}
-- | Use "runhaskell Setup.hs test" or "cabal test" to run these tests.
module Main where

import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Utils
import Language.Haskell.GHC.ExactPrint.Types


import GHC.Paths ( libdir )

import qualified Bag           as GHC
import qualified DynFlags      as GHC
import qualified FastString    as GHC
import qualified GHC           as GHC
import qualified HscTypes      as GHC
import qualified MonadUtils    as GHC
import qualified OccName       as GHC
import qualified Outputable    as GHC
import qualified RdrName       as GHC
import qualified StringBuffer  as GHC

import qualified Data.Generics as SYB
import qualified GHC.SYB.Utils as SYB

import Data.IORef
import Control.Exception
import Control.Monad
import System.Directory
import System.FilePath
import System.IO
import System.Exit
import qualified Data.Map as Map

import Test.HUnit

import Control.Applicative
import Data.List (partition)

-- ---------------------------------------------------------------------

ghead :: String -> [a] -> a
ghead s [] = error ("Empty list at: " ++ s)
ghead s (x:xs) = x

main :: IO ()
main = do
  cnts <- runTestTT tests
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
  , mkTestMod "Arrows.hs"                "Main"
  , mkTestMod "Associated.hs"            "Main"
  , mkTestMod "B.hs"                     "Main"
  , mkTestMod "BCase.hs"                 "Main"
  , mkTestMod "BangPatterns.hs"          "Main"
  , mkTestMod "Cg008.hs"                 "Cg008"
  , mkTestMod "DataDecl.hs"              "Main"
  , mkTestMod "DataFamilies.hs"          "DataFamilies"
  , mkTestMod "Dead1.hs"                 "Dead1"
  , mkTestMod "Default.hs"               "Main"
  , mkTestMod "Deprecation.hs"           "Deprecation"
  , mkTestMod "Deriving.hs"              "Main"
  , mkTestMod "DerivingOC.hs"            "Main"
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
  , mkTestMod "RankNTypes.hs"            "Main"
  , mkTestMod "RdrNames.hs"              "RdrNames"
  , mkTestMod "RebindableSyntax.hs"      "Main"
  , mkTestMod "RecordUpdate.hs"          "Main"
  , mkTestMod "RecursiveDo.hs"           "Main"
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
  -- , mkTestMod "Unicode.hs"               "Main"

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

  ]

mkTestMain :: FilePath -> Test
mkTestMain fileName = TestCase (do r <- manipulateAstTest fileName "Main"
                                   assertBool fileName r )

mkTestMod :: FilePath -> String -> Test
mkTestMod fileName modName
  = TestCase (do r <- manipulateAstTest fileName modName
                 assertBool fileName r )

mkTestModChange :: (GHC.ParsedSource -> GHC.ParsedSource) -> FilePath -> String -> Test
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

tt :: IO ()
tt = formatTT =<< partition snd <$> sequence [ return ("", True)
    {-
    -}
    -- , manipulateAstTestWFname "ExprPragmas.hs"           "ExprPragmas"
    {-
    , manipulateAstTestWFname "ListComprehensions.hs"    "Main"
    , manipulateAstTestWFname "MonadComprehensions.hs"   "Main"
    , manipulateAstTestWFname "RecursiveDo.hs"           "Main"
    , manipulateAstTestWFname "TypeFamilies.hs"          "Main"
    , manipulateAstTestWFname "MultiParamTypeClasses.hs" "Main"
    , manipulateAstTestWFname "DataFamilies.hs"          "DataFamilies"
    , manipulateAstTestWFname "Deriving.hs"              "Main"
    , manipulateAstTestWFname "Default.hs"               "Main"
    , manipulateAstTestWFname "ForeignDecl.hs"           "ForeignDecl"
    , manipulateAstTestWFname "Warning.hs"               "Warning"
    -}
    -- , manipulateAstTestWFname "Annotations.hs"           "Annotations"
    {-
    , manipulateAstTestWFnameTH "QuasiQuote.hs"          "QuasiQuote"
    , manipulateAstTestWFname "Roles.hs"                 "Roles"
    , manipulateAstTestWFname "Splice.hs"                "Splice"
    , manipulateAstTestWFname "ImportsSemi.hs"           "ImportsSemi"
    , manipulateAstTestWFname "Stmts.hs"                 "Stmts"
    -}
    -- , manipulateAstTestWFname "Mixed.hs"                 "Main"
    {-
    , manipulateAstTestWFname "Arrow.hs"                 "Arrow"
    , manipulateAstTestWFname "PatSynBind.hs"            "Main"
    -}
    -- , manipulateAstTestWFname "HsDo.hs"                  "HsDo"
    {-
    , manipulateAstTestWFname "ForAll.hs"                "ForAll"
    , manipulateAstTestWFname "BangPatterns.hs"          "Main"
    , manipulateAstTestWFname "Associated.hs"            "Main"
    -}
    -- , manipulateAstTestWFname "Move1.hs"                 "Move1"
    {-
    , manipulateAstTestWFname "TypeOperators.hs"         "Main"
    ,  manipulateAstTestWFname "NullaryTypeClasses.hs"    "Main"
    , manipulateAstTestWFname "FunctionalDeps.hs"        "Main"
    , manipulateAstTestWFname "DerivingOC.hs"            "Main"
    , manipulateAstTestWFname "GenericDeriving.hs"       "Main"
    , manipulateAstTestWFname "OverloadedStrings.hs"     "Main"
    , manipulateAstTestWFname "RankNTypes.hs"            "Main"
    -}
    -- , manipulateAstTestWFname "Existential.hs"           "Main"
    {-
    , manipulateAstTestWFname "ScopedTypeVariables.hs"   "Main"
    , manipulateAstTestWFname "Arrows.hs"                "Main"
    , manipulateAstTestWFname "TH.hs"                    "Main"
    , manipulateAstTestWFname "StaticPointers.hs"        "Main"
    , manipulateAstTestWFname "Guards.hs"                "Main"
    , manipulateAstTestWFname "RdrNames.hs"              "RdrNames"
    -}
    -- , manipulateAstTestWFname "Vect.hs"                  "Vect"
    {-
    , manipulateAstTestWFname "Tuple.hs"                 "Main"
    , manipulateAstTestWFname "ExtraConstraints1.hs"     "ExtraConstraints1"
    , manipulateAstTestWFname "AddAndOr3.hs"             "AddAndOr3"
    -}
    -- , manipulateAstTestWFname "Ann01.hs"                 "Ann01"
    -- , manipulateAstTestWFname "StrictLet.hs"             "Main"
    {-
    , manipulateAstTestWFname "Cg008.hs"                 "Cg008"
    , manipulateAstTestWFname "T2388.hs"                 "T2388"
    , manipulateAstTestWFname "T3132.hs"                 "T3132"
    , manipulateAstTestWFname "Stream.hs"                "Stream"
    , manipulateAstTestWFname "Trit.hs"                  "Trit"
    , manipulateAstTestWFname "DataDecl.hs"              "Main"
    , manipulateAstTestWFname "Zipper.hs"                "Zipper"
    -}
    -- , manipulateAstTestWFname "Sigs.hs"                  "Sigs"
    -- , manipulateAstTestWFname "Utils2.hs"                "Utils2"
    {-
    , manipulateAstTestWFname "EmptyMostlyInst.hs"       "EmptyMostlyInst"
    , manipulateAstTestWFname "EmptyMostlyNoSemis.hs"    "EmptyMostlyNoSemis"
    , manipulateAstTestWFname "EmptyMostly.hs"           "EmptyMostly"
    , manipulateAstTestWFname "FromUtils.hs"             "Main"
    , manipulateAstTestWFname "DocDecls.hs"              "DocDecls"
    , manipulateAstTestWFname "RecordUpdate.hs"          "Main"
    -- manipulateAstTestWFname "Unicode.hs"               "Main"
    , manipulateAstTestWFname "B.hs"                     "Main"
    , manipulateAstTestWFname "LayoutWhere.hs"           "Main"
    -}
    -- , manipulateAstTestWFname "Deprecation.hs"           "Deprecation"
    {-
    , manipulateAstTestWFname "Infix.hs"                 "Main"
    , manipulateAstTestWFname "BCase.hs"                 "Main"
    , manipulateAstTestWFname "LetExprSemi.hs"           "LetExprSemi"
    , manipulateAstTestWFname "LetExpr2.hs"              "Main"
    , manipulateAstTestWFname "LetStmt.hs"               "Layout.LetStmt"
    -}
    {-
    , manipulateAstTestWFname "RebindableSyntax.hs"      "Main"
    , manipulateAstTestWithMod changeLayoutLet3 "LayoutLet4.hs" "LayoutLet4"
    , manipulateAstTestWithMod changeLayoutLet5 "LayoutLet5.hs" "LayoutLet5"
    , manipulateAstTestWFname "EmptyMostly2.hs"          "EmptyMostly2"
    , manipulateAstTestWFname "WhereIn4.hs"              "WhereIn4"
    -}
    -- , manipulateAstTestWFname "Dead1.hs"                 "Dead1"
    {-
    , manipulateAstTestWFname "DocDecls.hs"              "DocDecls"
    , manipulateAstTestWFname "ViewPatterns.hs"          "Main"
    , manipulateAstTestWFname "FooExpected.hs"          "Main"
    , manipulateAstTestWithMod changeLayoutLet2 "LayoutLet2.hs" "LayoutLet2"
    , manipulateAstTestWFname "LayoutIn1.hs"                 "LayoutIn1"
    , manipulateAstTestWithMod changeLayoutIn1  "LayoutIn1.hs" "LayoutIn1"
    , manipulateAstTestWFname "LocToName.hs"                 "LocToName"
    , manipulateAstTestWithMod changeLayoutIn4  "LayoutIn4.hs" "LayoutIn4"
    , manipulateAstTestWithMod changeLocToName  "LocToName.hs" "LocToName"
    , manipulateAstTestWithMod changeLayoutLet3 "LayoutLet3.hs" "LayoutLet3"
    , manipulateAstTestWithMod changeRename1    "Rename1.hs"  "Main"
    , manipulateAstTestWFname    "Rename1.hs"  "Main"
    -}
    -- , manipulateAstTestWFname "AltsSemis.hs"             "Main"
    -- , manipulateAstTestWFname "LetExpr.hs"               "LetExpr"
    -- , manipulateAstTestWFname "Rules.hs"                 "Rules"
    -- , manipulateAstTestWFname "LayoutLet2.hs"             "LayoutLet2"
    -- , manipulateAstTestWFname "LayoutIn3.hs"             "LayoutIn3"
    -- , manipulateAstTestWFname "LayoutIn3a.hs"             "LayoutIn3a"
    -- , manipulateAstTestWFnameMod changeLayoutIn3  "LayoutIn3a.hs" "LayoutIn3a"
    -- , manipulateAstTestWFname "LetIn1.hs"             "LetIn1"
    -- , manipulateAstTestWFnameMod changeLetIn1  "LetIn1.hs" "LetIn1"
    -- , manipulateAstTestWFnameMod changeLayoutIn3  "LayoutIn3b.hs" "LayoutIn3b"
    -- , manipulateAstTestWFnameMod changeLayoutIn3  "LayoutIn3.hs" "LayoutIn3"
    -- , manipulateAstTestWFname "LayoutLet2.hs"             "LayoutLet2"
    -- , manipulateAstTestWFname "LayoutLet.hs"             "Main"
    -- , manipulateAstTestWFname "Simple.hs"             "Main"
    -- , manipulateAstTestWFname "FunDeps.hs"               "Main"
    -- , manipulateAstTestWFname "IfThenElse3.hs"              "Main"
    , manipulateAstTestWFname "ImplicitParams.hs"        "Main"
    -- , manipulateAstTestWFname "PArr.hs"                  "PArr"
    -- , manipulateAstTestWFname "DataDecl.hs"              "Main"
    {-
    , manipulateAstTestWFname "ParensAroundContext.hs"   "ParensAroundContext"
    , manipulateAstTestWithMod changeWhereIn4 "WhereIn4.hs" "WhereIn4"
    , manipulateAstTestWFname "Cpp.hs"                   "Main"
    , manipulateAstTestWFname "Lhs.lhs"                  "Main"
    , manipulateAstTestWFname "Foo.hs"                   "Main"
-}
    ]

-- ---------------------------------------------------------------------

changeLayoutLet2 :: GHC.ParsedSource -> GHC.ParsedSource
changeLayoutLet2 parsed = rename "xxxlonger" [((7,5),(7,8)),((8,24),(8,27))] parsed

changeLocToName :: GHC.ParsedSource -> GHC.ParsedSource
changeLocToName parsed = rename "LocToName.newPoint" [((20,1),(20,11)),((20,28),(20,38)),((24,1),(24,11))] parsed

changeLayoutIn3 :: GHC.ParsedSource -> GHC.ParsedSource
changeLayoutIn3 parsed = rename "anotherX" [((7,13),(7,14)),((7,37),(7,38)),((8,37),(8,38))] parsed
-- changeLayoutIn3 parsed = rename "anotherX" [((7,13),(7,14)),((7,37),(7,38))] parsed

changeLayoutIn4 :: GHC.ParsedSource -> GHC.ParsedSource
changeLayoutIn4 parsed = rename "io" [((7,8),(7,13)),((7,28),(7,33))] parsed

changeLayoutIn1 :: GHC.ParsedSource -> GHC.ParsedSource
changeLayoutIn1 parsed = rename "square" [((7,17),(7,19)),((7,24),(7,26))] parsed

changeRename1 :: GHC.ParsedSource -> GHC.ParsedSource
changeRename1 parsed = rename "bar2" [((3,1),(3,4))] parsed

changeLayoutLet3 :: GHC.ParsedSource -> GHC.ParsedSource
changeLayoutLet3 parsed = rename "xxxlonger" [((7,5),(7,8)),((9,14),(9,17))] parsed

changeLayoutLet5 :: GHC.ParsedSource -> GHC.ParsedSource
changeLayoutLet5 parsed = rename "x" [((7,5),(7,8)),((9,14),(9,17))] parsed

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

changeWhereIn4 :: GHC.ParsedSource -> GHC.ParsedSource
changeWhereIn4 parsed
  = SYB.everywhere (SYB.mkT replace) parsed
  where
    replace :: GHC.Located GHC.RdrName -> GHC.Located GHC.RdrName
    replace (GHC.L ln _n)
      | ss2span ln == ((12,16),(12,17)) = GHC.L ln (GHC.mkRdrUnqual (GHC.mkVarOcc "p_2"))
    replace x = x

-- ---------------------------------------------------------------------

changeLetIn1 :: GHC.ParsedSource -> GHC.ParsedSource
changeLetIn1 parsed
  = SYB.everywhere (SYB.mkT replace) parsed
  where
    replace :: GHC.HsExpr GHC.RdrName -> GHC.HsExpr GHC.RdrName
    replace x@(GHC.HsLet localDecls expr@(GHC.L _ _))
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

manipulateAstTestWithMod :: (GHC.ParsedSource -> GHC.ParsedSource) -> FilePath -> String -> IO Bool
manipulateAstTestWithMod change file modname = manipulateAstTest' (Just change) False file modname

manipulateAstTestWFnameMod :: (GHC.ParsedSource -> GHC.ParsedSource) -> FilePath -> String -> IO (FilePath,Bool)
manipulateAstTestWFnameMod change fileName modname
  = do r <- manipulateAstTestWithMod change fileName modname
       return (fileName,r)

manipulateAstTest :: FilePath -> String -> IO Bool
manipulateAstTest file modname = manipulateAstTest' Nothing False file modname

manipulateAstTestWFname :: FilePath -> String -> IO (FilePath, Bool)
manipulateAstTestWFname file modname = (file,) <$> manipulateAstTest file modname

manipulateAstTestTH :: FilePath -> String -> IO Bool
manipulateAstTestTH file modname = manipulateAstTest' Nothing True file modname

manipulateAstTest' :: Maybe (GHC.ParsedSource -> GHC.ParsedSource) -> Bool -> FilePath -> String -> IO Bool
manipulateAstTest' mchange useTH file' modname = do
  let testpath = "./tests/examples/"
      file     = testpath </> file'
      out      = file <.> "out"
      expected = file <.> "expected"

  contents <- case mchange of
                   Nothing -> readUTF8File file
                   Just _  -> readUTF8File expected
  (ghcAnns',t) <- parsedFileGhc file modname useTH
  let
    parsedOrig = GHC.pm_parsed_source $ GHC.tm_parsed_module t
    (ghcAnns,parsed) = fixBugsInAst ghcAnns' parsedOrig
    parsedAST = SYB.showData SYB.Parser 0 parsed
    -- parsedAST = showGhc parsed
       -- `debug` ("getAnn:=" ++ (show (getAnnotationValue (snd ann) (GHC.getLoc parsed) :: Maybe AnnHsModule)))
    -- try to pretty-print; summarize the test result
    ann = relativiseApiAnns parsed ghcAnns
      `debug` ("ghcAnns:" ++ showGhc ghcAnns)

    parsed' = case mchange of
                   Nothing -> parsed
                   Just change -> change parsed
    printed = exactPrintWithAnns parsed' ann -- `debug` ("ann=" ++ (show $ map (\(s,a) -> (ss2span s, a)) $ Map.toList ann))
    result =
            if printed == contents
              then "Match\n"
              else printed ++ "\n==============\n"
                    ++ "lengths:" ++ show (length printed,length contents) ++ "\n"
                    ++ showAnnData ann 0 parsed'
                    ++ "\n========================\n"
                    ++ showGhc ann
                    ++ "\n========================\n"
                    ++ showGhc ghcAnns
                    ++ "\n========================\n"
                    ++ parsedAST
  writeFile out $ result
  -- putStrLn $ "Test:parsed=" ++ parsedAST
  -- putStrLn $ "Test:showdata:parsedOrig" ++ SYB.showData SYB.Parser 0 parsedOrig
  -- putStrLn $ "Test:ann :" ++ showGhc ann
  -- putStrLn $ "Test:ghcAnns :" ++ showGhc ghcAnns
  -- putStrLn $ "Test:ghcAnns' :" ++ showGhc ghcAnns'
  -- putStrLn $ "Test:showdata:" ++ showAnnData ann 0 parsed
  -- putStrLn $ "Test:showdata:parsed'" ++ SYB.showData SYB.Parser 0 parsed'
  -- putStrLn $ "Test:showdata:parsed'" ++ showAnnData ann 0 parsed'
  return ("Match\n" == result)


-- ---------------------------------------------------------------------
-- |Result of parsing a Haskell source file. It is simply the
-- TypeCheckedModule produced by GHC.
type ParseResult = GHC.TypecheckedModule

parsedFileGhc :: String -> String -> Bool -> IO (GHC.ApiAnns,ParseResult)
parsedFileGhc fileName modname useTH = do
    -- putStrLn $ "parsedFileGhc:" ++ show fileName
    GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut $ do
      GHC.runGhc (Just libdir) $ do
        dflags <- GHC.getSessionDynFlags
        let dflags'' = dflags { GHC.importPaths = ["./tests/examples/","../tests/examples/",
                                                   "./src/","../src/"] }

            tgt = if useTH then GHC.HscInterpreted
                           else GHC.HscNothing -- allows FFI
            dflags''' = dflags'' { GHC.hscTarget = tgt,
                                   GHC.ghcLink =  GHC.LinkInMemory
                                  , GHC.packageFlags = [GHC.ExposePackage (GHC.PackageArg "ghc") (GHC.ModRenaming False [])]
                                 }

            dflags4 = if False -- useHaddock
                        then GHC.gopt_set (GHC.gopt_set dflags''' GHC.Opt_Haddock)
                                       GHC.Opt_KeepRawTokenStream
                        else GHC.gopt_set dflags'''
                                       GHC.Opt_KeepRawTokenStream
                        -- else GHC.gopt_set (GHC.gopt_unset dflags''' GHC.Opt_Haddock)
                        --               GHC.Opt_KeepRawTokenStream

        (dflags5,_args,_warns) <- GHC.parseDynamicFlagsCmdLine dflags4 [GHC.noLoc "-package ghc"]
        -- GHC.liftIO $ putStrLn $ "dflags set:(args,warns)" ++ show (map GHC.unLoc args,map GHC.unLoc warns)
        void $ GHC.setSessionDynFlags dflags5
        -- GHC.liftIO $ putStrLn $ "dflags set"

        target <- GHC.guessTarget fileName Nothing
        GHC.setTargets [target]
        -- GHC.liftIO $ putStrLn $ "target set:" ++ showGhc (GHC.targetId target)
        void $ GHC.load GHC.LoadAllTargets -- Loads and compiles, much as calling make
        -- GHC.liftIO $ putStrLn $ "targets loaded"
        -- g <- GHC.getModuleGraph
        -- let showStuff ms = show (GHC.moduleNameString $ GHC.moduleName $ GHC.ms_mod ms,GHC.ms_location ms)
        -- GHC.liftIO $ putStrLn $ "module graph:" ++ (intercalate "," (map showStuff g))

        modSum <- GHC.getModSummary $ GHC.mkModuleName modname
        -- GHC.liftIO $ putStrLn $ "got modSum"
        -- let modSum = head g
{-
        (sourceFile, source, flags) <- getModuleSourceAndFlags (GHC.ms_mod modSum)
        strSrcBuf <- getPreprocessedSrc sourceFile
        GHC.liftIO $ putStrLn $ "preprocessedSrc====\n" ++ strSrcBuf ++ "\n================\n"
-}
        p <- GHC.parseModule modSum
        -- GHC.liftIO $ putStrLn $ "got parsedModule"
        t <- GHC.typecheckModule p
        -- GHC.liftIO $ putStrLn $ "typechecked"
        -- toks <- GHC.getRichTokenStream (GHC.ms_mod modSum)
        -- GHC.liftIO $ putStrLn $ "toks" ++ show toks
        let anns = GHC.pm_annotations p
        -- GHC.liftIO $ putStrLn $ "anns"
        return (anns,t)

readUTF8File :: FilePath -> IO String
readUTF8File fp = openFile fp ReadMode >>= \h -> do
        hSetEncoding h utf8
        hGetContents h

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

-- | The preprocessed files are placed in a temporary directory, with
-- a temporary name, and extension .hscpp. Each of these files has
-- three lines at the top identifying the original origin of the
-- files, which is ignored by the later stages of compilation except
-- to contextualise error messages.
getPreprocessedSrc ::
  -- GHC.GhcMonad m => FilePath -> m GHC.StringBuffer
  GHC.GhcMonad m => FilePath -> m String
getPreprocessedSrc srcFile = do
  df <- GHC.getSessionDynFlags
  d <- GHC.liftIO $ getTempDir df
  fileList <- GHC.liftIO $ getDirectoryContents d
  let suffix = "hscpp"

  let cppFiles = filter (\f -> getSuffix f == suffix) fileList
  origNames <- GHC.liftIO $ mapM getOriginalFile $ map (\f -> d </> f) cppFiles
  let tmpFile = ghead "getPreprocessedSrc" $ filter (\(o,_) -> o == srcFile) origNames
  -- buf <- GHC.liftIO $ GHC.hGetStringBuffer $ snd tmpFile
  -- return buf
  GHC.liftIO $ readUTF8File (snd tmpFile)

-- ---------------------------------------------------------------------

getSuffix :: FilePath -> String
getSuffix fname = reverse $ fst $ break (== '.') $ reverse fname

-- | A GHC preprocessed file has the following comments at the top
-- @
-- # 1 "./test/testdata/BCpp.hs"
-- # 1 "<command-line>"
-- # 1 "./test/testdata/BCpp.hs"
-- @
-- This function reads the first line of the file and returns the
-- string in it.
-- NOTE: no error checking, will blow up if it fails
getOriginalFile :: FilePath -> IO (FilePath,FilePath)
getOriginalFile fname = do
  fcontents <- readFile fname
  let firstLine = ghead "getOriginalFile" $ lines fcontents
  let (_,originalFname) = break (== '"') firstLine
  return $ (tail $ init $ originalFname,fname)


-- ---------------------------------------------------------------------
-- Copied from the GHC source, since not exported

getModuleSourceAndFlags :: GHC.GhcMonad m => GHC.Module -> m (String, GHC.StringBuffer, GHC.DynFlags)
getModuleSourceAndFlags modu = do
  m <- GHC.getModSummary (GHC.moduleName modu)
  case GHC.ml_hs_file $ GHC.ms_location m of
    Nothing ->
               do dflags <- GHC.getDynFlags
                  GHC.liftIO $ throwIO $ GHC.mkApiErr dflags (GHC.text "No source available for module " GHC.<+> GHC.ppr modu)
    Just sourceFile -> do
        source <- GHC.liftIO $ GHC.hGetStringBuffer sourceFile
        return (sourceFile, source, GHC.ms_hspp_opts m)


-- return our temporary directory within tmp_dir, creating one if we
-- don't have one yet
getTempDir :: GHC.DynFlags -> IO FilePath
getTempDir dflags
  = do let ref = GHC.dirsToClean dflags
           tmp_dir = GHC.tmpDir dflags
       mapping <- readIORef ref
       case Map.lookup tmp_dir mapping of
           Nothing -> error "should already be a tmpDir"
           Just d -> return d
