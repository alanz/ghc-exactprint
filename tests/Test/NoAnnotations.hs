{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
module Test.NoAnnotations where

-- import Control.Monad.State
import Data.Algorithm.Diff
import Data.Algorithm.DiffOutput
-- import Data.Data (Data, toConstr, showConstr, cast)
-- import Data.Generics (extQ, ext1Q, ext2Q, gmapQ)
import Data.List
-- import Data.Ord (comparing)
-- import qualified Data.ByteString as B

import Language.Haskell.GHC.ExactPrint
-- import Language.Haskell.GHC.ExactPrint.ExactPrint
import Language.Haskell.GHC.ExactPrint.Parsers
import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Utils

import qualified GHC.Utils.Outputable   as GHC
import qualified Control.Monad.IO.Class as GHC
import qualified GHC                    as GHC hiding (parseModule)
-- import qualified GHC.Driver.Ppr         as GHC
import qualified GHC.Hs.Dump            as GHC

import System.Directory
import System.FilePath

import Test.Common

import Test.HUnit

{-# ANN module "HLint: ignore Eta reduce" #-}

-- ---------------------------------------------------------------------

noAnnotationTests :: Test
noAnnotationTests = TestLabel "no annotation tests" $ TestList
  [
    TestLabel "no annotations"
       (TestList noAnnTests)
  ]

noAnnTests :: [Test]
noAnnTests = [

        ]

-- ---------------------------------------------------------------------

mkPrettyRoundtrip :: LibDir -> FilePath -> FilePath -> Test
mkPrettyRoundtrip libdir dir fp = mkParsingTest (prettyRoundtripTest libdir) dir fp

prettyRoundtripTest :: LibDir -> FilePath -> IO Report
prettyRoundtripTest libdir origFile = do
      -- res <- parseModuleApiAnnsWithCpp defaultCppOptions origFile
      res <- parseModuleEpAnnsWithCpp libdir defaultCppOptions origFile
      case res of
        Left m -> return . Left $ ParseFailure (showErrorMessages m)
        Right (injectedComments, _dflags, parsed)  -> do
          res2 <- GHC.liftIO (runPrettyRoundTrip libdir origFile parsed injectedComments)
          case res2 of
            Left m -> return . Left $ ParseFailure (showErrorMessages m)
            Right parsed' -> do
              let
                originalStructure  = astStructure parsed
                roundtripStructure = astStructure parsed'
                (status,debugTxt') = if roundtripStructure == originalStructure
                  then (Success, "ok")
                  else (RoundTripFailure,diffText originalStructure roundtripStructure
                         ++ sep ++ originalStructure ++ sep ++ roundtripStructure)
                cppStatus = Nothing
                debugTxt = intercalate sep [ debugTxt'
                                           , originalStructure
                                           , roundtripStructure
                                           , showAst parsed
                                           ]
                sep = "\n=====================================\n"
              return $ Right Report {debugTxt,status,cppStatus}

-- ---------------------------------------------------------------------

runPrettyRoundTrip :: LibDir -> FilePath -> GHC.ParsedSource
                   -> [GHC.LEpaComment]
                   -> IO (ParseResult GHC.ParsedSource)
runPrettyRoundTrip libdir origFile !parsedOrig _cs = do
  let priorComments = GHC.priorComments $ GHC.epAnnComments $ GHC.hsmodAnn
        $ GHC.hsmodExt $ GHC.unLoc parsedOrig
  let comments = map tokComment priorComments
  let pragmas = filter (\(Comment c _ _ _) -> isPrefixOf "{-#" c ) comments
  let pragmaStr = intercalate "\n" $ map commentContents pragmas

  let !printed = pragmaStr ++ "\n" ++ exactPrint parsedOrig

  parseString libdir origFile printed parsedOrig


parseString :: LibDir -> FilePath -> String -> GHC.ParsedSource
            -> IO (ParseResult GHC.ParsedSource)
parseString libdir origFile src origParsed = do
  tmpDir <- getTemporaryDirectory
  let workDir = tmpDir </> "ghc-exactprint" </> "noannotations"
  -- putStrLn $ "workDir=" ++ workDir
  createDirectoryIfMissing True workDir
  let fileName = workDir </> takeFileName origFile
  writeFile (workDir </> takeFileName origFile <.> ".anns")
      (showAst  origParsed)
  writeFile fileName src
  parseModule libdir fileName

-- ---------------------------------------------------------------------

diffText :: String -> String -> String
diffText f1 f2 = diff
  where
    d = getGroupedDiff (lines f1) (lines f2)
    diff = ppDiff d

-- ---------------------------------------------------------------------

-- |Convert an AST with comments into a string representing the structure only
-- (i.e. ignoring locations), to be used for comparisons between the original
-- AST and the one after pretty-print roundtripping.
astStructure :: GHC.ParsedSource -> String
astStructure parsed = r
  where
    r = GHC.showSDocUnsafe
        $ GHC.showAstData GHC.BlankSrcSpanFile GHC.NoBlankEpAnnotations parsed
