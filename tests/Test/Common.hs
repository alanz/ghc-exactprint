{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Test.Common (
                RoundtripReport (..)
              , Report
              , ParseFailure(..)
              , ReportType(..)
              , roundTripTest
              , roundTripTestBC
              , roundTripTestMD
              , mkParsingTest
              , getModSummaryForFile

              , testList
              , testPrefix
              , Changer
              , genTest
              , noChange
              , changeMakeDelta
              , mkDebugOutput
              , showErrorMessages
              , LibDir
              ) where



import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Utils
import Language.Haskell.GHC.ExactPrint.Parsers
import Language.Haskell.GHC.ExactPrint.Preprocess

import qualified Control.Monad.IO.Class as GHC
import GHC hiding (moduleName)
import GHC.Driver.Errors.Types
import qualified GHC.Driver.Session    as GHC
import GHC.Types.Error
import GHC.Utils.Error
import GHC.Utils.Outputable (renderWithContext, defaultSDocContext, vcat)

import qualified GHC.LanguageExtensions as LangExt

import Control.Monad
import Data.List hiding (find)

import System.Directory

import Test.HUnit
import System.FilePath

testPrefix :: FilePath
testPrefix = "." </> "tests" </> "examples"

testList :: String -> [Test] -> Test
testList s ts = TestLabel s (TestList ts)

-- ---------------------------------------------------------------------
-- Roundtrip machinery

type Report = Either ParseFailure RoundtripReport

data RoundtripReport =
  Report
   { debugTxt     :: String
   , status       :: ReportType
   , cppStatus    :: Maybe String -- Result of CPP if invoked
   -- , inconsistent :: Maybe [(AnnSpan, (GHC.AnnKeywordId, [AnnSpan]))]
   }

data ParseFailure = ParseFailure String

data ReportType =
   Success
 | RoundTripFailure deriving (Eq, Show)


roundTripTest :: LibDir -> FilePath -> IO Report
roundTripTest libdir f = genTest libdir noChange f f

roundTripTestBC :: LibDir -> FilePath -> IO Report
roundTripTestBC libdir f = genTest libdir changeBalanceComments f f

roundTripTestMD :: LibDir -> FilePath -> IO Report
roundTripTestMD libdir f = genTest libdir changeMakeDelta f f

mkParsingTest :: (FilePath -> IO Report) -> FilePath -> FilePath -> Test
mkParsingTest tester dir fp =
  let basename       = testPrefix </> dir </> fp
      writeFailure   = writeFile (basename <.> "out")
      writeHsPP      = writeFile (basename <.> "hspp")
      -- writeIncons s  = writeFile (basename <.> "incons") (showGhc s)
  in
    TestCase (do r <- either (\(ParseFailure s) -> error (s ++ basename)) id
                        <$> tester basename
                 writeFailure (debugTxt r)
                 -- forM_ (inconsistent r) writeIncons
                 forM_ (cppStatus r) writeHsPP
                 assertBool fp (status r == Success))

type Changer = LibDir -> (GHC.ParsedSource -> IO GHC.ParsedSource)

noChange :: Changer
noChange _libdir parsed = return parsed

changeBalanceComments :: Changer
changeBalanceComments _libdir top = do
  let (GHC.L l p) = top
  let decls0 = GHC.hsmodDecls p
      decls = balanceCommentsList decls0
  let p2 = p { GHC.hsmodDecls = decls}
  return (GHC.L l p2)

changeMakeDelta :: Changer
changeMakeDelta _libdir m = do
  return (makeDeltaAst m)

genTest :: LibDir -> Changer -> FilePath -> FilePath -> IO Report
genTest libdir f origFile expectedFile  = do
      res <- parseModuleEpAnnsWithCpp libdir defaultCppOptions origFile
      expected <- GHC.liftIO $ readFileGhc expectedFile
      orig <- GHC.liftIO $ readFileGhc origFile
      -- let pristine = removeSpaces expected
      let pristine = expected

      case res of
        Left m -> return . Left $ ParseFailure (showErrorMessages m)
        Right (injectedComments, dflags, pmod)  -> do
          (printed', pmod') <- GHC.liftIO (runRoundTrip libdir f pmod injectedComments)
          let useCpp = GHC.xopt LangExt.Cpp dflags
              printed = trimPrinted printed'
              -- Clang cpp adds an extra newline character
              -- Do not remove this line!
              trimPrinted p = if useCpp
                                then unlines $ take (length (lines pristine)) (lines p)
                                else p
              debugTxt = mkDebugOutput origFile printed pristine pmod'
              -- consistency = checkConsistency apianns pmod
              -- inconsistent = if null consistency then Nothing else Just consistency
              status = if printed == pristine then Success else RoundTripFailure
              cppStatus = if useCpp then Just orig else Nothing
          return $ Right Report {..}

showErrorMessages :: Messages GhcMessage -> String
showErrorMessages msgs =
  renderWithContext defaultSDocContext
    $ vcat
    $ pprMsgEnvelopeBagWithLocDefault
    $ getMessages
    $ msgs

-- showErrorMessages :: Messages GhcMessage -> String
-- showErrorMessages msgs =
--   renderWithContext defaultSDocContext
--     $ vcat
--     $ pprMsgEnvelopeBagWithLocDefault
--     $ getMessages
--     $ msgs


mkDebugOutput :: FilePath -> String -> String
              -> GHC.ParsedSource -> String
mkDebugOutput filename printed original parsed =
  intercalate sep [ printed
                 , filename
                 , "lengths:" ++ show (length printed,length original) ++ "\n"
                 -- , showAnnData anns 0 parsed
                 , showAst parsed
                 -- , showGhc anns
                ]
  where
    sep = "\n==============\n"



runRoundTrip :: LibDir
             -> Changer
             -> GHC.Located (GHC.HsModule GHC.GhcPs)
             -> [GHC.LEpaComment]
             -> IO (String, GHC.ParsedSource)
runRoundTrip libdir f !parsedOrig cs = do
  -- putStrLn $ "comments:" ++ showAst cs
  let !parsedOrigWithComments = insertCppComments parsedOrig cs
  -- putStrLn $ "parsedOrigWithComments:" ++ showAst parsedOrigWithComments
  pmod <- f libdir parsedOrigWithComments
  let !printed = exactPrint pmod
  return (printed, pmod)

-- ---------------------------------------------------------------------`

canonicalizeGraph ::
  [GHC.ModSummary] -> IO [(Maybe (FilePath), GHC.ModSummary)]
canonicalizeGraph graph = do
  let mm = map (\m -> (GHC.ml_hs_file $ GHC.ms_location m, m)) graph
      canon ((Just fp),m) = do
        fp' <- canonicalizePath fp
        return $ (Just fp',m)
      canon (Nothing,m)  = return (Nothing,m)

  mm' <- mapM canon mm

  return mm'

-- ---------------------------------------------------------------------

getModSummaryForFile :: (GHC.GhcMonad m) => FilePath -> m (Maybe GHC.ModSummary)
getModSummaryForFile fileName = do
  cfileName <- GHC.liftIO $ canonicalizePath fileName

  graph <- GHC.getModuleGraph
  cgraph <- GHC.liftIO $ canonicalizeGraph (GHC.mgModSummaries graph)

  let mm = filter (\(mfn,_ms) -> mfn == Just cfileName) cgraph
  case mm of
   (f:_) -> return (Just (snd f))
   _ -> return Nothing

-- ---------------------------------------------------------------------
