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
              , useGhcCpp
              , getHackageVersionMacros
              , presetHackageVersionMacros
              ) where



import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Utils
import Language.Haskell.GHC.ExactPrint.Parsers
import Language.Haskell.GHC.ExactPrint.Preprocess

import qualified Control.Monad.IO.Class as GHC
import GHC hiding (moduleName)
import GHC.Driver.Errors.Types
import qualified Data.List.NonEmpty as NE
import qualified GHC.Data.FastString    as GHC
import qualified GHC.Data.StringBuffer  as GHC
import qualified GHC.Driver.Config.Parser as GHC
import qualified GHC.Driver.Session    as GHC
import qualified GHC.Parser            as GHC
import qualified GHC.Parser.Lexer      as Lexer
import qualified GHC.Parser.PreProcess.State as GHC
import qualified GHC.Types.SrcLoc       as GHC

import qualified GHC.LanguageExtensions as LangExt
import GHC.Types.Error
import GHC.Utils.Error
import GHC.Utils.Outputable (renderWithContext, defaultSDocContext, vcat)

import Control.Monad
import Data.IORef
import Data.List hiding (find)
import qualified Data.Map as Map

import System.Directory

import Test.CommonUtils (hackageVersionMacros)

import Test.HUnit
import System.FilePath
import qualified GHC.Paths

useGhcCpp :: Bool
useGhcCpp = True
-- useGhcCpp = False

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
      res <- parseModuleEpAnnsWithCpp libdir useGhcCpp defaultCppOptions origFile
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

presetHackageVersionMacros :: LibDir -> IO ()
presetHackageVersionMacros libdir = do
  -- ms <- getHackageVersionMacros
  ms <- getMacroDefines libdir
  writeIORef macroIORef (Just ms)

getHackageVersionMacros :: IO (Maybe String)
getHackageVersionMacros =
  if useGhcCpp
      then Just <$> hackageVersionMacros
      else return Nothing

parseMacroDefines :: DynFlags -> String -> GHC.MacroDefines
parseMacroDefines df0 macro_defs = defines
  where
    df = GHC.xopt_set df0 LangExt.GhcCpp
    opts = GHC.initParserOpts df
    pos = GHC.mkRealSrcLoc (GHC.mkFastString "macros") 1 1
    p_state0 = Lexer.initParserState (GHC.initPpState { GHC.pp_defines = GHC.predefinedMacros df
                                                      , GHC.pp_scope = (GHC.PpScope True GHC.PpNoGroup) NE.:| [] })
                                     opts (GHC.stringToStringBuffer macro_defs) pos
    defines =
        case Lexer.unP GHC.parseHeader p_state0 of
          Lexer.PFailed _ -> Map.empty
          -- Lexer.PFailed _ -> error $ "blah:"
          Lexer.POk st  _ -> GHC.pp_defines (Lexer.pp st)
          -- Lexer.POk st r -> error $ "sss:" ++ show (GHC.pp_defines $ Lexer.pp st)


getMacroDefines :: LibDir -> IO GHC.MacroDefines
getMacroDefines libdir = ghcWrapper libdir $ do
    dflags <- GHC.getSessionDynFlags
    macro_defs <- GHC.liftIO hackageVersionMacros
    return $ parseMacroDefines dflags macro_defs


fff = do
  let libdir = GHC.Paths.libdir
  hhh libdir

hhh :: LibDir -> IO GHC.MacroDefines
hhh libdir = ghcWrapper libdir $ do
    dflags <- GHC.getSessionDynFlags
    let macro_defs = concat
            [ ""
            , "#define VERSION_logging_effect_extra_handler \"2.0.1\""
            , "#define MIN_VERSION_logging_effect_extra_handler(major1,major2,minor) (\\"
            , "  (major1) <  2 || \\"
            , "  (major1) == 2 && (major2) <  0 || \\"
            , "  (major1) == 2 && (major2) == 0 && (minor) <= 1)"
            , ""
            ]
    return $ parseMacroDefines dflags macro_defs
