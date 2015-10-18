{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Test.Common (
                RoundtripReport (..)
              , Report
              , ParseFailure(..)
              , ReportType(..)
              , roundTripTest
              , getModSummaryForFile

              , testList
              , testPrefix
              , Changer
              , genTest
              , noChange
              ) where



import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Utils
import Language.Haskell.GHC.ExactPrint.Preprocess

import GHC.Paths (libdir)

import qualified ApiAnnotation as GHC
import qualified DynFlags      as GHC
import qualified FastString    as GHC
import qualified GHC           as GHC hiding (parseModule)
import qualified HeaderInfo    as GHC
import qualified Lexer         as GHC
import qualified MonadUtils    as GHC
import qualified Outputable    as GHC
import qualified Parser        as GHC
import qualified SrcLoc        as GHC
import qualified StringBuffer  as GHC

import qualified Data.Map as Map

import Data.List hiding (find)

import Control.Monad
import System.Directory

import Test.Consistency

import Control.Arrow (first)
import Test.HUnit
import System.FilePath

-- import Debug.Trace
testPrefix :: FilePath
testPrefix = "tests" </> "examples"

testList :: String -> [Test] -> Test
testList s ts = TestLabel s (TestList ts)

-- ---------------------------------------------------------------------
-- Roundtrip machinery

type Report = Either ParseFailure RoundtripReport

data RoundtripReport =
  Report
   { debugTxt :: String
   , status   :: ReportType
   , cppStatus :: Maybe String -- Result of CPP if invoked
   , inconsistent :: Maybe [(GHC.SrcSpan, (GHC.AnnKeywordId, [GHC.SrcSpan]))]
   }

data ParseFailure = ParseFailure GHC.SrcSpan String

data ReportType =
   Success
 | RoundTripFailure deriving (Eq, Show)

runParser :: GHC.P a -> GHC.DynFlags -> FilePath -> String -> GHC.ParseResult a
runParser parser flags filename str = GHC.unP parser parseState
    where
      location = GHC.mkRealSrcLoc (GHC.mkFastString filename) 1 1
      buffer = GHC.stringToStringBuffer str
      parseState = GHC.mkPState flags buffer location

parseFile :: GHC.DynFlags -> FilePath -> String -> GHC.ParseResult (GHC.Located (GHC.HsModule GHC.RdrName))
parseFile = runParser GHC.parseModule

mkApiAnns :: GHC.PState -> GHC.ApiAnns
mkApiAnns pstate = (Map.fromListWith (++) . GHC.annotations $ pstate
                   , Map.fromList ((GHC.noSrcSpan, GHC.comment_q pstate) : (GHC.annotations_comments pstate)))

removeSpaces :: String -> String
removeSpaces = map (\case {'\160' -> ' '; s -> s})

initDynFlags :: GHC.GhcMonad m => FilePath -> m GHC.DynFlags
initDynFlags file = do
  dflags0 <- GHC.getSessionDynFlags
  let dflags1 = GHC.gopt_set dflags0 GHC.Opt_KeepRawTokenStream
  src_opts <- GHC.liftIO $ GHC.getOptionsFromFile dflags1 file
  (!dflags2, _, _)
    <- GHC.parseDynamicFilePragma dflags1 src_opts
  void $ GHC.setSessionDynFlags dflags2
  return dflags2

roundTripTest :: FilePath -> IO Report
roundTripTest f = genTest noChange f f

type Changer = (Anns -> GHC.ParsedSource -> IO (Anns,GHC.ParsedSource))

noChange :: Changer
noChange ans parsed = return (ans,parsed)

genTest :: Changer -> FilePath -> FilePath -> IO Report
genTest f origFile expectedFile  =
  GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut $
    GHC.runGhc (Just libdir) $ do
      dflags <- initDynFlags origFile
      let useCpp = GHC.xopt GHC.Opt_Cpp dflags
      (fileContents, injectedComments) <-
        if useCpp
          then do
            contents <- getPreprocessedSrcDirect defaultCppOptions origFile
            cppComments <- getCppTokensAsComments defaultCppOptions origFile
            return (contents,cppComments)
          else do
            txt <- GHC.liftIO $ readFile origFile
            let (contents1,lp) = stripLinePragmas txt
            return (contents1,lp)

      orig <- GHC.liftIO $ readFile origFile
      expected <- GHC.liftIO $ readFile expectedFile
      let origContents = removeSpaces fileContents
          pristine     = removeSpaces expected
      case parseFile dflags origFile origContents of
        GHC.PFailed ss m -> return . Left $ ParseFailure ss (GHC.showSDoc dflags m)
        GHC.POk (mkApiAnns -> apianns) pmod   -> do
          let -- Clang cpp adds an extra newline character
              -- Do not remove this line!
              trimPrinted p = if useCpp
                                then unlines $ take (length (lines pristine)) (lines p)
                                else p
          (printed', anns, pmod') <- GHC.liftIO (runRoundTrip f apianns pmod injectedComments)
          let printed = trimPrinted printed'
          let debugTxt = mkDebugOutput origFile printed pristine apianns anns pmod'
              consistency = checkConsistency apianns pmod
              inconsistent = if null consistency then Nothing else Just consistency
              status = if printed == pristine then Success else RoundTripFailure
              cppStatus = if useCpp then Just origContents else Nothing
          return $ Right Report {..}


mkDebugOutput :: FilePath -> String -> String
              -> GHC.ApiAnns
              -> Anns
              -> GHC.ParsedSource -> String
mkDebugOutput filename printed original apianns anns parsed =
  intercalate sep [ printed
                 , filename
                 , "lengths:" ++ show (length printed,length original) ++ "\n"
                 , showAnnData anns 0 parsed
                 , showGhc anns
                 , showGhc apianns
                ]
  where
    sep = "\n==============\n"



runRoundTrip :: Changer
             -> GHC.ApiAnns -> GHC.Located (GHC.HsModule GHC.RdrName)
             -> [Comment]
             -> IO (String, Anns, GHC.ParsedSource)
runRoundTrip f !anns !parsedOrig cs = do
  let !relAnns = relativiseApiAnnsWithComments cs parsedOrig anns
  (annsMod, pmod) <- f relAnns parsedOrig
  let !printed = exactPrint pmod annsMod
  return (printed,  relAnns, pmod)

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
  cgraph <- GHC.liftIO $ canonicalizeGraph graph

  let mm = filter (\(mfn,_ms) -> mfn == Just cfileName) cgraph
  case mm of
   [] -> return Nothing
   fs -> return (Just (snd $ head fs))
