{-# LANGUAGE CPP #-}
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
              , mkParsingTest
              , getModSummaryForFile

              , testList
              , testPrefix
              , Changer
              , genTest
              , noChange
              , mkDebugOutput
#if __GLASGOW_HASKELL__ >= 808
              , showErrorMessages
#endif
              ) where



import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Utils
import Language.Haskell.GHC.ExactPrint.Parsers (parseModuleApiAnnsWithCpp)
import Language.Haskell.GHC.ExactPrint.Preprocess
import Language.Haskell.GHC.ExactPrint.Types


#if __GLASGOW_HASKELL__ >= 808
import qualified Control.Monad.IO.Class as GHC
import qualified GHC hiding (parseModule)
import qualified GHC.Data.Bag          as GHC
import qualified GHC.Driver.Session    as GHC
import qualified GHC.Utils.Error       as GHC
import qualified GHC.Utils.Outputable  as GHC
#else
import qualified ApiAnnotation as GHC
import qualified DynFlags      as GHC
#if __GLASGOW_HASKELL__ >= 808
import qualified Bag           as GHC
import qualified ErrUtils      as GHC
#endif
import qualified GHC           as GHC hiding (parseModule)
import qualified MonadUtils    as GHC
#endif

#if __GLASGOW_HASKELL__ <= 710
#else
import qualified GHC.LanguageExtensions as LangExt
#endif

-- import qualified Data.Map as Map

import Control.Monad
import Data.List hiding (find)

import System.Directory

import Test.Consistency

import Test.HUnit
import System.FilePath

-- import Debug.Trace
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
   , inconsistent :: Maybe [(AnnSpan, (GHC.AnnKeywordId, [AnnSpan]))]
   }

data ParseFailure = ParseFailure String

data ReportType =
   Success
 | RoundTripFailure deriving (Eq, Show)

{-
runParser :: GHC.P a -> GHC.DynFlags -> FilePath -> String -> GHC.ParseResult a
runParser parser flags filename str = GHC.unP parser parseState
    where
      location = GHC.mkRealSrcLoc (GHC.mkFastString filename) 1 1
      buffer = GHC.stringToStringBuffer str
      parseState = GHC.mkPState flags buffer location

parseFile :: GHC.DynFlags -> FilePath -> String -> GHC.ParseResult (GHC.Located (GHC.HsModule GhcPs))
parseFile = runParser GHC.parseModule

mkApiAnns :: GHC.PState -> GHC.ApiAnns
mkApiAnns pstate = (Map.fromListWith (++) . GHC.annotations $ pstate
                   , Map.fromList ((GHC.noSrcSpan, GHC.comment_q pstate) : (GHC.annotations_comments pstate)))

removeSpaces :: String -> String
removeSpaces = map (\case {'\160' -> ' '; s -> s})
-}

roundTripTest :: FilePath -> IO Report
roundTripTest f = genTest noChange f f


mkParsingTest :: (FilePath -> IO Report) -> FilePath -> FilePath -> Test
mkParsingTest tester dir fp =
  let basename       = testPrefix </> dir </> fp
      writeFailure   = writeFile (basename <.> "out")
      writeHsPP      = writeFile (basename <.> "hspp")
      writeIncons s  = writeFile (basename <.> "incons") (showGhc s)
  in
    TestCase (do r <- either (\(ParseFailure s) -> error (s ++ basename)) id
                        <$> tester basename
                 writeFailure (debugTxt r)
                 forM_ (inconsistent r) writeIncons
                 forM_ (cppStatus r) writeHsPP
                 assertBool fp (status r == Success))


type Changer = (Anns -> GHC.ParsedSource -> IO (Anns,GHC.ParsedSource))

noChange :: Changer
noChange ans parsed = return (ans,parsed)

genTest :: Changer -> FilePath -> FilePath -> IO Report
genTest f origFile expectedFile  = do
      res <- parseModuleApiAnnsWithCpp defaultCppOptions origFile
      expected <- GHC.liftIO $ readFileGhc expectedFile
      orig <- GHC.liftIO $ readFileGhc origFile
      -- let pristine = removeSpaces expected
      let pristine = expected

      case res of
#if __GLASGOW_HASKELL__ >= 808
        Left m -> return . Left $ ParseFailure (showErrorMessages m)
#else
        Left (_ss, m) -> return . Left $ ParseFailure m
#endif
        Right (apianns, injectedComments, dflags, pmod)  -> do
          (printed', anns, pmod') <- GHC.liftIO (runRoundTrip f apianns pmod injectedComments)
#if __GLASGOW_HASKELL__ <= 710
          let useCpp = GHC.xopt GHC.Opt_Cpp dflags
#else
          let useCpp = GHC.xopt LangExt.Cpp dflags
#endif
              printed = trimPrinted printed'
          -- let (printed, anns) = first trimPrinted $ runRoundTrip apianns pmod injectedComments
              -- Clang cpp adds an extra newline character
              -- Do not remove this line!
              trimPrinted p = if useCpp
                                then unlines $ take (length (lines pristine)) (lines p)
                                else p
              debugTxt = mkDebugOutput origFile printed pristine apianns anns pmod'
              consistency = checkConsistency apianns pmod
              inconsistent = if null consistency then Nothing else Just consistency
              status = if printed == pristine then Success else RoundTripFailure
              cppStatus = if useCpp then Just orig else Nothing
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
#if __GLASGOW_HASKELL__ >= 808
             -> GHC.ApiAnns -> GHC.Located GHC.HsModule
#else
             -> GHC.ApiAnns -> GHC.Located (GHC.HsModule GhcPs)
#endif
             -> [Comment]
             -> IO (String, Anns, GHC.ParsedSource)
runRoundTrip f !anns !parsedOrig cs = do
  let !relAnns = relativiseApiAnnsWithComments cs parsedOrig anns
  (annsMod, pmod) <- f relAnns parsedOrig
  let !printed = exactPrint pmod annsMod
  -- return (printed,  relAnns, pmod)
  return (printed,  annsMod, pmod)

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
#if __GLASGOW_HASKELL__ >= 804
  cgraph <- GHC.liftIO $ canonicalizeGraph (GHC.mgModSummaries graph)
#else
  cgraph <- GHC.liftIO $ canonicalizeGraph graph
#endif

  let mm = filter (\(mfn,_ms) -> mfn == Just cfileName) cgraph
  case mm of
   [] -> return Nothing
   fs -> return (Just (snd $ head fs))

-- ---------------------------------------------------------------------

#if __GLASGOW_HASKELL__ >= 808
showErrorMessages :: GHC.ErrorMessages -> String
showErrorMessages m = show $ GHC.bagToList m
#endif

-- ---------------------------------------------------------------------

#if __GLASGOW_HASKELL__ >= 808
instance GHC.Outputable GHC.ApiAnns where
  ppr (GHC.ApiAnns items eof comments rogueComments)
    = GHC.text "ApiAnns" GHC.<+> GHC.ppr items
                         GHC.<+> GHC.ppr eof
                         GHC.<+> GHC.ppr comments
                         GHC.<+> GHC.ppr rogueComments
#endif

-- ---------------------------------------------------------------------
