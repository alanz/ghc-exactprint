{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
module Common where

import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Transform
import Language.Haskell.GHC.ExactPrint.Utils
import Language.Haskell.GHC.ExactPrint.Preprocess

import GHC.Paths (libdir)

import qualified ApiAnnotation as GHC
import qualified BasicTypes    as GHC
import qualified DynFlags      as GHC
import qualified Exception     as GHC
import qualified FastString    as GHC
import qualified GHC           as GHC hiding (parseModule)
import qualified HeaderInfo    as GHC
import qualified HscTypes      as GHC
import qualified HsSyn         as GHC
import qualified Lexer         as GHC
import qualified MonadUtils    as GHC
import qualified Outputable    as GHC
import qualified Parser        as GHC
import qualified RdrName       as GHC
import qualified SrcLoc        as GHC
import qualified StringBuffer  as GHC

import Control.Applicative
import Data.List.Utils (merge)
import qualified Data.Map as Map
import qualified Data.Text.IO as T
import qualified Data.Text as T

import Data.List hiding (find)

import Data.IORef
import Control.Exception
import Control.Monad
import System.Directory
import System.FilePath
import System.IO

import Test.HUnit

import Consistency

-- Roundtrip machinery


data Report =
   Success String -- Lazy evaluation, debugtxt will only be generated if needed.
 | ParseFailure GHC.SrcSpan String
 | RoundTripFailure String
 | CPP
 | InconsistentAnnotations String [(GHC.SrcSpan, (GHC.AnnKeywordId, [GHC.SrcSpan]))]

instance Eq Report where
  Success _          == Success _          = True
  ParseFailure _ _   == ParseFailure _ _   = True
  RoundTripFailure _ == RoundTripFailure _ = True
  CPP                == CPP                = True
  InconsistentAnnotations _ _ == InconsistentAnnotations _ _ = True
  _ == _ = False


instance Show Report where
  show (Success _) = "Success"
  show (ParseFailure _ s) = "ParseFailure: " ++ s
  show (RoundTripFailure _) = "RoundTripFailure"
  show CPP              = "CPP"
  show (InconsistentAnnotations _ s) = "InconsistentAnnotations: " ++ showGhc s

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

getDynFlags :: IO GHC.DynFlags
getDynFlags =
  GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut $
    GHC.runGhc (Just libdir) GHC.getSessionDynFlags

removeSpaces :: String -> String
removeSpaces = map (\case {'\160' -> ' '; s -> s})


roundTripTest :: (String -> IO ()) -> FilePath -> IO Report
roundTripTest writeHsPP file = do
  GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut $ do
    GHC.runGhc (Just libdir) $ do
      dflags0 <- GHC.getSessionDynFlags
      -- dflags0 <- getDynFlags
      let dflags1 = GHC.gopt_set dflags0 GHC.Opt_KeepRawTokenStream
      src_opts <- GHC.liftIO $ GHC.getOptionsFromFile dflags1 file
      (!dflags2, _, _)
               <- GHC.parseDynamicFilePragma dflags1 src_opts
      fileContents <- GHC.liftIO $ T.readFile file
        -- if
        --   | GHC.xopt GHC.Opt_Cpp dflags2 -> do
        --       contents <- getPreprocessedSrc dflags2 file
        --       writeHsPP contents
        --       return (T.pack contents)
        --   | otherwise -> T.readFile file

      let origContents = removeSpaces . T.unpack $ fileContents
      let (contents, linePragmas) = stripLinePragmas $ origContents
      cppComments <- getCppTokensAsComments file
      case parseFile dflags2 file contents of
        GHC.PFailed ss m -> return $ ParseFailure ss (GHC.showSDoc dflags2 m)
        GHC.POk (mkApiAnns -> apianns) pmod   -> do
            let (printed, anns) = runRoundTrip apianns pmod linePragmas
                debugtxt = mkDebugOutput file printed contents apianns anns pmod
                consistency = checkConsistency apianns pmod
            if
              | not (null consistency)  -> return $ InconsistentAnnotations debugtxt consistency
              | printed == origContents -> return $ Success debugtxt
              | otherwise -> return $ RoundTripFailure debugtxt


mkDebugOutput :: FilePath -> String -> String
              -> GHC.ApiAnns
              -> Anns
              -> GHC.Located (GHC.HsModule GHC.RdrName) -> String
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


runRoundTrip :: GHC.ApiAnns -> GHC.Located (GHC.HsModule GHC.RdrName)
             -> [Comment]
             -> (String, Anns)
runRoundTrip !anns !parsedOrig cs =
  let
--    (!_, !parsed) = fixBugsInAst anns parsedOrig
    !relAnns = relativiseApiAnnsWithComments cs parsedOrig anns
    !printed = exactPrintWithAnns parsedOrig relAnns
  in (printed,  relAnns)

-- ---------------------------------------------------------------------`

-- | Get the preprocessor directives as comment tokens from the
-- source.
getPreprocessorAsComments :: FilePath -> String -> [(GHC.Located GHC.Token, String)]
getPreprocessorAsComments srcFile fcontents =
  let
    directives = filter (\(_lineNum,line) -> line /= [] && head line == '#') $ zip [1..] $ lines fcontents

    mkTok (lineNum,line) = (GHC.L l (GHC.ITlineComment line),line)
       where
         start = GHC.mkSrcLoc (GHC.mkFastString srcFile) lineNum 1
         end   = GHC.mkSrcLoc (GHC.mkFastString srcFile) lineNum (length line)
         l = GHC.mkSrcSpan start end

    toks = map mkTok directives
  in toks

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

  let canonMaybe filepath = GHC.ghandle handler (canonicalizePath filepath)
        where
          handler :: SomeException -> IO FilePath
          handler _e = return filepath

  let mm = filter (\(mfn,_ms) -> mfn == Just cfileName) cgraph
  case mm of
   [] -> return Nothing
   fs -> return (Just (snd $ head fs))
