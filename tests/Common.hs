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
  dflags0 <- getDynFlags
  let dflags1 = GHC.gopt_set dflags0 GHC.Opt_KeepRawTokenStream
  src_opts <- GHC.getOptionsFromFile dflags1 file
  (!dflags2, _, _)
           <- GHC.parseDynamicFilePragma dflags1 src_opts
  fileContents <- T.readFile file
    -- if
    --   | GHC.xopt GHC.Opt_Cpp dflags2 -> do
    --       contents <- getPreprocessedSrc dflags2 file
    --       writeHsPP contents
    --       return (T.pack contents)
    --   | otherwise -> T.readFile file

  let origContents = removeSpaces . T.unpack $ fileContents
  let (contents, linePragmas) = stripLinePragmas $ origContents
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


-- | The preprocessed files are placed in a temporary directory, with
-- a temporary name, and extension .hscpp. Each of these files has
-- three lines at the top identifying the original origin of the
-- files, which is ignored by the later stages of compilation except
-- to contextualise error messages.
getPreprocessedSrc ::
  -- GHC.GhcMonad m => FilePath -> m GHC.StringBuffer
  -- GHC.GhcMonad m => FilePath -> m String
  -- (Monad m) => GHC.DynFlags -> FilePath -> m String
  GHC.DynFlags -> FilePath -> IO String
getPreprocessedSrc df srcFile = do
  -- df <- GHC.getSessionDynFlags
  -- d <- GHC.liftIO $ getTempDir df
  d <- getTempDir df
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

readUTF8File :: FilePath -> IO String
readUTF8File fp = openFile fp ReadMode >>= \h -> do
        hSetEncoding h utf8
        hGetContents h

-- ---------------------------------------------------------------------
-- Putting these here for the time being, to avoid import loops

ghead :: String -> [a] -> a
ghead  info []    = error $ "ghead "++info++" []"
ghead _info (h:_) = h

glast :: String -> [a] -> a
glast  info []    = error $ "glast " ++ info ++ " []"
glast _info h     = last h

gtail :: String -> [a] -> [a]
gtail  info []   = error $ "gtail " ++ info ++ " []"
gtail _info h    = tail h

gfromJust :: String -> Maybe a -> a
gfromJust _info (Just h) = h
gfromJust  info Nothing = error $ "gfromJust " ++ info ++ " Nothing"
