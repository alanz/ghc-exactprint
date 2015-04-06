{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Utils
import Language.Haskell.GHC.ExactPrint.Transform

import GHC.Paths (libdir)

import qualified DynFlags      as GHC
import qualified FastString    as GHC
import qualified Outputable    as GHC
import qualified RdrName       as GHC
import qualified StringBuffer  as GHC
import qualified HeaderInfo  as GHC
import qualified SrcLoc  as GHC
import qualified Parser  as GHC
import qualified Lexer  as GHC
import qualified ApiAnnotation  as GHC
import qualified HsSyn  as GHC
import qualified GHC  as GHC hiding (parseModule)

import System.FilePath
import qualified Data.Map as Map

import qualified Data.Text.IO as T
import qualified Data.Text as T

import System.IO (hClose)

import Data.List hiding (find)

import System.Exit

import System.Directory

import Test.HUnit

import System.FilePath.Find

import Debug.Trace
import Control.Monad
import System.Environment

import System.IO.Temp

import qualified Data.Set as S

data Verbosity = Debug | Status | None deriving (Eq, Show, Ord, Enum)

verb :: Verbosity
verb = Debug

cppFile = "cpp.txt"
parseFailFile = "pfail.txt"
processed = "processed.txt"


main :: IO ()
main = do
  as <- getArgs
  case as of
    [] -> putStrLn "Must enter directory to process"
    ds -> () <$ (runTests =<< (TestList <$> mapM tests ds))

runTests :: Test -> IO Counts
runTests t = do
  let n = testCaseCount t
  putStrLn $ "Running " ++ show n ++ " tests."
  putStrLn $ "Verbosity: " ++ show verb
  runTestTT t

tests :: FilePath -> IO Test
tests dir = do
  done <- S.fromList . lines . T.unpack <$> T.readFile processed
  traceShowM done
  roundTripHackage done dir

mkDirTest :: FilePath -> [(FilePath, Report)] -> Test
mkDirTest d ts = TestLabel d (TestList (map (uncurry mkTest) ts))

-- Selection:

-- Given base directory finds all haskell source files
findSrcFiles :: S.Set String -> FilePath -> IO [FilePath]
findSrcFiles done = find filterDirectory filterFilename

filterDirectory :: FindClause Bool
filterDirectory =
  p <$> fileName
  where
    p x
      | "." `isPrefixOf` x = False
      | otherwise = True

filterFilename :: FindClause Bool
filterFilename = do
  ext <- extension
  fname <- fileName
  return (ext == ".hs" && p fname)
  where
    p x
      | "refactored" `isInfixOf` x = False
      | "Setup.hs" `isInfixOf` x = False
      | "HLint.hs" `isInfixOf` x = False -- HLint config files
      | otherwise                 = True

-- Hackage dir
roundTripHackage :: S.Set String -> FilePath -> IO Test
roundTripHackage done hackageDir = do
  packageDirs <- drop 2 <$> getDirectoryContents hackageDir
  when (verb <= Debug) (traceShowM packageDirs)
  TestList <$> mapM (roundTripPackage done) (zip [0..] (map (hackageDir </>) packageDirs))


roundTripPackage :: S.Set String -> (Int, FilePath) -> IO Test
roundTripPackage done (n, dir) = do
  putStrLn (show n)
  when (verb <= Status) (traceM dir)
  hsFiles <- filter (flip S.notMember done)  <$> findSrcFiles done dir

  r <- mapM roundTripTest hsFiles
  return (TestLabel (dropFileName dir) (TestList $ zipWith mkTest (map takeFileName hsFiles) r))


-- Roundtrip machinery

mkTest :: FilePath -> Report -> Test
mkTest file r =
  case r of
    Success -> TestLabel file (TestCase (return ()))
    a -> traceShow a $ TestLabel file (TestCase exitFailure)


data Report =
   Success
 | ParseFailure GHC.SrcSpan GHC.SDoc
 | RoundTripFailure
 | CPP

instance Show Report where
  show Success = "Success"
  show (ParseFailure _ s) = "ParseFailure: " ++ GHC.showSDocUnsafe s
  show (RoundTripFailure) = "RoundTripFailure"
  show (CPP)              = "CPP"

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


roundTripTest :: FilePath -> IO Report
roundTripTest file = do
  when (verb <= Debug) (traceM file)
  writeProcessed file
  dflags0 <- getDynFlags
  let dflags1 = GHC.gopt_set dflags0 GHC.Opt_KeepRawTokenStream
  src_opts <- GHC.getOptionsFromFile dflags1 file
  (!dflags2, _, _)
           <- GHC.parseDynamicFilePragma dflags1 src_opts
  if GHC.xopt GHC.Opt_Cpp dflags2
    then CPP <$ writeCPP file
    else do
      contents <- T.unpack <$> T.readFile file
      case parseFile dflags2 file contents of
        GHC.PFailed ss m -> ParseFailure ss m <$ writeParseFail file (GHC.showSDoc dflags2 m)
        GHC.POk s pmod   -> do
          let (printed, anns) = runRoundTrip (mkApiAnns s) pmod
              debugtxt = mkDebugOutput file printed contents (mkApiAnns s) anns pmod
          if printed == contents
            then return Success
            else do
              let outdir      = "tests" </> "roundtrip"
                  outname     = takeFileName file <.> "out"
              (fname, handle) <- openTempFile outdir outname
              RoundTripFailure <$ (T.hPutStr handle (T.pack debugtxt) >> hClose handle)


writeCPP :: FilePath -> IO ()
writeCPP fp = T.appendFile cppFile (T.pack ('\n' : fp))

writeParseFail :: FilePath -> String -> IO ()
writeParseFail fp s = T.appendFile parseFailFile (T.pack ('\n' : (fp ++ " " ++ s)))

writeProcessed :: FilePath -> IO ()
writeProcessed fp = T.appendFile processed (T.pack ('\n' : fp))



removeTrailingWhitespace :: String -> String
removeTrailingWhitespace = unlines . map (reverse . dropWhile (==' ') . reverse) . lines

mkDebugOutput :: FilePath -> String -> String
              -> GHC.ApiAnns
              -> Anns
              -> GHC.Located (GHC.HsModule GHC.RdrName) -> String
mkDebugOutput filename printed original apianns anns parsed =
  intercalate sep [ printed
                 , filename
                 , "lengths:" ++ show (length printed,length original) ++ "\n"
                -- , showAnnData anns 0 parsed
                -- , showGhc anns
                -- , showGhc apianns ]
                ]
  where
    sep = "\n==============\n"




runRoundTrip :: GHC.ApiAnns -> GHC.Located (GHC.HsModule GHC.RdrName)
              -> (String, Anns)
runRoundTrip !anns !parsedOrig =
  let
    (!ghcAnns, !parsed) = fixBugsInAst anns parsedOrig
    !relAnns = relativiseApiAnns parsedOrig anns
    !printed = exactPrintWithAnns parsed relAnns
  in (printed,  relAnns)

