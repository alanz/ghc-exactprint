{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
-- | Use "runhaskell Setup.hs test" or "cabal test" to run these tests.
module Main where

import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Types

import GHC.Paths ( libdir )
import qualified DynFlags      as GHC
import qualified GHC           as GHC

import qualified GHC.SYB.Utils as SYB

import Test.Tasty hiding (defaultMain)
import Test.Tasty.Golden
import Test.Tasty.Golden.Manage
import System.FilePath
import System.FilePath.Find
import System.IO
import System.Directory
import Control.Monad
import Control.Monad.Trans
import Control.Applicative
import Data.Generics

import qualified Data.Map as Map

import Debug.Trace

debug :: c -> String -> c
debug = flip trace

main :: IO ()
main = do
  sources  <- getTestFiles examplesDir
  sources2 <- getTestFiles examplesDir2
  defaultMain $ testGroup "Tests" $
    [ -- exactPrinterTests (sources)
    -- exactPrinterTests (sources2)
    manipulateAstTest (sources2)
    ]

-- | Where all the tests are to be found
examplesDir :: FilePath
examplesDir = "tests" </> "examples"

examplesDir2 :: FilePath
examplesDir2 = "examples"

getTestFiles :: MonadIO m => FilePath -> m [FilePath]
getTestFiles dir = liftIO $ find (return True) (extension ==? ".hs" ||? extension ==? ".lhs") dir

exactPrinterTests :: [FilePath] -> TestTree -- {{{
exactPrinterTests sources = testGroup "Exact printer tests" $ do
  -- list monad
  file <- sources
  let
    out    = file <.> "exactprinter" <.> "out"
    golden = file <.> "exactprinter" <.> "golden"
    run = do
      contents <- readUTF8File file
      (t,toks) <- parsedFileGhc file
      let
        parsed = GHC.pm_parsed_source $ GHC.tm_parsed_module t
        parsedAST = SYB.showData SYB.Parser 0 parsed
        comments = toksToComments toks
        -- try to pretty-print; summarize the test result
        -- printed = exactPrint parsed comments toks
        printed = exactPrintAnnotated parsed comments toks
        result =
                if printed == contents
                  then "Match"
                  else printed ++ "\n==============\n" ++ parsedAST
      writeBinaryFile out $ result ++ "\n"
  return $ goldenVsFile (takeBaseName file) golden out run
-- }}}

manipulateAstTest :: [FilePath] -> TestTree -- {{{
manipulateAstTest sources = testGroup "Exact printer tests" $ do
  -- list monad
  file <- take 1 $ drop 2 sources
  let
    out    = file <.> "exactprinter" <.> "out"
    golden = file <.> "exactprinter" <.> "golden"
    run = do
      contents <- readUTF8File file
      (t,toks) <- parsedFileGhc file
      let
        parsed@(GHC.L l hsmod) = GHC.pm_parsed_source $ GHC.tm_parsed_module t
        parsedAST = SYB.showData SYB.Parser 0 parsed
        comments = toksToComments toks
        -- try to pretty-print; summarize the test result
        -- printed = exactPrint parsed comments toks
        -- ann = annotate parsed comments toks
        ann = annotate parsed comments toks
        Just exps = GHC.hsmodExports hsmod

        secondExp@(GHC.L l2 _) = head $ tail exps
        Just [(Ann cs ll (AnnIEVar mc))] = Map.lookup l2 ann
        ann' = Map.insert l2 [(Ann cs ll (AnnIEVar Nothing))] ann
        -- parsed' = (GHC.L l (hsmod { GHC.hsmodExports = Just (tail exps) }))

        -- parsed' = (GHC.L l (hsmod { GHC.hsmodExports = Just (head exps : (drop 2 exps)) }))
        -- parsed' = (GHC.L l (hsmod { GHC.hsmodExports = Just (init exps) }))

        parsed' = (GHC.L l (hsmod { GHC.hsmodExports = Just (head exps:(head $ tail exps):tail exps) }))
        -- printed = exactPrintAnnotation parsed' comments ann
        -- printed = exactPrintAnnotation parsed' [] ann
        printed = exactPrintAnnotation parsed [] ann -- `debug` ("ann=" ++ show ann)
        result =
                if printed == contents
                  then "Match\n"
                  else printed ++ "\n==============\n" ++ parsedAST
      writeBinaryFile out $ result
  return $ goldenVsFile (takeBaseName file) golden out run
-- }}}

-- ---------------------------------------------------------------------
-- |Result of parsing a Haskell source file. It is simply the
-- TypeCheckedModule produced by GHC.
type ParseResult = GHC.TypecheckedModule

parsedFileGhc :: String -> IO (ParseResult,[(GHC.Located GHC.Token, String)])
parsedFileGhc fileName = do
#if __GLASGOW_HASKELL__ > 704
    GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut $ do
#else
    GHC.defaultErrorHandler GHC.defaultLogAction $ do
#endif
      GHC.runGhc (Just libdir) $ do
        dflags <- GHC.getSessionDynFlags
        let dflags' = foldl GHC.xopt_set dflags
                           [GHC.Opt_Cpp, GHC.Opt_ImplicitPrelude, GHC.Opt_MagicHash]

            dflags'' = dflags' { GHC.importPaths = ["./test/testdata/","../test/testdata/"] }

            dflags''' = dflags'' { GHC.hscTarget = GHC.HscInterpreted,
                                   GHC.ghcLink =  GHC.LinkInMemory }

        void $ GHC.setSessionDynFlags dflags'''
        -- GHC.liftIO $ putStrLn $ "dflags set"

        target <- GHC.guessTarget fileName Nothing
        GHC.setTargets [target]
        -- GHC.liftIO $ putStrLn $ "targets set"
        void $ GHC.load GHC.LoadAllTargets -- Loads and compiles, much as calling make
        -- GHC.liftIO $ putStrLn $ "targets loaded"
        g <- GHC.getModuleGraph
        let showStuff ms = show (GHC.moduleNameString $ GHC.moduleName $ GHC.ms_mod ms,GHC.ms_location ms)
        -- GHC.liftIO $ putStrLn $ "module graph:" ++ (intercalate "," (map showStuff g))
        -- modSum <- GHC.getModSummary $ GHC.mkModuleName "BCpp"
        let modSum = head g
        p <- GHC.parseModule modSum
        t <- GHC.typecheckModule p
        -- GHC.liftIO $ putStrLn $ "parsed"
        toks <- GHC.getRichTokenStream (GHC.ms_mod modSum)
        return (t,toks)

readUTF8File :: FilePath -> IO String
readUTF8File fp = openFile fp ReadMode >>= \h -> do
        hSetEncoding h utf8
        hGetContents h


-- ---------------------------------------------------------------------

pwd :: IO FilePath
pwd = getCurrentDirectory

cd :: FilePath -> IO ()
cd = setCurrentDirectory

