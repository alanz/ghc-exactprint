{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
-- | Use "runhaskell Setup.hs test" or "cabal test" to run these tests.
module Main where

import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Utils

import GHC.Paths ( libdir )
import qualified FastString    as GHC
import qualified DynFlags      as GHC
import qualified GHC           as GHC

import System.FilePath
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
    putStrLn "hello"
    let sources2 = ["examples/LetExpr.hs"]
    manipulateAstTest (sources2)
    putStrLn "done"

-- | Where all the tests are to be found
examplesDir :: FilePath
examplesDir = "tests" </> "examples"

examplesDir2 :: FilePath
examplesDir2 = "examples"

manipulateAstTest :: [FilePath] -> IO ()
manipulateAstTest sources = do
  let file = head sources
  let out    = file <.> "exactprinter" <.> "out"

  contents <- readUTF8File file
  (ghcAnns,t,toks) <- parsedFileGhc file
  let
    parsed@(GHC.L l hsmod) = GHC.pm_parsed_source $ GHC.tm_parsed_module t
    -- parsedAST = SYB.showData SYB.Parser 0 parsed
    parsedAST = showGhc parsed
    comments = toksToComments toks
    -- try to pretty-print; summarize the test result
    -- printed = exactPrint parsed comments toks
    -- ann = annotate parsed comments toks
    ann = annotate parsed comments toks ghcAnns
      -- `debug` ("toks:" ++ show toks)
      `debug` ("ghcAnns:" ++ showGhc ghcAnns)
    Just (GHC.L le exps) = GHC.hsmodExports hsmod
    secondExp@(GHC.L l2 _) = head $ tail exps
    Just [(Ann cs ll (AnnIEVar mc))] = Map.lookup l2 ann
    ann' = Map.insert l2 [(Ann cs ll (AnnIEVar Nothing))] ann
    -- parsed' = (GHC.L l (hsmod { GHC.hsmodExports = Just (tail exps) }))
    -- parsed' = (GHC.L l (hsmod { GHC.hsmodExports = Just (head exps : (drop 2 exps)) }))
    -- parsed' = (GHC.L l (hsmod { GHC.hsmodExports = Just (init exps) }))

    parsed' = (GHC.L l (hsmod { GHC.hsmodExports = Just (GHC.L le (head exps:(head $ tail exps):tail exps)) }))
    -- printed = exactPrintAnnotation parsed' comments ann
    -- printed = exactPrintAnnotation parsed' [] ann
    -- ((16,9),(16,27))
    ss = GHC.mkSrcSpan (GHC.mkSrcLoc (GHC.mkFastString "examples/PatBind.hs") 16 9)
                       (GHC.mkSrcLoc (GHC.mkFastString "examples/PatBind.hs") 16 27)

    Just [Ann cs1 dp1 as1,Ann cs2 dp2 as2] = Map.lookup ss ann
    ann2 = Map.insert ss [Ann cs1 (DP (0,6)) as1,Ann cs2 dp2 as2] ann
    printed = exactPrintAnnotation parsed [] ann -- `debug` ("ann=" ++ (show $ map (\(s,a) -> (ss2span s, a)) $ Map.toList ann))
       `debug` ("ann=" ++ (show $ Map.lookup ss ann))

    result =
            if printed == contents
              then "Match\n"
              else printed ++ "\n==============\n" ++ parsedAST
  writeFile out $ result
  return ()
-- }}}

-- ---------------------------------------------------------------------
-- |Result of parsing a Haskell source file. It is simply the
-- TypeCheckedModule produced by GHC.
type ParseResult = GHC.TypecheckedModule

parsedFileGhc :: String -> IO (GHC.ApiAnns,ParseResult,[(GHC.Located GHC.Token, String)])
parsedFileGhc fileName = do
    putStrLn $ "parsedFileGhc:" ++ show fileName
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

            dflags4 = if True -- useHaddock
                        then GHC.gopt_set (GHC.gopt_set dflags''' GHC.Opt_Haddock)
                                       GHC.Opt_KeepRawTokenStream
                        else GHC.gopt_set (GHC.gopt_unset dflags''' GHC.Opt_Haddock)
                                       GHC.Opt_KeepRawTokenStream

        void $ GHC.setSessionDynFlags dflags4
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
        let anns = GHC.pm_annotations p
        return (anns,t,toks)

readUTF8File :: FilePath -> IO String
readUTF8File fp = openFile fp ReadMode >>= \h -> do
        hSetEncoding h utf8
        hGetContents h


-- ---------------------------------------------------------------------

pwd :: IO FilePath
pwd = getCurrentDirectory

cd :: FilePath -> IO ()
cd = setCurrentDirectory

