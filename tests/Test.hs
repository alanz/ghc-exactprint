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
import qualified Outputable    as GHC

import qualified GHC.SYB.Utils as SYB

import System.FilePath
import System.IO
import System.Directory
import Control.Monad
import Control.Monad.Trans
import Control.Applicative
import Data.Generics

import Test.HUnit

import qualified Data.Map as Map

import Debug.Trace

debug :: c -> String -> c
debug = flip trace

main :: IO ()
main = do
    putStrLn "hello"
    -- manipulateAstTest ["examples/LetExpr.hs"]
    -- manipulateAstTest ["examples/LetExprSemi.hs"]
    manipulateAstTest ["examples/Tuple.hs"]
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
    parsedAST = SYB.showData SYB.Parser 0 parsed
    -- parsedAST = showGhc parsed
       -- `debug` ("getAnn:=" ++ (show (getAnnotationValue (snd ann) (GHC.getLoc parsed) :: Maybe AnnHsModule)))
    -- try to pretty-print; summarize the test result
    ann = annotateAST parsed ghcAnns
      `debug` ("ghcAnns:" ++ showGhc ghcAnns)
      -- `debug` ("ann:" ++ (show $ snd ann))
      -- `debug` ("ann:" ++ (show $ fst ann))

    Just (GHC.L le exps) = GHC.hsmodExports hsmod
    secondExp@(GHC.L l2 _) = head $ tail exps
    -- Just [(Ann cs ll (AnnIEVar mc))] = Map.lookup l2 ann
    -- ann' = Map.insert l2 [(Ann cs ll (AnnIEVar Nothing))] ann
    -- parsed' = (GHC.L l (hsmod { GHC.hsmodExports = Just (tail exps) }))
    -- parsed' = (GHC.L l (hsmod { GHC.hsmodExports = Just (head exps : (drop 2 exps)) }))
    -- parsed' = (GHC.L l (hsmod { GHC.hsmodExports = Just (init exps) }))

    -- parsed' = (GHC.L l (hsmod { GHC.hsmodExports = Just (GHC.L le (head exps:(head $ tail exps):tail exps)) }))
    -- ((16,9),(16,27))
    ss = GHC.mkSrcSpan (GHC.mkSrcLoc (GHC.mkFastString "examples/PatBind.hs") 16 9)
                       (GHC.mkSrcLoc (GHC.mkFastString "examples/PatBind.hs") 16 27)

    -- Just [Ann cs1 dp1 as1,Ann cs2 dp2 as2] = Map.lookup ss ann
    -- ann2 = Map.insert ss [Ann cs1 (DP (0,6)) as1,Ann cs2 dp2 as2] ann
    printed = exactPrintAnnotation parsed [] ann -- `debug` ("ann=" ++ (show $ map (\(s,a) -> (ss2span s, a)) $ Map.toList ann))
       -- `debug` ("ann=" ++ (show (snd ann)))
       -- `debug` ("getAnn:=" ++ (show (getAnnotationValue (snd ann) (GHC.getLoc parsed) :: Maybe AnnHsModule)))
    result =
            if printed == contents
              then "Match\n"
              else printed ++ "\n==============\n"
                    ++ "lengths:" ++ show (length printed,length contents) ++ "\n"
                    ++ parsedAST
  -- putStrLn $ "Test:parsed=" ++ parsedAST
  -- putStrLn $ "Test:ghcAnns:fst=" ++ show (fst ghcAnns)
  -- putStrLn $ "Test:ghcAnns:snd=" ++ showGhc (snd ghcAnns)
  -- putStrLn $ "Test2:empty ann=" ++ show ((Map.empty,Map.empty) :: Anns)
  -- putStrLn $ "Test2:ann=[" ++ show (annotateAST parsed ghcAnns) ++ "]"
  -- putStrLn $ "Test3:ann=[" ++ show ann ++ "]"
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

            dflags'' = dflags' { GHC.importPaths = ["./tests/examples/","../tests/examples/"] }

            dflags''' = dflags'' { GHC.hscTarget = GHC.HscInterpreted,
                                   GHC.ghcLink =  GHC.LinkInMemory }

            dflags4 = if True -- useHaddock
                        -- then GHC.gopt_set (GHC.gopt_set dflags''' GHC.Opt_Haddock)
                        then GHC.gopt_set dflags'''
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
{-
testAP :: AP a -> [Comment] -> GHC.ApiAnns -> IO a
testAP (AP f) cs ga = do
  let st = S [] [] cs ga
      (r,st',(se,su)) = f st
  return r

testAPSrcSpans = do
  let ss1 = mkSs (1,2) (3,4)
      ss2 = mkSs (2,3) (4,5)
      ss3 = mkSs (3,4) (5,6)

      sst :: AP (GHC.SrcSpan,GHC.SrcSpan)
      sst = do
        pushSrcSpan (GHC.L ss1 ())
        pushSrcSpan (GHC.L ss2 ())
        r1 <- getSrcSpanAP
        popSrcSpan
        r2 <- getSrcSpanAP
        return (r1,r2)

  ss <- testAP sst [] (Map.empty,Map.empty)
  putStrLn $ "testAPStuff: ss=" ++ showGhc ss
-}

-- ---------------------------------------------------------------------

pwd :: IO FilePath
pwd = getCurrentDirectory

cd :: FilePath -> IO ()
cd = setCurrentDirectory

-- ---------------------------------------------------------------------

mkSs :: (Int,Int) -> (Int,Int) -> GHC.SrcSpan
mkSs (sr,sc) (er,ec)
  = GHC.mkSrcSpan (GHC.mkSrcLoc (GHC.mkFastString "examples/PatBind.hs") sr sc)
                  (GHC.mkSrcLoc (GHC.mkFastString "examples/PatBind.hs") er ec)
