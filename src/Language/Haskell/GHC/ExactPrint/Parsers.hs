{-# LANGUAGE ViewPatterns #-}
module Language.Haskell.GHC.ExactPrint.Parsers (
        -- * Utility
          Parser

        , parseModule
        , parseExpr
        , parseImport
        , parseType
        , parseDecl
        , parsePattern
        , parseStmt

        , parseWith
        , withDynFlags ) where

import Language.Haskell.GHC.ExactPrint.Annotate
import Language.Haskell.GHC.ExactPrint.Delta
import Language.Haskell.GHC.ExactPrint.Preprocess
import Language.Haskell.GHC.ExactPrint.Internal.Types

import Control.Monad.RWS
import Control.Exception

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
import qualified RdrHsSyn      as GHC ( checkPattern )
import qualified SrcLoc        as GHC
import qualified StringBuffer  as GHC

import qualified OrdList as OL

import qualified Data.Map as Map

import Distribution.Helper


-- ---------------------------------------------------------------------

parseWith :: Annotate w
          => GHC.DynFlags
          -> FilePath
          -> GHC.P (GHC.Located w)
          -> String
          -> Either (GHC.SrcSpan, String) (Anns, GHC.Located w)
parseWith dflags fileName parser s =
  case runParser parser dflags fileName s of
    GHC.PFailed ss m                    -> Left (ss, GHC.showSDoc dflags m)
    GHC.POk (mkApiAnns -> apianns) pmod -> Right (as, pmod)
      where as = relativiseApiAnns pmod apianns

-- ---------------------------------------------------------------------

runParser :: GHC.P a -> GHC.DynFlags -> FilePath -> String -> GHC.ParseResult a
runParser parser flags filename str = GHC.unP parser parseState
    where
      location = GHC.mkRealSrcLoc (GHC.mkFastString filename) 1 1
      buffer = GHC.stringToStringBuffer str
      parseState = GHC.mkPState flags buffer location

-- ---------------------------------------------------------------------

withDynFlags :: (GHC.DynFlags -> a) -> IO a
withDynFlags action =
  GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut $
    GHC.runGhc (Just libdir) $ do
      dflags <- GHC.getSessionDynFlags
      void $ GHC.setSessionDynFlags dflags
      return (action dflags)

-- ---------------------------------------------------------------------

parseFile :: GHC.DynFlags -> FilePath -> String -> GHC.ParseResult (GHC.Located (GHC.HsModule GHC.RdrName))
parseFile = runParser GHC.parseModule

-- ---------------------------------------------------------------------

type Parser a = GHC.DynFlags -> FilePath -> String
                -> Either (GHC.SrcSpan, String)
                          (Anns, a)

parseExpr :: Parser (GHC.LHsExpr GHC.RdrName)
parseExpr df fp = parseWith df fp GHC.parseExpression

parseImport :: Parser (GHC.LImportDecl GHC.RdrName)
parseImport df fp = parseWith df fp GHC.parseImport

parseType :: Parser (GHC.LHsType GHC.RdrName)
parseType df fp = parseWith df fp GHC.parseType

-- safe, see D1007
parseDecl :: Parser (GHC.LHsDecl GHC.RdrName)
parseDecl df fp = parseWith df fp (head . OL.fromOL <$> GHC.parseDeclaration)

parseStmt :: Parser (GHC.ExprLStmt GHC.RdrName)
parseStmt df fp = parseWith df fp GHC.parseStatement

-- Interim, see D1005
-- will not parse bang patterns properly
parsePattern :: Parser (GHC.LPat GHC.RdrName)
parsePattern df fp = parseWith df fp (GHC.parseExpression >>= GHC.checkPattern GHC.empty)
-- parsePattern df fp = parseWith df fp GHC.parsePattern

-- ---------------------------------------------------------------------
--

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = catch

generateMacroFile :: IO ()
generateMacroFile = catchAny (writeAutogenFiles "dist/")
                      (\_ -> putStrLn "Failed to generate macro file")


parseModule :: FilePath -> IO (Either (GHC.SrcSpan, String) (Anns, (GHC.Located (GHC.HsModule GHC.RdrName))))
parseModule file = do
  generateMacroFile
  GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut $
    GHC.runGhc (Just libdir) $ do
      dflags <- initDynFlags file
      let useCpp = GHC.xopt GHC.Opt_Cpp dflags
      (fileContents, injectedComments) <-
        if useCpp
          then do
            let macros = (Just "dist/build/autogen/cabal_macros.h")
            contents <- getPreprocessedSrcDirect macros file
            cppComments <- getCppTokensAsComments macros file
            return (contents,cppComments)
          else do
            txt <- GHC.liftIO $ readFile file
            let (contents1,lp) = stripLinePragmas txt
            return (contents1,lp)
      return $
        case parseFile dflags file fileContents of
          GHC.PFailed ss m -> Left $ (ss, (GHC.showSDoc dflags m))
          GHC.POk (mkApiAnns -> apianns) pmod  ->
            let as = relativiseApiAnnsWithComments injectedComments pmod apianns in
            Right $ (as, pmod)

-- ---------------------------------------------------------------------

initDynFlags :: GHC.GhcMonad m => FilePath -> m GHC.DynFlags
initDynFlags file = do
  dflags0 <- GHC.getSessionDynFlags
  let dflags1 = GHC.gopt_set dflags0 GHC.Opt_KeepRawTokenStream
  src_opts <- GHC.liftIO $ GHC.getOptionsFromFile dflags1 file
  (dflags2, _, _)
    <- GHC.parseDynamicFilePragma dflags1 src_opts
  void $ GHC.setSessionDynFlags dflags2
  return dflags2

-- ---------------------------------------------------------------------

mkApiAnns :: GHC.PState -> GHC.ApiAnns
mkApiAnns pstate
  = ( Map.fromListWith (++) . GHC.annotations $ pstate
    , Map.fromList ((GHC.noSrcSpan, GHC.comment_q pstate) : (GHC.annotations_comments pstate)))
