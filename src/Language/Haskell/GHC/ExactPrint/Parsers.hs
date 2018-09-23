{-# LANGUAGE CPP #-}
{-# LANGUAGE ViewPatterns #-}
-----------------------------------------------------------------------------
-- |
-- This module rexposes wrapped parsers from the GHC API. Along with
-- returning the parse result, the corresponding annotations are also
-- returned such that it is then easy to modify the annotations and print
-- the result.
--
----------------------------------------------------------------------------
module Language.Haskell.GHC.ExactPrint.Parsers (
        -- * Utility
          Parser
        , withDynFlags
        , CppOptions(..)
        , defaultCppOptions

        -- * Module Parsers
        , parseModule
        , parseModuleFromString
        , parseModuleWithOptions
        , parseModuleWithCpp

        -- * Basic Parsers
        , parseExpr
        , parseImport
        , parseType
        , parseDecl
        , parsePattern
        , parseStmt

        , parseWith

        -- * Internal

        , ghcWrapper

        , initDynFlags
        , initDynFlagsPure
        , parseModuleFromStringInternal
        , parseModuleApiAnnsWithCpp
        , parseModuleApiAnnsWithCppInternal
        , postParseTransform
        ) where

import Language.Haskell.GHC.ExactPrint.Annotate
import Language.Haskell.GHC.ExactPrint.Delta
import Language.Haskell.GHC.ExactPrint.Preprocess
import Language.Haskell.GHC.ExactPrint.Types

import Control.Monad.RWS

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

#if __GLASGOW_HASKELL__ <= 710
import qualified OrdList as OL
#else
import qualified GHC.LanguageExtensions as LangExt
#endif

import qualified Data.Map as Map

{-# ANN module "HLint: ignore Eta reduce" #-}
{-# ANN module "HLint: ignore Redundant do" #-}
{-# ANN module "HLint: ignore Reduce duplication" #-}
-- ---------------------------------------------------------------------

-- | Wrapper function which returns Annotations along with the parsed
-- element.
parseWith :: Annotate w
          => GHC.DynFlags
          -> FilePath
          -> GHC.P (GHC.Located w)
          -> String
          -> Either (GHC.SrcSpan, String) (Anns, GHC.Located w)
parseWith dflags fileName parser s =
  case runParser parser dflags fileName s of
#if __GLASGOW_HASKELL__ >= 804
    GHC.PFailed _ ss m                    -> Left (ss, GHC.showSDoc dflags m)
#else
    GHC.PFailed ss m                    -> Left (ss, GHC.showSDoc dflags m)
#endif
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

-- | Provides a safe way to consume a properly initialised set of
-- 'DynFlags'.
--
-- @
-- myParser fname expr = withDynFlags (\\d -> parseExpr d fname expr)
-- @
withDynFlags :: (GHC.DynFlags -> a) -> IO a
withDynFlags action = ghcWrapper $ do
  dflags <- GHC.getSessionDynFlags
  void $ GHC.setSessionDynFlags dflags
  return (action dflags)

-- ---------------------------------------------------------------------

parseFile :: GHC.DynFlags -> FilePath -> String -> GHC.ParseResult (GHC.Located (GHC.HsModule GhcPs))
parseFile = runParser GHC.parseModule

-- ---------------------------------------------------------------------

type Parser a = GHC.DynFlags -> FilePath -> String
                -> Either (GHC.SrcSpan, String)
                          (Anns, a)

parseExpr :: Parser (GHC.LHsExpr GhcPs)
parseExpr df fp = parseWith df fp GHC.parseExpression

parseImport :: Parser (GHC.LImportDecl GhcPs)
parseImport df fp = parseWith df fp GHC.parseImport

parseType :: Parser (GHC.LHsType GhcPs)
parseType df fp = parseWith df fp GHC.parseType

-- safe, see D1007
parseDecl :: Parser (GHC.LHsDecl GhcPs)
#if __GLASGOW_HASKELL__ <= 710
parseDecl df fp = parseWith df fp (head . OL.fromOL <$> GHC.parseDeclaration)
#else
parseDecl df fp = parseWith df fp GHC.parseDeclaration
#endif

parseStmt :: Parser (GHC.ExprLStmt GhcPs)
parseStmt df fp = parseWith df fp GHC.parseStatement

parsePattern :: Parser (GHC.LPat GhcPs)
parsePattern df fp = parseWith df fp GHC.parsePattern

-- ---------------------------------------------------------------------
--

-- | This entry point will also work out which language extensions are
-- required and perform CPP processing if necessary.
--
-- @
-- parseModule = parseModuleWithCpp defaultCppOptions
-- @
--
-- Note: 'GHC.ParsedSource' is a synonym for 'GHC.Located' ('GHC.HsModule' 'GhcPs')
parseModule
  :: FilePath -> IO (Either (GHC.SrcSpan, String) (Anns, GHC.ParsedSource))
parseModule = parseModuleWithCpp defaultCppOptions normalLayout


-- | This entry point will work out which language extensions are
-- required but will _not_ perform CPP processing.
-- In contrast to `parseModoule` the input source is read from the provided
-- string; the `FilePath` parameter solely exists to provide a name
-- in source location annotations.
parseModuleFromString
  :: FilePath
  -> String
  -> IO (Either (GHC.SrcSpan, String) (Anns, GHC.ParsedSource))
parseModuleFromString fp s = ghcWrapper $ do
  dflags <- initDynFlagsPure fp s
  return $ parseModuleFromStringInternal dflags fp s

-- | Internal part of 'parseModuleFromString'.
parseModuleFromStringInternal
  :: GHC.DynFlags
  -> FilePath
  -> String
  -> Either (GHC.SrcSpan, String) (Anns, GHC.ParsedSource)
parseModuleFromStringInternal dflags fileName str =
  let (str1, lp) = stripLinePragmas str
      res        = case runParser GHC.parseModule dflags fileName str1 of
#if __GLASGOW_HASKELL__ >= 804
        GHC.PFailed _ ss m  -> Left (ss, GHC.showSDoc dflags m)
#else
        GHC.PFailed ss m    -> Left (ss, GHC.showSDoc dflags m)
#endif
        GHC.POk     x  pmod -> Right $ (mkApiAnns x, lp, dflags, pmod)
  in  postParseTransform res normalLayout

parseModuleWithOptions :: DeltaOptions
                       -> FilePath
                       -> IO (Either (GHC.SrcSpan, String)
                                     (Anns, GHC.ParsedSource))
parseModuleWithOptions opts fp =
  parseModuleWithCpp defaultCppOptions opts fp


-- | Parse a module with specific instructions for the C pre-processor.
parseModuleWithCpp
  :: CppOptions
  -> DeltaOptions
  -> FilePath
  -> IO (Either (GHC.SrcSpan, String) (Anns, GHC.ParsedSource))
parseModuleWithCpp cpp opts fp = do
  res <- parseModuleApiAnnsWithCpp cpp fp
  return $ postParseTransform res opts

-- ---------------------------------------------------------------------

-- | Low level function which is used in the internal tests.
-- It is advised to use 'parseModule' or 'parseModuleWithCpp' instead of
-- this function.
parseModuleApiAnnsWithCpp
  :: CppOptions
  -> FilePath
  -> IO
       ( Either
           (GHC.SrcSpan, String)
           (GHC.ApiAnns, [Comment], GHC.DynFlags, GHC.ParsedSource)
       )
parseModuleApiAnnsWithCpp cppOptions file = ghcWrapper $ do
  dflags <- initDynFlags file
  parseModuleApiAnnsWithCppInternal cppOptions dflags file

-- | Internal function. Default runner of GHC.Ghc action in IO.
ghcWrapper :: GHC.Ghc a -> IO a
ghcWrapper =
  GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut
    . GHC.runGhc (Just libdir)

-- | Internal function. Exposed if you want to muck with DynFlags
-- before parsing.
parseModuleApiAnnsWithCppInternal
  :: GHC.GhcMonad m
  => CppOptions
  -> GHC.DynFlags
  -> FilePath
  -> m
       ( Either
           (GHC.SrcSpan, String)
           (GHC.ApiAnns, [Comment], GHC.DynFlags, GHC.ParsedSource)
       )
parseModuleApiAnnsWithCppInternal cppOptions dflags file = do
#if __GLASGOW_HASKELL__ <= 710
  let useCpp = GHC.xopt GHC.Opt_Cpp dflags
#else
  let useCpp = GHC.xopt LangExt.Cpp dflags
#endif
  (fileContents, injectedComments, dflags') <-
    if useCpp
      then do
        (contents,dflags1) <- getPreprocessedSrcDirect cppOptions file
        cppComments <- getCppTokensAsComments cppOptions file
        return (contents,cppComments,dflags1)
      else do
        txt <- GHC.liftIO $ readFileGhc file
        let (contents1,lp) = stripLinePragmas txt
        return (contents1,lp,dflags)
  return $
    case parseFile dflags' file fileContents of
#if __GLASGOW_HASKELL__ >= 804
      GHC.PFailed _ ss m -> Left $ (ss, (GHC.showSDoc dflags m))
#else
      GHC.PFailed ss m -> Left $ (ss, (GHC.showSDoc dflags m))
#endif
      GHC.POk (mkApiAnns -> apianns) pmod  ->
        Right $ (apianns, injectedComments, dflags', pmod)

-- | Internal function. Exposed if you want to muck with DynFlags
-- before parsing. Or after parsing.
postParseTransform
  :: Either a (GHC.ApiAnns, [Comment], GHC.DynFlags, GHC.ParsedSource)
  -> DeltaOptions
  -> Either a (Anns, GHC.ParsedSource)
postParseTransform parseRes opts = either Left mkAnns parseRes
  where
    mkAnns (apianns, cs, _, m) =
      Right (relativiseApiAnnsWithOptions opts cs m apianns, m)

-- | Internal function. Initializes DynFlags value for parsing.
--
-- Passes "-hide-all-packages" to the GHC API to prevent parsing of
-- package environment files. However this only works if there is no
-- invocation of `setSessionDynFlags` before calling `initDynFlags`.
-- See ghc tickets #15513, #15541.
initDynFlags :: GHC.GhcMonad m => FilePath -> m GHC.DynFlags
initDynFlags file = do
  dflags0         <- GHC.getSessionDynFlags
  src_opts        <- GHC.liftIO $ GHC.getOptionsFromFile dflags0 file
  (dflags1, _, _) <- GHC.parseDynamicFilePragma dflags0 src_opts
  -- Turn this on last to avoid T10942
  let dflags2 = dflags1 `GHC.gopt_set` GHC.Opt_KeepRawTokenStream
  -- Prevent parsing of .ghc.environment.* "package environment files"
  (dflags3, _, _) <- GHC.parseDynamicFlagsCmdLine
    dflags2
    [GHC.noLoc "-hide-all-packages"]
  _ <- GHC.setSessionDynFlags dflags3
  return dflags3

-- | Requires GhcMonad constraint because there is
-- no pure variant of `parseDynamicFilePragma`. Yet, in constrast to
-- `initDynFlags`, it does not (try to) read the file at filepath, but
-- solely depends on the module source in the input string.
--
-- Passes "-hide-all-packages" to the GHC API to prevent parsing of
-- package environment files. However this only works if there is no
-- invocation of `setSessionDynFlags` before calling `initDynFlagsPure`.
-- See ghc tickets #15513, #15541.
initDynFlagsPure :: GHC.GhcMonad m => FilePath -> String -> m GHC.DynFlags
initDynFlagsPure fp s = do
  -- I was told we could get away with using the unsafeGlobalDynFlags.
  -- as long as `parseDynamicFilePragma` is impure there seems to be
  -- no reason to use it.
  dflags0 <- GHC.getSessionDynFlags
  let pragmaInfo = GHC.getOptions dflags0 (GHC.stringToStringBuffer $ s) fp
  (dflags1, _, _) <- GHC.parseDynamicFilePragma dflags0 pragmaInfo
  -- Turn this on last to avoid T10942
  let dflags2 = dflags1 `GHC.gopt_set` GHC.Opt_KeepRawTokenStream
  -- Prevent parsing of .ghc.environment.* "package environment files"
  (dflags3, _, _) <- GHC.parseDynamicFlagsCmdLine
    dflags2
    [GHC.noLoc "-hide-all-packages"]
  _ <- GHC.setSessionDynFlags dflags3
  return dflags3

-- ---------------------------------------------------------------------

mkApiAnns :: GHC.PState -> GHC.ApiAnns
mkApiAnns pstate
  = ( Map.fromListWith (++) . GHC.annotations $ pstate
    , Map.fromList ((GHC.noSrcSpan, GHC.comment_q pstate) : GHC.annotations_comments pstate))
