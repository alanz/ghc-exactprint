{-# LANGUAGE FlexibleContexts #-}
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
        , ParseResult
        , withDynFlags
        , CppOptions(..)
        , defaultCppOptions
        , LibDir

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
        , macroIORef
        , macrosFromIORef

        , ghcWrapper

        , initDynFlags
        , initDynFlagsPure
        , parseModuleFromStringInternal
        , parseModuleEpAnnsWithCpp
        , parseModuleEpAnnsWithCppInternal
        , postParseTransform
        ) where

import Language.Haskell.GHC.ExactPrint.Preprocess

import Data.Functor (void)
import Data.IORef
import System.IO
import System.IO.Unsafe
import qualified Data.Map as Map
import Data.Maybe

import qualified GHC hiding (parseModule)
import qualified Control.Monad.IO.Class as GHC
import qualified GHC.Data.Bag           as GHC
import qualified GHC.Data.FastString    as GHC
import qualified GHC.Data.StringBuffer  as GHC
import qualified GHC.Driver.Config.Diagnostic as GHC
import qualified GHC.Driver.Config.Parser as GHC
import qualified GHC.Driver.Env.Types     as GHC
import qualified GHC.Driver.Errors.Types  as GHC
import qualified GHC.Driver.Session     as GHC
import qualified GHC.Parser             as GHC
import qualified GHC.Parser.Header      as GHC
import qualified GHC.Parser.Lexer       as GHC hiding (initParserState, initPragState,lexer)
import qualified GHC.Parser.PreProcess.State as GHC
import qualified GHC.Parser.PostProcess as GHC
import qualified GHC.Types.Error        as GHC
import qualified GHC.Types.SrcLoc       as GHC
import qualified GHC.Unit.Env           as GHC
import qualified GHC.Utils.Misc         as GHC
import qualified GHC.Parser.Lexer       as Lexer


import GHC.Utils.Exception as Exception
import qualified GHC.LanguageExtensions as LangExt
import qualified GHC.Parser.PreProcess as GHC
import GHC (GhcMonad(getSession))
import Debug.Trace

-- ---------------------------------------------------------------------

-- | Wrapper function which returns Annotations along with the parsed
-- element.
parseWith :: GHC.DynFlags
          -> FilePath
          -> GHC.P GHC.PpState w
          -> String
          -> ParseResult w
parseWith dflags fileName parser s =
  case runParser parser dflags fileName s of
    GHC.PFailed pst
      -> Left (GHC.GhcPsMessage <$> GHC.getPsErrorMessages pst)
    GHC.POk _ pmod
      -> Right pmod


parseWithECP :: (GHC.DisambECP w)
          => GHC.DynFlags
          -> FilePath
          -> GHC.P GHC.PpState GHC.ECP
          -> String
          -> ParseResult (GHC.LocatedA w)
parseWithECP dflags fileName parser s =
    case runParser (parser >>= \p -> GHC.runPV $ GHC.unECP p) dflags fileName s of
      GHC.PFailed pst
        -> Left (GHC.GhcPsMessage <$> GHC.getPsErrorMessages pst)
      GHC.POk _ pmod
        -> Right pmod

-- ---------------------------------------------------------------------

runParser :: GHC.P GHC.PpState a -> GHC.DynFlags -> FilePath -> String -> GHC.ParseResult GHC.PpState a
runParser parser flags filename str = GHC.unP parser parseState
    where
      location = GHC.mkRealSrcLoc (GHC.mkFastString filename) 1 1
      buffer = GHC.stringToStringBuffer str
      -- parseState = GHC.initParserState (GHC.initParserOpts flags) buffer location
      -- parseState = GHC.initParserStateWithMacros flags Nothing (GHC.initParserOpts flags) buffer location

      macros = fromMaybe Map.empty macrosFromIORef
      -- macros = fromMaybe (error "macroIORef not set up") macrosFromIORef

      -- opts0 = GHC.initParserOpts flags
      -- opts1 = GHC.enableExtBit GHC.UsePosPragsBit opts0
      -- opts = opts1
      opts = myInitParserOpts flags
      parseState0 = GHC.initParserStateWithMacrosString flags Nothing opts buffer location
      parseState = parseState0 { GHC.pp = (GHC.pp parseState0) { GHC.pp_defines = macros }
                               , GHC.buffer = buffer }


macroIORef :: IORef (Maybe GHC.MacroDefines)
{-# NOINLINE macroIORef #-}
macroIORef = unsafePerformIO (newIORef Nothing)

macrosFromIORef :: Maybe GHC.MacroDefines
macrosFromIORef =  unsafePerformIO (readIORef macroIORef)

myInitParserOpts :: GHC.DynFlags -> GHC.ParserOpts
myInitParserOpts =
  GHC.mkParserOpts
    <$> GHC.extensionFlags
    <*> GHC.initDiagOpts
    <*> GHC.safeImportsOn
    <*> GHC.gopt GHC.Opt_Haddock
    <*> GHC.gopt GHC.Opt_KeepRawTokenStream
    <*> const False -- do not use LINE/COLUMN to update the internal location


-- ---------------------------------------------------------------------

-- | Provides a safe way to consume a properly initialised set of
-- 'DynFlags'.
--
-- @
-- myParser fname expr = withDynFlags (\\d -> parseExpr d fname expr)
-- @
withDynFlags :: LibDir -> (GHC.DynFlags -> a) -> IO a
withDynFlags libdir action = ghcWrapper libdir $ do
  dflags <- GHC.getSessionDynFlags
  void $ GHC.setSessionDynFlags dflags
  return (action dflags)

-- ---------------------------------------------------------------------

parseFile :: GHC.DynFlags -> FilePath -> String -> GHC.ParseResult GHC.PpState (GHC.Located (GHC.HsModule GHC.GhcPs))
parseFile = runParser GHC.parseModule

-- ---------------------------------------------------------------------

type LibDir = FilePath

type ParseResult a = Either GHC.ErrorMessages a

type Parser a = GHC.DynFlags -> FilePath -> String
                -> ParseResult a

parseExpr :: Parser (GHC.LHsExpr GHC.GhcPs)
parseExpr df fp = parseWithECP df fp GHC.parseExpression

parseImport :: Parser (GHC.LImportDecl GHC.GhcPs)
parseImport df fp = parseWith df fp GHC.parseImport

parseType :: Parser (GHC.LHsType GHC.GhcPs)
parseType df fp = parseWith df fp GHC.parseType

-- safe, see D1007
parseDecl :: Parser (GHC.LHsDecl GHC.GhcPs)
parseDecl df fp = parseWith df fp GHC.parseDeclaration

parseStmt :: Parser (GHC.ExprLStmt GHC.GhcPs)
parseStmt df fp = parseWith df fp GHC.parseStatement

parsePattern :: Parser (GHC.LPat GHC.GhcPs)
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
parseModule :: LibDir -> FilePath -> IO (ParseResult GHC.ParsedSource)
parseModule libdir file = parseModuleWithCpp libdir defaultCppOptions file


-- | This entry point will work out which language extensions are
-- required but will _not_ perform CPP processing.
-- In contrast to `parseModoule` the input source is read from the provided
-- string; the `FilePath` parameter solely exists to provide a name
-- in source location annotations.
parseModuleFromString
  :: LibDir -- GHC libdir
  -> FilePath
  -> String
  -> IO (ParseResult GHC.ParsedSource)
parseModuleFromString libdir fp s = ghcWrapper libdir $ do
  dflags <- initDynFlagsPure fp s
  return $ parseModuleFromStringInternal dflags fp s

-- | Internal part of 'parseModuleFromString'.
parseModuleFromStringInternal :: Parser GHC.ParsedSource
parseModuleFromStringInternal dflags fileName str =
  let (str1, lp) = stripLinePragmas str
      res        = case runParser GHC.parseModule dflags fileName str1 of
        GHC.PFailed pst
          -> Left (GHC.GhcPsMessage <$> GHC.getPsErrorMessages pst)
        GHC.POk     _  pmod
          -> Right (lp, dflags, pmod)
  in  postParseTransform res

parseModuleWithOptions :: LibDir -- ^ GHC libdir
                       -> FilePath
                       -> IO (ParseResult GHC.ParsedSource)
parseModuleWithOptions libdir fp =
  parseModuleWithCpp libdir defaultCppOptions fp


-- | Parse a module with specific instructions for the C pre-processor.
parseModuleWithCpp
  :: LibDir -- ^ GHC libdir
  -> CppOptions
  -> FilePath -- ^ File to be parsed
  -> IO (ParseResult GHC.ParsedSource)
parseModuleWithCpp libdir cpp fp = do
  res <- parseModuleEpAnnsWithCpp libdir False cpp fp
  return $ postParseTransform res

-- ---------------------------------------------------------------------

-- | Low level function which is used in the internal tests.
-- It is advised to use 'parseModule' or 'parseModuleWithCpp' instead of
-- this function.
parseModuleEpAnnsWithCpp
  :: LibDir -- ^ GHC libdir
  -> Bool -- ^ Use GhcCpp
  -> CppOptions
  -> FilePath -- ^ File to be parsed
  -> IO
       ( Either
           GHC.ErrorMessages
           ([GHC.LEpaComment], GHC.DynFlags, GHC.ParsedSource)
       )
parseModuleEpAnnsWithCpp libdir useGhcCpp cppOptions file = ghcWrapper libdir $ do
  dflags <- initDynFlags useGhcCpp file
  if useGhcCpp
     then parseModuleEpAnnsWithGhcCppInternal cppOptions dflags file
     else parseModuleEpAnnsWithCppInternal cppOptions dflags file

-- | Internal function. Default runner of GHC.Ghc action in IO.
ghcWrapper :: LibDir -> GHC.Ghc a -> IO a
ghcWrapper libdir a =
  GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut
    $ GHC.runGhc (Just libdir) a


-- | Internal function. Exposed if you want to muck with DynFlags
-- before parsing.
parseModuleEpAnnsWithGhcCppInternal
  :: GHC.GhcMonad m
  => CppOptions
  -> GHC.DynFlags
  -> FilePath
  -> m
       ( Either
           GHC.ErrorMessages
           ([GHC.LEpaComment], GHC.DynFlags, GHC.ParsedSource)
       )
parseModuleEpAnnsWithGhcCppInternal cppOptions dflags file = do
  let useCpp = GHC.xopt LangExt.Cpp dflags
  (fileContents, injectedComments, dflags') <-
    -- if useCpp
    if True
      then do
        txt <- GHC.liftIO $ readFileGhc file
        let (contents1,lp) = (txt, [])
        let no_cpp_dflags = GHC.xopt_unset dflags LangExt.Cpp
        return (contents1, lp, GHC.xopt_set no_cpp_dflags LangExt.GhcCpp)
      else do
        txt <- GHC.liftIO $ readFileGhc file
        let (contents1,lp) = stripLinePragmas txt
        return (contents1,lp,dflags)
  return $
    case parseFile dflags' file fileContents of
      GHC.PFailed pst
        -> Left (GHC.GhcPsMessage <$> GHC.getPsErrorMessages pst)
      GHC.POk _ pmod
        -> Right $ (injectedComments, dflags', fixModuleComments pmod)


-- | Internal function. Exposed if you want to muck with DynFlags
-- before parsing.
parseModuleEpAnnsWithCppInternal
  :: GHC.GhcMonad m
  => CppOptions
  -> GHC.DynFlags
  -> FilePath
  -> m
       ( Either
           GHC.ErrorMessages
           ([GHC.LEpaComment], GHC.DynFlags, GHC.ParsedSource)
       )
parseModuleEpAnnsWithCppInternal cppOptions dflags file = do
  let useCpp = GHC.xopt LangExt.Cpp dflags
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
      GHC.PFailed pst
        -> Left (GHC.GhcPsMessage <$> GHC.getPsErrorMessages pst)
      GHC.POk _ pmod
        -> Right $ (injectedComments, dflags', fixModuleComments pmod)

-- | Internal function. Exposed if you want to muck with DynFlags
-- before parsing. Or after parsing.
postParseTransform
  :: Either a ([GHC.LEpaComment], GHC.DynFlags, GHC.ParsedSource)
  -> Either a (GHC.ParsedSource)
postParseTransform parseRes = fmap mkAnns parseRes
  where
    mkAnns (_cs, _, m) = fixModuleComments m

fixModuleComments :: GHC.ParsedSource -> GHC.ParsedSource
fixModuleComments p = fixModuleHeaderComments $ fixModuleTrailingComments p

fixModuleTrailingComments :: GHC.ParsedSource -> GHC.ParsedSource
fixModuleTrailingComments (GHC.L l p) = GHC.L l p'
  where
    an' = case GHC.hsmodAnn $ GHC.hsmodExt p of
      (GHC.EpAnn a an ocs) -> GHC.EpAnn a an (rebalance ocs)
    p' = p { GHC.hsmodExt = (GHC.hsmodExt p){ GHC.hsmodAnn = an' } }

    rebalance :: GHC.EpAnnComments -> GHC.EpAnnComments
    rebalance cs = cs'
      where
        cs' = case GHC.hsmodLayout $ GHC.hsmodExt p of
          GHC.EpExplicitBraces _ (GHC.EpTok (GHC.EpaSpan (GHC.RealSrcSpan ss _))) ->
            let
              pc = GHC.priorComments cs
              fc = GHC.getFollowingComments cs
              bf (GHC.L anc _) = GHC.epaLocationRealSrcSpan anc > ss

              (prior,f) = break bf fc
              cs'' = GHC.EpaCommentsBalanced (pc <> prior) f
            in cs''
          _ -> cs

-- Deal with https://gitlab.haskell.org/ghc/ghc/-/issues/23984
-- The Lexer works bottom-up, so does not have module declaration info
-- when the first top decl processed
fixModuleHeaderComments :: GHC.ParsedSource -> GHC.ParsedSource
fixModuleHeaderComments (GHC.L l p) = GHC.L l p'
  where
    moveComments :: GHC.EpaLocation -> GHC.LHsDecl GHC.GhcPs -> GHC.EpAnnComments
                 -> (GHC.LHsDecl GHC.GhcPs, GHC.EpAnnComments)
    moveComments GHC.EpaDelta{} dd cs = (dd,cs)
    moveComments (GHC.EpaSpan (GHC.UnhelpfulSpan _)) dd cs = (dd,cs)
    moveComments (GHC.EpaSpan (GHC.RealSrcSpan r _)) (GHC.L (GHC.EpAnn anc an csd) a) cs = (dd,css)
      where
        -- Move any comments on the decl that occur prior to the location
        pc = GHC.priorComments csd
        fc = GHC.getFollowingComments csd
        bf (GHC.L anch _) = GHC.epaLocationRealSrcSpan anch > r
        (move,keep) = break bf pc
        csd' = GHC.EpaCommentsBalanced keep fc

        dd = GHC.L (GHC.EpAnn anc an csd') a
        css = cs <> GHC.EpaComments move

    (ds',an') = rebalance (GHC.hsmodDecls p, GHC.hsmodAnn $ GHC.hsmodExt p)
    p' = p { GHC.hsmodExt = (GHC.hsmodExt p){ GHC.hsmodAnn = an' },
             GHC.hsmodDecls = ds'
           }

    rebalance :: ([GHC.LHsDecl GHC.GhcPs], GHC.EpAnn GHC.AnnsModule)
              -> ([GHC.LHsDecl GHC.GhcPs], GHC.EpAnn GHC.AnnsModule)
    rebalance (ds, GHC.EpAnn a an cs) = (ds1, GHC.EpAnn a an cs')
      where
        (ds1,cs') = case GHC.am_where an of
                     GHC.EpTok whereLoc ->
                           case GHC.hsmodDecls p of
                               (d:ds0) -> (d':ds0, cs0)
                                   where (d',cs0) = moveComments whereLoc d cs
                               ds0 -> (ds0,cs)
                     _ -> (ds,cs)



-- | Internal function. Initializes DynFlags value for parsing.
--
-- Passes "-hide-all-packages" to the GHC API to prevent parsing of
-- package environment files. However this only works if there is no
-- invocation of `setSessionDynFlags` before calling `initDynFlags`.
-- See ghc tickets #15513, #15541.
initDynFlags :: GHC.GhcMonad m
    => Bool -- ^ Use GhcCpp
    -> FilePath
    -> m GHC.DynFlags
initDynFlags useGhcCpp file = do
  -- Based on GHC backpack driver doBackPack
  dflags         <- GHC.getSessionDynFlags
  let dflags0 = if useGhcCpp
          then GHC.xopt_set dflags LangExt.GhcCpp
          else dflags
  let parser_opts0 = GHC.initParserOpts dflags0
  hsc <- GHC.getSession
  let logger = GHC.hsc_logger hsc
  let unit_env = GHC.hsc_unit_env hsc
  let supported_pragmas = "JavaScriptFFI" : GHC.supportedLanguagePragmas dflags0
  (_, src_opts)   <- GHC.liftIO $ myGetOptionsFromFile dflags0 unit_env parser_opts0 supported_pragmas file
  -- error $ "initDynFlags:src_opts: " ++ show  (map GHC.unLoc src_opts)
  (dflags1, _, _) <- GHC.parseDynamicFilePragma logger dflags0 src_opts
  -- Turn this on last to avoid T10942
  let dflags2 = dflags1 `GHC.gopt_set` GHC.Opt_KeepRawTokenStream
  -- Prevent parsing of .ghc.environment.* "package environment files"
  (dflags3, _, _) <- GHC.parseDynamicFlagsCmdLine
    logger
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
  -- AZ Note: "I" below appears to be Lennart Spitzner
  -- I was told we could get away with using the unsafeGlobalDynFlags.
  -- as long as `parseDynamicFilePragma` is impure there seems to be
  -- no reason to use it.
  dflags0 <- GHC.getSessionDynFlags
  logger <- GHC.getLogger
  let parser_opts0 = GHC.initParserOpts dflags0
  let (_, pragmaInfo) = GHC.getOptions parser_opts0 (GHC.supportedLanguagePragmas dflags0) (GHC.stringToStringBuffer $ s) fp
  (dflags1, _, _) <- GHC.parseDynamicFilePragma logger dflags0 pragmaInfo
  -- Turn this on last to avoid T10942
  let dflags2 = dflags1 `GHC.gopt_set` GHC.Opt_KeepRawTokenStream
  -- Prevent parsing of .ghc.environment.* "package environment files"
  (dflags3, _, _) <- GHC.parseDynamicFlagsCmdLine
    logger
    dflags2
    [GHC.noLoc "-hide-all-packages"]
  _ <- GHC.setSessionDynFlags dflags3
  return dflags3

-- ---------------------------------------------------------------------

myGetOptionsFromFile :: GHC.DynFlags
                   -> GHC.UnitEnv
                   -> GHC.ParserOpts
                   -> [String] -- ^ Supported LANGUAGE pragmas
                   -> FilePath            -- ^ Input file
                   -> IO (GHC.Messages GHC.PsMessage, [GHC.Located String]) -- ^ Parsed options, if any.
myGetOptionsFromFile df unit_env popts supported filename
    = Exception.bracket
              (openBinaryFile filename ReadMode)
              (hClose)
              (\handle -> do
                  (warns, opts) <- fmap (GHC.getOptions' popts supported)
                               (myGetPragState df unit_env popts' filename handle
                               -- >>= \prag_state -> traceToks <$> GHC.lazyGetToks prag_state handle)
                               >>= \prag_state -> GHC.lazyGetToks prag_state handle)
                  GHC.seqList opts
                    $ GHC.seqList (GHC.bagToList $ GHC.getMessages warns)
                    $ return (warns, opts))
    where -- We don't need to get haddock doc tokens when we're just
          -- getting the options from pragmas, and lazily lexing them
          -- correctly is a little tricky: If there is "\n" or "\n-"
          -- left at the end of a buffer then the haddock doc may
          -- continue past the end of the buffer, despite the fact that
          -- we already have an apparently-complete token.
          -- We therefore just turn Opt_Haddock off when doing the lazy
          -- lex.
          popts' = GHC.disableHaddock popts

blockSize :: Int
-- blockSize = 17 -- for testing :-)
blockSize = 1024

myGetPragState :: GHC.DynFlags -> GHC.UnitEnv -> GHC.ParserOpts -> FilePath -> Handle -> IO (GHC.PState GHC.PpState)
myGetPragState df unit_env popts filename handle = do
  buf <- GHC.hGetStringBufferBlock handle blockSize

  let macros = fromMaybe Map.empty macrosFromIORef
  -- let macros = fromMaybe (error "macroIORef not set up") macrosFromIORef

      -- opts0 = GHC.initParserOpts flags
      -- opts1 = GHC.enableExtBit GHC.UsePosPragsBit opts0
      -- opts = opts1
  let popts = myInitParserOpts df
  let loc  = GHC.mkRealSrcLoc (GHC.mkFastString filename) 1 1
  let prag_state = if Lexer.ghcCppEnabled popts
        then GHC.initPragStateWithMacros df unit_env popts buf loc
        else GHC.initPragState popts buf loc
  return prag_state { GHC.pp = (GHC.pp prag_state) { GHC.pp_defines = macros }
                    , GHC.buffer = buf }


-- getOptions :: ParserOpts
--            -> [String] -- ^ Supported LANGUAGE pragmas
--            -> StringBuffer -- ^ Input Buffer
--            -> FilePath     -- ^ Source filename.  Used for location info.
--            -> (Messages PsMessage,[Located String]) -- ^ warnings and parsed options.
-- getOptions opts supported buf filename
--     = getOptions' opts supported (getToks opts filename buf)
-- myGetOptions :: GHC.ParserOpts
--            -> [String] -- ^ Supported LANGUAGE pragmas
--            -> GHC.StringBuffer -- ^ Input Buffer
--            -> FilePath     -- ^ Source filename.  Used for location info.
--            -> (GHC.Messages GHC.PsMessage,[GHC.Located String]) -- ^ warnings and parsed options.
-- myGetOptions opts supported buf filename
--     = GHC.getOptions' opts supported (myGetToks opts filename buf)

-- getOptions' :: GHC.ParserOpts
--             -> [String]
--             -> [GHC.Located GHC.Token]      -- Input buffer
--             -> (GHC.Messages GHC.PsMessage,[GHC.Located String])     -- Options.
-- getOptions' opts supported toks =
--     error $ "getOptions': toks " ++ show toks

traceToks :: [GHC.Located GHC.Token] -> [GHC.Located GHC.Token]
traceToks [] = []
traceToks (h:t) = trace ("tok: " ++ show (GHC.unLoc h)) h : traceToks t


-- getToks :: ParserOpts -> FilePath -> StringBuffer -> [Located Token]
-- getToks popts filename buf = lexAll pstate
--  where
--   pstate = initPragState popts buf loc
--   loc  = mkRealSrcLoc (mkFastString filename) 1 1

--   lexAll state = case unP (lexer False return) state of
--                    POk _      t@(L _ ITeof) -> [t]
--                    POk state' t -> t : lexAll state'
--                    _ -> [L (mkSrcSpanPs (last_loc state)) ITeof]

-- lazyGetToks :: DynFlags -> UnitEnv -> ParserOpts -> FilePath -> Handle -> IO [Located Token]
-- lazyGetToks df unit_env popts filename handle = do
--   buf <- hGetStringBufferBlock handle blockSize
--   let prag_state = if Lexer.ghcCppEnabled popts
--         then initPragStateWithMacros df unit_env popts buf loc
--         else initPragState popts buf loc
--   unsafeInterleaveIO $ lazyLexBuf handle prag_state False blockSize
--  where
--   loc  = mkRealSrcLoc (mkFastString filename) 1 1

--   ....

myGetToks :: GHC.DynFlags -> GHC.UnitEnv -> GHC.ParserOpts -> FilePath -> Handle -> IO [GHC.Located GHC.Token]
myGetToks df unit_env popts filename handle = do
  buf <- GHC.hGetStringBufferBlock handle blockSize
  let loc = GHC.mkRealSrcLoc (GHC.mkFastString filename) 1 1
  let prag_state = if Lexer.ghcCppEnabled popts
        then GHC.initPragStateWithMacros df unit_env popts buf loc
        else GHC.initPragState popts buf loc
  -- GHC.getToks popts filename buf
  return $ lexAll prag_state
  where
  lexAll state = case GHC.unP (GHC.lexer False return) state of
                   GHC.POk _      t@(GHC.L _ GHC.ITeof) -> [t]
                   GHC.POk state' t -> t : lexAll state'
                   _ -> [GHC.L (GHC.mkSrcSpanPs (GHC.last_loc state)) GHC.ITeof]


-- getToks :: ParserOpts -> FilePath -> StringBuffer -> [Located Token]
-- getToks popts filename buf = lexAll pstate
--  where
--   pstate = initPragState popts buf loc
--   loc  = mkRealSrcLoc (mkFastString filename) 1 1

--   lexAll state = case unP (lexer False return) state of
--                    POk _      t@(L _ ITeof) -> [t]
--                    POk state' t -> t : lexAll state'
--                    _ -> [L (mkSrcSpanPs (last_loc state)) ITeof]


-- doGetToks :: GHC.ParserOpts -> FilePath -> GHC.StringBuffer -> [GHC.Located GHC.Token]
-- doGetToks popts filename buf = lexAll pstate
--  where
--   pstate = initPragState popts buf loc
--   loc  = mkRealSrcLoc (mkFastString filename) 1 1

--   lexAll state = case unP (lexer False return) state of
--                    GHC.POk _      t@(GHC.L _ GHC.ITeof) -> [t]
--                    GHC.POk state' t -> t : lexAll state'
--                    _ -> [L (mkSrcSpanPs (last_loc state)) GHC.ITeof]

-- ---------------------------------------------------------------------
