module Main where

import GHC
import GHC.ResponseFile (unescapeArgs)
import GHC.Settings
import GHC.Settings.IO
import GHC.Driver.DynFlags
import GHC.Driver.Session
import GHC.Driver.Env

import Control.Monad.Trans.Except (runExceptT)
import qualified Data.Set as Set
import Data.List (intercalate)
import System.Directory
import System.Environment
import System.IO (hPutStrLn, stderr)
import System.Exit (exitWith, ExitCode(ExitFailure))

-- Precondition: this test case must be executed in a directory with a space.
--
-- First we get the current settings file and amend it with extra arguments that we *know*
-- contain spaces by construction.
-- Then, we write this new settings file to disk where we know one of the parent
-- directories contains a space by virtue of the ghc test suite. This is important
-- for testing variable substitution containing spaces in the settings file.
-- At last, we parse the settings file again and compare the options to the original settings
-- file. As we added a fixed number of options, we verify that relevant all config options parser
-- escaped the spaces appropriately.
main :: IO ()
main = do
  libdir:_args <- getArgs

  (rawSettingOpts, originalSettings) <- runGhc (Just libdir) $ do
    dflags <- hsc_dflags <$> getSession
    pure (rawSettings dflags, settings dflags)

  top_dir <- makeAbsolute "./ghc-install-folder/lib with spaces"

  let argsWithSpaces = "\"-some option\" -some\\ other"
      numberOfExtraArgs = length $ unescapeArgs argsWithSpaces
      -- These are all options that can have multiple 'String' or 'Option' values.
      -- We explicitly do not add 'C compiler link flags' here, as 'initSettings'
      -- already adds the options of "C compiler flags" to this config field.
      multipleArguments = Set.fromList
        [ "Haskell CPP flags"
        , "JavaScript CPP flags"
        , "C-- CPP flags"
        , "C compiler flags"
        , "C++ compiler flags"
        , "CPP flags"
        , "Merge objects flags"
        ]

  let rawSettingOptsWithExtraArgs =
        map (\(name, args) -> if Set.member name multipleArguments
          then (name, args ++ " " ++ argsWithSpaces)
          else (name, args)) rawSettingOpts

  -- write out the modified settings. We try to keep it legible
  writeFile (top_dir ++ "/settings") $
    "[" ++ (intercalate "\n," (map show rawSettingOptsWithExtraArgs)) ++ "]"

  settingsm <- runExceptT $ initSettings top_dir

  case settingsm of
    Left (SettingsError_MissingData msg) -> do
      hPutStrLn stderr $ "WARNING: " ++ show msg
      hPutStrLn stderr $ "dont know target platform"
      exitWith $ ExitFailure 1
    Left (SettingsError_BadData msg) -> do
      hPutStrLn stderr msg
      exitWith $ ExitFailure 1
    Right ghc_settings -> do
      let
        recordSetting :: String -> (Settings -> [String]) -> IO ()
        recordSetting label selector = do
          let opts = selector ghc_settings
              origOpts = selector originalSettings
              -- At least one of the options must contain a space
              containsSpaces = any (' ' `elem`) opts
          hPutStrLn stderr
              $ "=== '" <> label <> "' contains " ++ show numberOfExtraArgs ++ " new entries: "
              ++ show (length opts == length origOpts + numberOfExtraArgs)
          hPutStrLn stderr $ "    Contains spaces: " ++ show containsSpaces

        recordSettingM :: String -> (Settings -> Maybe [a]) -> IO ()
        recordSettingM label selector = do
          let optsM = selector ghc_settings
              origOptsM = selector originalSettings
          hPutStrLn stderr
              $ "=== '" <> label <> "' contains expected entries: "
              ++ show (case (optsM, origOptsM) of
                  (Just opts, Just origOpts) -> length opts == length origOpts + numberOfExtraArgs
                  (Nothing, Nothing) -> True
                  (Just _, Nothing) -> False
                  (Nothing, Just _) -> False
              )

        recordFpSetting :: String -> (Settings -> String) -> IO ()
        recordFpSetting label selector = do
          let fp = selector ghc_settings
              containsEscapedSpaces ('\\':' ':_) = True
              containsEscapedSpaces (' ':xs) = containsEscapedSpaces xs
              containsEscapedSpaces [] = False
              containsEscapedSpaces (_:xs) = containsEscapedSpaces xs

          -- Filepath should not contain escaped spaces
          hPutStrLn stderr $ "=== FilePath '" <> label <> "' contains escaped spaces: " ++ show (containsEscapedSpaces fp)

      -- Assertions
      -- Assumption: this test case is executed in a directory with a space.

      -- Setting 'Haskell CPP flags' contains '$topdir' reference.
      -- Resolving those while containing spaces, should not introduce more options.
      recordSetting "Haskell CPP flags" (map showOpt . snd . toolSettings_pgm_P . sToolSettings)
      -- Setting 'JavaScript CPP flags' contains '$topdir' reference.
      -- Resolving those while containing spaces, should not introduce more options.
      recordSetting "JavaScript CPP flags" (map showOpt . snd . toolSettings_pgm_JSP . sToolSettings)
      -- Setting 'C-- CPP flags' contains '$topdir' reference.
      -- Resolving those while containing spaces, should not introduce more options.
      recordSetting "C-- CPP flags" (map showOpt . snd . toolSettings_pgm_CmmP . sToolSettings)
      -- Setting 'C compiler flags' contains strings with spaces.
      -- GHC should not split these by word.
      recordSetting "C compiler flags" (toolSettings_opt_c . sToolSettings)
      -- Setting 'C compiler link flags' contains strings with spaces.
      -- GHC should not split these by word.
      -- While we did not explicitly add the extra arguments, 'initSettings' adds "C compiler flags" options
      -- to this field.
      recordSetting "C compiler link flags" (map showOpt . snd . toolSettings_pgm_l . sToolSettings)
      -- Setting 'C++ compiler flags' contains strings with spaces.
      -- GHC should not split these by word.
      recordSetting "C++ compiler flags" (toolSettings_opt_cxx . sToolSettings)
      -- Setting 'CPP flags' contains strings with spaces.
      -- GHC should not split these by word.
      recordSetting "CPP flags" (map showOpt . snd . toolSettings_pgm_cpp . sToolSettings)
      -- Setting 'Merge objects flags' contains strings with spaces.
      -- GHC should not split these by word.
      -- If 'Nothing', ignore this test, otherwise the same assertion holds as before.
      recordSettingM "Merge objects flags" (fmap (map showOpt . snd) . toolSettings_pgm_lm . sToolSettings)
      -- Setting 'C compiler command' contains '$topdir' reference.
      -- Spaces in the final filepath should not be escaped.
      recordFpSetting "C compiler" (toolSettings_pgm_c . sToolSettings)

