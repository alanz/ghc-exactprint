-- cabal-helper: Simple interface to Cabal's configuration state
-- Copyright (C) 2015-2018  Daniel Gröber <cabal-helper@dxld.at>
--
-- SPDX-License-Identifier: Apache-2.0
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0

{-# LANGUAGE GHC_CPP, BangPatterns, RecordWildCards, RankNTypes, ViewPatterns,
  TupleSections #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-  # OPTIONS_GHC -Wno-missing-signatures #-}
{-  # OPTIONS_GHC -fno-warn-incomplete-patterns #-}

#ifdef MIN_VERSION_Cabal
#undef CH_MIN_VERSION_Cabal
#define CH_MIN_VERSION_Cabal MIN_VERSION_Cabal
#endif

module CabalHelper.Runtime.HelperMain (helper_main) where

import Distribution.Simple.Utils (cabalVersion)
import Distribution.Simple.Configure
import Distribution.Package
  ( PackageIdentifier
  , PackageId
  , packageName
  , packageVersion
  )
import Distribution.PackageDescription
  ( PackageDescription
  , GenericPackageDescription(..)
  , Flag(..)
  , FlagName
  , FlagAssignment
  , Executable(..)
  , Library(..)
  , TestSuite(..)
  , Benchmark(..)
  , BuildInfo(..)
  , TestSuiteInterface(..)
  , BenchmarkInterface(..)
  , withLib
  )
import Distribution.PackageDescription.Configuration
  ( flattenPackageDescription
  )
import Distribution.Simple.Program
  ( requireProgram
  , ghcProgram
  )
import Distribution.Simple.Program.Types
  ( ConfiguredProgram(..)
  )
import Distribution.Simple.Configure
  ( getPersistBuildConfig
  )
import Distribution.Simple.LocalBuildInfo
  ( LocalBuildInfo(..)
  , Component(..)
  , ComponentName(..)
  , ComponentLocalBuildInfo(..)
  , componentBuildInfo
  , withAllComponentsInBuildOrder
  , withLibLBI
  , withExeLBI
  )
import Distribution.Simple.GHC
  ( componentGhcOptions
  )
import Distribution.Simple.Program.GHC
  ( GhcOptions(..)
  , renderGhcOptions
  )
import Distribution.Simple.Setup
  ( ConfigFlags(..)
  , Flag(..)
  , fromFlagOrDefault
  )
import Distribution.Simple.Build
  ( initialBuildSteps
  )
import Distribution.Simple.BuildPaths
  ( cppHeaderName
  )
import Distribution.Simple.Compiler
  ( PackageDB(..)
  , compilerId
  )
import Distribution.Compiler
  ( CompilerId(..)
  )
import Distribution.ModuleName
  ( components
  )
import qualified Distribution.ModuleName as C
  ( ModuleName
  )
import Distribution.Text
  ( display
  )
import Distribution.Verbosity
  ( Verbosity
  , silent
  , deafening
  , normal
  )
import Distribution.Version
  ( Version
  )

#if CH_MIN_VERSION_Cabal(1,22,0)
-- CPP >= 1.22
import Distribution.Utils.NubList
#endif

#if CH_MIN_VERSION_Cabal(1,23,0)
-- >= 1.23
import Distribution.Simple.LocalBuildInfo
  ( localUnitId
  )
#else
-- <= 1.22
import Distribution.Simple.LocalBuildInfo
  ( inplacePackageId
  )
#endif

#if CH_MIN_VERSION_Cabal(1,25,0)
-- >=1.25
import Distribution.PackageDescription
  ( unFlagName
  -- , mkFlagName
  )
import Distribution.Types.ForeignLib
  ( ForeignLib(..)
  )
import Distribution.Types.UnqualComponentName
  ( UnqualComponentName
  , unUnqualComponentName
  )
#else
-- <1.25
import Distribution.PackageDescription
  ( FlagName(FlagName)
  )
#endif

#if CH_MIN_VERSION_Cabal(2,0,0)
-- CPP >= 2.0
import Distribution.Simple.LocalBuildInfo
  ( allLibModules
  , componentBuildDir
  )
import Distribution.Backpack
  ( OpenUnitId(..),
    OpenModule(..)
  )
import Distribution.ModuleName
  ( ModuleName
  )
import Distribution.Types.ComponentId
  ( unComponentId
  )
import Distribution.Types.ComponentLocalBuildInfo
  ( maybeComponentInstantiatedWith
  )
import Distribution.Types.ModuleRenaming
  ( ModuleRenaming(..),
    isDefaultRenaming
  )
import Distribution.Types.MungedPackageId
  ( MungedPackageId
  )
import Distribution.Types.UnitId
  ( UnitId
  , unDefUnitId
  , unUnitId
  )
import Distribution.Types.UnitId
  ( DefUnitId
  )
import Distribution.Utils.NubList
  ( toNubListR
  )
import Distribution.Version
  ( versionNumbers
  , mkVersion
  )
import qualified Distribution.InstalledPackageInfo as Installed
#endif

import Control.Applicative ((<$>), (<*>), ZipList(..))
import Control.Arrow (first, second, (&&&))
import Control.Monad
import Control.Exception (catch, PatternMatchFail(..))
import Data.List
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Monoid
import Data.IORef
import qualified Data.Version as DataVersion
import System.Environment
import System.Directory
import System.FilePath
import System.Exit
import System.IO
import System.IO.Unsafe (unsafeInterleaveIO, unsafePerformIO)
import Text.Printf

import CabalHelper.Shared.Common
import CabalHelper.Shared.InterfaceTypes
import CabalHelper.Runtime.Compat

usage :: IO ()
usage = do
  prog <- getProgName
  hPutStr stderr $ "Usage: " ++ prog ++ " " ++ usageMsg
 where
   usageMsg = ""
     ++"CABAL_FILE DIST_DIR (v1|v2)\n"
     ++"  ( version\n"
     ++"  | flags\n"
     ++"  | config-flags\n"
     ++"  | non-default-config-flags\n"
     ++"  | write-autogen-files\n"
     ++"  | compiler-id\n"
     ++"  | component-info\n"
     ++"  | print-lbi [--human]\n"
     ++"  ) ...\n"

commands :: [String]
commands = [ "flags"
           , "config-flags"
           , "non-default-config-flags"
           , "write-autogen-files"
           , "compiler-id"
           , "package-db-stack"
           , "component-info"
           , "print-lbi"
           ]

helper_main :: [String] -> IO [Maybe ChResponse]
helper_main args = do
  cfile : distdir : pt : args'
    <- case args of
         [] -> usage >> exitFailure
         _ -> return args

  ddexists <- doesDirectoryExist distdir
  when (not ddexists) $ do
         errMsg $ "distdir '"++distdir++"' does not exist"
         exitFailure

  v <- maybe silent (const deafening) . lookup  "CABAL_HELPER_DEBUG" <$> getEnvironment
  lbi <- unsafeInterleaveIO $ getPersistBuildConfig distdir
  gpd <- unsafeInterleaveIO $ readPackageDescription v cfile
  let pd = localPkgDescr lbi
  let lvd = (lbi, v, distdir)

  let
      -- a =<< b $$ c   ==  (a =<< b) $$ c
      infixr 2 $$
      ($$) = ($)

      collectCmdOptions :: [String] -> [[String]]
      collectCmdOptions =
          reverse . map reverse . foldl f [] . dropWhile isOpt
       where
         isOpt = ("--" `isPrefixOf`)
         f [] x = [[x]]
         f (a:as) x
             | isOpt x = (x:a):as
             | otherwise = [x]:(a:as)

  let cmds = collectCmdOptions args'

  flip mapM cmds $$ \x -> do
  case x of
    "version":[] ->
      return $ Just $ ChResponseVersion ("Cabal", toDataVersion cabalVersion)

    "package-id":[] ->
      return $ Just $ ChResponseVersion $ (,)
        (display (packageName gpd))
        (toDataVersion (packageVersion gpd))

    "flags":[] -> do
      return $ Just $ ChResponseFlags $ sort $
        map (flagName' &&& flagDefault) $ genPackageFlags gpd

    "config-flags":[] -> do
      return $ Just $ ChResponseFlags $ sort $
        map (first unFlagName)
          $ unFlagAssignment
          $ configConfigurationsFlags
          $ configFlags lbi

    "non-default-config-flags":[] -> do
      let flagDefinitons = genPackageFlags gpd
          flagAssgnments =
#if CH_MIN_VERSION_Cabal(2,2,0)
            unFlagAssignment $ configConfigurationsFlags
#else
            configConfigurationsFlags
#endif
              $ configFlags lbi
          nonDefaultFlags =
              [ (flag_name, val)
              | MkFlag {flagName=(unFlagName -> flag_name'), flagDefault=def_val} <- flagDefinitons
              , (unFlagName -> flag_name, val) <- flagAssgnments
              , flag_name == flag_name'
              , val /= def_val
              ]
      return $ Just $ ChResponseFlags $ sort nonDefaultFlags

    "write-autogen-files":[] -> do
      initialBuildStepsForAllComponents distdir pd lbi v
      return Nothing

    "compiler-id":[] -> do
      let CompilerId comp ver = compilerId $ compiler lbi
      return $ Just $ ChResponseVersion $ (,) (show comp) (toDataVersion ver)

    "component-info":[] -> do
      res <- componentsInfo lvd pt
      return $ Just $ ChResponseComponentsInfo res

    "print-lbi":flags ->
      case flags of
        ["--human"] -> print lbi >> return Nothing
        _           -> return $ Just $ ChResponseLbi $ show lbi

    cmd:_ | not (cmd `elem` commands) ->
            errMsg ("Unknown command: " ++ cmd) >> usage >> exitFailure
    _ ->
            errMsg "Invalid usage!" >> usage >> exitFailure

type ProjectType = String -- either "v1" or "v2"

componentsInfo
    :: (LocalBuildInfo, Verbosity, FilePath)
    -> ProjectType
    -> IO (Map.Map ChComponentName ChComponentInfo)
componentsInfo lvd@(lbi, v, distdir) pt = do
      let mod_ghc_opts opts
            | pt == "v1" = opts {
                ghcOptPackageDBs =
                  -- c.f. Simple/Build.hs createInternalPackageDB call
                  ghcOptPackageDBs opts ++
                  [SpecificPackageDB $ internalPackageDBPath lbi distdir]
                }
            | pt == "v2" = opts
            | otherwise = error $ "Unknown project-type '"++pt++"'!"

      ciGhcOptions <- componentOptions lvd mod_ghc_opts

      ciSourceDirs <- componentsMap lbi v distdir $ \_ _ bi -> return $ hsSourceDirs bi

      ciEntrypoints <- componentsMap lbi v distdir $ \c _clbi _bi ->
               return $ componentEntrypoints c

      let comp_name = map fst ciGhcOptions
          uiComponents = Map.fromList
                      $ map (ciComponentName &&& id)
                      $ getZipList
                      $ ChComponentInfo
                     <$> ZipList comp_name
                     <*> ZipList (map snd ciGhcOptions)
                     <*> ZipList (map snd ciSourceDirs)
                     <*> ZipList (map snd ciEntrypoints)

      return uiComponents


flagName' :: Distribution.PackageDescription.Flag -> String
flagName' = unFlagName . flagName

componentsMap :: LocalBuildInfo
              -> Verbosity
              -> FilePath
              -> (   Component
                  -> ComponentLocalBuildInfo
                  -> BuildInfo
                  -> IO a)
              -> IO [(ChComponentName, a)]
componentsMap lbi _v _distdir f = do
    let pd = localPkgDescr lbi

    lr <- newIORef []

    -- withComponentsLBI is deprecated but also exists in very old versions
    -- it's equivalent to withAllComponentsInBuildOrder in newer versions
    withAllComponentsInBuildOrder pd lbi $ \c clbi -> do
        let bi = componentBuildInfo c
            name = componentNameToCh $ componentNameFromComponent c

        l' <- readIORef lr
        r <- f c clbi bi
        writeIORef lr $ (name, r) : l'

    reverse <$> readIORef lr

componentOptions'
    :: (LocalBuildInfo, Verbosity, FilePath)
    -> (LocalBuildInfo -> Verbosity -> GhcOptions -> IO a)
    -> (GhcOptions -> GhcOptions)
    -> IO [(ChComponentName, a)]
componentOptions' (lbi, v, distdir) rf f = do
  componentsMap lbi v distdir $ \c clbi bi ->
         let
           outdir = componentOutDir lbi c
           opts = componentGhcOptions normal lbi bi clbi outdir

         in rf lbi v $ f opts

componentOptions :: (LocalBuildInfo, Verbosity, FilePath)
                 -> (GhcOptions -> GhcOptions)
                 -> IO [(ChComponentName, [String])]
componentOptions (lbi, v, distdir) f =
    componentOptions' (lbi, v, distdir) renderGhcOptions' f

gmModuleName :: C.ModuleName -> ChModuleName
gmModuleName = ChModuleName . intercalate "." . components


initialBuildStepsForAllComponents
    :: FilePath
    -> PackageDescription
    -> LocalBuildInfo
    -> Verbosity
    -> IO ()
initialBuildStepsForAllComponents distdir pd lbi v =
  initialBuildSteps distdir pd lbi v



#if !CH_MIN_VERSION_Cabal(1,25,0)
-- CPP < 1.25
unFlagName :: FlagName -> String
unFlagName (FlagName n) = n
-- mkFlagName n = FlagName n
#endif

toDataVersion :: Version -> DataVersion.Version
--fromDataVersion :: DataVersion.Version -> Version
#if CH_MIN_VERSION_Cabal(2,0,0)
toDataVersion v = DataVersion.Version (versionNumbers v) []
--fromDataVersion (DataVersion.Version vs _) = mkVersion vs
#else
toDataVersion = id
--fromDataVersion = id
#endif



componentEntrypoints :: Component -> ChEntrypoint
componentEntrypoints (CLib Library {..})
    = ChLibEntrypoint
        (map gmModuleName exposedModules)
        (map gmModuleName $ otherModules libBuildInfo)
#if CH_MIN_VERSION_Cabal(2,0,0)
        (map gmModuleName signatures)
#else
        [] -- no signatures prior to Cabal 2.0
#endif
#if CH_MIN_VERSION_Cabal(2,0,0)
componentEntrypoints (CFLib (ForeignLib{..}))
    = ChLibEntrypoint
        []
        (map gmModuleName $ otherModules foreignLibBuildInfo)
        []
#endif
componentEntrypoints (CExe Executable {..})
    = ChExeEntrypoint
        modulePath
        (map gmModuleName $ otherModules buildInfo)
componentEntrypoints (CTest TestSuite { testInterface = TestSuiteExeV10 _ fp, ..})
    = ChExeEntrypoint fp (map gmModuleName $ otherModules testBuildInfo)
componentEntrypoints (CTest TestSuite { testInterface = TestSuiteLibV09 _ mn, ..})
    = ChLibEntrypoint [gmModuleName mn] (map gmModuleName $ otherModules testBuildInfo) []
componentEntrypoints (CTest TestSuite {})
    = ChLibEntrypoint [] [] []
componentEntrypoints (CBench Benchmark { benchmarkInterface = BenchmarkExeV10 _  fp, ..})
    = ChExeEntrypoint fp (map gmModuleName $ otherModules benchmarkBuildInfo)
componentEntrypoints (CBench Benchmark {})
    = ChLibEntrypoint [] [] []

renderGhcOptions' :: LocalBuildInfo
                  -> Verbosity
                  -> GhcOptions
                  -> IO [String]
#if !CH_MIN_VERSION_Cabal(1,20,0)
renderGhcOptions' lbi v opts = do
-- CPP < 1.20
  (ghcProg, _) <- requireProgram v ghcProgram (withPrograms lbi)
  let Just ghcVer = programVersion ghcProg
  return $ renderGhcOptions ghcVer opts
#elif CH_MIN_VERSION_Cabal(1,20,0) && !CH_MIN_VERSION_Cabal(1,24,0)
renderGhcOptions' lbi _v opts = do
-- CPP >= 1.20 && < 1.24
  return $ renderGhcOptions (compiler lbi) opts
#else
renderGhcOptions' lbi _v opts = do
-- CPP >= 1.24
  return $ renderGhcOptions (compiler lbi) (hostPlatform lbi) opts
#endif

