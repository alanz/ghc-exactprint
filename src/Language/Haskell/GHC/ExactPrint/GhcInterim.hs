-- functions from GHC copied here until they can be exported in the next version.
module Language.Haskell.GHC.ExactPrint.GhcInterim where

import DriverPhases
import DriverPipeline
import DynFlags
import ErrUtils
import Finder
import GhcMonad
import HeaderInfo
import HsSyn
import HscTypes
import Module
import RdrName          ( RdrName )
import TcIface          ( typecheckIface )
import TcRnMonad        ( initIfaceCheck )

import ApiAnnotation
import Bag              ( listToBag )
import BasicTypes
import Digraph
import Exception        ( tryIO, gbracket, gfinally )
import FastString
import Lexer
import Maybes           ( expectJust )
import MonadUtils       ( allM, MonadIO )
import Outputable
import Panic
import SrcLoc
import StringBuffer
import SysTools
import UniqFM
import Util

import Data.Either ( rights, partitionEithers )
import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Set as Set
import qualified FiniteMap as Map ( insertListWith )

import Control.Concurrent ( forkIOWithUnmask, killThread )
import qualified GHC.Conc as CC
import Control.Concurrent.MVar
import Control.Concurrent.QSem
import Control.Exception
import Control.Monad
import Data.IORef
import Data.List
import qualified Data.List as List
import Data.Maybe
import Data.Ord ( comparing )
import Data.Time
import System.Directory
import System.FilePath
import System.IO        ( fixIO )
import System.IO.Error  ( isDoesNotExistError )

import GHC.Conc ( getNumProcessors, getNumCapabilities, setNumCapabilities )

-- ---------------------------------------------------------------------
{-
-----------------------------------------------------------------------------
-- Summarising modules

-- We have two types of summarisation:
--
--    * Summarise a file.  This is used for the root module(s) passed to
--      cmLoadModules.  The file is read, and used to determine the root
--      module name.  The module name may differ from the filename.
--
--    * Summarise a module.  We are given a module name, and must provide
--      a summary.  The finder is used to locate the file in which the module
--      resides.

summariseFile
        :: HscEnv
        -> [ModSummary]                 -- old summaries
        -> FilePath                     -- source file name
        -> Maybe Phase                  -- start phase
        -> Bool                         -- object code allowed?
        -> Maybe (StringBuffer,UTCTime)
        -> IO ModSummary

summariseFile hsc_env old_summaries file mb_phase obj_allowed maybe_buf
        -- we can use a cached summary if one is available and the
        -- source file hasn't changed,  But we have to look up the summary
        -- by source file, rather than module name as we do in summarise.
   | Just old_summary <- findSummaryBySourceFile old_summaries file
   = do
        let location = ms_location old_summary
            dflags = hsc_dflags hsc_env

        src_timestamp <- get_src_timestamp
                -- The file exists; we checked in getRootSummary above.
                -- If it gets removed subsequently, then this
                -- getModificationUTCTime may fail, but that's the right
                -- behaviour.

                -- return the cached summary if the source didn't change
        if ms_hs_date old_summary == src_timestamp &&
           not (gopt Opt_ForceRecomp (hsc_dflags hsc_env))
           then do -- update the object-file timestamp
                  obj_timestamp <-
                    if isObjectTarget (hscTarget (hsc_dflags hsc_env))
                        || obj_allowed -- bug #1205
                        then liftIO $ getObjTimestamp location NotBoot
                        else return Nothing
                  hi_timestamp <- maybeGetIfaceDate dflags location
                  return old_summary{ ms_obj_date = obj_timestamp
                                    , ms_iface_date = hi_timestamp }
           else
                new_summary src_timestamp

   | otherwise
   = do src_timestamp <- get_src_timestamp
        new_summary src_timestamp
  where
    get_src_timestamp = case maybe_buf of
                           Just (_,t) -> return t
                           Nothing    -> liftIO $ getModificationUTCTime file
                        -- getMofificationUTCTime may fail

    new_summary src_timestamp = do
        let dflags = hsc_dflags hsc_env

        let hsc_src = if isHaskellSigFilename file then HsigFile else HsSrcFile

        (dflags', hspp_fn, buf)
            <- preprocessFile hsc_env file mb_phase maybe_buf

        (srcimps,the_imps, L _ mod_name) <- getImports dflags' buf hspp_fn file

        -- Make a ModLocation for this file
        location <- liftIO $ mkHomeModLocation dflags mod_name file

        -- Tell the Finder cache where it is, so that subsequent calls
        -- to findModule will find it, even if it's not on any search path
        mod <- liftIO $ addHomeModuleToFinder hsc_env mod_name location

        -- when the user asks to load a source file by name, we only
        -- use an object file if -fobject-code is on.  See #1205.
        obj_timestamp <-
            if isObjectTarget (hscTarget (hsc_dflags hsc_env))
               || obj_allowed -- bug #1205
                then liftIO $ modificationTimeIfExists (ml_obj_file location)
                else return Nothing

        hi_timestamp <- maybeGetIfaceDate dflags location

        return (ModSummary { ms_mod = mod, ms_hsc_src = hsc_src,
                             ms_location = location,
                             ms_hspp_file = hspp_fn,
                             ms_hspp_opts = dflags',
                             ms_hspp_buf  = Just buf,
                             ms_srcimps = srcimps, ms_textual_imps = the_imps,
                             ms_hs_date = src_timestamp,
                             ms_iface_date = hi_timestamp,
                             ms_obj_date = obj_timestamp })
-}

-- ---------------------------------------------------------------------
-- From Lexer.x
commentToAnnotation :: Located Token -> Located AnnotationComment
commentToAnnotation (L l (ITdocCommentNext s))  = L l (AnnDocCommentNext s)
commentToAnnotation (L l (ITdocCommentPrev s))  = L l (AnnDocCommentPrev s)
commentToAnnotation (L l (ITdocCommentNamed s)) = L l (AnnDocCommentNamed s)
commentToAnnotation (L l (ITdocSection n s))    = L l (AnnDocSection n s)
commentToAnnotation (L l (ITdocOptions s))      = L l (AnnDocOptions s)
commentToAnnotation (L l (ITdocOptionsOld s))   = L l (AnnDocOptionsOld s)
commentToAnnotation (L l (ITlineComment s))     = L l (AnnLineComment s)
commentToAnnotation (L l (ITblockComment s))    = L l (AnnBlockComment s)
