module FrontEnd.Tc.Main (tiExpr, tiProgram, makeProgram, isTypePlaceholder ) where

import Control.Monad.Reader
import Control.Monad.Writer
import Data.Graph(stronglyConnComp, SCC(..))
import System.IO(hPutStr,stderr)
import Text.Printf
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Text.PrettyPrint.HughesPJ as P

import Doc.DocLike
import Doc.PPrint as PPrint
import FrontEnd.Class
import FrontEnd.DeclsDepends(getDeclDeps)
import FrontEnd.Diagnostic
import FrontEnd.HsPretty
import FrontEnd.HsSyn
import FrontEnd.KindInfer
import FrontEnd.SrcLoc
import FrontEnd.Tc.Class
import FrontEnd.Tc.Kind
import FrontEnd.Tc.Monad hiding(listenPreds)
import FrontEnd.Tc.Type
import FrontEnd.Tc.Unify
import FrontEnd.Utils(getDeclName,maybeGetDeclName)
import FrontEnd.Warning
import GenUtil
import Name.Name
import Name.Names
import Name.VConsts
import Options
import Support.FreeVars
import Util.Progress
import qualified FlagDump as FD
import qualified FlagOpts as FO


tiNonRecImpl decl = withContext $ do
    let f n s = do
        let (TForAll vs _) = toSigma s
        addCoerce n (ctAbs vs)
        when (dump FD.BoxySteps) $ liftIO $ putStrLn $ "*** " ++ show n ++ " :: " ++ prettyPrintType s
        return (n,s)
    return (fst res, nenv)

