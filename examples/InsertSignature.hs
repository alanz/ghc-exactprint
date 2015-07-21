{-# LANGUAGE NamedFieldPuns #-}
module InsertSignature where

import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Parsers
import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Utils
import Data.Generics.Schemes

import qualified Data.Map as Map

import qualified HsSyn as GHC
import qualified RdrName as GHC
import qualified SrcLoc as GHC
import qualified HsDecls as GHC

type Module = GHC.Located (GHC.HsModule GHC.RdrName)

main :: IO ()
main = do
  Right (as, m) <- parseModule "test.hs"
  (finalAs, finalM) <- addSignature "baz" "baz :: String -> Int" as m
  putStrLn $ exactPrintWithAnns finalM finalAs


addSignature :: String -- ^ Function to add a signature for
             -> String -- ^ Type signature
             -> Anns
             -> Module
             -> IO (Anns, Module)
addSignature funid tsig as (GHC.L l m) = do
  Right (sigAnns, sig) <- withDynFlags (\d -> parseDecl d "template" tsig)
  let (before, (bind: after)) = span findFunBind (GHC.hsmodDecls m)
      newAs = Map.union as sigAnns
      Just bindAnn@Ann{annEntryDelta, annPriorComments} = Map.lookup (mkAnnKey bind) newAs
      finalAnns = Map.adjust (\sigAnn -> sigAnn { annEntryDelta = annEntryDelta
                                                , annPriorComments = annPriorComments })
                             (mkAnnKey sig)
                  . Map.adjust (\bindAnn -> bindAnn { annEntryDelta = DP (1, 0)
                                                    , annPriorComments = [] })
                               (mkAnnKey bind) $ newAs

      finalMod = m { GHC.hsmodDecls = before ++ [sig, bind] ++ after }

  return (finalAnns, GHC.L l finalMod)


  where
    findFunBind :: GHC.LHsDecl GHC.RdrName -> Bool
    findFunBind (GHC.L _ (GHC.ValD b@(GHC.FunBind {})))
      | showGhc (GHC.unLoc (GHC.fun_id  b)) == funid = True
    findFunBind _ = False



