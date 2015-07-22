{-# LANGUAGE NamedFieldPuns #-}
module InsertSignature where

import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Parsers
import Language.Haskell.GHC.ExactPrint.Transform
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
  -- (finalAs, finalM) <- addSignature2 "baz" "baz :: String -> Int" as m
  putStrLn $ exactPrintWithAnns finalM finalAs


addSignature :: String -- ^ Function to add a signature for
             -> String -- ^ Type signature
             -> Anns
             -> Module
             -> IO (Anns, Module)
addSignature funid tsig as (GHC.L l m) = do
  Right (sigAnns, sig) <- withDynFlags (\d -> parseDecl d "template" tsig)
  let (before, (bind: after)) = break findFunBind (GHC.hsmodDecls m)
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

-- ---------------------------------------------------------------------

addSignature2 :: String -- ^ Function to add a signature for
             -> String -- ^ Type signature
             -> Anns
             -> Module
             -> IO (Anns, Module)
addSignature2 funid tsig as m = do
  Right (sigAnns, sig@(GHC.L ls (GHC.SigD s))) <- withDynFlags (\d -> parseDecl d "template" tsig)
  let sigAnns' = setPrecedingLines sigAnns (GHC.L ls s) 1 0

      doAddSig = do
         tlDecs <- hsDecls m
         let (before, (bind: after)) = break findFunBind tlDecs
         -- balanceComments (tail before) bind

         modifyAnnsT (\ans1 -> setPrecedingLines ans1  sig 1 0)
         modifyAnnsT (\ans1 -> setPrecedingLines ans1 bind 1 0)

         replaceDecls m (before ++ [sig,bind] ++ after)

  let (lp',(ans',_),_w) = runTransform as doAddSig
  return (mergeAnnList [sigAnns',ans'],lp')

  where
    findFunBind :: GHC.LHsDecl GHC.RdrName -> Bool
    findFunBind (GHC.L _ (GHC.ValD b@(GHC.FunBind {})))
      | showGhc (GHC.unLoc (GHC.fun_id  b)) == funid = True
    findFunBind _ = False
