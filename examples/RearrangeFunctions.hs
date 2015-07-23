{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module RearrangeFunctions where

-- We parse a module and rearrange the functions depending on their order in the import list.


import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Utils
import HsSyn
import RdrName
import SrcLoc
import Data.Maybe
import qualified Data.Map as Map

main :: IO ()
main = do
  Right (as, L l m) <- parseModule "imports.hs"
  let decls = hsmodDecls m
      newDecls =
        case hsmodExports m of
            Nothing -> decls
            Just exp ->
              let cares = map showGhc $ mapMaybe (sortExports . unLoc) (unLoc exp)
                  mkKey v = maybe "default" showGhc (getKey . unLoc $ v)
                  declMap = foldr (\v -> Map.insertWith (++) (mkKey v) [v]) Map.empty decls
                  (sorted, leftover)
                    = foldr (\k (r, m) -> case Map.lookup k m of
                                      Nothing -> (r,m)
                                      Just vs -> (vs ++ r, Map.delete k m)) ([], declMap) cares
              in sorted ++ concatMap snd (Map.toAscList leftover)
  -- We do not need to change the annotations at all
  putStrLn $ exactPrintWithAnns (L l m { hsmodDecls = newDecls }) as


-- | Find the names of things we might care about
sortExports :: IE RdrName -> Maybe RdrName
sortExports (IEVar n)  = Just (unLoc n)
sortExports (IEThingAbs n) = Just (unLoc n)
sortExports (IEThingAll n) = Just (unLoc n)
sortExports (IEThingWith n _) = Just (unLoc n)
sortExports (IEModuleContents _) = Nothing
-- Haddock constructors
sortExports _ = Nothing

-- Get the key to sort decls by
getKey :: HsDecl RdrName -> Maybe RdrName
--getKey (TyClDecl t) = Just (unLoc $ tyClDeclLname  t)
getKey (SigD t) =
  case t of
       TypeSig (n:_) _ _ -> Just (unLoc $ n)
       PatSynSig n _ _ _ _ -> Just (unLoc $ n)
       _ -> Nothing
getKey (ValD t) =
  case t of
       FunBind{fun_id} -> Just (unLoc $ fun_id)
       PatSynBind b    -> Just (unLoc $ psb_id b)
       -- Can't appear at the top level
       PatBind{} -> Nothing
       -- Renamed
       _ -> Nothing
getKey _ = Nothing
