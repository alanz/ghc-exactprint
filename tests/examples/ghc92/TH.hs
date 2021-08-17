{-|
Module      : Language.Grammars.AspectAG.TH
Description : Boilerplate generation
Copyright   : (c) Juan García Garland
License     : GPL
Maintainer  : jpgarcia@fing.edu.uy
Stability   : experimental
Portability : POSIX
-}

{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE TypeOperators             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE UndecidableInstances      #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE FunctionalDependencies    #-}
{-# LANGUAGE TemplateHaskell           #-}

module Language.Grammars.AspectAG.TH where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (showName)
import Data.Proxy
import Data.Either
import GHC.TypeLits
import Data.List
import Data.Set (Set)
import qualified Data.Set as S

import Control.Monad

import Data.GenRec.Label
import Data.GenRec
import Language.Grammars.AspectAG
import Language.Grammars.AspectAG.RecordInstances
import qualified Data.Kind as DK


-- * Attribute labels

-- | makes a type level lit (Symbol) from a String
str2Sym s = litT$ strTyLit s -- th provides nametoSymbol, btw


-- | TH function to define a typed attribute label given a name
-- and a quoted type
attLabel :: String -> Name -> DecsQ
attLabel s t
  = [d| $(varP (mkName s)) = Label :: Label ( 'Att $(str2Sym s)
                                            $(conT t)) |]

-- | for completness, to have a name as the next one
attMono = attLabel

-- | TH function to define a polymorphic attribute
attPoly :: String -> DecsQ
attPoly s
    = [d| $(varP (mkName s)) = Label :: forall a . Label ( 'Att $(str2Sym s) a) |]

-- | multiple monomorphic attributes at once
attLabels :: [(String,Name)] -> Q [Dec]
attLabels xs = liftM concat . sequence $ [attLabel att ty | (att,ty) <- xs ]

-- * Non terminals

-- | add a non terminal symbol
addNont :: String -> Q [Dec]
addNont s
  = liftM concat . sequence $ [addNTLabel s, addNTType s]

addNTLabel :: String -> Q [Dec]
addNTLabel s
  = [d| $(varP (mkName ("nt_" ++ s))) = Label :: Label ('NT $(str2Sym s)) |]

addNTType :: String -> Q [Dec]
addNTType s
  = return [TySynD (mkName ("Nt_"++ s)) [] (AppT (PromotedT 'NT) (LitT (StrTyLit s)))]


-- * Productions
--data Symbol = N String | Te Name
type family Terminal s :: Either NT T where
  Terminal s = 'Right ('T s)

type family NonTerminal s where
  NonTerminal s = 'Left s


data SymTH = Ter Name | NonTer Name | Poly


addChi  :: String -- chi name
        -> Name   -- prd
        -> SymTH  -- symbol type
        -> Q [Dec]
addChi chi prd (Ter typ)
  = [d| $(varP (mkName ("ch_" ++chi)))
           = Label :: Label ( 'Chi $(str2Sym chi)
                                   $(conT prd)
                                    (Terminal $(conT typ)))|]
addChi chi prd (NonTer typ)
  = [d| $(varP (mkName ("ch_" ++chi)))
           = Label :: Label ( 'Chi $(str2Sym chi)
                                   $(conT prd)
                                    (NonTerminal $(conT typ)))|]
addChi chi prd poly
  = [d| $(varP (mkName ("ch_" ++chi)))
           = Label :: forall a . Label ( 'Chi $(str2Sym chi)
                                   $(conT prd)
                                    ('Right ('T a)))|]

-- | only prod symbol
addPrd :: String  --name
       -> Name    --nonterm
       -> Q [Dec]
addPrd prd nt = liftM concat . sequence
              $ [addPrdType prd nt, addPrdLabel prd nt]

addPrdLabel prd nt
  = [d| $(varP (mkName ("p_" ++ prd)))
         = Label :: Label ('Prd $(str2Sym prd) $(conT nt))|]

addPrdType prd nt
  = return [TySynD (mkName ("P_"++ prd)) []
            (AppT (AppT (PromotedT 'Prd) (LitT (StrTyLit prd))) (ConT nt))]


-- | Productions
addProd :: String             -- name
        -> Name               -- nt
        -> [(String, SymTH)]  -- chiLst
        -> Q [Dec]
addProd prd nt xs
  = liftM concat . sequence $
      addPrd prd nt
    : addInstance nt prd (map preProc xs)
    : [addChi chi (mkName ("P_" ++ prd)) sym | (chi, sym) <- xs]
    where preProc (n, Ter a)    = (mkName n, a)
          preProc (n, NonTer a) = (mkName n, a)
          preProc (n, Poly)     = (mkName n, mkName "a")

-- | class
class Prods (lhs :: NT) (name :: Symbol) (rhs :: [(Symbol, Symbol)]) where {}

-- get a list of instances
getInstances :: Q [InstanceDec]
getInstances = do
  ClassI _ instances <- reify ''Prods
  return instances

-- convert the list of instances into an Exp so they can be displayed in GHCi
showInstances :: Q Exp
showInstances = do
  ins <- getInstances
  return . LitE . stringL $ show $ head ins

addInstance :: Name -> String -> [(Name, Name)] -> Q [Dec]
addInstance nt name rhs
  = [d| instance Prods $(conT nt) $(str2Sym name) $(typeList rhs) where {}  |]

typeList :: [(Name, Name)] -> Q Type
typeList = foldr f promotedNilT
    -- where f = \x xs -> appT (appT promotedConsT (nameToSymbolBase x)) xs
  where f = \(n,t) xs
          -> appT (appT promotedConsT (appT (appT (promotedTupleT 2)
                                              (nameToSymbol n))
                                       (nameToSymbolBase t))) xs

nameToSymbol = litT . strTyLit . show
nameToSymbolBase = litT . strTyLit . nameBase

isNTName :: Name -> Bool
isNTName n
  = "Nt_" `isPrefixOf` nameBase n

closeNT :: Name -> Q [Dec]
closeNT nt
  = do decs <- getInstances
       let consts = map mkCon $ filter (isInstanceOf nt) decs
       return [ DataD []
                (mkName $ drop 3 $ nameBase nt) [] Nothing
                consts [DerivClause Nothing [ConT ''Show, ConT ''Eq, ConT ''Read]]]

isInstanceOf nt (InstanceD _ _ (AppT (AppT (AppT (ConT prods) (ConT nt')) _ ) _) _)
  = nameBase nt == nameBase nt'
isInstanceOf _ _ = False

mkCon :: InstanceDec -> Con
mkCon i
  = case i of
  InstanceD _ [] (AppT (AppT (AppT (ConT _prods) (ConT nt)) (LitT (StrTyLit prdname))) tlist) _
    -> RecC (mkName prdname) (map mkBangPR $ getTList tlist)

mkBangP  (_, a) = (Bang NoSourceUnpackedness NoSourceStrictness, ConT a)
mkBangPR (n, a) = (n, Bang NoSourceUnpackedness NoSourceStrictness, ConT a)

getTList :: Type -> [(Name, Name)]
getTList (SigT _ _) = []
getTList (AppT (AppT (PromotedConsT)
                (AppT (AppT (PromotedTupleT 2)
                       (LitT (StrTyLit n)))
                  (LitT (StrTyLit pos))))
           ts)
  = (mkName n,
     if "Nt_" `isPrefixOf` pos then mkName $ drop 3 pos else mkName pos)
    : getTList ts
getTList _ = []

-- | keeps nt info
getTListNT :: Type -> [(Name, Name)]
getTListNT (SigT _ _) = []
getTListNT (AppT (AppT (PromotedConsT)
                (AppT (AppT (PromotedTupleT 2)
                       (LitT (StrTyLit n)))
                  (LitT (StrTyLit pos))))
           ts)
  = (mkName n, mkName pos) : getTListNT ts
getTListNT _ = []

-- | like |mkCon| in semantic functions, builds a case
mkClause :: InstanceDec -> Clause
mkClause i
  = case i of
  InstanceD _ [] (AppT (AppT (AppT (ConT _prods)
                               (ConT nt))
                         (LitT (StrTyLit prdname)))
                   tlist) _
    -> Clause [VarP (mkName "asp"),
               ConP (mkName $ prdname) [ VarP a | a <- map fst (getTList tlist)]]
    (NormalB ((AppE (AppE (AppE (VarE $ mkName "knitAspect")
                           (VarE $ mkName $ "p_"++ prdname))
                      (VarE $ mkName "asp"))
                (toSemRec (getTListNT tlist)))))
    []

toSemRec :: [(Name, Name)] -> Exp
toSemRec
  = foldr mkChSem (VarE (mkName "emptyGenRec"))
  where mkChSem (n,pos) xs
          | "Nt_" `isPrefixOf` nameBase pos =
          (AppE (AppE (VarE $ mkName ".*.")
                 (AppE (AppE (VarE $ mkName ".=.")
                        (VarE $ mkName $ "ch_" ++ nameBase n))
                   (AppE (AppE (VarE $ mkName $ "sem_" ++ (drop 3 $ nameBase pos))
                          (VarE $ mkName "asp"))
                     (VarE $ n))))
            xs)
          | otherwise =
            (AppE (AppE (VarE $ mkName ".*.")
                   (AppE (AppE (VarE $ mkName ".=.")
                          (VarE $ mkName $ "ch_" ++ nameBase n))
                    (AppE (VarE $ mkName "sem_Lit")
                      (VarE $ n))))
            xs)
closeNTs :: [Name] -> Q [Dec]
closeNTs = liftM concat . sequence . map (closeNT)

mkSemFunc :: Name -- nonterm
          -> Q [Dec]
mkSemFunc nt =
  do decs <- getInstances
     let clauses = map mkClause $ filter (isInstanceOf nt) decs
     return [FunD (mkName $ "sem_" ++ drop 3 (nameBase nt)) clauses ]

mkSemFuncs :: [Name] -> Q [Dec]
mkSemFuncs
  = liftM concat . sequence . map (mkSemFunc)

