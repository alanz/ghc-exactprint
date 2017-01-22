{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, TypeFamilies, FlexibleInstances, OverlappingInstances, TypeOperators, PatternGuards #-}
module Object.Templates(
        makeName,
        makeObject,
        makeObjectFlexible
        ) where

import Object.Letters
import Object.Types

import Prelude hiding ((.))
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Char
import Data.Maybe

-- |
-- implements 'makeObject' or 'makeObjectFlexible' depending on the first argument
makeObject' :: Bool -> Name -> Q [Dec]
makeObject' flexible name = go name where
        go :: Name -> Q [Dec]
        go obj = do
                (name, vars, fields) <- reify name >>= getInfo
                let objType = foldl AppT (ConT name) (VarT<*vars)
                outputDecls <- if flexible
                        then return []
                        else [d|
                                type instance Output $(return objType) (Method m) =
                                        MethodOutput $(return objType) (Method m)
                                type instance Output $(return objType) (Method m := input) =
                                        MethodOutput $(return objType) (Method m := input)
                                |]
                fieldDecls <- (sequence $ makeField name vars <* fields) *> concat
                return $ outputDecls ++ fieldDecls
-- "(Object.Example.Foo,[x_1627454179],[(Object.Example._bar,NotStrict,ConT GHC.Types.Int),(Object.Example._baz,NotStrict,ConT GHC.Types.Char),(Object.Example._blub,NotStrict,VarT x_1627454179)])"
        makeField ::  Name -> [Name] -> VarStrictType -> Q [Dec]
        makeField _ _ (name,_,_) | '_' /= head (nameBase name) = fail $ show name ++ " did not start with underscore"
        makeField name vars (fName, _, fType) = do
                (decs1,(typeName,dataName)) <- makeName' (tail $ nameBase fName)
                methodOutput <- lookupTypeName "Object.Types.MethodOutput" *> fromMaybe (error "no MethodOutput in scope")
                let objType = foldl AppT (ConT name) (VarT<*vars)

                let methodOutInst = TySynInstD methodOutput $ TySynEqn [objType, ConT typeName] fType
                actionInst <- [d|
                        instance Action $(return objType) $(return $ ConT typeName) where
                                object . _ = $(return $ VarE fName) object
                        |]

                matchType <- [t| $(return $ ConT typeName) := $(return $ VarT $ mkName "value") |]
                let methodSetOutInst = TySynInstD methodOutput $ TySynEqn [objType, matchType] objType
                actionSetInst <- [d|
                        instance (value ~ $(return fType)) => Action $(return objType) $(return matchType) where
                                object . ( _ := v) = $(recUpdE [e|object|] [return (fName, VarE $ mkName "v")])
                        |]

                return $ [methodOutInst,methodSetOutInst] ++ actionInst ++ actionSetInst ++ decs1

