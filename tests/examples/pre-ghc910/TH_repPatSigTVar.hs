{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Main
where

import Language.Haskell.TH

$([d| f = \(_ :: Either a b) -> $(sigE (varE 'undefined) (varT ''c)) |])

main :: IO ()
main = return ()

