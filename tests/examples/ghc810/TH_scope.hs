-- Test for #2188
{-# LANGUAGE TemplateHaskellQuotes #-}

module TH_scope where

f g = [d| f :: Int
          f = g
          g :: Int
          g = 4 |]

