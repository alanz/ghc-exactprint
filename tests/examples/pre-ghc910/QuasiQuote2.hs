{-# LANGUAGE QuasiQuotes #-}
module Test where

a = bar
    where
        bar = [q|
            |]

b = bar
    where
        bar = [q|
           |]

c = bar
    where
        bar = [q|
             |]

d = [q|
            |]
