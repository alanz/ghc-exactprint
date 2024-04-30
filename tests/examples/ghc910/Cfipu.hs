{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}

module Main where

instructions =
    [ ('2', \m r -> do
                r m
                ; `catch` get m r)
    ]

