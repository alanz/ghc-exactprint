{-# LANGUAGE GADTs #-}
module PragmaSpans where

-- The following pragma gets the wrong previous span.
-- See https://gitlab.haskell.org/ghc/ghc/-/issues/20720
{-# LANGUAGE TypeFamilies #-}
