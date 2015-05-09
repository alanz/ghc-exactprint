{-# LANGUAGE UnicodeSyntax #-}

module Unicode where

-- import Control.Monad.Trans.State.Strict

-- -- | We'll start off with a monad in which to manipulate ABTs; we'll need some
-- -- state for fresh variable generation.
-- --
-- newtype M α
--   = M
--   { _M ∷ State Int α
--   }

-- -- | We'll run an ABT computation by starting the variable counter at @0@.
-- --
-- runM ∷ M α → α
-- runM (M m) = evalState m 0


-- | To indicate that a term is in normal form.
--
stepsExhausted
  ∷ Applicative m
  ⇒ StepT m α
stepsExhausted = StepT . MaybeT $ pure Nothing

stepsExhausted2
  ∷ Applicative m
  => m α
stepsExhausted2 = undefined
