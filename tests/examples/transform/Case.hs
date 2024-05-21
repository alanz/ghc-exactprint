module Case where

-- From https://github.com/alanz/ghc-exactprint/issues/97#issue-743080894

f = \x -> case scrut of
  pat -> body

-- initially the annEntryDelta of pat is relative to the starting
-- position of "case", in this case DP (1, -8).

-- Once it is refactored to

f x = case scrut of
  pat -> body

-- the annEntryDelta of pat must be updated based on the new position
-- of "case". The problem is that there doesn't seem to be an easy and
-- non-hacky way to obtain the new position of "case".

f = \x -> foo
  bar

-- in this case the annEntryDelta of bar does not depend on the
-- position of foo, and is always DP (1, 2).

-- Is it possible to make the annEntryDelta of the first pattern not
-- depend on where the "case" is?
