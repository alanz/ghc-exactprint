module Feldspar.Memoize where

import qualified Prelude

import Feldspar

-- | Accelerate the function @f@ using a lookup table.
-- The table will span all possible input values.
tabulate :: (Bits i, Integral i, Syntax a)
         => (Data i -> a) -> Data i -> a
tabulate f i = tabulateLen (2 ^ bitSize i) f i

-- | Accelerate the function @f@ by creating a lookup table of the results for the
-- @len@ first argument values
--
-- Note. To really get a table the function must be closed after the
-- application to @i@
--
tabulateLen :: (Integral i, Syntax a)
            => Data Length -> (Data i -> a) -> Data i -> a
-- tabulateLen len f i = sugar $ share (parallel len (desugar.f.i2n)) (!i2n i)
tabulateLen len f i = sugar $ share (parallel len (desugar.f.i2n)) (! i2n i)

