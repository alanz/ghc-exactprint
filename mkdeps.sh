#!/bin/sh

(cd ../../ekmett/bifunctors && cabal clean && cabal install --allow-newer)
# (cd ../../ekmett/comonad    && cabal clean && cabal install --allow-newer)
(cd ../../ekmett/free       && cabal clean && cabal install --allow-newer)
(cd ../../alanz/HUnit       && cabal clean && cabal install --allow-newer)

cabal install  --allow-newer -f-semigroups contravariant
# cabal clean && cabal install   --dependencies-only --allow-newer
cabal clean && cabal install   --enable-tests --dependencies-only --allow-newer
# cabal clean && cabal configure --enable-tests --allow-newer

