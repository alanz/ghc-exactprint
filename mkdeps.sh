#!/bin/sh

# https://github.com/ekmett/bifunctors.git master
(cd ../../ekmett/bifunctors && cabal clean && cabal install --allow-newer)

# https://github.com/ekmett/free.git master
(cd ../../ekmett/free       && cabal clean && cabal install --allow-newer)

# https://github.com/alanz/HUnit.git ghc-head
(cd ../../alanz/HUnit       && cabal clean && cabal install --allow-newer)

cabal install  --allow-newer -f-semigroups contravariant
# cabal clean && cabal install   --dependencies-only --allow-newer
cabal clean && cabal install   --enable-tests --dependencies-only --allow-newer
# cabal clean && cabal configure --enable-tests --allow-newer

