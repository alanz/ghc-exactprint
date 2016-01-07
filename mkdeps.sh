#!/bin/sh

# https://github.com/alanz/bifunctors.git master
(cd ../../alanz/bifunctors && cabal clean && cabal install --allow-newer)

# https://github.com/alanz/free.git master
(cd ../../alanz/free       && cabal clean && cabal install --allow-newer)

# https://github.com/alanz/HUnit.git ghc-head
(cd ../../alanz/HUnit       && cabal clean && cabal install --allow-newer)

#cabal install  --allow-newer -f-semigroups contravariant
cabal clean && cabal install   --enable-tests --dependencies-only --allow-newer

