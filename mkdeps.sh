#!/bin/sh

# export PROFILING=--enable-profiling
export PROFILING=

# https://github.com/alanz/bifunctors.git master
(cd ../../alanz/bifunctors && cabal clean && cabal install --allow-newer $PROFILING)

# https://github.com/alanz/free.git master
(cd ../../alanz/free       && cabal clean && cabal install --allow-newer $PROFILING)

# # https://github.com/alanz/HUnit.git ghc-head
# (cd ../../alanz/HUnit       && cabal clean && cabal install --allow-newer $PROFILING)

# https://github.com/hvr/HUnit.git ghc8
(cd ../../hvr/HUnit       && cabal clean && cabal install --allow-newer $PROFILING)
``
cabal install --allow-newer cpphs # otherwise cpp will fail
#cabal install  --allow-newer -f-semigroups contravariant
cabal clean && cabal install   --enable-tests --dependencies-only --allow-newer $PROFILING

