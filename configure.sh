#!/bin/sh

# For GHC-8/master
# cabal install --allow-newer turtle Diff
# cabal configure -froundtrip --enable-tests --allow-newer
# cabal configure -froundtrip --enable-tests --allow-newer --enable-profiling

cabal new-configure --enable-tests --with-compiler=/opt/ghc/8.5.20180614/bin/ghc --allow-newer
