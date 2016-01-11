#!/bin/sh

# For GHC-8/master
cabal install --allow-newer turtle Diff
cabal configure -froundtrip --enable-tests --allow-newer
# cabal configure -froundtrip --enable-tests --allow-newer --enable-profiling
