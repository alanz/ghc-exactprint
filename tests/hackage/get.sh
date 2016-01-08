#!/bin/sh

cabal list --simple-output | awk '{ print $1 }' | uniq

then cabal unpack each one

caching can happen in ~/.cabal
