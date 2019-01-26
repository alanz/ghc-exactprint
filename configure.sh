#!/bin/sh

rm -fr dist*
rm .ghc.env*

# For GHC-8/master
# cabal install --allow-newer turtle Diff
# cabal configure -froundtrip --enable-tests --allow-newer
# cabal configure -froundtrip --enable-tests --allow-newer --enable-profiling

# cabal new-configure -fdev --enable-tests --with-compiler=ghc-8.4.3
# cabal new-configure -fdev --enable-tests --with-compiler=/opt/ghc/8.5.20180614/bin/ghc --allow-newer
# cabal new-configure -fdev --enable-tests --with-compiler=/opt/ghc/8.5.20180617/bin/ghc --allow-newer
# cabal new-configure -fdev --enable-tests --with-compiler=/opt/ghc/8.5.20180619/bin/ghc --allow-newer
# cabal new-configure -fdev --enable-tests --with-compiler=/opt/ghc/8.6.0.20180620/bin/ghc --allow-newer
# cabal new-configure -fdev --enable-tests --with-compiler=/opt/ghc/8.7.20180622/bin/ghc --allow-newer
# cabal new-configure -fdev --enable-tests --with-compiler=/opt/ghc/8.7.20180625/bin/ghc --allow-newer
# cabal new-configure -fdev --enable-tests --with-compiler=/opt/ghc/8.6.0.20180627/bin/ghc --allow-newer

# cabal new-configure -froundtrip -fdev --enable-tests --with-compiler=/opt/ghc/8.6.0.20180627/bin/ghc --allow-newer
# cabal new-configure -froundtrip -fdev --enable-tests --with-compiler=/opt/ghc/8.6.0.20180712/bin/ghc --allow-newer
# cabal new-configure -froundtrip -fdev --enable-tests --with-compiler=/opt/ghc/8.6.0.20180714/bin/ghc --allow-newer
# cabal new-configure -froundtrip -fdev --enable-tests --with-compiler=/opt/ghc/8.6.0.20180810/bin/ghc --allow-newer
# cabal-2.4 new-configure -froundtrip -fdev --enable-tests --with-compiler=/opt/ghc/8.6.0.20180810/bin/ghc --allow-newer

# cabal new-configure -froundtrip --enable-tests --with-compiler=/opt/ghc/8.5.20180617/bin/ghc --allow-newer

# cabal new-configure -froundtrip --enable-tests --with-compiler=/opt/ghc/8.6.1/bin/ghc
# cabal new-configure --with-compiler=ghc-8.6.1 --allow-newer
# cabal new-configure --with-compiler=ghc-8.6.1

# cabal new-configure -froundtrip --enable-tests --with-compiler=ghc-7.10.3
# cabal new-configure -froundtrip --enable-tests --with-compiler=ghc-8.0.1
# cabal new-configure -froundtrip --enable-tests --with-compiler=ghc-8.0.2
# cabal new-configure -froundtrip --enable-tests --with-compiler=ghc-8.2.2
# cabal new-configure -froundtrip --enable-tests --with-compiler=ghc-8.4.3
# cabal new-configure -froundtrip --enable-tests --with-compiler=ghc-8.6.1
# cabal new-configure -froundtrip --enable-tests --with-compiler=ghc-8.6.3

# cabal new-configure -froundtrip --enable-tests --with-compiler=/opt/ghc/8.7.20190116/bin/ghc --allow-newer
# cabal new-configure  --with-compiler=/opt/ghc/8.7.20190118/bin/ghc --allow-newer
# cabal new-configure --enable-tests --with-compiler=/opt/ghc/8.7.20190118/bin/ghc --allow-newer
# cabal new-configure -fdev --enable-tests --with-compiler=/opt/ghc/8.7.20190118/bin/ghc --allow-newer

# cabal new-configure -fdev --enable-tests --with-compiler=/opt/ghc/8.7.20190122/bin/ghc --allow-newer
cabal new-configure -fdev --enable-tests --with-compiler=/opt/ghc/8.7.20190124/bin/ghc --allow-newer
