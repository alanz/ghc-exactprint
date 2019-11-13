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
# cabal new-configure -fdev --enable-tests --with-compiler=/opt/ghc/8.7.20190124/bin/ghc --allow-newer
# cabal new-configure -fdev --enable-tests --with-compiler=/opt/ghc/8.7.20190204/bin/ghc --allow-newer

# cabal new-configure -froundtrip -fdev --enable-tests --with-compiler=/opt/ghc/8.7.20190124/bin/ghc --allow-newer
# cabal new-configure -froundtrip -fdev --enable-tests --with-compiler=/opt/ghc/8.7.20190204/bin/ghc --allow-newer
# cabal new-configure -froundtrip -fdev --enable-tests --with-compiler=/opt/ghc/8.7.20190207/bin/ghc --allow-newer
#cabal new-configure -froundtrip -fdev --enable-tests --with-compiler=/opt/ghc/8.7.20190208/bin/ghc --allow-newer
#cabal new-configure -froundtrip -fdev --enable-tests --with-compiler=/opt/ghc/8.7.20190216/bin/ghc --allow-newer
# cabal new-configure -froundtrip -fdev --enable-tests --with-compiler=/opt/ghc/8.7.20190226/bin/ghc --allow-newer

# cabal new-configure -froundtrip -fdev --enable-tests --with-compiler=/opt/ghc/8.8.20190402/bin/ghc --allow-newer
# cabal new-configure -froundtrip -fdev --enable-tests --with-compiler=/opt/ghc/8.8.20190405/bin/ghc --allow-newer --constraint=exceptions==0.10.0

# cabal new-configure -froundtrip -fdev --enable-tests --with-compiler=/opt/ghc/8.8.20190419/bin/ghc --allow-newer --constraint=exceptions==0.10.0
# cabal new-configure -froundtrip -fdev --enable-tests --with-compiler=/opt/ghc/8.8.20190419/bin/ghc
# cabal new-configure -froundtrip -fdev --enable-tests --with-compiler=/opt/ghc/8.8.0.20190424/bin/ghc --allow-newer

# ---------- GHC 8.10

# cabal new-configure -froundtrip -fdev --enable-tests --with-compiler=/opt/ghc/8.9.0.20190920/bin/ghc --allow-newer
# cabal new-configure --with-compiler=/opt/ghc/8.9.0.20190920/bin/ghc --allow-newer
# cabal new-configure --with-compiler=/opt/ghc/8.9.0.20190923/bin/ghc --allow-newer
# cabal new-configure -froundtrip -fdev --enable-tests --with-compiler=/opt/ghc/8.9.0.20190924/bin/ghc --allow-newer
# cabal new-configure -froundtrip -fdev --enable-tests --with-compiler=/opt/ghc/8.9.0.20191001/bin/ghc --allow-newer
# cabal new-configure -froundtrip -fdev --enable-tests --with-compiler=/opt/ghc/8.9.0.20191016/bin/ghc --allow-newer
# cabal new-configure -froundtrip -fdev --enable-tests --with-compiler=/opt/ghc/8.9.0.20191023/bin/ghc --allow-newer
# cabal new-configure -froundtrip -fdev --enable-tests --with-compiler=/opt/ghc/8.9.0.20191030/bin/ghc --allow-newer
# cabal new-configure -froundtrip -fdev --enable-tests --with-compiler=/opt/ghc/8.9.0.20191102/bin/ghc --allow-newer
# cabal new-configure -froundtrip -fdev --enable-tests --with-compiler=/opt/ghc/8.9.0.20191103/bin/ghc --allow-newer
cabal new-configure -froundtrip -fdev --enable-tests --with-compiler=/opt/ghc/8.9.0.20191112/bin/ghc --allow-newer
# cabal new-configure -froundtrip -fdev --enable-tests --with-compiler=/opt/ghc/8.9.0.20191113/bin/ghc --allow-newer
