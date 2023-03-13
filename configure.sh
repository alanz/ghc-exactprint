#!/bin/sh

rm -fr dist*
# rm .ghc.env*

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
# cabal new-configure -froundtrip -fdev --enable-tests --with-compiler=/opt/ghc/8.9.0.20191112/bin/ghc --allow-newer
# cabal new-configure -froundtrip -fdev --enable-tests --with-compiler=/opt/ghc/8.9.0.20191113/bin/ghc --allow-newer
# cabal new-configure -froundtrip -fdev --enable-tests --with-compiler=/opt/ghc/8.10.0.20191122/bin/ghc --allow-newer
# cabal new-configure -froundtrip -fdev --enable-tests --with-compiler=/opt/ghc/8.10.0.20191122/bin/ghc

# cabal new-configure -froundtrip -fdev --enable-tests --with-compiler=/opt/ghc/8.10.1/bin/ghc --allow-newer
# cabal new-configure -froundtrip -fdev --enable-tests --with-compiler=/opt/ghc/8.10.1/bin/ghc
# cabal new-configure -froundtrip -fdev --enable-tests --with-compiler=ghc-8.8.4
# cabal new-configure -froundtrip -fdev --enable-tests --with-compiler=ghc-8.10.1
# cabal new-configure -froundtrip -fdev --enable-tests --with-compiler=ghc-8.10.2


#--------------------------------------------------------------------
# cabal new-configure -froundtrip -fdev --enable-tests --with-compiler=/opt/ghc/9.0.0.20200728/bin/ghc --allow-newer
# cabal new-configure -fdev --with-compiler=/opt/ghc/9.0.0.20200808/bin/ghc --allow-newer
# cabal new-configure -fdev --enable-tests --with-compiler=/opt/ghc/9.0.0.20200808/bin/ghc --allow-newer
# cabal new-configure -fdev -froundtrip --enable-tests --with-compiler=/opt/ghc/9.0.0.20200808/bin/ghc --allow-newer

# cabal new-configure -fdev -froundtrip --enable-tests --with-compiler=/opt/ghc/9.0.0.20200920/bin/ghc --allow-newer
# cabal new-configure -fdev -froundtrip --enable-tests --with-compiler=ghc-9.0.1-alpha1
# cabal new-configure -fdev -froundtrip --enable-tests --with-compiler=/home/alanz/.ghcup/ghc/9.0.0.20200925/bin/ghc --allow-newer
# cabal new-configure -fdev -froundtrip --enable-tests --with-compiler=ghc-9.0 --allow-newer

# cabal new-configure -fdev -froundtrip --enable-tests --with-compiler=/opt/ghc/9.0.0.20200929/bin/ghc --allow-newer
# cabal new-configure -fdev -froundtrip --enable-tests --with-compiler=/opt/ghc/9.0.0.20201001/bin/ghc --allow-newer
# cabal new-configure -fdev -froundtrip --enable-tests --with-compiler=/opt/ghc/9.0.0.20201004/bin/ghc --allow-newer
# cabal new-configure -fdev -froundtrip --enable-tests --with-compiler=/opt/ghc/9.0.0.20201006/bin/ghc --allow-newer

# cabal new-configure -fdev -froundtrip --enable-tests --with-compiler=/opt/ghc/9.0.0.20201024/bin/ghc --allow-newer --constraint=Cabal==3.2.0.0

# cabal new-configure -fdev -froundtrip --enable-tests --with-compiler=/opt/ghc/9.0.0.20201110/bin/ghc --allow-newer --constraint=Cabal==3.2.0.0
# cabal configure -fdev -froundtrip --enable-tests --with-compiler=ghc-9.0.0.20201227 --allow-newer --constraint=Cabal==3.2.0.0
# cabal configure -fdev -froundtrip --enable-tests --with-compiler=ghc-9.0.0.20201227 --allow-newer --constraint=Cabal==3.2.0.0
# cabal configure -fdev -froundtrip --enable-tests --with-compiler=ghc-9.0.0.20201227 --allow-newer --constraint=Cabal==3.4.0.0
# cabal configure -fdev -froundtrip --enable-tests --with-compiler=ghc-9.0.0.20201227 --allow-newer

#--------------------------------------------------------------------
#- GHC 9.0.1 released
# cabal configure -fdev -froundtrip --enable-tests --with-compiler=ghc-9.0.1 --allow-newer
# cabal configure -fdev -froundtrip --enable-tests --with-compiler=ghc-9.0.1

#--------------------------------------------------------------------
#- GHC 9.2. alpha

# cabal configure -fdev -froundtrip --enable-tests --with-compiler=ghc-9.2 --allow-newer
# cabal configure  --with-compiler=ghc-9.2 --allow-newer
# cabal configure -fdev --enable-tests --with-compiler=ghc-9.2 --allow-newer

# cabal configure -fdev --enable-tests --with-compiler=/opt/ghc/9.2.0.20210622/bin/ghc --allow-newer
# cabal configure -fdev -froundtrip --enable-tests --with-compiler=/opt/ghc/9.2.0.20210804/bin/ghc --allow-newer
# cabal configure -fdev -froundtrip --enable-tests --with-compiler=/opt/ghc/9.2.0.20210806/bin/ghc --allow-newer
# cabal configure -fdev -froundtrip --enable-tests --with-compiler=/opt/ghc/9.2.0.20210818/bin/ghc --allow-newer


# cabal configure -fdev -froundtrip --enable-tests --with-compiler=ghc-9.2 --allow-newer


# cabal configure -fdev -froundtrip --enable-tests --with-compiler=/opt/ghc/9.2.0.20211023/bin/ghc --allow-newer
# cabal configure -fdev -froundtrip --enable-tests --with-compiler=ghc-9.2.1 --allow-newer
# cabal configure -fdev -froundtrip --enable-tests --with-compiler=ghc-9.2.1
# cabal configure -fdev -froundtrip --enable-tests --with-compiler=ghc-9.2.2
# cabal configure -fdev --enable-tests --with-compiler=ghc-9.2.4


#--------------------------------------------------------------------
#- GHC 9.4 HEAD
# cabal configure -fdev -froundtrip --enable-tests --with-compiler=/opt/ghc/9.3.0/bin/ghc --allow-newer
# cabal configure -fdev --enable-tests --with-compiler=/opt/ghc/9.3.0/bin/ghc --allow-newer
# cabal configure -fdev --with-compiler=/opt/ghc/9.3.0/bin/ghc --allow-newer
# cabal configure -fdev --enable-tests --with-compiler=/opt/ghc/9.3.20220407/bin/ghc --allow-newer

# cabal configure -fdev --enable-tests --with-compiler=/opt/ghc/9.4.0.20220523/bin/ghc --allow-newer


# cabal configure -fdev --enable-tests --with-compiler=ghc-9.4.2
# cabal configure -fdev --enable-tests --with-compiler=ghc-9.4.3
# cabal configure -fdev --enable-tests --with-compiler=ghc-9.4.4

# cabal configure -fdev --enable-tests --with-compiler=/opt/ghc/9.5.0.20221014/bin/ghc --allow-newer
# cabal configure -fdev --enable-tests --with-compiler=/opt/ghc/9.5.20221014/bin/ghc
#--------------------------------------------------------------------
#- GHC 9.6 HEAD
# cabal configure -fdev --enable-tests --with-compiler=/opt/ghc/9.6.0.20230111/bin/ghc --allow-newer
# cabal configure -fdev                --with-compiler=/opt/ghc/9.6.0.20230111/bin/ghc --allow-newer


# cabal configure -fdev -froundtrip --enable-tests --with-compiler=/opt/ghc/9.6.0.20230111/bin/ghc --allow-newer
# cabal configure -fdev -froundtrip --enable-tests --with-compiler=/opt/ghc/9.6.0.20230128/bin/ghc --allow-newer
# cabal configure -fdev -froundtrip --enable-tests --with-compiler=/opt/ghc/9.6.0.20230201/bin/ghc --allow-newer
# cabal configure -fdev -froundtrip --enable-tests --with-compiler=/opt/ghc/9.6.0.20230209/bin/ghc --allow-newer

# cabal configure -fdev -froundtrip --enable-tests --with-compiler=/opt/ghc/9.6.0.20230214/bin/ghc --allow-newer

# cabal configure -fdev -froundtrip --enable-tests --with-compiler=/opt/ghc/9.6.1/bin/ghc --allow-newer
# cabal configure  --with-compiler=/opt/ghc/9.6.1/bin/ghc
cabal configure -fdev  --enable-tests --with-compiler=/opt/ghc/9.6.1/bin/ghc
