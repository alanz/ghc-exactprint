#!/bin/sh

#(cd ../ghc-syb/utils                && cabal clean && cabal install --allow-newer)
#(cd ../hslogger                     && cabal clean && cabal install --allow-newer)
#(cd ~/tmp/hackage/djinn-ghc-0.0.2.2 && cabal clean && cabal install --allow-newer)
#(cd ../../DanielG/ghc-mod           && cabal clean && cabal install --allow-newer)
#(cd ../ghc-exactprint               && cabal clean && cabal install --allow-newer)
(cd ~/tmp/hackage/comonad-4.2.7.2 && cabal clean && cabal install --allow-newer)
# (cd ~/tmp/hackage/base-orphans-0.4.5 && cabal clean && cabal install --allow-newer)
(cd ~/tmp/hackage/free-4.12.1 && cabal clean && cabal install --allow-newer)

cabal install  --allow-newer -f-semigroups contravariant
cabal install  --allow-newer -f-semigroups comonad
# cabal install  --allow-newer -f-semigroups bifunctors
cabal clean && cabal install   --dependencies-only --allow-newer
# cabal clean && cabal install   --enable-tests --dependencies-only --allow-newer
# cabal clean && cabal configure --enable-tests --allow-newer

