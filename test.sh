#!/bin/sh

# ghc-config 7.11.20150621
ghc-config 7.11.20150709
stack --skip-ghc-check --stack-yaml stack-7.11.yaml test

# ghc-config 7.10.1.20150619
ghc-config 7.10.1.20150630
stack test
