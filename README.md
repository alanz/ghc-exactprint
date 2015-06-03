ghc-exactprint
==============

[![Available on Hackage][badge-hackage]][hackage]
[![License BSD3][badge-license]][license]
[![Build Status][badge-travis]][travis]

[badge-travis]: https://travis-ci.org/alanz/ghc-exactprint.png?branch=master
[travis]: https://travis-ci.org/alanz/ghc-exactprint
[badge-hackage]: https://img.shields.io/hackage/v/ghc-exactprint.svg?dummy
[hackage]: https://hackage.haskell.org/package/ghc-exactprint
[badge-license]: https://img.shields.io/badge/license-BSD3-green.svg?dummy
[license]: https://github.com/alanz/ghc-exactprint/blob/master/LICENSE

GHC version of haskell-src-exts exactPrint

Note: This only works with GHC 7.10.2 and beyond.
      As of now (2015-06-03) GHC 7.10.2 is not yet released


Current Limitations
-------------------

* Does not process CPP properly [should be sorted soon]
* Does not process Lhs files properly
* Does not properly process multi-line strings in WARNING and DEPRECATED pragmas
  [ see https://phabricator.haskell.org/D907 which missed the cut for 7.10.2 due to AST change  ]
* Does not preserve the unicode * character, reducing it to a standard one.

