ghc-exactprint
==============

[![Available on Hackage][badge-hackage]][hackage]
[![License BSD3][badge-license]][license]
[![Build Status][badge-travis]][travis]

![Github Build Status](https://github.com/alanz/ghc-exactprint/actions/workflows/haskell-ci.yml/badge.svg)


[badge-travis]: https://travis-ci.org/alanz/ghc-exactprint.png?branch=master
[travis]: https://travis-ci.org/alanz/ghc-exactprint
[badge-hackage]: https://img.shields.io/hackage/v/ghc-exactprint.svg?dummy
[hackage]: https://hackage.haskell.org/package/ghc-exactprint
[badge-license]: https://img.shields.io/badge/license-BSD3-green.svg?dummy
[license]: https://github.com/alanz/ghc-exactprint/blob/master/LICENSE

GHC version of haskell-src-exts exactPrint

master has been updated to use the new [GHC Exact Print Annotations](https://gitlab.haskell.org/ghc/ghc/-/wikis/api-annotations) which landed in GHC 9.2

So ghc-exactprint-1.1 and later supports GHC 9.2 and onwards
ghc-exactprint-0.6.4 supports GHC from 7.10 to 9.0

Links
-----

[HIW 2021 talk on GHC Exactprint for GHC 9.2](https://alanz.github.io/hiw2021/)

[HIW 2020 talk on plans for GHC Exactprint for GHC 9.2](https://alanz.github.io/hiw2020/)

[Blog Post](https://blog.shaynefletcher.org/2021/05/annotations-in-ghc.html) by @shayne-fletcher on experiences converting to use the new annotations.

Current Limitations
-------------------

* Does not process CPP properly
* Does not process Lhs files properly
