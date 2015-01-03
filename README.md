ghc-exactprint
==============

GHC version of haskell-src-exts exactPrint

Note: This only works with GHC ghc-7.10 RC1 (with https://phabricator.haskell.org/D538 merged)

It also requires package ghc-syb to be installed from
https://github.com/alanz/ghc-syb until
https://github.com/nominolo/ghc-syb/pull/8 is merged

Note too that this is a work in progress.

Development proceeds by loading tests/Test.hs into ghci, and running
it.

The output is in tests/examples, where the output file for each test
has a "exactprinter.out" suffix.

If the file is reproduced exactly it will have the word "Match" only,
otherwise it will have the ghc-exactprint output, followed by a dump
of the ParsedSource.

The original GHC Api Annotations are dumped to stdout in ghci at the
start of the run.

Current Limitations
-------------------

* Does not process CPP properly
* Does not process Lhs files properly

