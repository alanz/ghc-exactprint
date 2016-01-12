### Verifying ghc-exactprint against all of hackage

There is infrastructure in this project to perform the identity transformation
on the most recent version of all packages in hackage.

### Preparations

```
cabal configure -froundtrip --enable-tests
cabal build
```

This will generate three additional executables

1. ./dist/build/prepare-hackage/prepare-hackage

  This will manage the `cabal` programme to call `cabal get` for each package on
  hackage, into `./hackage-roundtrip-work`. It also untabifies each haskell file,
  and deletes trailing whitespace.

  If there is a package it cannot process, add it to
  `./roundtrip-config/badpackages.txt`

  At the moment there is only one of these, `MagicHaskeller` which has a file
  which it iso8859-1 encoded, having the euro symbol and various others like that
  in it in the comments. The whitespace cleanup cannot write the updated file, it
  gets encoding issues.

2. ./dist/build/roundtrip/roundtrip

  This actually runs the roundtrip test

  Invoke it as one of

  ```
  ./dist/build/roundtrip/roundtrip ./hackage-roundtrip-work/* +RTS -N2
  ./dist/build/roundtrip/roundtrip clean
  ./dist/build/roundtrip/roundtrip failures
  ```

  It expects its arguments to be a list of unpacked hackage package directories.

  The `-N2` option uses one core for processing and one for the GC, otherwise the
  memory usage can get quite high.

  This program uses the following configuration files

  `./roundtrip-config/blacklist.txt` is a list of files known to cause a segfault,
  which are normally ones generated, particularly by `alex` and `happy`.

  `,.roundtrip-config/knownfailures.txt` is a list of files that fail to
  roundtrip, but cannot be fixed. Examples of these are ones that use CPP to
  `#define` a constant which is used in the code. Because of the way
  `ghc-exactprint` manages CPP, this results in both the defined name and its
  value to appear in the output.

  It generates the following state files and output

  `./roundtrip-work/cpp.txt` - Keeps track of files that fail to parse because of
  having CPP in them.

  `./roundtrip-work/pfail.txt` - Keeps track of files that fail to parse for any other reason.

  `./roundtrip-work/failed.txt` - A list of files that parsed properly but could
  not be reproduced. It is used to re-run failures once the code is updated to fix
  the problem.

  `./roundtrip-work/processed.txt` - Keeps track of files that have already been
  processed, whether they pass or fail.

  `./roundtrip-work/roundtrip.log` - A log of files as they are processed. This is
  written and flushed immediately prior to attempting to process the file.

  `./roundtrip-work/failures` - A directory containing an entry for each processed
  file which failed, giving the ghc-exactprint output and annotated parse tree.

3. ./dist/build/static/static

  Executable that can build a static web site to view the failures.
