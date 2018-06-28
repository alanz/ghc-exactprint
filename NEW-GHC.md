## Bringing tests over for a new version of GHC

### Copy the raw files over

    $ mkdir /tmp/ghc-tests
    $ cp -r ~/mysrc/git.haskell.org/ghc/testsuite/tests/ /tmp/ghc-tests/

Edit the directories in /tmp/ghc-tests to remove the parse fail etc
tests. Generally remove any `should_fail` directory.

    $ mkdir tests/examples/ghc86-copied
    $ find /tmp/ghc-tests -iname "*.hs"  | xargs cp  --backup=numbered -t ./tests/examples/ghc86-copied/

Note: there is a pathological file `parsing001.hs`, which should be deleted

### turn the name-clash backups into normal files

`cp` will create backups of the form `A.hs.~1~`. We rename them to have a `.hs`
extension.

You may need to do `apt-get install mmv` first.
See http://manpages.ubuntu.com/manpages/zesty/en/man1/mmv.1.html

    $ cd tests/examples/ghc86-copied
    $ mmv "*.hs.~*~" "#1.#2.hs"

### cleanup whitespace in the files

    $ cd tests/examples/ghc86-copied
    $ ../../../emacs-ws-cleanup.sh


### capture the failures

Set the tests in `Tests.hs` to test the new files only

```haskell
-- | Directories to automatically find roundtrip tests
testDirs :: [FilePath]
testDirs =
  case ghcVersion of
    GHC710 -> ["ghc710-only","ghc710"]
    GHC80  -> ["ghc710", "ghc80"]
    GHC82  -> ["ghc710", "ghc80", "ghc82"]
    -- GHC84  -> ["ghc710", "ghc80", "ghc82", "ghc84", "ghc84-copied"]
    GHC86  -> ["ghc86-copied"]
    -- GHC84  -> ["ghc84"
```

Also comment out the running of the `noAnnotationTests` and
`prettyRoundTripTests` in `mkTests`, otherwise the test *.out files
will not be useful.

Run the tests, and move the failing test files over to the
`tests/examples/ghc84` directory
