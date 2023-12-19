## Bringing tests over for a new version of GHC

### New streamlined approach

#### Copy the changed files over

Change to the current GHC git repository (for the new version of GHC)

    $ mkdir /tmp/ghc-tests
    $ export DESTINATION=/tmp/ghc-tests
    $ export PREVIOUS=ghc-9.6

Sanity check

    git diff --name-only $PREVIOUS | grep testsuite/tests | grep "\.hs"

Do it

    cp -pv --parents `git diff --name-only $PREVIOUS | grep testsuite/tests | grep "\.hs"` $DESTINATION

### Clean up

Edit the directories in /tmp/ghc-tests to remove the parse fail etc
tests. Generally remove any `should_fail` directory.

    find /tmp/ghc-tests/ -iname "should_fail" | xargs rm -fr

In the ghc-exactprint directory

    $ mkdir tests/examples/ghc98-copied
    $ find /tmp/ghc-tests -iname "*.hs"  | xargs cp  --backup=numbered -t ./tests/examples/ghc98-copied/

Note: there is a pathological file `parsing001.hs`, which should be deleted

### turn the name-clash backups into normal files

`cp` will create backups of the form `A.hs.~1~`. We rename them to have a `.hs`
extension.

You may need to do `apt-get install mmv` first.
See http://manpages.ubuntu.com/manpages/zesty/en/man1/mmv.1.html

    $ cd tests/examples/ghc98-copied
    $ mmv "*.hs.~*~" "#1.#2.hs"

### cleanup whitespace in the files

    $ cd tests/examples/ghc96-copied
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
    GHC96  -> ["ghc96-copied"]
    -- GHC84  -> ["ghc84"
```

Also comment out the running of the `roundTripBalanceCommentsTests` and
`roundTripMakeDeltaTests` in `mkTests`, otherwise the test *.out files
will not be useful.

Run the tests, and move the failing test files over to the
`tests/examples/ghc96` directory
