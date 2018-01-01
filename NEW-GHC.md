## Bringing tests over for a new version of GHC

### Copy the raw files over

    $ mkdir tests/examples/ghc84-copied
    $ find ~/mysrc/git.haskell.org/ghc/testsuite/tests/ -iname "*.hs"  | xargs cp  --backup=numbered -t ./tests/examples/ghc84-copied/

### turn the name-clash backups into normal files

`cp` will create backups of the form `A.hs.~1~`. We rename them to have a `.hs`
extension.

You may need to do `apt-get install mmv` first.
See http://manpages.ubuntu.com/manpages/zesty/en/man1/mmv.1.html

    $ cd tests/examples/ghc84-copied
    $ mmv "*.hs.~*~" "#1.#2.hs"

### cleanup whitespace in the files

    $ cd tests/examples/ghc84-copied
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
    GHC84  -> ["ghc84-copied"]
    -- GHC84  -> ["ghc84"
```

Run the tests, and move the failing test files over to the
`tests/examples/ghc84` directory
