{-# LANGUAGE OverloadedStrings #-}  --

import Control.Monad
import Data.Char
import Data.List.Extra
import qualified Data.Set as Set
import GHC.IO.Exception
import System.Directory
import System.FilePath.Posix
import System.Process
import Test.CommonUtils

import Test.HUnit

main :: IO ()
main = do
  packages <- allCabalPackages
  -- packages <- allCabalPackagesTest
  myecho ("number of packages:" ++ (show $ length packages))
  packageDirsFull <- drop 2 <$> getDirectoryContents hackageWorkDir
  let cond c = c == '.' || c == '-' || isDigit c
  let packageDirs = map (dropWhileEnd cond ) packageDirsFull
  isBadPackages <- doesFileExist badpackagesFile
  badPackages <- if isBadPackages
                   then lines <$> readFile badpackagesFile
                   else return []
  let alreadyUnpacked = Set.fromList $ packageDirs ++ badPackages
  -- _ <- shell ("mkdir -p " <> (T.pack hackageWorkDir)) empty
  _ <- callCommand ("mkdir -p " <> hackageWorkDir)
  mapM_ (preparePackage alreadyUnpacked) packages

-- ---------------------------------------------------------------------

preparePackage :: Set.Set String -> String -> IO ()
preparePackage alreadyUnpacked package = do
  myecho $ "preparePackage:" <> package
  if Set.member package alreadyUnpacked
     then myecho $ "already unpacked:" <> package
     else preparePackage' package

preparePackage' :: String -> IO ()
preparePackage' package = do
  -- (ec,dir) <- shellStrict ("cabal get --destdir=" <> T.pack hackageWorkDir <> " " <> package) empty
  (ec,dir,_err) <- readCreateProcessWithExitCode (shell ("cabal get --destdir=" <> hackageWorkDir <> " " <> package)) ""
  -- myecho (T.pack $ "cabal get:" ++ show dir)
  myecho (show ec)
  when (ec == ExitSuccess) $ do
    let bits = splitOn " " (head $ lines dir)
    myecho ("cabal get:dir=" ++ show (last bits))
    cleanPackage (last bits)
  return ()

-- ---------------------------------------------------------------------

-- |Clean up whitespace in a package
cleanPackage :: String -> IO ()
cleanPackage dir = do
  myecho ("cleaning:" <> dir)
  fs <- findSrcFiles dir
  let
    doOne :: FilePath -> IO ()
    doOne fn = do
      myecho ("doOne:" <> fn)
      let tmpFn = fn <.> "clean"
      clean <- cleanupWhiteSpace fn
      writeFile tmpFn clean
      removeFile fn
      renameFile tmpFn fn
      return ()
  mapM_ doOne fs
  myecho ("cleaned up:" <> dir)

-- ---------------------------------------------------------------------
-- | The computation 'writeFile' @file str@ function writes the string @str@,
-- to the file @file@.
-- writeFileUtf8 :: FilePath -> String -> IO ()
-- writeFileUtf8 ff txt = withFile ff WriteMode (\ hdl -> hSetEncoding hdl utf8 >> GHC.hPutStr hdl txt)

-- ---------------------------------------------------------------------

-- allCabalPackagesTest :: IO [Text]
-- allCabalPackagesTest
--   = return ["3d-graphics-examples","3dmodels","4Blocks","AAI","ABList"]
--   -- = return ["airship"]


allCabalPackages :: IO [String]
allCabalPackages = do
  -- let cmd = "cabal list --simple-output | awk '{ print $1 }' | uniq"
  let cmd = "cabal list --simple-output | awk '{ print $1 }' | sort | uniq"
  -- (_ec,r) <- shellStrict cmd empty
  (_ec,r,_err) <- readCreateProcessWithExitCode (shell cmd) ""
  let packages = lines r
  myecho (show $ take 5 packages)
  return packages

-- ---------------------------------------------------------------------

-- |strip trailing whitespace, and turn tabs into spaces
cleanupWhiteSpace :: FilePath -> IO String
cleanupWhiteSpace file = do
  contents <- readFileGhc file
  let cleaned = map cleanupOneLine (lines $ contents)
  return (unlines cleaned)

tabWidth :: Int
tabWidth = 8

-- nonBreakingSpace :: Char
-- nonBreakingSpace = '\xa0'

cleanupOneLine :: String -> String
cleanupOneLine str = str'
  where
    numSpacesForTab n = tabWidth - (n `mod` tabWidth)
    -- loop over the line, keeping current pos. Where a tab is found, insert
    -- spaces until the next tab stop. Discard any trailing whitespace.
    go col res cur =
      case cur of
        [] -> res
        ('\t':cur') -> go (col + toAdd) ((replicate toAdd ' ') ++ res) cur'
                where
                  toAdd = numSpacesForTab col
        ('\xa0':cur') -> go (col + 1) (' ':res) cur'

        -- convert ISO 8859-16 euro symbol to the UTF8 equivalent
        -- ('\xa4':cur') -> go (col + 1) ('\x20ac':res) cur'
        (c:cur') ->go (col + 1) (c:res) cur'
    str1 = go 0 [] str
    str' = reverse $ dropWhile isSpace str1

-- ---------------------------------------------------------------------

tt :: IO Counts
tt = runTestTT $ TestList
                  [ testCleanupOneLine
                  , testTabs
                  ]

testCleanupOneLine :: Test
testCleanupOneLine = do
  let
    makeCase n = (show n
                 ,(replicate n ' ') <> "\t|" <> replicate n ' ' <> "\t"
                 ,(replicate 8 ' ' <> "|"))
    mkTest n = TestCase $ assertEqual name outp (cleanupOneLine inp)
      where (name,inp,outp) = makeCase n
  testList "cleanupOneLine" $ map mkTest [1..7]

testTabs :: Test
testTabs = TestCase $ assertEqual "testTabs" t2tabsExpected (cleanupOneLine t2tabs)
  where
    t2tabs = "import Data.Foldable\t\t    ( foldMap )"
    t2tabsExpected ="import Data.Foldable                ( foldMap )"

testList :: String -> [Test] -> Test
testList str ts = TestLabel str (TestList ts)

-- ---------------------------------------------------------------------

myecho :: String -> IO ()
myecho t = putStrLn t

-- ---------------------------------------------------------------------

pwd :: IO FilePath
pwd = getCurrentDirectory

mcd :: FilePath -> IO ()
mcd = setCurrentDirectory
