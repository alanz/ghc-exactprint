{-# LANGUAGE OverloadedStrings #-}  --

import Data.Char
import Data.Monoid
import System.Directory
import System.FilePath.Posix
import System.IO
import Test.CommonUtils
import Turtle hiding (FilePath,(<.>))
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.IO as T

import qualified GHC.IO.Handle.Text as GHC

import Test.HUnit

main :: IO ()
main = do
  packages <- allCabalPackages
  -- packages <- allCabalPackagesTest
  echo (T.pack $ "number of packages:" ++ (show $ length packages))
  packageDirsFull <- drop 2 <$> getDirectoryContents hackageWorkDir
  let cond c = c == '.' || c == '-' || isDigit c
  let packageDirs = map (T.dropWhileEnd cond . T.pack) packageDirsFull
  isBadPackages <- doesFileExist badpackagesFile
  badPackages <- if isBadPackages
                   then T.lines <$> T.readFile badpackagesFile
                   else return []
  let alreadyUnpacked = Set.fromList $ packageDirs ++ badPackages
  _ <- shell ("mkdir -p " <> (T.pack hackageWorkDir)) empty
  mapM_ (preparePackage alreadyUnpacked) packages

-- ---------------------------------------------------------------------

preparePackage :: Set.Set Text -> Text -> IO ()
preparePackage alreadyUnpacked package = do
  echo $ "preparePackage:" <> package
  if Set.member package alreadyUnpacked
     then echo $ "already unpacked:" <> package
     else preparePackage' package

preparePackage' :: Text -> IO ()
preparePackage' package = do
  (ec,dir) <- shellStrict ("cabal get --destdir=" <> T.pack hackageWorkDir <> " " <> package) empty
  -- echo (T.pack $ "cabal get:" ++ show dir)
  echo (T.pack $ show ec)
  when (ec == ExitSuccess) $ do
    let bits = T.splitOn " " (head $ T.lines dir)
    echo (T.pack $ "cabal get:dir=" ++ show (last bits))
    cleanPackage (last bits)
  return ()

-- ---------------------------------------------------------------------

-- |Clean up whitespace in a package

cleanPackage :: Text -> IO ()
cleanPackage dir = do
  echo ("cleaning:" <> dir)
  fs <- findSrcFiles (T.unpack dir)
  let
    doOne :: FilePath -> IO ()
    doOne fn = do
      echo ("doOne:" <> T.pack fn)
      let tmpFn = fn <.> "clean"
      clean <- cleanupWhiteSpace fn
      writeFile tmpFn clean
      -- writeFileUtf8 tmpFn clean
      -- T.writeFile tmpFn (T.pack clean) -- convert to Text to deal with encoding issues of is8859 euro symbol
      removeFile fn
      renameFile tmpFn fn
      return ()
  mapM_ doOne fs
  echo ("cleaned up:" <> dir)

-- ---------------------------------------------------------------------
-- | The computation 'writeFile' @file str@ function writes the string @str@,
-- to the file @file@.
writeFileUtf8 :: FilePath -> String -> IO ()
-- writeFileUtf8 ff txt = withFile ff WriteMode (\ hdl -> hSetEncoding hdl utf8 >> hPutStr hdl txt)
writeFileUtf8 ff txt = withFile ff WriteMode (\ hdl -> hSetEncoding hdl utf8 >> GHC.hPutStr hdl txt)

-- ---------------------------------------------------------------------

allCabalPackagesTest :: IO [Text]
allCabalPackagesTest
  = return ["3d-graphics-examples","3dmodels","4Blocks","AAI","ABList"]
  -- = return ["airship"]


allCabalPackages :: IO [Text]
allCabalPackages = do
  -- let cmd = "cabal list --simple-output | awk '{ print $1 }' | uniq"
  let cmd = "cabal list --simple-output | awk '{ print $1 }' | sort | uniq"
  (_ec,r) <- shellStrict cmd empty
  let packages = T.lines r
  echo (T.pack $ show $ take 5 packages)
  return packages

-- ---------------------------------------------------------------------

-- |strip trailing whitespace, and turn tabs into spaces
cleanupWhiteSpace :: FilePath -> IO String
cleanupWhiteSpace file = do
  contents <- readFileGhc file
  let cleaned = map cleanupOneLine (lines $ contents)
  return (unlines cleaned)
{-
cleanupWhiteSpace :: FilePath -> IO T.Text
cleanupWhiteSpace file = do
  contents <- readFileGhc file
  let cleaned = map cleanupOneLine (T.lines $ T.pack contents)
  return (T.unlines cleaned)
-}
tabWidth :: Int
tabWidth = 8

nonBreakingSpace :: Char
nonBreakingSpace = '\xa0'

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
{-
cleanupOneLine :: T.Text -> T.Text
cleanupOneLine str = str'
  where
    numSpacesForTab n = tabWidth - (n `mod` tabWidth)
    -- loop over the line, keeping current pos. Where a tab is found, insert
    -- spaces until the next tab stop. Discard any trailing whitespace.
    go col res cur =
      if T.null cur
         then res
         else
           case T.head cur of
             '\t' -> go (col + toAdd) (res <> T.replicate toAdd " ") (T.tail cur)
                where
                  toAdd = numSpacesForTab col
             '\xa0' -> go (col + 1) (T.snoc res ' ') (T.tail cur)
             -- nonBreakingSpace -> go (col + 1) (T.snoc res ' ') (T.tail cur)
             c -> go (col + 1) (T.snoc res c) (T.tail cur)
    str1 = go 0 T.empty str
    -- str2 = T.map (\c -> if c == nonBreakingSpace then ' ' else c) str1
    str' = T.dropWhileEnd isSpace str1
-}

-- ---------------------------------------------------------------------

tt :: IO Counts
tt = runTestTT testCleanupOneLine

testCleanupOneLine :: Test
testCleanupOneLine = do
  let
    makeCase n = (show n
                 ,(replicate n ' ') <> "\t|" <> replicate n ' ' <> "\t"
                 ,(replicate 8 ' ' <> "|"))
    mkTest n = TestCase $ assertEqual name outp (cleanupOneLine inp)
      where (name,inp,outp) = makeCase n
  testList "cleanupOneLine" $ map mkTest [1..7]

testList :: String -> [Test] -> Test
testList str ts = TestLabel str (TestList ts)

-- ---------------------------------------------------------------------

pwd :: IO FilePath
pwd = getCurrentDirectory

mcd :: FilePath -> IO ()
mcd = setCurrentDirectory
