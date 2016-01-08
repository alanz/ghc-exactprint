{-# LANGUAGE OverloadedStrings #-}  --
                                    --
import Turtle
import qualified Data.Text as T

main = do
  packages <- allCabalPackages
  -- packages <- allCabalPackagesTest
  echo (T.pack $ show $ take 5 packages)
  -- preparePackage "AAI"
  mapM_ preparePackage packages

-- ---------------------------------------------------------------------

preparePackage :: Text -> IO ()
preparePackage package = do
  shell ("mkdir -p " <> workDir) empty
  (ec,dir) <- shellStrict ("cabal get --destdir=" <> workDir <> " " <> package) empty
  echo (T.pack $ "cabal get:" ++ show dir)
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
  echo ("still need to clean up:" <> dir)
  -- return ()

-- ---------------------------------------------------------------------

allCabalPackagesTest :: IO [Text]
allCabalPackagesTest
  = return ["3d-graphics-examples","3dmodels","4Blocks","AAI","ABList"]


allCabalPackages :: IO [Text]
allCabalPackages = do
  let cmd = "cabal list --simple-output | awk '{ print $1 }' | uniq"
  (ec,r) <- shellStrict cmd empty
  let packages = T.lines r
  echo (T.pack $ show $ take 5 packages)
  return packages

-- ---------------------------------------------------------------------

workDir :: Text
workDir = "./hackage-roundtrip-work"
