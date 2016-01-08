{-# LANGUAGE OverloadedStrings #-}  --
                                    --
import Turtle
import qualified Data.Text as T

main = do
  packages <- allCabalPackages
  echo (T.pack $ show $ take 5 packages)

allCabalPackages :: IO [Text]
allCabalPackages = do
  let cmd = "cabal list --simple-output | awk '{ print $1 }' | uniq"
  (ec,r) <- shellStrict cmd empty
  let packages = T.lines r
  echo (T.pack $ show $ take 5 packages)
  return packages

workDir :: Text
workDir = "./hackage-roundtrip-work"
