-- Originally ./hackage-roundtrip-work/cabal-graphdeps-0.1.3/src/Main.hs
-- |
-- Module: Main
-- License: MIT
--
-- Run like:
-- >$ cabal-graphdeps mypackage | tred | dot -Tpng > ~/mypackage.png
module Main where

import           Control.Applicative
import           Control.Exception
import           Control.Monad (foldM, forM_)
import           Data.Char (isSpace)
import           Data.List (isInfixOf, stripPrefix)
import           Data.List.Split (splitWhen)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Options
import qualified System.Directory as Filesystem
import           System.Exit (exitFailure)
import qualified System.Process as Process
import qualified System.IO as IO
import           System.IO.Temp (withSystemTempDirectory)
import qualified Text.Parsec as Parsec
import qualified Text.Parsec.String as Parsec

data MainOptions = MainOptions
        { optGlobalPackageDb :: String
        , optFundamental :: [String]
        , optExclude :: Set.Set String
        }

instance Options MainOptions where
        defineOptions = pure MainOptions
                <*> simpleOption "global-package-db" ""
                    ""
                <*> defineOption (optionType_list ',' optionType_string) (\o -> o
                        { optionLongFlags = ["fundamental-packages"]
                        , optionDefault = ["base"]
                        , optionDescription = "These packages and their dependencies should be considered fundamental to the package DB."
                        })
                <*> defineOption (optionType_set ',' optionType_string) (\o -> o
                        { optionLongFlags = ["exclude-packages"]
                        , optionDefault = Set.empty
                        , optionDescription = "These packages and their dependencies will be excluded when rendering the graph."
                        })

resolveDeps :: MainOptions -> Map.Map String (Set.Set String) -> String -> IO (Map.Map String (Set.Set String))
resolveDeps _ allDeps pkg | Map.member pkg allDeps = return allDeps
resolveDeps opts allDeps pkg | Set.member (extractPkgName pkg) (optExclude opts) = return allDeps
resolveDeps opts allDeps pkg = do
        let header1 = "Resolving dependencies...\nIn order, the following would be installed (use -v for more details):\n"
        let header2 = "Resolving dependencies...\nIn order, the following would be installed:\n"

        IO.hPutStrLn IO.stderr ("[" ++ pkg ++ "]")
        output <- Process.readProcess "cabal" ["--package-db=clear", "install", "--dry-run", "--ghc-pkg-options=--global-package-db=./empty-db", pkg] ""
        if "All the requested packages are already installed:" `isInfixOf` output
                then return allDeps
                else do
                        trimmedOutput <- case stripPrefix header1 output of
                                Just stripped -> return stripped
                                Nothing -> case stripPrefix header2 output of
                                        Just stripped -> return stripped
                                        Nothing -> do
                                                IO.hPutStrLn IO.stderr (show output)
                                                error "unknown output format"
                        let newDeps = linesToDeps pkg (splitWhen (== '\n') trimmedOutput)
                        let insertedDeps = Map.insertWith Set.union pkg newDeps allDeps
                        foldM (resolveDeps opts) insertedDeps (Set.toList newDeps)

linesToDeps :: String -> [String] -> Set.Set String
linesToDeps pkg lines = Set.fromList $ do
        line <- filter (not . null) lines
        parsedLine <- case stringMatch lineOnlyVersion line of
                Just _ -> return line
                Nothing -> case stringMatch lineVersionWithNote line of
                        Just version -> return version
                        Nothing -> error ("can't parse line " ++ show line)
        if parsedLine == pkg
                then []
                else [parsedLine]

stringMatch :: Parsec.Parser a -> String -> Maybe a
stringMatch parser input = case Parsec.parse parser "" input of
        Left _ -> Nothing
        Right x -> Just x

lineOnlyVersion :: Parsec.Parser ()
lineOnlyVersion = do
        Parsec.skipMany1 alphaNumDot
        Parsec.eof

lineVersionWithNote :: Parsec.Parser String
lineVersionWithNote = do
        version <- Parsec.many1 alphaNumDot
        _ <- Parsec.char ' '
        _ <- Parsec.char '('
        Parsec.skipMany1 Parsec.anyChar
        Parsec.eof
        return version

alphaNumDot :: Parsec.Parser Char
alphaNumDot = Parsec.lower <|> Parsec.upper <|> Parsec.digit <|> Parsec.oneOf "-."

renderDeps :: MainOptions -> Map.Map String (Set.Set String) -> String -> Set.Set String
renderDeps opts deps rootPkg = rendered where
        (_, _, rendered) = loop rootPkg (rootPkg, Set.empty, Set.empty)

        -- This package has already been visited, so we don't need to continue
        -- any further.
        loop pkg acc@(_, visited, _) | Set.member pkg visited = acc

        -- map "foo-bar-baz-1.0" to "foo-bar-baz" for excluded packages
        loop pkg acc | Set.member (extractPkgName pkg) (optExclude opts) = acc

        loop pkg (parent, visited, lines) = let
                pkgDeps = Set.toList (Map.findWithDefault Set.empty pkg deps)
                visited' = Set.insert pkg visited
                lines' = Set.union lines $ Set.fromList $ do
                        dep <- pkgDeps
                        -- map "foo-bar-baz-1.0" to "foo-bar-baz" for excluded packages
                        if Set.member (extractPkgName dep) (optExclude opts)
                                then []
                                else [show pkg ++ " -> " ++ show dep]
                (_, visited'', lines'') = foldr loop (pkg, visited', lines') pkgDeps
                in (parent, visited'', lines'')

extractPkgName :: String -> String
extractPkgName pkg = case stripPrefix "-" (dropWhile (/= '-') (reverse pkg)) of
        Nothing -> pkg
        Just rev -> reverse rev

printDeps :: MainOptions -> Map.Map String (Set.Set String) -> String -> IO ()
printDeps opts deps pkg = forM_ (Set.toAscList (renderDeps opts deps pkg)) putStrLn

readGhcPkgField :: String -> String -> IO String
readGhcPkgField pkgName fieldName = do
        rawField <- Process.readProcess "ghc-pkg" ["field", pkgName, fieldName] ""
        case stripPrefix (fieldName ++ ":") rawField of
                Nothing -> error ("Unexpected output from ghc-pkg field: " ++ show rawField)
                Just s -> return (dropWhile isSpace (dropWhileEnd isSpace s))

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = foldr (\x xs -> if p x && null xs then [] else x : xs) []

initSandbox :: MainOptions -> IO ()
initSandbox opts = do
        _ <- Process.readProcess "cabal" ["sandbox", "init"] ""
        Filesystem.createDirectory "empty-db"

        globalDb <- case optGlobalPackageDb opts of
                -- Look for the global package DB
                "" -> do
                        rawGlobalDb <- Process.readProcess "ghc-pkg" ["list", "--no-user-package-db"] ""
                        case lines rawGlobalDb of
                                firstLine:_ ->
                                        -- ghc-pkg adds a ':' to the first line when not
                                        -- run from a terminal
                                        return (reverse (drop 1 (reverse firstLine)))
                                _ -> error "Unexpected output from ghc-pkg list"
                path -> return path

        -- these packages and their dependencies will be excluded from the
        -- graph for being fundamental.
        forM_ (optFundamental opts) $ \pkg -> do
                -- find package id (used in .conf filename)
                pkgId <- readGhcPkgField pkg "id"
                let pkgConf = pkgId ++ ".conf"

                -- try to figure out what the package depends on; in the GHC
                -- installed on my system, ::depends includes all dependencies
                -- of the package.
                deps <- readGhcPkgField pkg "depends"
                let splitDeps = words deps
                let emptyPackageDbConfs = pkgConf : [s ++ ".conf" | s <- splitDeps]
                forM_ emptyPackageDbConfs $ \conf -> Filesystem.copyFile (globalDb ++ "/" ++ conf) ("empty-db/" ++ conf)

withCurrentDirectory :: FilePath -> IO a -> IO a
withCurrentDirectory dir io = bracket
        (do
                cwd <- Filesystem.getCurrentDirectory
                Filesystem.setCurrentDirectory dir
                return cwd)
        Filesystem.setCurrentDirectory
        (\_ -> io)

main :: IO ()
main = runCommand $ \opts args -> do
        rootPackageName <- case args of
                [x] -> return x
                _ -> do
                        IO.hPutStrLn IO.stderr "Usage: cabal-graphdeps <package-name>"
                        exitFailure
        deps <- withSystemTempDirectory "cabal-graphdeps.d-" $ \dir -> withCurrentDirectory dir $ do
                initSandbox opts
                resolveDeps opts Map.empty rootPackageName
        putStrLn "digraph {"
        printDeps opts deps rootPackageName
        putStrLn "}"

