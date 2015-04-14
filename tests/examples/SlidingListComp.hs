

foo =
  [concatMap (\(n, f) -> [findPath copts v >>= f (listArg "ghc" as) | v <- listArg n as]) [
                    ("project", Update.scanProject),
                    ("file", Update.scanFile),
                    ("path", Update.scanDirectory)],
                map (Update.scanCabal (listArg "ghc" as)) cabals]
