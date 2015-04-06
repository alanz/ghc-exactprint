
selfQualify mod rsets = let defs = Set.fromList (map rs_name rsets)
                        in map (descend (f defs))
                               (map (\RS{..} -> RS{rs_name = qualify mod rs_name, ..}) rsets)
