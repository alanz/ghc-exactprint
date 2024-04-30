module Expression where

eh1  =  try (do spaces; char '('; spaces; r <- parseEinh ; spaces; char ')'; return r;) <|> (do
                return $ Dims $ Map.singleton n i)
