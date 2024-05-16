module Expression where

eh1  =  try (do return r;) <|> (do
                return r)
