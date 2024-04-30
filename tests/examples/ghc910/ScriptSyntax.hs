module ScriptSyntax where

scriptLink = manyTill anyChar (try $ do char ':'; char ']';)
