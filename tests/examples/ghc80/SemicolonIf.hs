module Bar where

import Data.Text as Text

replace :: Text -> Text
replace = Text.map (\c -> if c == '_' then '.'; else c)

replace1 :: Text -> Text
replace1 = Text.map (\c -> if c == '_' ; then '.' else c)

replace2 :: Text -> Text
replace2 = Text.map (\c -> if c == '_'; then '.'; else c)

replace4 :: Text -> Text
replace4 = Text.map (\c -> if c == '_' then '.' else c)
