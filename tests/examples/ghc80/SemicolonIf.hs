module Bar where

import Data.Text as Text

replace :: Text -> Text
replace = Text.map (\c -> if c == '_' then '.'; else c)
