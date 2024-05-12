module GA1r where
import Text.ParserCombinators.Parsec

parseStr :: CharParser () String
parseStr = char '"' *> (many1 (noneOf "\"")) <* char '"'
