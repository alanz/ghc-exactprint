module Records where

data Record = Record
  { rOne :: Int
  , rTwo :: String
  }

defR :: Record
defR = Record 1 "record"

main :: IO ()
main = do
  print $ defR { rOne = 42 }
  print $ Record { rTwo = "foo" }
