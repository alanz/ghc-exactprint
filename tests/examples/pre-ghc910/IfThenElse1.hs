-- From http://lpaste.net/81623, courtesy of Albert Y. C. Lai
main = do
  cs <- if True
  then getLine
  else return "computer input"
  putStrLn cs

