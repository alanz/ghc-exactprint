


getPath :: [String] -> Filter
getPath names elms =
  let follow = foldl (\f n -> \els-> subElems n $ f els) id' names :: Filter
      id' = id :: Filter
  in  follow elms
