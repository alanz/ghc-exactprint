{-# LANGUAGE ScopedTypeVariables #-}

pTokenCost :: forall loc state a .((Show a, Eq a,  loc `IsLocationUpdatedBy` a, LL.ListLike state a) => [a] -> Int -> P (Str  a state loc) [a])
pTokenCost as cost = 5

pTokenCostStr :: forall a .((Show a) => [a] -> Int -> String)
pTokenCostStr as cost = "5"
