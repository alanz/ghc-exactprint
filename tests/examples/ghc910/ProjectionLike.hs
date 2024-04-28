module Agda.TypeChecking.ProjectionLike where

makeProjection =
  case defn of
    Function{funMutual = VV, -- comment
             funAbstr = ConcreteDef} -> undefined
