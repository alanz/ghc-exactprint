module Disambiguate where

instance Disambiguatable P_ObjDef where
  disambInfo (P_Obj a b c -- c1
                        v -- c2
                        d -- c3
                        f) -- c4
                        env -- c5
   = (P_Obj a b c' v d' f, Cnstr (sourceConstraintsOf env2) [] -- c6
     )
