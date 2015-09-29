{-# LANGUAGE DataKinds, TypeOperators, GADTs #-}

give :: b -> Pattern '[b] a
give b = Pattern (const (Just $ oneT b))


pfail :: Pattern '[] a
pfail = is (const False)

(/\) :: Pattern vs1 a -> Pattern vs2 a -> Pattern (vs1 :++: vs2) a
(/\) = mk2 (\a -> Just (a,a))

data Pattern :: [*] -> * where
  Nil :: Pattern '[]
  Cons :: Maybe h -> Pattern t -> Pattern (h ': t)

type Pos = '("vpos", V3 GLfloat)
type Tag = '("tagByte", V1 Word8)

-- | Alias for the 'In' type from the 'Direction' kind, allows users to write
-- the 'BroadcastChan In a' type without enabling DataKinds.
type In = 'In
-- | Alias for the 'Out' type from the 'Direction' kind, allows users to write
-- the 'BroadcastChan Out a' type without enabling DataKinds.
type Out = 'Out
