

give :: b -> Pattern '[b] a
give b = Pattern (const (Just $ oneT b))


pfail :: Pattern '[] a
pfail = is (const False)

(/\) :: Pattern vs1 a -> Pattern vs2 a -> Pattern (vs1 :++: vs2) a
(/\) = mk2 (\a -> Just (a,a))
