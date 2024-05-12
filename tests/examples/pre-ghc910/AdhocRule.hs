{-# RULES "adhoc1" forall r i. r { rOne = i } = r { rOne = i + 12 } #-}
{-# RULES "adhoc2" forall s. Record { rTwo = s } = Record { rTwo = s ++ s } #-}

