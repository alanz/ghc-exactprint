{-# LANGUAGE PatternSynonyms #-}

-- | The pattern synonym equivalent of 'destIff'.
pattern l :<=> r <- Comb (Comb (Const "=" (TyBool :-> TyBool :-> TyBool)) l) r

-- | Destructor for boolean conjunctions.
destConj :: HOLTerm -> Maybe (HOLTerm, HOLTerm)
destConj = destBinary "/\\"

-- | The pattern synonym equivalent of 'destConj'.
pattern l :/\ r <- Binary "/\\" l r

-- | Destructor for boolean implications.
destImp :: HOLTerm -> Maybe (HOLTerm, HOLTerm)
destImp = destBinary "==>"

-- | The pattern synonym equivalent of 'destImp'.
pattern l :==> r <- Binary "==>" l r
