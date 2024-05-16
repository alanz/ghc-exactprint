
-- ./hackage-roundtrip-work/ajhc-0.8.0.10/src/C/FromGrin2.hs
-- orig line 588
convertExp :: Exp -> C (Statement,Expression)
convertExp (Prim Func { primArgTypes = as, primRetType = r, primRetArgs = rs@(_:_), ..} vs ty) = do
    return (mempty,e)
