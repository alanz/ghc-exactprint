module Math.LowlinSpec where

specs =
    [Spec "*\\" $ 0 =~= norm (s1*\v2 - (207,477))
    ,Spec "/*"  $ 0 =~= norm (v2/*s1 - (207,477))
    ,Spec "//"  $ 0 =~= norm (v2//s1 - (2.55555556, 5.88888889))
    ,Spec "norm 2" $ 0 =~= (norm v2 - 57.77542730261716)
    ]

