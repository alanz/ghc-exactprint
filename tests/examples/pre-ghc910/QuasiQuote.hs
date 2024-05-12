{-# LANGUAGE QuasiQuotes #-}
module QuasiQuote where

import T7918A

ex1 = [qq|e1|]
ex2 = [qq|e2|]
ex3 = [qq|e3|]
ex4 = [qq|e4|]

tx1 = undefined :: [qq|t1|]
tx2 = undefined :: [qq|t2|]
tx3 = undefined :: [qq|t3|]
tx4 = undefined :: [qq|t4|]

px1 [qq|p1|] = undefined
px2 [qq|p2|] = undefined
px3 [qq|p3|] = undefined
px4 [qq|p4|] = undefined

{-# LANGUAGE QuasiQuotes #-}

testComplex    = assertBool "" ([istr|
        ok
#{Foo 4 "Great!" : [Foo 3 "Scott!"]}
        then
|] == ("\n" ++
    "        ok\n" ++
    "[Foo 4 \"Great!\",Foo 3 \"Scott!\"]\n" ++
    "        then\n"))

