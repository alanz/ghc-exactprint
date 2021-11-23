module TopLevelSemis2  where

x = 1
;

-- foo: two matches, with params
foo [] = []
-- After foo1
;

foo x = x
-- After foo2
;

-- bar: one match, with params
bar a = a
-- after bar
;

-- baz: one match, no params
baz = 2
-- after baz
;

y = 3
