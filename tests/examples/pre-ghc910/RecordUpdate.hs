
data Foo = F { f1 :: Int, f2 :: String }

foo :: Int -> Foo -> Foo
foo v f = f { f1 = v }

