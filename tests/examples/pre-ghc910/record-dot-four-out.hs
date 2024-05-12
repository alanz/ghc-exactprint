{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedRecordUpdate #-}
{-# LANGUAGE RebindableSyntax #-}

data Foo = Foo {bar :: Foo}

mfoo = fmap (.bar) $ Nothing

baz = (Foo 1).bar

fooplus f n = f{foo = f.bar + n}

nestedFoo f = f.bar.bar.bar.bar.bar

nestedFooUpdate f = f{bar.bar = f.bar} <> f{bar.bar.bar.bar}
