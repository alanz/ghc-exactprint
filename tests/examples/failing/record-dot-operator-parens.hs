{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedRecordUpdate #-}
{-# LANGUAGE RebindableSyntax #-}

data Foo = Foo {bar :: Foo}

operatorUpdate f = f{(+) = 1}
