{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

foo = $footemplate

makeSplices ''Foo

old = $(old)

bar = $$bartemplate

bar = [e| quasi |]

bar = [| quasi |]

baz = [quoter| quasi |]

[t| Map.Map T.Text $tc |]
