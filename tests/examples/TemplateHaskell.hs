{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

foo = $footemplate

old = $(old)

bar = $$bartemplate

bar = [e| quasi |]

bar = [| quasi |]

baz = [quoter| quasi |]
