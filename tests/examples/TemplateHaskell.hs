{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

foo = $footemplate

bar = [e| quasi |]

baz = [quoter| quasi |]
