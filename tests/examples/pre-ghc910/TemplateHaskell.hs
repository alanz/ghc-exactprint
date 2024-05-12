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

{-# ANN module $([| 1 :: Int |]) #-}

foo = [t| HT.HashTable $(varT s) Int
                   (Result $(varT str) $tt) |]

objc_emit

objc_import [""]


$(do
    return $ foreignDecl cName ("build" ++ a) ([[t| Ptr Builder |]] ++ ats ++ [[t| CString |]]) [t| Ptr $(rt) |]
 )

foo = do
  let elemSize = [|sizeOf (undefined :: $(elemType))|]
      alignment _ = alignment (undefined :: $(elemType))
  return bar

class QQExp a b where
  qqExp x = [||fst $ runState $$(qqExpM x) ((0,M.empty) :: (Int,M.Map L.Name [L.Operand]))||]

class QQExp2 a b where
  qqExp x = [e||fst $ runState $$(qqExpM x) ((0,M.empty) :: (Int,M.Map L.Name [L.Operand]))||]
