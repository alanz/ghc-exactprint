{-# LANGUAGE ImplicitParams #-}

foo = do
  ev <- let ?mousePosition = relative<$>Reactive (Size 1 1) _size<|*>_mousePos
            ?buttonChanges = _button
        in sink
  return baz
