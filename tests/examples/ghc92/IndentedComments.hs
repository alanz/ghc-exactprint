module IndentedComments where

-- | 'ls_get strsMany n' ls_get strs 'n' elements in order, without blowing the stack.
ls_getMany strs n = go [] n
 where
    go xs 0 = return $! reverse xs
    go xs i = do x <- ls_get strs
                 -- indented comment
                 x `seq` go (x:xs) (i-1)

-- compat newtype for deserialization of v2-v4 CaptureData
newtype IntLen a = IntLen { fromIntLen :: a }
