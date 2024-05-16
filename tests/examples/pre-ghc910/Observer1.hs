module Observer1 where

instance HasPdu Int where
  data Pdu Int r where
    --a comment
    RegisterObserver :: Int
    --b comment
    ForgetObserver :: Int
    --c comment
    deriving (Typeable)
