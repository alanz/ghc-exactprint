module Observer1 where

instance HasPdu (ObserverRegistry event) where
  data Pdu (ObserverRegistry event) r where
    --a comment
    RegisterObserver :: Int
    --b comment
    ForgetObserver :: Int
    --c comment
    deriving (Typeable)
