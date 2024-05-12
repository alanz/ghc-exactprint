
data TourView a = Null
                | Single {-# UNPACK #-} !(Elem a)
                | (PSQ a) `Play` (PSQ a)
