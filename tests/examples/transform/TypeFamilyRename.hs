{-# LANGUAGE TypeFamilies #-}
module TypeFamilyRename where

type family F a where F Int = Bool
                      F a   = a
