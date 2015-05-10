{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeSynonymInstances       #-}


instance HasTrie R2Basis where
    data R2Basis :->: x = R2Trie x x
    trie f = R2Trie (f XB) (f YB)
    untrie (R2Trie x _y) XB = x
    untrie (R2Trie _x y) YB = y
    enumerate (R2Trie x y)  = [(XB,x),(YB,y)]
