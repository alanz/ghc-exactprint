{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE TypeFamilies        #-}


type ( f :->   g) (r :: Type -> Type) ix = f r ix -> g r ix

type ( f :-->  g)  b ix = f b ix -> g b ix

type ((f :---> g)) b ix = f b ix -> g b ix
