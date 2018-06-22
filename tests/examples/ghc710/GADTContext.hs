{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

data StackItem a where
  Snum :: forall a. Fractional a => a -> StackItem a
  Sop  :: OpDesc -> StackItem a
deriving instance Show a => Show (StackItem a)

type MPI = ?mpi_secret :: MPISecret

mkPoli = mkBila . map ((,,(),,()) <$> P.base <*> P.pos <*> P.form)

data MaybeDefault v where
    SetTo :: forall v . ( Eq v, Show v ) => !v -> MaybeDefault v
    SetTo4 :: forall v a. (( Eq v, Show v ) => v -> MaybeDefault v
                                            -> a -> MaybeDefault [a])
    TestParens :: (forall v . (Eq v) -> MaybeDefault v)

[t| Map.Map T.Text $tc |]

bar $( [p| x |] ) = x
