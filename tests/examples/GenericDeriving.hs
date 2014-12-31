{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
import GHC.Generics

-- from https://ocharles.org.uk/blog/posts/2014-12-16-derive-generic.html

data Valid e a = Error e | OK a
  deriving (Generic)

toEither :: Valid e a -> Either e a
toEither (Error e) = Left e
toEither (OK a) = Right a

fromEither :: Either e a -> Valid e a
fromEither (Left e) = Error e
fromEither (Right a) = OK a

-- ---------------------------------------------------------------------

class GetError rep e | rep -> e where
  getError' :: rep a -> Maybe e

instance GetError f e => GetError (M1 i c f) e where
  getError' (M1 m1) = getError' m1

instance GetError l e => GetError (l :+: r) e where
  getError' (L1 l) = getError' l
  getError' (R1 _) = Nothing

instance GetError (K1 i e) e where
  getError' (K1 e) = Just e

getError :: (Generic (errorLike e a), GetError (Rep (errorLike e a)) e) => errorLike e a -> Maybe e
getError = getError' . from

