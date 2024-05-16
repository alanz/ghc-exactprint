{-# LANGUAGE ScopedTypeVariables, TemplateHaskell, StandaloneDeriving, TypeFamilies, GADTs
           , ViewPatterns, TypeOperators, TypeApplications, StandaloneDeriving
           , UnicodeSyntax, PatternSynonyms, FlexibleContexts, DataKinds, UndecidableInstances
           , TypeFamilyDependencies #-}

-- invoke as: ghci Main.hs -ddump-parsed -ddump-rn
-- or in GHCi: :set -ddump-parsed -ddump-rn

import TyFamWitnesses
import Language.Haskell.TH hiding (Type)
import Data.Type.Equality hiding (apply)
import Type.Reflection
import Unsafe.Coerce (unsafeCoerce)
import Data.Char (ord)
import GHC.TypeLits


stuffhave = [d| type family Foo a b where
                  Foo a a = Int
                  Foo (IO a) a = Float
                  Foo (IO a) b = Bool
                  Foo a Char = String
                  Foo a b = Char |]

stuffhave1 = [d| type family Bar a where Bar Bool = Bool; Bar a = IO a |]

stuffhave2 = [d| type family Elim a b where Elim a (a -> b) = b; Elim a (c -> b) = c -> Elim a b |]


witnesses [d| type family Foo a b where
                Foo a a = Int
                Foo (IO a) a = Float
                Foo (IO a) b = Bool
                Foo a Char = String
                Foo a b = Char |]

deriving instance Show (FooRefl a b)

-- now you can:
--
-- >>> :info FooRefl
-- type role FooRefl nominal nominal
-- data FooRefl a b where
--   Foo0 :: (Foo a a ~ Int) => FooRefl a a
--   Foo1 :: (Foo (IO b) b ~ Float) => FooRefl (IO b) b
--   Foo2 :: (Foo (IO a1) b ~ Bool) => FooRefl (IO a1) b
--   Foo3 :: (Foo a Char ~ String) => FooRefl a Char
--   Foo4 :: (Foo a b ~ Char) => FooRefl a b
--      -- Defined at Main.hs:13:1
-- instance Show (FooRefl a b) -- Defined at Main.hs:14:1
--
-- >>> :info reify_Foo
-- reify_Foo :: TypeRep a -> TypeRep b -> Maybe (FooRefl a b)
--      -- Defined at Main.hs:13:1
--
-- >>> reify_Foo (typeOf getChar) (typeRep @Char)
-- Just Foo1
--

witnesses [d| type family Bar a where Bar Bool = Bool; Bar a = IO a |]
deriving instance Show (BarRefl a)

witnesses [d| type family Elim a b where Elim a (a -> b) = b; Elim a (c -> b) = c -> Elim a b |]
deriving instance Show (ElimRefl a b)

pure []

stuffwant2 = [d|
  fooRefl :: forall a b . TypeRep a -> TypeRep b -> Maybe (FooRefl a b)
  fooRefl a b | Just HRefl <- eqTypeRep a b = pure Foo0
  fooRefl a b | Refl <- unsafeCoerce Refl :: Foo a b :~: Char = pure Foo1
  |]

stuffhave3 = [d| data Peano = Z | S Peano
                 type family ToPeano (n :: Nat) :: Peano where ToPeano 0 = Z; ToPeano n = S (ToPeano (n-1))
                 type family FromPeano (p :: Peano) :: Nat where FromPeano Z = 0; FromPeano (S n) = 1 + FromPeano n
               |]


witnesses [d| data Peano = Z | S Peano
              type family ToPeano (n :: Nat) :: Peano where ToPeano 0 = Z; ToPeano n = S (ToPeano (n-1))
              type family FromPeano (p :: Peano) :: Nat where FromPeano Z = 0; FromPeano (S n) = 1 + FromPeano n
            |]


deriving instance Show (ToPeanoRefl n)
deriving instance Show (FromPeanoRefl p)

witnesses [d| type family Unspell (w :: Symbol) = (r :: Nat) | r -> w where
                          Unspell "zero" = 0
                          Unspell "one" = 1
                          Unspell "two" = 2
                          Unspell "three" = 3
            |]
deriving instance Show (UnspellRefl w)

main = runQ (witnesses stuffhave2) >>= print

test@Just{} = reify_Elim (typeRep @Integer) (typeOf ((+1)::Integer->Integer))

lemma :: v -> TypeRep f -> TypeRep v
      -> Maybe (TypeRep (v `Elim` f), f -> v `Elim` f)
lemma w f v = do d `Fun'` c <- pure f
                 witness <- v `reify_Elim` f
                 case witness of
                   Elim0 -> pure (c, ($ w))
                   Elim1 -> do (e, g) <- lemma w c v
                               pure (d `Fun` e, (g.))

data Tag = Source | Destination | CheckSource Bool | CheckDest Bool | Cut Bool

newtype TaggedAction (t :: Tag) = Tagged (IO ())

data Action where
  Action :: Typeable t ⇒ TaggedAction t → Action
  Catalyst :: Typeable (c → d) ⇒ (c → d) → Action

pattern A :: forall k a. () => forall b. (TaggedAction b ~~ a) => TypeRep b → TypeRep a
pattern A b ← (eqTypeRep (typeRep @TaggedAction) → Just HRefl) `App` b


reaction :: Action → Action → Maybe Action
reaction (Catalyst f) (Action v) = do (rep, f') ← lemma v (typeOf f) (typeOf v)
                                      pure $ case rep of
                                        _ `Fun'` _ → withTypeable rep (Catalyst $ f' f)
                                        A indx → withTypeable indx (Action $ f' f)


member :: Eq a => a -> [a] -> Bool
member = elem

y $$ x = ($ y).($ x)

--Just (t3r, (($ elem) -> t3)) = lemma 'j' (typeOf $ member @Char) (typeOf 'j') -- https://ghc.haskell.org/trac/ghc/ticket/14293
Just (t3r, (($ "joe").($ elem) -> t3@True)) = lemma 'j' (typeOf $ member @Char) (typeOf 'j')
Just (t4r, (($ 'o').($ elem) -> t4@True)) = lemma "joe" (typeOf $ member @Char) (typeOf "joe")
-- Just (t4'r, ('x' $$ elem -> t4'@False)) = lemma "joe" (typeOf $ member @Char) (typeOf "joe") -- Same bug
Just (t4'r, (($ 'x').($ elem) -> t4'@False)) = lemma "joe" (typeOf $ member @Char) (typeOf "joe")

t0@Nothing = lemma "joe" (typeOf ord) (typeOf "joe")


Just u0 = reify_FromPeano (typeRep @Z)
Just u1 = reify_FromPeano (typeRep @(S Z))
Just u2 = reify_FromPeano (typeRep @(S (S Z)))

Just un0 = reify_Unspell (typeRep @"zero")
Just un1 = reify_Unspell (typeRep @"one")
Just un2 = reify_Unspell (typeRep @"two")

