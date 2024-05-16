{-# language GADTs, RankNTypes #-}
{-# language FlexibleContexts, DefaultSignatures #-}
{-# language TypeOperators #-}
{-# language LambdaCase #-}
{-# language EmptyCase #-}
module Hedgehog.Function.Internal where

import Control.Monad.Trans.Maybe (MaybeT(..))
import Data.Bifunctor (first)
import Data.Functor.Contravariant (Contravariant(..))
import Data.Functor.Contravariant.Divisible (Divisible(..), Decidable(..))
import Data.Functor.Identity (Identity(..))
import Data.Int (Int8, Int16, Int32, Int64)
import Data.Maybe (fromJust)
import Data.Void (Void, absurd)
import Data.Word (Word8, Word64)
import Hedgehog.Internal.Gen (GenT(..), Gen, runGenT)
import Hedgehog.Internal.Seed (Seed(..))
import Hedgehog.Internal.Tree (Tree(..), Node(..))
import Hedgehog.Internal.Property (PropertyT, forAll)

import GHC.Generics

import qualified Hedgehog.Internal.Tree as Tree

infixr 5 :->

-- | Shrinkable, showable functions
--
-- Claessen, K. (2012, September). Shrinking and showing functions:(functional pearl).
-- In ACM SIGPLAN Notices (Vol. 47, No. 12, pp. 73-80). ACM.
data a :-> c where
  Unit :: c -> () :-> c
  Nil :: a :-> c
  Pair :: a :-> b :-> c -> (a, b) :-> c
  Sum :: a :-> c -> b :-> c -> Either a b :-> c
  Map :: (a -> b) -> (b -> a) -> b :-> c -> a :-> c

instance Functor ((:->) r) where
  fmap f (Unit c) = Unit $ f c
  fmap _ Nil = Nil
  fmap f (Pair a) = Pair $ fmap (fmap f) a
  fmap f (Sum a b) = Sum (fmap f a) (fmap f b)
  fmap f (Map a b c) = Map a b (fmap f c)

-- | Tabulate the function
table :: a :-> c -> [(a, c)]
table (Unit c) = [((), c)]
table Nil = []
table (Pair f) = do
  (a, bc) <- table f
  (b, c) <- table bc
  pure ((a, b), c)
table (Sum a b) =
  [(Left x, c) | (x, c) <- table a] ++
  [(Right x, c) | (x, c) <- table b]
table (Map _ g a) = first g <$> table a

class GArg a where
  gbuild' :: (a x -> c) -> a x :-> c

-- | Reify a function whose domain has an instance of 'Generic'
gbuild :: (Generic a, GArg (Rep a)) => (a -> c) -> a :-> c
gbuild = gvia from to

-- | @instance Arg A where@ allows functions which take @A@s to be reified
class Arg a where
  build :: (a -> c) -> a :-> c
  default build :: (Generic a, GArg (Rep a)) => (a -> c) -> a :-> c
  build = gbuild

variant :: Word64 -> GenT m b -> GenT m b
variant n (GenT f) = GenT $ \sz sd -> f sz (sd { seedValue = seedValue sd + n})

variant' :: Word64 -> CoGenT m b -> CoGenT m b
variant' n (CoGenT f) =
  CoGenT $ \a -> variant n . f a

class GVary a where
  gvary' :: CoGenT m (a x)

instance GVary V1 where
  gvary' = conquer

instance GVary U1 where
  gvary' = conquer

instance (GVary a, GVary b) => GVary (a :+: b) where
  gvary' =
    choose
      (\case; L1 a -> Left a; R1 a -> Right a)
      (variant' 0 gvary')
      (variant' 1 gvary')

instance (GVary a, GVary b) => GVary (a :*: b) where
  gvary' =
    divide
      (\(a :*: b) -> (a, b))
      (variant' 0 gvary')
      (variant' 1 gvary')

instance GVary c => GVary (M1 a b c) where
  gvary' = contramap unM1 gvary'

instance Vary b => GVary (K1 a b) where
  gvary' = contramap unK1 vary

-- | Build a co-generator for a type which has a 'Generic' instance
gvary :: (Generic a, GVary (Rep a)) => CoGenT m a
gvary = CoGenT $ \a -> applyCoGenT gvary' (from a)

-- | 'Vary' provides a canonical co-generator for a type.
--
-- While technically there are many possible co-generators for a given type, we don't get any
-- benefit from caring.
class Vary a where
  vary :: CoGenT m a
  default vary :: (Generic a, GVary (Rep a)) => CoGenT m a
  vary = gvary

-- | Build a co-generator for an 'Integral' type
varyIntegral :: Integral a => CoGenT m a
varyIntegral = CoGenT $ variant . fromIntegral

-- |
-- A @'CoGenT' m a@ is used to perturb a @'GenT' m b@ based on the value of the @a@. This way,
-- the generated function will have a varying (but still deterministic) right hand side.
--
-- Co-generators can be built using 'Divisible' and 'Decidable', but it is recommended to
-- derive 'Generic' and use the default instance of the 'Vary' type class.
--
-- @'CoGenT' m ~ 'Data.Functor.Contravariabe.Op' ('Data.Monoid.Endo' ('GenT' m b))@
newtype CoGenT m a = CoGenT { applyCoGenT :: forall b. a -> GenT m b -> GenT m b }
type CoGen = CoGenT Identity

instance Contravariant (CoGenT m) where
  contramap f (CoGenT g) = CoGenT (g . f)

instance Divisible (CoGenT m) where
  divide f (CoGenT gb) (CoGenT gc) =
    CoGenT $ \a ->
    let (b, c) = f a in gc c . gb b
  conquer = CoGenT $ const id

instance Decidable (CoGenT m) where
  choose f (CoGenT gb) (CoGenT gc) =
    CoGenT $ \a ->
    case f a of
      Left b -> gb b . variant 0
      Right c -> gc c . variant 1
  lose f = CoGenT $ \a -> absurd (f a)

instance (Show a, Show b) => Show (a :-> b) where
  show = show . table

-- | Evaluate a possibly partial function
apply' :: a :-> b -> a -> Maybe b
apply' (Unit c) () = Just c
apply' Nil _ = Nothing
apply' (Pair f) (a, b) = do
  f' <- apply' f a
  apply' f' b
apply' (Sum f _) (Left a) = apply' f a
apply' (Sum _ g) (Right a) = apply' g a
apply' (Map f _ g) a = apply' g (f a)

-- | Evaluate a total function. Unsafe.
unsafeApply :: a :-> b -> a -> b
unsafeApply f = fromJust . apply' f

-- | The type of randomly-generated functions
data Fn a b = Fn b (a :-> Tree (MaybeT Identity) b)

-- | Extract the root value from a 'Tree'. Unsafe.
unsafeFromTree :: Functor m => Tree (MaybeT m) a -> m a
unsafeFromTree =
  fmap (maybe (error "empty generator in function") nodeValue) .
  runMaybeT .
  runTree

instance (Show a, Show b) => Show (Fn a b) where
  show (Fn b a) =
    case table a of
      [] -> "_ -> " ++ show b
      ta -> showTable ta ++ "_ -> " ++ show b
    where
      showTable :: (Show a, Show b) => [(a, Tree (MaybeT Identity) b)] -> String
      showTable [] = "<empty function>\n"
      showTable (x : xs) = unlines (showCase <$> x : xs)
        where
          showCase (lhs, rhs) = show lhs ++ " -> " ++ show (runIdentity $ unsafeFromTree rhs)

-- | Shrink the function
shrinkFn :: (b -> [b]) -> a :-> b -> [a :-> b]
shrinkFn shr (Unit a) = Unit <$> shr a
shrinkFn _ Nil = []
shrinkFn shr (Pair f) =
  (\case; Nil -> Nil; a -> Pair a) <$> shrinkFn (shrinkFn shr) f
shrinkFn shr (Sum a b) =
  fmap (\case; Sum Nil Nil -> Nil; x -> x) $
  [ Sum a Nil | notNil b ] ++
  [ Sum Nil b | notNil a ] ++
  fmap (`Sum` b) (shrinkFn shr a) ++
  fmap (a `Sum`) (shrinkFn shr b)
  where
    notNil Nil = False
    notNil _ = True
shrinkFn shr (Map f g a) = (\case; Nil -> Nil; x -> Map f g x) <$> shrinkFn shr a

shrinkTree :: Monad m => Tree (MaybeT m) a -> m [Tree (MaybeT m) a]
shrinkTree (Tree m) = do
  a <- runMaybeT m
  case a of
    Nothing -> pure []
    Just (Node _ cs) -> pure cs

-- | Evaluate an 'Fn'
apply :: Fn a b -> a -> b
apply (Fn b f) = maybe b (runIdentity . unsafeFromTree) . apply' f

-- | Generate a function using the user-supplied co-generator
fnWith :: Arg a => CoGen a -> Gen b -> Gen (Fn a b)
fnWith cg gb =
  Fn <$>
  gb <*>
  genFn (\a -> applyCoGenT cg a gb)
  where
    genFn :: Arg a => (a -> Gen b) -> Gen (a :-> Tree (MaybeT Identity) b)
    genFn g =
      GenT $ \sz sd ->
      Tree.unfold (shrinkFn $ runIdentity . shrinkTree) .
      fmap (runGenT sz sd) $ build g

-- | Generate a function
fn :: (Arg a, Vary a) => Gen b -> Gen (Fn a b)
fn = fnWith vary

-- | Run the function generator to retrieve a function
forAllFn :: (Show a, Show b, Monad m) => Gen (Fn a b) -> PropertyT m (a -> b)
forAllFn = fmap apply . forAll

instance Vary ()
instance (Vary a, Vary b) => Vary (Either a b)
instance (Vary a, Vary b) => Vary (a, b)
instance Vary Void
instance Vary Bool
instance Vary Ordering
instance Vary a => Vary (Maybe a)
instance Vary a => Vary [a]
instance Vary Int8 where; vary = varyIntegral
instance Vary Int16 where; vary = varyIntegral
instance Vary Int32 where; vary = varyIntegral
instance Vary Int64 where; vary = varyIntegral
instance Vary Int where; vary = varyIntegral
instance Vary Integer where; vary = varyIntegral
instance Vary Word8 where; vary = varyIntegral

-- | Reify a function via an isomorphism.
--
-- If your function's domain has no instance of 'Generic' then you can still reify it using
-- an isomorphism to a better domain type. For example, the 'Arg' instance for 'Integral'
-- uses an isomorphism from @Integral a => a@ to @(Bool, [Bool])@, where the first element
-- is the sign, and the second element is the bit-string.
--
-- Note: @via f g@ will only be well-behaved if @g . f = id@ and @f . g = id@
via :: Arg b => (a -> b) -> (b -> a) -> (a -> c) -> a :-> c
via a b f = Map a b . build $ f . b

instance Arg Void where
  build _ = Nil

instance Arg () where
  build f = Unit $ f ()

instance (Arg a, Arg b) => Arg (a, b) where
  build f = Pair . build $ \a -> build $ \b -> f (a, b)

instance (Arg a, Arg b) => Arg (Either a b) where
  build f = Sum (build $ f . Left) (build $ f . Right)

gvia :: GArg b => (a -> b x) -> (b x -> a) -> (a -> c) -> a :-> c
gvia a b f = Map a b . gbuild' $ f . b

instance GArg V1 where
  gbuild' _ = Nil

instance GArg U1 where
  gbuild' f = Map (\U1 -> ()) (\() -> U1) (Unit $ f U1)

instance (GArg a, GArg b) => GArg (a :*: b) where
  gbuild' f = Map fromPair toPair $ Pair . gbuild' $ \a -> gbuild' $ \b -> f (a :*: b)
    where
      fromPair (a :*: b) = (a, b)
      toPair (a, b) = (a :*: b)

instance (GArg a, GArg b) => GArg (a :+: b) where
  gbuild' f = Map fromSum toSum $ Sum (gbuild' $ f . L1) (gbuild' $ f . R1)
    where
      fromSum = \case; L1 a -> Left a; R1 a -> Right a
      toSum = either L1 R1

instance GArg c => GArg (M1 a b c) where
  gbuild' = gvia unM1 M1

instance Arg b => GArg (K1 a b) where
  gbuild' f = Map unK1 K1 . build $ f . K1

-- | Reify a function on 'Integral's
buildIntegral :: (Arg a, Integral a) => (a -> c) -> (a :-> c)
buildIntegral = via toBits fromBits
  where
    toBits :: Integral a => a -> (Bool, [Bool])
    toBits n
      | n >= 0 = (True, go n)
      | otherwise = (False, go $ -n - 1)
      where
        go 0 = []
        go m =
          let
            (q, r) = quotRem m 2
          in
            (r == 1) : go q

    fromBits :: Integral a => (Bool, [Bool]) -> a
    fromBits (pos, bts)
      | pos = go bts
      | otherwise = negate $ go bts + 1
      where
        go [] = 0
        go (x:xs) = (if x then 1 else 0) + 2 * go xs

instance Arg Bool
instance Arg Ordering
instance Arg a => Arg (Maybe a)
instance Arg a => Arg [a]
instance Arg Int8 where; build = buildIntegral
instance Arg Int16 where; build = buildIntegral
instance Arg Int32 where; build = buildIntegral
instance Arg Int64 where; build = buildIntegral
instance Arg Int where; build = buildIntegral
instance Arg Integer where; build = buildIntegral

