-- | Types are great. Lifting them into some sort of applicative functor makes
-- them even better. This module is an homage to our favorite applicatives, and
-- to the semigroups with which they are instrinsically connected.

{-# LANGUAGE NoImplicitPrelude #-} -- Prelude is bad
{-# LANGUAGE DeriveFunctor     #-} -- Writing Functor instances is boring

module Acme.Functors
    (
    -- * Lifted-but-why
      LiftedButWhy (..)

    -- * Or-not
    , OrNot (..)

    -- * Two
    , Two (..)

    -- * Any-number-of
    , AnyNumberOf (..), (~~)

    -- * One-or-more
    , OneOrMore (..)

    -- * Also-extra-thing
    , Also (..)

    -- * Or-instead-other-thing
    , OrInstead (..)

    -- * Or-instead-other-thing ("first" variant)
    , OrInsteadFirst (..)

    -- * Determined-by-parameter
    , DeterminedBy (..)

    ) where

import Acme.Functors.Classes


--------------------------------------------------------------------------------
--  Lifted-but-why
--------------------------------------------------------------------------------

-- | __@LiftedButWhy@__ is a boring functor that just has one value and no other
-- structure or interesting properties.

data LiftedButWhy a =

    LiftedButWhy a
    -- ^ A value that has been lifted for some damned reason.
    --
    -- ... Okay, to be honest, this one is /nobody's/ favorite, but it is
    -- included here for completeness.

    deriving (Eq, Functor, Show)

-- | > pure = LiftedButWhy
-- >
-- > LiftedButWhy f <*> LiftedButWhy a = LiftedButWhy (f a)

instance Applicative LiftedButWhy where

    pure = LiftedButWhy

    LiftedButWhy f <*> LiftedButWhy a = LiftedButWhy (f a)

-- | > LiftedButWhy a >>= f = f a
instance Monad LiftedButWhy where

    LiftedButWhy a >>= f = f a

-- | > LiftedButWhy x <> LiftedButWhy y = LiftedButWhy (x <> y)

instance Semigroup a => Semigroup (LiftedButWhy a) where

    LiftedButWhy x <> LiftedButWhy y = LiftedButWhy (x <> y)

-- | > mempty = LiftedButWhy mempty

instance Monoid a => Monoid (LiftedButWhy a) where

    mempty = LiftedButWhy mempty


--------------------------------------------------------------------------------
--  Or-not
--------------------------------------------------------------------------------

-- | __@OrNot@__ is somehow slightly more interesting than @LiftedButWhy@, even
-- though it may actually contain /less/. Instead of a value, there might /not/
-- be a value.
--
-- When you combine stuff with @(\<*\>)@ or @(\<\>)@, all of the values need to
-- be present. If any of them are absent, the whole expression evaluates to
-- @Nope@.

data OrNot a = ActuallyYes a -- ^ Some normal value.
             | Nope          -- ^ Chuck Testa.
    deriving (Eq, Functor, Show)

-- | If you have a function @f@ that might not actually be there, and a value
-- @a@ that might not actually be there, lifted application @(\<*\>)@ gives you
-- @f a@ only if both of them are actually there.
--
-- > pure = ActuallyYes
-- >
-- > ActuallyYes f <*> ActuallyYes a = ActuallyYes (f a)
-- > _             <*> _             = Nope

instance Applicative OrNot where

    pure = ActuallyYes

    ActuallyYes f <*> ActuallyYes a = ActuallyYes (f a)
    _             <*> _             = Nope

instance Monad OrNot where

    ActuallyYes a  >>= f = f a
    Nope           >>= _ = Nope

-- | If you have value @a@ that may not actually be there, and another value
-- @a'@ that might not actually be there, the lifted semigroup operation
-- @(\<\>)@ gives you @a \<\> a'@ only if both of them are actually there.
--
-- > ActuallyYes a <> ActuallyYes a' = ActuallyYes (a <> a')
-- > _             <> _              = Nope

instance Semigroup a => Semigroup (OrNot a) where

    ActuallyYes a <> ActuallyYes a' = ActuallyYes (a <> a')
    _             <> _              = Nope

-- | > mempty = ActuallyYes mempty

instance Monoid a => Monoid (OrNot a) where

    mempty = ActuallyYes mempty


--------------------------------------------------------------------------------
--  Two
--------------------------------------------------------------------------------

-- | __@Two@__ is /two/ values. Yep. Just two values.

data Two a = Two { firstOfTwo  :: a -- ^ One value.
                 , secondOfTwo :: a -- ^ Another value.
                 }
    deriving (Eq, Functor, Show)

-- | If you have two functions @f@ and @g@ and two values @a@ and @a'@, then you
-- can apply them with @(\<*\>)@ to get two results @f a@ and @g a'@.
--
-- > pure a = Two a a
-- >
-- > Two f g <*> Two a a' = Two (f a) (g a')

instance Applicative Two where

    pure a = Two a a

    Two f g <*> Two a a' = Two (f a) (g a')

-- | > Two x y <> Two x' y' = Two (x <> x') (y <> y')

instance Semigroup a => Semigroup (Two a) where

    Two x y <> Two x' y' = Two (x <> x') (y <> y')

-- | > mempty = Two mempty mempty

instance Monoid a => Monoid (Two a) where

    mempty = Two mempty mempty


--------------------------------------------------------------------------------
--  Any-number-of
--------------------------------------------------------------------------------

-- | __@AnyNumberOf@__ starts to get exciting. Any number of values you want.
-- Zero... one ... two ... three ... four ... five ... The possibilities are
-- /truly/ endless.

data AnyNumberOf a =

    OneAndMaybeMore a (AnyNumberOf a)
    -- ^ One value, and maybe even more after that!

    | ActuallyNone -- ^ Oh. Well this is less fun.

    deriving (Eq, Functor, Show)

-- | Alias for 'OneAndMaybeMore' which provides some brevity.

(~~) :: a -> AnyNumberOf a -> AnyNumberOf a
(~~) = OneAndMaybeMore

infixr 5 ~~

-- | You can use this to apply any number of functions to any number of
-- arguments.
--
-- > pure a = OneAndMaybeMore a ActuallyNone
-- >
-- > OneAndMaybeMore f fs <*> OneAndMaybeMore x xs =
-- >     OneAndMaybeMore (f x) (fs <*> xs)
-- > _ <*> _ = ActuallyNone
--
-- Example:
--
-- >     ( (+ 1) ~~ (* 2) ~~ (+ 5) ~~       ActuallyNone )
-- > <*> (    1  ~~    6  ~~    4  ~~ 37 ~~ ActuallyNone )
-- >  =  (    7  ~~   12  ~~    9  ~~       ActuallyNone )
--
-- This example demonstrates how when there are more arguments than functions,
-- any excess arguments (in this case, the @37@) are ignored.

instance Applicative AnyNumberOf where

    pure a = OneAndMaybeMore a ActuallyNone

    OneAndMaybeMore f fs <*> OneAndMaybeMore x xs =
        OneAndMaybeMore (f x) (fs <*> xs)
    _ <*> _ = ActuallyNone

-- | The operation of combining some number of @a@ with some other number of @a@
-- is sometimes referred to as /zipping/.
--
-- > OneAndMaybeMore x xs <> OneAndMaybeMore y ys =
-- >     OneAndMaybeMore (x <> y) (xs <> ys)
-- > _ <> _ = ActuallyNone

instance Semigroup a => Semigroup (AnyNumberOf a) where

    OneAndMaybeMore x xs <> OneAndMaybeMore y ys =
        OneAndMaybeMore (x <> y) (xs <> ys)
    _ <> _ = ActuallyNone

-- | > mempty = mempty ~~ mempty

instance Monoid a => Monoid (AnyNumberOf a) where

    mempty = mempty ~~ mempty


--------------------------------------------------------------------------------
--  One-or-more
--------------------------------------------------------------------------------

-- | __@OneOrMore@__ is more restrictive than AnyNumberOf, yet somehow actually
-- /more/ interesting, because it excludes that dull situation where there
-- aren't any values at all.

data OneOrMore a = OneOrMore
    { theFirstOfMany :: a -- ^ Definitely at least this one.
    , possiblyMore :: AnyNumberOf a -- ^ And perhaps others.
    } deriving (Eq, Functor, Show)

-- | > pure a = OneOrMore a ActuallyNone
-- >
-- > OneOrMore f fs <*> OneOrMore x xs = OneOrMore (f x) (fs <*> xs)

instance Applicative OneOrMore where

    pure a = OneOrMore a ActuallyNone

    OneOrMore f fs <*> OneOrMore x xs = OneOrMore (f x) (fs <*> xs)

-- |
-- > OneOrMore a more <> OneOrMore a' more' =
-- >     OneOrMore a (more <> OneAndMaybeMore a' more')

instance Semigroup a => Semigroup (OneOrMore a) where

    OneOrMore a more <> OneOrMore a' more' =
        OneOrMore a (more <> OneAndMaybeMore a' more')

-- | > mempty = OneOrMore mempty ActuallyNone

instance Monoid a => Monoid (OneOrMore a) where

    mempty = OneOrMore mempty ActuallyNone


--------------------------------------------------------------------------------
--  Also-extra-thing
--------------------------------------------------------------------------------

-- | __@Also extraThing@__ is a functor in which each value has an @extraThing@
-- of some other type that tags along with it.

data (Also extraThing) a = Also
    { withoutExtraThing :: a          -- ^ A value.
    , theExtraThing     :: extraThing -- ^ An additional thing that tags along.
    }
    deriving (Eq, Functor, Show)

-- | Dragging the @extraThing@ along can be a bit of a burden. It prevents @Also
-- extraThing@ from being an applicative functor — unless the @extraThing@ can
-- pull its weight by bringing a monoid to the table.
--
-- > pure = (`Also` mempty)
-- >
-- > (f `Also` extra1) <*> (a `Also` extra2) = f a
-- >                                           `Also` (extra1 <> extra2)

instance Monoid extraThing => Applicative (Also extraThing) where

    pure = (`Also` mempty)

    (f `Also` extra1) <*> (a `Also` extra2) = f a
                                              `Also` (extra1 <> extra2)

-- |
-- > (a `Also` extra1) <> (a' `Also` extra2) = (a <> a')
-- >                                           `Also` (extra1 <> extra2)

instance (Semigroup extraThing, Semigroup a) => Semigroup ((Also extraThing) a)
  where

    (a `Also` extra1) <> (a' `Also` extra2) = (a <> a')
                                              `Also` (extra1 <> extra2)

-- | > mempty = Also mempty mempty

instance (Monoid extraThing, Monoid a) => Monoid ((Also extraThing) a)
  where

    mempty = Also mempty mempty


--------------------------------------------------------------------------------
--  Or-instead-other-thing
--------------------------------------------------------------------------------

-- | __@OrInstead otherThing@__ is a functor in which, instead of having a
-- value, can actually just have some totally unrelated @otherThing@ instead.
--
-- When you combine stuff with @(\<*\>)@ or @(\<\>)@, all of the values need to
-- be present. If any of them are the @otherThing@ instead, then the whole
-- expression evaluates to the combination of the @otherThing@s.

data (OrInstead otherThing) a =
      NotInstead a       -- ^ Some normal value.
    | Instead otherThing -- ^ Some totally unrelated other thing.
    deriving (Eq, Functor, Show)

-- | The possibility of having an @otherThing@ obstructs this functor's ability
-- to be applicative, much like the extra thing in @Also extraThing@ does. In
-- this case, since we do not need an empty value for the @otherThing@, it needs
-- only a semigroup to be in compliance.
--
-- > pure = NotInstead
-- >
-- > NotInstead f   <*> NotInstead a   = NotInstead (f a)
-- > Instead other1 <*> Instead other2 = Instead (other1 <> other2)
-- > Instead other  <*> _              = Instead other
-- > _              <*> Instead other  = Instead other

instance Semigroup otherThing => Applicative (OrInstead otherThing) where

    pure = NotInstead

    NotInstead f   <*> NotInstead a   = NotInstead (f a)
    Instead other1 <*> Instead other2 = Instead (other1 <> other2)
    Instead other  <*> _              = Instead other
    _              <*> Instead other  = Instead other

-- |
-- > NotInstead a   <> NotInstead a'  = NotInstead (a <> a')
-- > Instead other1 <> Instead other2 = Instead (other1 <> other2)
-- > Instead other  <> _              = Instead other
-- > _              <> Instead other  = Instead other

instance (Semigroup otherThing, Semigroup a) =>
  Semigroup ((OrInstead otherThing) a) where

    NotInstead a   <> NotInstead a'  = NotInstead (a <> a')
    Instead other1 <> Instead other2 = Instead (other1 <> other2)
    Instead other  <> _              = Instead other
    _              <> Instead other  = Instead other

-- > mempty = NotInstead mempty

instance (Semigroup otherThing, Monoid a) => Monoid ((OrInstead otherThing) a)
  where

    mempty = NotInstead mempty


--------------------------------------------------------------------------------
--  Or-instead-first-thing
--------------------------------------------------------------------------------

-- | __@OrInsteadFirst otherThing@__ looks a lot like @OrInstead otherThing@,
-- but it manages to always be an applicative functor — and even a monad too —
-- by handling the @otherThing@s a bit more hamfistedly.
--
-- When you combine stuff with @(\<*\>)@ or @(\<\>)@, all of the values need to
-- be present. If any of them are the @otherThing@ instead, then the whole
-- expression evaluates to the /first/ @otherThing@ encountered, ignoring any
-- additional @otherThings@ that may subsequently pop up

data (OrInsteadFirst otherThing) a =
      NotInsteadFirst a       -- ^ Some normal value.
    | InsteadFirst otherThing -- ^ Some totally unrelated other thing.
    deriving (Eq, Functor, Show)

-- |
-- > pure = NotInsteadFirst
-- >
-- > NotInsteadFirst f  <*> NotInsteadFirst a  = NotInsteadFirst (f a)
-- > InsteadFirst other <*> _                  = InsteadFirst other
-- > _                  <*> InsteadFirst other = InsteadFirst other

instance Applicative (OrInsteadFirst otherThing) where

    pure = NotInsteadFirst

    NotInsteadFirst f  <*> NotInsteadFirst a  = NotInsteadFirst (f a)
    InsteadFirst other <*> _                  = InsteadFirst other
    _                  <*> InsteadFirst other = InsteadFirst other

-- |
-- > InsteadFirst other >>= _ = InsteadFirst other
-- > NotInsteadFirst a  >>= f = f a

instance Monad (OrInsteadFirst otherThing) where

    InsteadFirst other >>= _ = InsteadFirst other
    NotInsteadFirst a  >>= f = f a

-- |
-- > NotInsteadFirst a  <> NotInsteadFirst a' = NotInsteadFirst (a <> a')
-- > InsteadFirst other <> _                  = InsteadFirst other
-- > _                  <> InsteadFirst other = InsteadFirst other

instance (Semigroup otherThing, Semigroup a) =>
  Semigroup ((OrInsteadFirst otherThing) a) where

    NotInsteadFirst a  <> NotInsteadFirst a' = NotInsteadFirst (a <> a')
    InsteadFirst other <> _                  = InsteadFirst other
    _                  <> InsteadFirst other = InsteadFirst other

-- | > mempty = NotInsteadFirst mempty

instance (Semigroup otherThing, Monoid a) =>
  Monoid ((OrInsteadFirst otherThing) a) where

    mempty = NotInsteadFirst mempty


--------------------------------------------------------------------------------
--  Determined-by-parameter
--------------------------------------------------------------------------------

-- | __@DeterminedBy parameter@__ is a value that... well, we're not really sure
-- what it is. We'll find out once a @parameter@ is provided.
--
-- The mechanism for deciding /how/ the value is determined from the
-- @parameter@ is opaque; all you can do is test it with different parameters
-- and see what results. There aren't even @Eq@ or @Show@ instances, which is
-- annoying.

data DeterminedBy parameter a = Determination ((->) parameter a)
    deriving (Functor)

-- |
-- > pure a = Determination (\_ -> a)
-- >
-- > Determination f <*> Determination a = Determination (\x -> f x (a x))

instance Applicative (DeterminedBy parameter) where

    pure a = Determination (\_ -> a)

    Determination f <*> Determination a = Determination (\x -> f x (a x))

-- |
-- > Determination fa >>= ff =
-- >     Determination (\x -> let Determination f = ff (fa x) in f x)

instance Monad (DeterminedBy parameter) where

    Determination fa >>= ff =
        Determination (\x -> let Determination f = ff (fa x) in f x)

-- | > Determination f <> Determination g = Determination (\x -> f x <> g x)

instance Semigroup a => Semigroup ((DeterminedBy parameter) a) where

    Determination f <> Determination g = Determination (\x -> f x <> g x)

-- | > mempty = Determination (\_ -> mempty)

instance Monoid a => Monoid ((DeterminedBy parameter) a) where

    mempty = Determination (\_ -> mempty)


{-

--------------------------------------------------------------------------------
--  Notes
--------------------------------------------------------------------------------

LiftedButWhy is Identity.

OrNot is Maybe, but with a different semigroup and monoid.

Two doesn't have an analogue in the standard library as far as I know.

AnyNumberOf is ZipList.

OneOrMore is NonEmpty.

Also is (,), the 2-tuple.

OrInstead is AccValidation from the 'validation' package.

OrInsteadFirst is Either.

DeterminedBy is (->) also known as a function, whose functor is also known as
Reader.

-}
