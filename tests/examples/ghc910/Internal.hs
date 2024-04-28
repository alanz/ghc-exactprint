{-# OPTIONS_GHC -Wno-redundant-constraints #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Diverse.Which.Internal (
      -- * 'Which' type
      Which(..) -- exporting constructor unsafely!

      -- * Single type
      -- ** Construction
    , impossible
    , impossible'
    -- , totally
    , pick
    , pick0
    , pickOnly
    , pickL
    , pickTag
    , pickN
      -- ** Destruction
    , obvious
    , trial
    , trial'
    , trial0
    , trial0'
    , trialL
    , trialL'
    , trialTag
    , trialTag'
    , trialN
    , trialN'
    , pattern W

      -- * Multiple types
      -- ** Injection
    , Diversify
    , diversify
    , diversify'
    , diversify0
    , DiversifyL
    , diversifyL
    , DiversifyN
    , diversifyN
      -- ** Inverse Injection
    , Reinterpret
    -- , Reinterpreted
    , reinterpret
    , Reinterpret'
    , reinterpret'
    , ReinterpretL
    -- , ReinterpretedL
    , reinterpretL
    , ReinterpretL'
    , reinterpretL'
    , ReinterpretN'
    , reinterpretN'

      -- * Catamorphism
    , Switch
    , switch
    , which
    , Switcher(..)
    , SwitchN
    , switchN
    , whichN
    , SwitcherN(..)
    ) where

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Data.Diverse.AFunctor
import Data.Diverse.ATraversable
import Data.Diverse.Case
import Data.Diverse.CaseFunc
import Data.Diverse.Reduce
import Data.Diverse.Reiterate
import Data.Diverse.TypeLevel
import Data.Kind
import Data.Semigroup (Semigroup(..))
import Data.Tagged
import Data.Void
import GHC.Exts (Any)
import qualified GHC.Generics as G
import GHC.TypeLits
import Text.ParserCombinators.ReadPrec
import Text.Read
import qualified Text.Read.Lex as L
import Unsafe.Coerce

-- | A 'Which' is an anonymous sum type (also known as a polymorphic variant, or co-record)
-- which can only contain one of the types in the typelist.
-- This is essentially a typed version of 'Data.Dynamic'.
--
-- The following functions are available can be used to manipulate unique types in the typelist
--
-- * constructor: 'pick'
-- * destructor: 'trial'
-- * injection: 'diversify' and 'reinterpret'
-- * catamorphism: 'which' or 'switch'
--
-- These functions are type specified. This means labels are not required because the types themselves can be used to access the 'Which'.
-- It is a compile error to use those functions for duplicate fields.
--
-- For duplicate types in the list of possible types, Nat-indexed version of the functions are available:
--
-- * constructor: 'pickN'
-- * destructor: 'trialN'
-- * inejction: 'diversifyN' and 'reinterpretN'
-- * catamorphism: 'whichN' or 'switchN'
--
-- Encoding: The variant contains a value whose type is at the given position in the type list.
-- This is the same encoding as <https://github.com/haskus/haskus-utils/blob/master/src/lib/Haskus/Utils/Variant.hs Haskus.Util.Variant> and <https://hackage.haskell.org/package/HList-0.4.1.0/docs/src/Data-HList-Variant.html Data.Hlist.Variant>.
--
-- The constructor is only exported in the "Data.Diverse.Which.Internal" module
data Which (xs :: [Type]) = Which {-# UNPACK #-} !Int Any

-- Just like Haskus and HList versions, inferred type is phantom which is wrong
-- representational means:
-- @
-- Coercible '[Int] '[IntLike] => Coercible (Which '[Int]) (Which '[IntLike])
-- @
type role Which representational

----------------------------------------------

-- | A terminating 'G.Generic' instance for no types encoded as a 'Which '[]'.
-- The 'G.C1' and 'G.S1' metadata are not encoded.
instance G.Generic (Which '[]) where
    type Rep (Which '[]) = G.V1
    from _ = {- G.V1 -} error "No generic representation for Which '[]"
    to _ = error "No values for Which '[]"

-- | A terminating 'G.Generic' instance for one type encoded with 'pick''.
-- The 'G.C1' and 'G.S1' metadata are not encoded.
instance G.Generic (Which '[x]) where
    type Rep (Which '[x]) = G.Rec0 x
    from v = {- G.Rec0 -} G.K1 (obvious v)
    to ({- G.Rec0 -} G.K1 a) = pickOnly a

-- | A 'G.Generic' instance encoded as either the 'x' value ('G.:+:') or the 'diversify0'ed remaining 'Which xs'.
-- The 'G.C1' and 'G.S1' metadata are not encoded.
instance G.Generic (Which (x ': x' ': xs)) where
    type Rep (Which (x ': x' ': xs)) = (G.Rec0 x) G.:+: (G.Rec0 (Which (x' ': xs)))
    from v = case trial0 v of
            Right x -> G.L1 ({- G.Rec0 -} G.K1 x)
            Left v' -> G.R1 ({- G.Rec0 -} G.K1 v')
    to {- G.Rec0 -} x = case x of
        G.L1 ({- G.Rec0 -} G.K1 a) -> pick0 a
        G.R1 ({- G.Rec0 -} G.K1 v) -> diversify0 v

-----------------------------------------------------------------------

instance Semigroup (Which '[]) where
    a <> _ = a

-- | Analogous to 'Data.Void.absurd'. Renamed 'impossible' to avoid conflicts.
--
-- Since 'Which \'[]' values logically don't exist, this witnesses the
-- logical reasoning tool of \"ex falso quodlibet\",
-- ie "from falsehood, anything follows".
--
-- A 'Which \'[]' is a 'Which' with no alternatives, which may occur as a 'Left'-over from 'trial'ing a @Which '[x]@ with one type.
-- It is an uninhabited type, just like 'Data.Void.Void'
impossible :: Which '[] -> a
impossible a = case a of {}
-- Copied from http://hackage.haskell.org/package/base/docs/src/Data.Void.html

-- | A @Which '[Void]@ is equivalent to @Which '[]@
-- A @Which '[Void]@ might occur if you lift a 'Void' into a @Which@ with 'pick'.
-- This allows you to convert it back to 'Void' or @Which '[]@
impossible' :: Which '[Void] -> a
impossible' a = case a of {}

-- -- | This function is useful to type restrict something that returns a polymorphic type
-- -- to return (Which '[]). Eg. use this that to prove at compile time that a
-- -- finished continuation monad has no more unhandled holes.
-- totally :: f (Which '[]) -> f (Which '[])
-- totally = id

-- | Lift a value into a 'Which' of possibly other types @xs@.
-- @xs@ can be inferred or specified with TypeApplications.
-- NB. forall is used to specify @xs@ first, so TypeApplications can be used to specify @xs@ first
--
-- @
-- 'pick' \'A' \@_ \@'[Int, Bool, Char, Maybe String] :: Which '[Int, Bool, Char, Maybe String]
-- @
pick :: forall x xs. UniqueMember x xs => x -> Which xs
pick = pick_

pick_ :: forall x xs n. (NatToInt n, n ~ IndexOf x xs) => x -> Which xs
pick_ = Which (natToInt @n) . unsafeCoerce

-- | A variation of 'pick' where @x@ is specified via a label
--
-- @
-- let y = 'pickL' \@Foo (Tagged (5 :: Int)) :: Which '[Bool, Tagged Foo Int, Tagged Bar Char]
--     x = 'trialL' \@Foo y
-- x `shouldBe` (Right (Tagged 5))
-- @
pickL :: forall l x xs. (UniqueLabelMember l xs, x ~ KindAtLabel l xs)
  => x -> Which xs
pickL = pick_ @x

-- | Variation of 'pick' specialized to 'Tagged' that automatically tags the value.
pickTag :: forall l x xs . (UniqueMember (Tagged l x) xs)
  => x -> Which xs
pickTag a = pick @(Tagged l x) (Tagged @l a)

-- | A variation of 'pick' into a 'Which' of a single type.
--
-- @
-- 'pickOnly' \'A' :: Which '[Char]
-- @
pickOnly :: x -> Which '[x]
pickOnly = pick0

-- | A variation of 'pick' into a 'Which' where @x@ is the first type.
--
-- @
-- 'pick0' \'A' :: Which '[Char, Int, Bool]
-- @
pick0 :: x -> Which (x ': xs)
pick0 = Which 0 . unsafeCoerce

-- | Lift a value into a 'Which' of possibly other (possibley indistinct) types, where the value is the @n@-th type.
--
-- @
-- 'pickN' \@4 (5 :: Int) :: Which '[Bool, Int, Char, Bool, Int, Char]
-- @
pickN :: forall n x xs. MemberAt n x xs => x -> Which xs
pickN = Which (natToInt @n) . unsafeCoerce

-- | It is 'obvious' what value is inside a 'Which' of one type.
--
-- @
-- let x = 'pick'' \'A' :: Which '[Char]
-- 'obvious' x \`shouldBe` \'A'
-- @
obvious :: Which '[a] -> a
obvious (Which _ v) = unsafeCoerce v

trial_
    :: forall n x xs.
       (NatToInt n, n ~ IndexOf x xs)
    => Which xs -> Either (Which (Remove x xs)) x
trial_ (Which n v) = let i = natToInt @n
                  in if n == i
                     then Right (unsafeCoerce v)
                     else if n > i
                          then Left (Which (n - 1) v)
                          else Left (Which n v)

-- | 'trialN' the n-th type of a 'Which', and get 'Either' the 'Right' value or the 'Left'-over possibilities.
--
-- @
-- let x = 'pick' \'A' \@_ \@'[Int, Bool, Char, Maybe String] :: 'Which' '[Int, Bool, Char, Maybe String]
-- 'trialN' \@1 x \`shouldBe` Left ('pick' \'A') :: 'Which' '[Int, Char, Maybe String]
-- @
trialN
    :: forall n x xs.
       (MemberAt n x xs)
    => Which xs -> Either (Which (RemoveIndex n xs)) x
trialN (Which n v) = let i = natToInt @n
                  in if n == i
                     then Right (unsafeCoerce v)
                     else if n > i
                          then Left (Which (n - 1) v)
                          else Left (Which n v)


-- | 'trial' a type in a 'Which' and 'Either' get the 'Right' value or the 'Left'-over possibilities.
--
-- @
-- let x = 'pick' \'A' \@'[Int, Bool, Char, Maybe String] :: 'Which' '[Int, Bool, Char, Maybe String]
-- 'trial' \@Char x \`shouldBe` Right \'A'
-- 'trial' \@Int x \`shouldBe` Left ('pick' \'A') :: 'Which' '[Bool, Char, Maybe String]
-- @
trial
    :: forall x xs.
       (UniqueMember x xs)
    => Which xs -> Either (Which (Remove x xs)) x
trial = trial_

-- | A variation of 'trial' where x is specified via a label
--
-- @
-- let y = 'pickL' \@Foo (Tagged (5 :: Int)) :: Which '[Bool, Tagged Foo Int, Tagged Bar Char]
--     x = 'trialL' \@Foo Proxy y
-- x `shouldBe` (Right (Tagged 5))
-- @
trialL
    :: forall l x xs.
       (UniqueLabelMember l xs, x ~ KindAtLabel l xs)
    => Which xs -> Either (Which (Remove x xs)) x
trialL = trial_ @_ @x


trial_'
    :: forall n x xs.
       (NatToInt n, n ~ IndexOf x xs)
    => Which xs -> Maybe x
trial_' (Which n v) = let i = natToInt @n
                  in if n == i
                     then Just (unsafeCoerce v)
                     else Nothing

-- | Variation of 'trialL' specialized to 'Tagged' which untags the field.
trialTag
    :: forall l x xs.
       (UniqueMember (Tagged l x) xs)
    => Which xs -> Either (Which (Remove (Tagged l x) xs)) x
trialTag xs = unTagged <$> trial @(Tagged l x) xs

-- | Variation of 'trialN' which returns a Maybe
trialN'
    :: forall n x xs.
       (MemberAt n x xs)
    => Which xs -> Maybe x
trialN' (Which n v) = let i = natToInt @n
                  in if n == i
                     then Just (unsafeCoerce v)
                     else Nothing

-- | Variation of 'trial' which returns a Maybe
trial'
    :: forall x xs.
       (UniqueMember x xs)
    => Which xs -> Maybe x
trial' = trial_'

-- | Variation of 'trialL' which returns a Maybe
trialL'
    :: forall l x xs.
       (UniqueLabelMember l xs, x ~ KindAtLabel l xs)
    => Which xs -> Maybe x
trialL' = trial_' @_ @x

-- | Variation of 'trialL'' specialized to 'Tagged' which untags the field.
trialTag'
    :: forall l x xs.
       (UniqueMember (Tagged l x) xs)
    => Which xs -> Maybe x
trialTag' xs = unTagged <$> trial' @(Tagged l x) xs

-- | A variation of a 'Which' 'trial' which 'trial's the first type in the type list.
--
-- @
-- let x = 'pick' \'A' \@'[Int, Bool, Char, Maybe String] :: 'Which' '[Int, Bool, Char, Maybe String]
-- 'trial0' x \`shouldBe` Left ('pick' \'A') :: 'Which' '[Bool, Char, Maybe String]
-- @
trial0 :: forall x xs. Which (x ': xs) -> Either (Which xs) x
trial0 (Which n v) = if n == 0
           then Right (unsafeCoerce v)
           else Left (Which (n - 1) v)

-- | Variation of 'trial0' which returns a Maybe
trial0' :: forall x xs.  Which (x ': xs) -> Maybe x
trial0' (Which n v) = if n == 0
           then Just (unsafeCoerce v)
           else Nothing

-- | Pattern synonym that makes pattern matching on Which possible.
--   For example, this will return @Just 5@:
--
-- @
-- let y = pick (5 :: Int) :: Which '[Bool, String, Int]
-- in  case y of
--       W (i :: Int) -> Just i
--       _ -> Nothing
-- @
--
-- Keep in mind, GHC is not smart enough and will always throw a warning about
-- incomplete pattern matches without a catch-all clause.
pattern W :: forall x xs. (UniqueMember x xs) => x -> Which xs
pattern W x <- (trial' -> Just x)
  where W x = pick x
-----------------------------------------------------------------

-- | A friendlier constraint synonym for 'diversify'.
type Diversify (branch :: [Type]) (tree :: [Type]) = Switch (CaseDiversify branch tree) (Which tree) branch

-- | Convert a 'Which' to another 'Which' that may include other possibilities.
-- That is, @branch@ is equal or is a subset of @tree@.
--
-- This can also be used to rearrange the order of the types in the 'Which'.
--
-- It is a compile error if @tree@ has duplicate types with @branch@.
--
-- NB. Use TypeApplications with @_ to specify @tree@.
--
-- @
-- let a = 'pick'' (5 :: Int) :: 'Which' '[Int]
--     b = 'diversify' \@_ \@[Int, Bool] a :: 'Which' '[Int, Bool]
--     c = 'diversify' \@_ \@[Bool, Int] b :: 'Which' '[Bool, Int]
-- @
diversify :: forall branch tree. Diversify branch tree => Which branch -> Which tree
diversify = which (CaseDiversify @branch @tree @_ @branch)

data CaseDiversify (branch :: [Type]) (tree :: [Type]) r (branch' :: [Type]) = CaseDiversify

type instance CaseResult (CaseDiversify branch tree r) x = r

instance Reiterate (CaseDiversify r branch tree) branch' where
    reiterate CaseDiversify = CaseDiversify

-- | The @Unique x branch@ is important to get a compile error if the from @branch@ doesn't have a unique x
instance (UniqueMember x tree, Unique x branch) =>
         Case (CaseDiversify branch tree (Which tree)) (x ': branch') where
    case' CaseDiversify = pick

-- | A simple version of 'diversify' which add another type to the front of the typelist.
diversify0 :: forall x xs. Which xs -> Which (x ': xs)
diversify0 (Which n v) = Which (n + 1) v

-- | A restricted version of 'diversify' which only rearranges the types
diversify' :: forall branch tree. (Diversify branch tree, SameLength branch tree) => Which branch -> Which tree
diversify' = diversify

------------------------------------------------------------------

-- | A friendlier constraint synonym for 'diversifyL'.
type DiversifyL (ls :: [k]) (branch :: [Type]) (tree :: [Type]) =
    ( Diversify branch tree
    , branch ~ KindsAtLabels ls tree
    , UniqueLabels ls tree
    , IsDistinct ls
    )

-- | A variation of 'diversify' where @branch@is additionally specified by a labels list.
--
-- @
-- let y = 'pickOnly' (5 :: Tagged Bar Int)
--     y' = 'diversifyL' \@'[Bar] y :: 'Which' '[Tagged Bar Int, Tagged Foo Bool]
--     y'' = 'diversifyL' \@'[Bar, Foo] y' :: 'Which' '[Tagged Foo Bool, Tagged Bar Int]
-- 'switch' y'' ('Data.Diverse.CaseFunc.CaseFunc' \@'Data.Typeable.Typeable' (show . typeRep . (pure \@Proxy))) \`shouldBe` \"Tagged * Bar Int"
-- @
diversifyL :: forall ls branch tree. (DiversifyL ls branch tree) => Which branch -> Which tree
diversifyL = which (CaseDiversify @branch @tree @_ @branch)

------------------------------------------------------------------

-- | A friendlier constraint synonym for 'diversifyN'.
type DiversifyN (ns :: [Nat]) (branch :: [Type]) (tree :: [Type]) =
    ( SwitchN Which (CaseDiversifyN ns) (Which tree) 0 branch
    , KindsAtIndices ns tree ~ branch
    )

-- | A variation of 'diversify' which uses a Nat list @indices@ to specify how to reorder the fields, where
--
-- @
-- indices[branch_idx] = tree_idx
-- @
--
-- This variation allows @tree@ to contain duplicate types with @branch@ since
-- the mapping is specified by @indicies@.
--
-- @
-- let y = 'pickOnly' (5 :: Int)
--     y' = 'diversifyN' \@'[0] \@_ \@[Int, Bool] y
--     y'' = 'diversifyN' \@[1,0] \@_ \@[Bool, Int] y'
-- 'switch' y'' ('Data.Diverse.CaseFunc.CaseFunc' \@'Data.Typeable.Typeable' (show . typeRep . (pure \@Proxy))) \`shouldBe` \"Int"
-- @
diversifyN :: forall ns branch tree. (DiversifyN ns branch tree) => Which branch -> Which tree
diversifyN = whichN (CaseDiversifyN @ns @_ @0 @branch)

data CaseDiversifyN (ns :: [Nat]) r (n :: Nat) (branch' :: [Type]) = CaseDiversifyN

type instance CaseResult (CaseDiversifyN ns r n) x = r

instance ReiterateN (CaseDiversifyN ns r) n branch' where
    reiterateN CaseDiversifyN = CaseDiversifyN

instance MemberAt (KindAtIndex n ns) x tree =>
         Case (CaseDiversifyN ns (Which tree) n) (x ': branch') where
    case' CaseDiversifyN v = pickN @(KindAtIndex n ns) v

------------------------------------------------------------------

-- | A friendlier constraint synonym for 'reinterpret'.
type Reinterpret (branch :: [Type]) (tree :: [Type]) = Switch (CaseReinterpret branch tree) (Either (Which (Complement tree branch)) (Which branch)) tree

-- -- | A variation of 'Reinterpret' that exposes @branchlessTree ~ Complement tree branch@
-- type Reinterpreted branch tree branchlessTree = (Reinterpret branch tree, branchlessTree ~ Complement tree branch)

-- | Convert a 'Which' into possibly another 'Which' with a totally different typelist.
-- Returns either a 'Which' with the 'Right' value, or a 'Which' with the 'Left'over @compliment@ types.
--
-- It is a compile error if @branch@ or @compliment@ has duplicate types with @tree@.
--
-- NB. forall used to specify @branch@ first, so TypeApplications can be used to specify @branch@ first.
--
-- @
--     let a = 'pick' \@[Int, Char, Bool] (5 :: Int) :: 'Which' '[Int, Char, Bool]
--     let  b = 'reinterpret' @[String, Char] y
--     b \`shouldBe` Left ('pick' (5 :: Int)) :: 'Which' '[Int, Bool]
--     let c = 'reinterpret' @[String, Int] a
--     c \`shouldBe` Right ('pick' (5 :: Int)) :: 'Which' '[String, Int]
-- @
reinterpret :: forall branch tree. (Reinterpret branch tree) =>
    Which tree -> Either (Which (Complement tree branch)) (Which branch)
reinterpret = which (CaseReinterpret @branch @tree @_ @tree)

data CaseReinterpret (branch :: [Type]) (tree :: [Type]) r (tree' :: [Type]) = CaseReinterpret

type instance CaseResult (CaseReinterpret branch tree r) x = r

instance Reiterate (CaseReinterpret branch tree r) tree' where
    reiterate CaseReinterpret = CaseReinterpret

instance ( MaybeUniqueMember x branch
         , comp ~ Complement tree branch
         , MaybeUniqueMember x comp
         , Unique x tree -- Compile error to ensure reinterpret only works with unique fields
         ) =>
         Case (CaseReinterpret branch tree (Either (Which comp) (Which branch))) (x ': tree') where
    case' CaseReinterpret a =
        case natToInt @(PositionOf x branch) of
            0 -> let j = natToInt @(PositionOf x comp)
                 -- safe use of partial! j will never be zero due to check above
                 in Left $ Which (j - 1) (unsafeCoerce a)
            i -> Right $ Which (i - 1) (unsafeCoerce a)

------------------------------------------------------------------

-- | A friendlier constraint synonym for 'reinterpret''.
type Reinterpret' (branch :: [Type]) (tree :: [Type]) = Switch (CaseReinterpret' branch tree) (Maybe (Which branch)) tree

-- | Variation of 'reinterpret' which returns a Maybe.
reinterpret' :: forall branch tree. (Reinterpret' branch tree) => Which tree -> Maybe (Which branch)
reinterpret' = which (CaseReinterpret' @branch @tree @_ @tree)

data CaseReinterpret' (branch :: [Type]) (tree :: [Type]) r (tree' :: [Type]) = CaseReinterpret'

type instance CaseResult (CaseReinterpret' branch tree r) x = r

instance Reiterate (CaseReinterpret' branch tree r) tree' where
    reiterate CaseReinterpret' = CaseReinterpret'

instance ( MaybeUniqueMember x branch
         , comp ~ Complement tree branch
         , Unique x tree -- Compile error to ensure reinterpret only works with unique fields
         ) =>
         Case (CaseReinterpret' branch tree (Maybe (Which branch))) (x ': tree') where
    case' CaseReinterpret' a =
        case natToInt @(PositionOf x branch) of
            0 -> Nothing
            i -> Just $ Which (i - 1) (unsafeCoerce a)

------------------------------------------------------------------

-- | A friendlier constraint synonym for 'reinterpretL'.
type ReinterpretL (ls :: [k]) (branch :: [Type]) (tree :: [Type]) =
    ( Reinterpret branch tree
    , branch ~ KindsAtLabels ls tree
    , UniqueLabels ls tree
    , IsDistinct ls
    )

-- -- | A variation of 'ReinterpretL' that exposes @branchlessTree ~ Complement tree branch@
-- type ReinterpretedL ls branch tree branchlessTree = (ReinterpretL ls branch tree, branchlessTree ~ Complement tree branch)

-- | A variation of 'reinterpret' where the @branch@ is additionally specified with a labels list.
--
-- @
-- let y = 'pick' \@[Tagged Bar Int, Tagged Foo Bool, Tagged Hi Char, Tagged Bye Bool] (5 :: Tagged Bar Int)
--     y' = 'reinterpretL' \@[Foo, Bar] y
--     x = 'pick' \@[Tagged Foo Bool, Tagged Bar Int] (5 :: Tagged Bar Int)
-- y' \`shouldBe` Right x
-- @
reinterpretL :: forall ls branch tree. (ReinterpretL ls branch tree)
  => Which tree -> Either (Which (Complement tree branch)) (Which branch)
reinterpretL = which (CaseReinterpret @branch @tree @_ @tree)

-- | A friendlier constraint synonym for 'reinterpretL'.
type ReinterpretL' (ls :: [k]) (branch :: [Type]) (tree :: [Type]) =
    ( Reinterpret' branch tree
    , branch ~ KindsAtLabels ls tree
    , UniqueLabels ls tree
    , IsDistinct ls
    )

-- | Variation of 'reinterpretL' which returns a Maybe.
reinterpretL' :: forall ls branch tree. (ReinterpretL' ls branch tree)
  => Which tree -> Maybe (Which branch)
reinterpretL' = which (CaseReinterpret' @branch @tree @_ @tree)

------------------------------------------------------------------

-- | A friendlier constraint synonym for 'reinterpretN'.
type ReinterpretN' (ns :: [Nat]) (branch :: [Type]) (tree :: [Type]) =
    ( SwitchN Which (CaseReinterpretN' ns) (Maybe (Which branch)) 0 tree
    , KindsAtIndices ns tree ~ branch)

-- | A limited variation of 'reinterpret'' which uses a Nat list @n@ to specify how to reorder the fields, where
--
-- @
-- indices[branch_idx] = tree_idx
-- @
--
-- This variation allows @tree@ to contain duplicate types with @branch@
-- since the mapping is specified by @indicies@.
--
-- However, unlike 'reinterpert', in this variation,
-- @branch@ must be a subset of @tree@ instead of any arbitrary Which.
-- Also it returns a Maybe instead of Either.
--
-- This is so that the same @indices@ can be used in 'narrowN'.
reinterpretN' :: forall ns branch tree. (ReinterpretN' ns branch tree)
  => Which tree -> Maybe (Which branch)
reinterpretN' = whichN (CaseReinterpretN' @ns @_ @0 @tree)

data CaseReinterpretN' (indices :: [Nat]) r (n :: Nat) (tree' :: [Type]) = CaseReinterpretN'

type instance CaseResult (CaseReinterpretN' indices r n) x = r

instance ReiterateN (CaseReinterpretN' indices r) n tree' where
    reiterateN CaseReinterpretN' = CaseReinterpretN'

instance (MaybeMemberAt n' x branch, n' ~ PositionOf n indices) =>
         Case (CaseReinterpretN' indices (Maybe (Which branch)) n) (x ': tree) where
    case' CaseReinterpretN' a =
        case natToInt @n' of
            0 -> Nothing
            i -> Just $ Which (i - 1) (unsafeCoerce a)

------------------------------------------------------------------

-- | 'Switcher' is an instance of 'Reduce' for which __'reiterate'__s through the possibilities in a 'Which',
-- delegating handling to 'Case', ensuring termination when 'Which' only contains one type.
newtype Switcher c r (xs :: [Type]) = Switcher (c r xs)

type instance Reduced (Switcher c r xs) = r

-- | 'trial0' each type in a 'Which', and either handle the 'case'' with value discovered, or __'reiterate'__
-- trying the next type in the type list.
instance ( Case (c r) (x ': x' ': xs)
         , Reduce (Which (x' ': xs)) (Switcher c r (x' ': xs))
         , Reiterate (c r) (x : x' : xs)
         , r ~ CaseResult (c r) x -- This means all @r@ for all typelist must be the same @r@
         ) =>
         Reduce (Which (x ': x' ': xs)) (Switcher c r (x ': x' ': xs)) where
    reduce (Switcher c) v =
        case trial0 v of
            Right a -> case' c a
            Left v' -> reduce (Switcher (reiterate c)) v'
    -- GHC 8.2.1 can optimize to single case statement. See https://ghc.haskell.org/trac/ghc/ticket/12877
    {-# INLINABLE reduce #-}
     -- This makes compiling tests a little faster than with no pragma

-- | Terminating case of the loop, ensuring that a instance of @Case '[]@
-- with an empty typelist is not required.
instance (Case (c r) '[x], r ~ CaseResult (c r) x) => Reduce (Which '[x]) (Switcher c r '[x]) where
    reduce (Switcher c) v = case obvious v of
            a -> case' c a

-- | Allow 'Which \'[]' to be 'reinterpret''ed or 'diversify'ed into anything else
-- This is safe because @Which '[]@ is uninhabited, and this is already something that
-- can be done with 'impossible'
instance Reduce (Which '[]) (Switcher c r '[]) where
    reduce _ = impossible

-- | Allow 'Void' to be 'reinterpret''ed or 'diversify'ed into anything else
-- This is safe because @Void@ is uninhabited, and this is already something that
-- can be done with 'impossible'
instance Reduce (Void) (Switcher c r '[]) where
    reduce _ = absurd

-- | Allow 'Which \'[Void]' to be 'reinterpret''ed or 'diversify'ed into anything else
-- This is safe because @Which '[Void]@ is uninhabited, and this is already something that
-- can be done with 'impossible'
instance Reduce (Which '[Void]) (Switcher c r '[]) where
    reduce _ = impossible'

------------------------------------------------------------------

-- | A friendlier constraint synonym for 'reinterpretN'.
type Switch c r xs = Reduce (Which xs) (Switcher c r xs)

-- | A switch/case statement for 'Which'. This is equivalent to @flip 'which'@
--
-- Use 'Case' instances like 'Data.Diverse.Cases.Cases' to apply a 'Which' of functions to a variant of values.
--
-- @
-- let y = 'Data.Diverse.Which.pick' (5 :: Int) :: 'Data.Diverse.Which.Which' '[Int, Bool]
-- 'Data.Diverse.Which.switch' y (
--     'Data.Diverse.Cases.cases' (show \@Bool
--         'Data.Diverse.Many../' show \@Int
--         'Data.Diverse.Many../' 'Data.Diverse.Many.nil')) \`shouldBe` "5"
-- @
--
-- Or 'Data.Diverse.CaseFunc.CaseFunc' \@'Data.Typeable.Typeable' to apply a polymorphic function that work on all 'Typeable's.
--
-- @
-- let y = 'Data.Diverse.Which.pick' (5 :: Int) :: 'Data.Diverse.Which.Which' '[Int, Bool]
-- 'Data.Diverse.Which.switch' y ('Data.Diverse.CaseFunc.CaseFunc' \@'Data.Typeable.Typeable' (show . typeRep . (pure \@Proxy))) \`shouldBe` "Int"
-- @
--
-- Or you may use your own custom instance of 'Case'.
switch :: Switch c r xs => Which xs -> c r xs -> r
switch w c = reduce (Switcher c) w

-- | Catamorphism for 'Which'. This is @flip 'switch'@.
which :: Switch c r xs => c r xs -> Which xs -> r
which = flip switch

------------------------------------------------------------------

-- | 'SwitcherN' is a variation of 'Switcher' which __'reiterateN'__s through the possibilities in a 'Which',
-- delegating work to 'CaseN', ensuring termination when 'Which' only contains one type.
newtype SwitcherN c r (n :: Nat) (xs :: [Type]) = SwitcherN (c r n xs)

type instance Reduced (SwitcherN c r n xs) = r

-- | 'trial0' each type in a 'Which', and either handle the 'case'' with value discovered, or __'reiterateN'__
-- trying the next type in the type list.
instance ( Case (c r n) (x ': x' ': xs)
         , Reduce (Which (x' ': xs)) (SwitcherN c r (n + 1) (x' ': xs))
         , ReiterateN (c r) n (x : x' : xs)
         , r ~ CaseResult (c r n) x -- This means all @r@ for all typelist must be the same @r@
         ) =>
         Reduce (Which (x ': x' ': xs)) (SwitcherN c r n (x ': x' ': xs)) where
    reduce (SwitcherN c) v =
        case trial0 v of
            Right a -> case' c a
            Left v' -> reduce (SwitcherN (reiterateN c)) v'
    -- Ghc 8.2.1 can optimize to single case statement. See https://ghc.haskell.org/trac/ghc/ticket/12877
    {-# INLINABLE reduce #-}
 -- This makes compiling tests a little faster than with no pragma

-- | Terminating case of the loop, ensuring that a instance of @Case '[]@
-- with an empty typelist is not required.
-- You can't reduce 'zilch'
instance (Case (c r n) '[x], r ~ CaseResult (c r n) x) => Reduce (Which '[x]) (SwitcherN c r n '[x]) where
    reduce (SwitcherN c) v = case obvious v of
            a -> case' c a

-- | Catamorphism for 'Which'. This is equivalent to @flip 'switchN'@.
whichN :: SwitchN w c r n xs => c r n xs -> w xs -> r
whichN = flip switchN

-- | A switch/case statement for 'Which'. This is equivalent to @flip 'whichN'@
--
-- Use 'Case' instances like 'Data.Diverse.Cases.CasesN' to apply a 'Which' of functions to a variant of values
-- in index order.
--
-- @
-- let y = 'pickN' \@0 (5 :: Int) :: 'Which' '[Int, Bool, Bool, Int]
-- 'switchN' y (
--     'Data.Diverse.Cases.casesN' (show \@Int
--         'Data.Diverse.Many../' show \@Bool
--         'Data.Diverse.Many../' show \@Bool
--         'Data.Diverse.Many../' show \@Int
--         'Data.Diverse.Many../' 'Data.Diverse.Many.nil')) \`shouldBe` "5"
-- @
--
-- Or you may use your own custom instance of 'Case'.
class SwitchN w c r (n :: Nat) xs where
    switchN :: w xs -> c r n xs -> r

instance Reduce (Which xs) (SwitcherN c r n xs) => SwitchN Which c r n xs where
    switchN w c = reduce (SwitcherN c) w

-----------------------------------------------------------------

-- | Two 'Which'es are only equal iff they both contain the equivalnet value at the same type index.
instance (Switch CaseEqWhich Bool (x ': xs)) =>
         Eq (Which (x ': xs)) where
    l@(Which i _) == (Which j u) =
        if i /= j
            then False
            else switch l (CaseEqWhich u)

-- | @('zilch' == 'zilch') == True@
instance Eq (Which '[]) where
    _ == _ = True

-- | Do not export constructor
-- Stores the right Any to be compared when the correct type is discovered
newtype CaseEqWhich r (xs :: [Type]) = CaseEqWhich Any

type instance CaseResult (CaseEqWhich r) x = r

instance Reiterate (CaseEqWhich r) (x ': xs) where
    reiterate (CaseEqWhich r) = CaseEqWhich r

instance Eq x => Case (CaseEqWhich Bool) (x ': xs) where
    case' (CaseEqWhich r) l = l == unsafeCoerce r

-----------------------------------------------------------------

-- | A 'Which' with a type at smaller type index is considered smaller.
instance ( Switch CaseEqWhich Bool (x ': xs)
         , Switch CaseOrdWhich Ordering (x ': xs)
         ) =>
         Ord (Which (x ': xs)) where
    compare l@(Which i _) (Which j u) =
        if i /= j
            then compare i j
            else switch l (CaseOrdWhich u)

-- | @('compare' 'zilch' 'zilch') == EQ@
instance Ord (Which '[]) where
    compare _ _ = EQ

-- | Do not export constructor
-- Stores the right Any to be compared when the correct type is discovered
newtype CaseOrdWhich r (xs :: [Type]) = CaseOrdWhich Any

type instance CaseResult (CaseOrdWhich r) x = r

instance Reiterate (CaseOrdWhich r) (x ': xs) where
    reiterate (CaseOrdWhich r) = CaseOrdWhich r

instance Ord x => Case (CaseOrdWhich Ordering) (x ': xs) where
    case' (CaseOrdWhich r) l = compare l (unsafeCoerce r)

------------------------------------------------------------------

-- | @show ('pick'' \'A') == "pick \'A'"@
instance (Switch CaseShowWhich ShowS (x ': xs)) =>
         Show (Which (x ': xs)) where
    showsPrec d v = showParen (d > app_prec) (which (CaseShowWhich 0) v)
      where
        app_prec = 10

instance Show (Which '[]) where
    showsPrec _ = impossible

newtype CaseShowWhich r (xs :: [Type]) = CaseShowWhich Int

type instance CaseResult (CaseShowWhich r) x = r

instance Reiterate (CaseShowWhich r) (x ': xs) where
    reiterate (CaseShowWhich i) = CaseShowWhich (i + 1)

instance Show x => Case (CaseShowWhich ShowS) (x ': xs) where
    case' (CaseShowWhich i) v = showString "pickN @" . showString (show i) . showChar ' ' . showsPrec (app_prec + 1) v
      where app_prec = 10

------------------------------------------------------------------

class WhichRead v where
    whichReadPrec :: Int -> Int -> ReadPrec v

-- | coerce Which to another type without incrementing Int.
-- THis is because 'WhichRead' instance already increments the int
coerceReadWhich :: forall x xs. Which xs -> Which (x ': xs)
coerceReadWhich (Which i x) = Which i x

-- | Succeed reading if the Int index match
readWhich :: forall x xs. Read x => Int -> Int -> ReadPrec (Which (x ': xs))
readWhich i j = guard (i == j) >> parens (prec app_prec $ (Which i . unsafeCoerce) <$> readPrec @x)
      where
        app_prec = 10

instance Read x => WhichRead (Which '[x]) where
    whichReadPrec = readWhich

instance (Read x, WhichRead (Which (x' ': xs))) => WhichRead (Which (x ': x' ': xs)) where
    whichReadPrec i j = readWhich i j
               <|> (coerceReadWhich <$> (whichReadPrec i (j + 1) :: ReadPrec (Which (x' ': xs))))
    {-# INLINABLE whichReadPrec #-} -- This makes compiling tests a little faster than with no pragma

-- | This 'Read' instance tries to read using the each type in the typelist, using the first successful type read.
instance WhichRead (Which (x ': xs)) =>
         Read (Which (x ': xs)) where
    readPrec =
        parens $ prec app_prec $ do
            lift $ L.expect (Ident "pickN")
            lift $ L.expect (Punc "@")
            i <- lift L.readDecP
            Which n v <- whichReadPrec i 0 :: ReadPrec (Which (x ': xs))
            pure $ Which n v
      where
        app_prec = 10

------------------------------------------------------------------
instance NFData (Which '[]) where
    rnf = impossible

instance (Reduce (Which (x ': xs)) (Switcher (CaseFunc NFData) () (x ': xs))) =>
  NFData (Which (x ': xs)) where
    rnf x = switch x (CaseFunc @NFData rnf)

------------------------------------------------------------------

-- class AFunctor f c xs where
--     afmap :: c xs -> f xs -> f (CaseResults c xs)

-- | Terminating AFunctor instance for empty type list
instance AFunctor Which c '[] where
    afmap _ = impossible

-- | Recursive AFunctor instance for non empty type list
-- delegate afmap'ing the remainder to an instance of Collector' with one less type in the type list
instance ( Reiterate c (a ': as)
         , AFunctor Which c as
         , Case c (a ': as)
         ) =>
         AFunctor Which c (a ': as) where
    afmap c v = case trial0 v of
        Right a' -> Which 0 (unsafeCoerce (case' c a'))
        Left v' -> diversify0 (afmap (reiterate c) v')
    {-# INLINABLE afmap #-}
    -- This makes compiling tests a little faster than with no pragma

------------------------------------------------------------------

instance ATraversable Which c m '[] where
    atraverse _ = impossible

instance ( Reiterate (c m) (a ': as)
         , ATraversable Which c m as
         , Case (c m) (a ': as)
         ) =>
         ATraversable Which c m (a ': as) where
    atraverse c v = case trial0 v of
        Right a' -> Which 0 <$> unsafeCoerce (case' c a')
        Left v' -> unsafeCoerce . diversify0 <$> atraverse (reiterate c) v'
    {-# INLINABLE atraverse #-}

