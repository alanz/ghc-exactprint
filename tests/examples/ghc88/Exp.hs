{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Array.Accelerate.Utility.Lift.Exp (
   Unlift,
   Unlifted,
   Tuple,
   unlift,
   modify,
   modify2,
   modify3,
   modify4,
   Exp(Exp), expr, atom,
   unliftPair,
   unliftTriple,
   unliftQuadruple,
   asExp,
   mapFst,
   mapSnd,
   fst3,
   snd3,
   thd3,
   indexCons,
   ) where

import qualified Data.Array.Accelerate.Data.Complex as Complex
import qualified Data.Array.Accelerate as A
import Data.Complex (Complex((:+)))
import Data.Array.Accelerate ((:.)((:.)))

import qualified Data.Tuple.HT as Tuple
import Data.Tuple.HT (mapTriple)


{- |
This class simplifies untupling of expressions.
If you have a function

> g :: ((Exp a, Exp b), Exp (c,d)) -> (Exp e, Exp f)

you cannot apply it to an array @arr :: Array sh ((a,b),(c,d))@ using 'A.map'.
Here, the 'modify' function helps:

> modify ((expr,expr),expr) g :: Exp ((a,b),(c,d)) -> Exp (e,f)

The 'expr'-pattern tells, how deep the tuple shall be unlifted.
This way you can write:

> A.map
>    (Exp.modify ((expr,expr),expr) $ \((a,b), cd) -> g ((a,b), cd))
>    arr

'modify' is based on 'unlift'.
In contrast to 'A.unlift' it does not only unlift one level of tupels,
but is guided by an 'expr'-pattern.
In the example I have demonstrated,
how the pair @(a,b)@ is unlifted, but the pair @(c,d)@ is not.
For the result tuple, 'modify' simply calls 'A.lift'.
In contrast to 'A.unlift',
'A.lift' lifts over all tupel levels until it obtains a single 'Exp'.
-}
class
   (A.Elt (Tuple pattern), A.Plain (Unlifted pattern) ~ Tuple pattern) =>
      Unlift pattern where
   type Unlifted pattern
   type Tuple pattern
   unlift :: pattern -> A.Exp (Tuple pattern) -> Unlifted pattern

modify ::
   (A.Lift A.Exp a, Unlift pattern) =>
   pattern ->
   (Unlifted pattern -> a) ->
   A.Exp (Tuple pattern) -> A.Exp (A.Plain a)
modify p f = A.lift . f . unlift p

modify2 ::
   (A.Lift A.Exp a, Unlift patternA, Unlift patternB) =>
   patternA ->
   patternB ->
   (Unlifted patternA -> Unlifted patternB -> a) ->
   A.Exp (Tuple patternA) -> A.Exp (Tuple patternB) -> A.Exp (A.Plain a)
modify2 pa pb f a b = A.lift $ f (unlift pa a) (unlift pb b)

modify3 ::
   (A.Lift A.Exp a, Unlift patternA, Unlift patternB, Unlift patternC) =>
   patternA ->
   patternB ->
   patternC ->
   (Unlifted patternA -> Unlifted patternB -> Unlifted patternC -> a) ->
   A.Exp (Tuple patternA) -> A.Exp (Tuple patternB) ->
   A.Exp (Tuple patternC) -> A.Exp (A.Plain a)
modify3 pa pb pc f a b c =
   A.lift $ f (unlift pa a) (unlift pb b) (unlift pc c)

modify4 ::
   (A.Lift A.Exp a,
    Unlift patternA, Unlift patternB, Unlift patternC, Unlift patternD) =>
   patternA ->
   patternB ->
   patternC ->
   patternD ->
   (Unlifted patternA -> Unlifted patternB ->
    Unlifted patternC -> Unlifted patternD -> a) ->
   A.Exp (Tuple patternA) -> A.Exp (Tuple patternB) ->
   A.Exp (Tuple patternC) -> A.Exp (Tuple patternD) -> A.Exp (A.Plain a)
modify4 pa pb pc pd f a b c d =
   A.lift $ f (unlift pa a) (unlift pb b) (unlift pc c) (unlift pd d)


instance (A.Elt a) => Unlift (Exp a) where
   type Unlifted (Exp a) = A.Exp a
   type Tuple (Exp a) = a
   unlift _ = id

data Exp e = Exp

expr :: Exp e
expr = Exp

{-# DEPRECATED atom "use expr instead" #-}
-- | for compatibility with accelerate-utility-0.0
atom :: Exp e
atom = expr


instance (Unlift pa, Unlift pb) => Unlift (pa,pb) where
   type Unlifted (pa,pb) = (Unlifted pa, Unlifted pb)
   type Tuple (pa,pb) = (Tuple pa, Tuple pb)
   unlift (pa,pb) ab =
      (unlift pa $ A.fst ab, unlift pb $ A.snd ab)

instance
   (Unlift pa, Unlift pb, Unlift pc) =>
      Unlift (pa,pb,pc) where
   type Unlifted (pa,pb,pc) = (Unlifted pa, Unlifted pb, Unlifted pc)
   type Tuple (pa,pb,pc) = (Tuple pa, Tuple pb, Tuple pc)
   unlift (pa,pb,pc) =
      mapTriple (unlift pa, unlift pb, unlift pc) . A.unlift


instance (Unlift pa, A.Slice (Tuple pa), int ~ Exp Int) => Unlift (pa :. int) where
   type Unlifted (pa :. int) = Unlifted pa :. A.Exp Int
   type Tuple (pa :. int) = Tuple pa :. Int
   unlift (pa:.pb) ab =
      (unlift pa $ A.indexTail ab) :. (unlift pb $ A.indexHead ab)


instance (Unlift p) => Unlift (Complex p) where
   type Unlifted (Complex p) = Complex (Unlifted p)
   type Tuple (Complex p) = Complex (Tuple p)
   unlift (preal:+pimag) z =
      unlift preal (Complex.real z)
      :+
      unlift pimag (Complex.imag z)


unliftPair :: (A.Elt a, A.Elt b) => A.Exp (a,b) -> (A.Exp a, A.Exp b)
unliftPair = A.unlift

unliftTriple ::
   (A.Elt a, A.Elt b, A.Elt c) => A.Exp (a,b,c) -> (A.Exp a, A.Exp b, A.Exp c)
unliftTriple = A.unlift

unliftQuadruple ::
   (A.Elt a, A.Elt b, A.Elt c, A.Elt d) =>
   A.Exp (a,b,c,d) -> (A.Exp a, A.Exp b, A.Exp c, A.Exp d)
unliftQuadruple = A.unlift

asExp :: A.Exp a -> A.Exp a
asExp = id

mapFst ::
   (A.Elt a, A.Elt b, A.Elt c) =>
   (A.Exp a -> A.Exp b) -> A.Exp (a,c) -> A.Exp (b,c)
mapFst f = modify (expr,expr) $ \(a,c) -> (f a, c)

mapSnd ::
   (A.Elt a, A.Elt b, A.Elt c) =>
   (A.Exp b -> A.Exp c) -> A.Exp (a,b) -> A.Exp (a,c)
mapSnd f = modify (expr,expr) $ \(a,b) -> (a, f b)


fst3 ::
   (A.Elt a, A.Elt b, A.Elt c) =>
   A.Exp (a,b,c) -> A.Exp a
fst3 = modify (expr,expr,expr) Tuple.fst3

snd3 ::
   (A.Elt a, A.Elt b, A.Elt c) =>
   A.Exp (a,b,c) -> A.Exp b
snd3 = modify (expr,expr,expr) Tuple.snd3

thd3 ::
   (A.Elt a, A.Elt b, A.Elt c) =>
   A.Exp (a,b,c) -> A.Exp c
thd3 = modify (expr,expr,expr) Tuple.thd3



indexCons ::
   (A.Slice ix) => A.Exp ix -> A.Exp Int -> A.Exp (ix :. Int)
indexCons ix n = A.lift $ ix:.n

