
{-
-- this is ugly too: can't use Data.Complex because the qd bindings do
-- not implement some low-level functions properly, leading to obscure
-- crashes inside various Data.Complex functions...
data Complex c = {-# UNPACK #-} !c :+ {-# UNPACK #-} !c deriving (Read, Show, Eq)

-- complex number arithmetic, with extra strictness and cost-centres
instance Num c => Num (Complex c) where
  (!(a :+ b)) + (!(c :+ d)) = {-# SCC "C+" #-} ((a + c) :+ (b + d))
  (!(a :+ b)) - (!(c :+ d)) = {-# SCC "C-" #-} ((a - c) :+ (b - d))
  (!(a :+ b)) * (!(c :+ d)) = {-# SCC "C*" #-} ((a * c - b * d) :+ (a * d + b * c))
  negate !(a :+ b) = (-a) :+ (-b)
  abs x = error $ "Complex.abs: " ++ show x
  signum x = error $ "Complex.signum: " ++ show x
  fromInteger !x = fromInteger x :+ 0
-}

