{-# LANGUAGE MultiWayIf #-}


instance Animatable Double where
    interpolate ease from to t =
        if | t <= 0 -> from
           | t >= 1 -> to
           | otherwise -> from + easeDouble ease t * (to - from)
    animAdd = (+)
    animSub = (-)
    animZero = 0
