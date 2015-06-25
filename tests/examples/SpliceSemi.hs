{-# LANGUAGE TemplateHaskell #-}


$(makePredicates ''TheType) ; $(makePredicatesNot ''TheType)
