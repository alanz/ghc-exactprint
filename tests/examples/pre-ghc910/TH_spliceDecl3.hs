-- test splicing of reified and renamed data declarations
{-# LANGUAGE TemplateHaskell #-}

module TH_spliceDecl3
where

import Language.Haskell.TH
import TH_spliceDecl3_Lib

data T = C

$(do { TyConI d <- reify ''T; rename' d})

