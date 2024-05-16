-- test splicing of a generated data declarations
{-# LANGUAGE TemplateHaskell #-}

module TH_spliceDecl1
where

import Language.Haskell.TH


-- splice a simple data declaration
$(return [DataD [] (mkName "T") [] Nothing [NormalC (mkName "C") []] []])

