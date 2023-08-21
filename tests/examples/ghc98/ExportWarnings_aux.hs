module ExportWarnings_aux (
    {-# WARNING "warn" #-} x,
    {-# WARNING  in    "x-custom-\72"  [ "is"  ,  "deprecated"] #-} S(S1),
  ) where
import ExportWarnings_base (x)

data S = S1 | S2
