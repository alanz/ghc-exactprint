{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import Data.Data


-- ---------------------------------------------------------------------

instance AnnotateP RdrName where
  annotateP l n = do
    case rdrName2String n of
      "[]" -> do
        addDeltaAnnotation AnnOpenS -- '['
        addDeltaAnnotation AnnCloseS -- ']'

-- ---------------------------------------------------------------------

class (Typeable ast) => AnnotateP ast where
  annotateP :: SrcSpan -> ast -> IO ()


type SrcSpan = Int
rdrName2String = undefined
type RdrName = String

data A = AnnOpenS | AnnCloseS

addDeltaAnnotation = undefined
