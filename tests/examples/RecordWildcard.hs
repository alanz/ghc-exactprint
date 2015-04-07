{-# LANGUAGE RecordWildCards #-}


parseArgs =
  Args
        { equalProb = E `elem` opts
        , ..
        }
