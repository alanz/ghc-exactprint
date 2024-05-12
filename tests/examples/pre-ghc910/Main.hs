{-# LANGUAGE OverloadedLabels, FlexibleContexts, OverloadedStrings, RecursiveDo, TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
module Main where


main :: IO ()
main = do
  Gtk.applicationNew (Just "de.weltraumschlangen.reflex-test") []
    >>= maybe
    (die "Failed to initialize GTK")
    ( \application -> do
        ret <- runReflexGtk application (Just argv) $ do
          runGtk $ do
            #add mainWindow
            #packStart outerBox

    )


            -- #add mainWindow
            -- #packStart outerBox
