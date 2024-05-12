import FooBarBaz -- non-existent import, check that we can still parse

save :: C -> IO ()
save state = saveFileDialog "Save file " (maybe Nothing (Just . (++) "*.") (filesuffix state)) $
             do \fileName ->
                    case onSaveCB state of
                      Nothing ->
                          return ()
                      Just callback ->
                          do
                            c <- callback
                            case c of
                              Nothing ->
                                   return ()
                              Just c' ->
                                  let realfn = maybe
                                                fileName
                                                (extendFileName fileName)
                                                (filesuffix state)
                                  in do
                                    L.writeFile realfn c'
                                    postGUIAsync $ labelSetText (View.statusL $ gui state) $ realfn ++ " Saved."
    where
      extendFileName fileName suffix = if isSuffixOf suffix fileName
                                         then fileName
                                         else fileName ++ "." ++ suffix
