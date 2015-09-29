

-- :bounds narrowing 35
bndCom tenv args =
  do { (bound,size) <- getBounds fail args
     ; let get (s,m,ref) = do { n <- readRef ref; return(s++" = "++show n++ m)}
     ; if bound == ""
          then do { xs <- mapM get boundRef; warnM [Dl xs "\n"]}
          else case find (\ (nm,info,ref) -> nm==bound) boundRef of
                Just (_,_,ref) -> writeRef ref size
                Nothing -> fail ("Unknown bound '"++bound++"'")
     ; return tenv
     }
