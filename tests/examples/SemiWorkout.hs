{-# LANGUAGE KindSignatures
           , GADTs
           , ScopedTypeVariables
           , PatternSignatures
           , MultiParamTypeClasses
           , FunctionalDependencies
           , FlexibleInstances
           , UndecidableInstances
           , TypeFamilies
           , FlexibleContexts
           #-}

instance forall init prog prog' fromO fromI progOut progIn
                sessionsToIdxMe sessionsToIdxThem idxsToPairStructsMe idxsToPairStructsThem
                keyToIdxMe idxToValueMe keyToIdxMe' idxToValueMe' idxOfThem current current' invertedSessionsMe invertedSessionsThem .
    ( ProgramToMVarsOutgoingT prog prog ~ progOut
    , ProgramToMVarsOutgoingT prog' prog' ~ progIn
    , SWellFormedConfig init (D0 E) prog
    , SWellFormedConfig init (D0 E) prog'
    , TyListIndex progOut init (MVar (ProgramCell (Cell fromO)))
    , TyListIndex progIn init (MVar (ProgramCell (Cell fromI)))
    , TyListIndex prog init current'
    , Expand prog current' current
    , MapLookup (TyMap sessionsToIdxMe idxsToPairStructsMe) init
                    (MVar (Map (RawPid, RawPid) (MVar (PairStruct init prog prog' ((Cons (Jump init) Nil), (Cons (Jump init) Nil), (Cons (Jump init) Nil))))))
    , TyListMember invertedSessionsThem init True
    , MapSize (TyMap keyToIdxMe idxToValueMe) idxOfThem
    , MapInsert (TyMap keyToIdxMe idxToValueMe) idxOfThem
                    (SessionState prog prog' (current, fromO, fromI)) (TyMap keyToIdxMe' idxToValueMe')
    ) =>
    CreateSession False init prog prog'
                  sessionsToIdxMe sessionsToIdxThem idxsToPairStructsMe idxsToPairStructsThem
                  keyToIdxMe idxToValueMe keyToIdxMe' idxToValueMe' idxOfThem invertedSessionsMe invertedSessionsThem where
                      createSession init FF (Pid remotePid _) =
                          InterleavedChain $
                              \ipid@(IPid (Pid localPid localSTMap) _) mp ->
                                  do { let pidFuncMapMVar :: MVar (Map (RawPid, RawPid)
                                                                       (MVar (PairStruct init prog prog'
                                                                              ((Cons (Jump init) Nil), (Cons (Jump init) Nil), (Cons (Jump init) Nil)))))
                                               = mapLookup localSTMap init
                                     ; pidFuncMap <- takeMVar pidFuncMapMVar
                                     ; emptyMVar :: MVar (TyMap keyToIdxMe' idxToValueMe') <- newEmptyMVar
                                     ; psMVar :: MVar (PairStruct init prog prog' ((Cons (Jump init) Nil), (Cons (Jump init) Nil), (Cons (Jump init) Nil)))
                                              <- case Map.lookup (localPid, remotePid) pidFuncMap of
                                                   Nothing
                                                       -> do { empty <- newEmptyMVar
                                                             ; putMVar pidFuncMapMVar (Map.insert (localPid, remotePid) empty pidFuncMap)
                                                             ; return empty
                                                             }
                                                   (Just mv)
                                                       -> do { putMVar pidFuncMapMVar pidFuncMap
                                                             ; return mv
                                                             }
                                     ; let idxOfThem :: idxOfThem = mapSize mp
                                           ps :: PairStruct init prog prog' ((Cons (Jump init) Nil), (Cons (Jump init) Nil), (Cons (Jump init) Nil))
                                              = PS localPid (f idxOfThem mp emptyMVar)
                                     ; putMVar psMVar ps
                                     ; mp' <- takeMVar emptyMVar
                                     ; return (idxOfThem, mp', ipid)
                                     }
