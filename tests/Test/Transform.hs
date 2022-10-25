{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

 -- Many of the tests match on a specific expected value,the other patterns should trigger a fail
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# LANGUAGE LambdaCase #-}
module Test.Transform where

import Language.Haskell.GHC.ExactPrint
-- import Language.Haskell.GHC.ExactPrint.ExactPrint
import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Parsers
import Language.Haskell.GHC.ExactPrint.Utils

import GHC                       as GHC
import GHC.Data.Bag              as GHC
import GHC.Data.FastString       as GHC
import GHC.Types.Name.Occurrence as GHC
import GHC.Types.Name.Reader     as GHC

import Data.Generics as SYB

import System.FilePath
import Data.List

import Test.Common

import Test.HUnit

transformTestsTT :: LibDir -> Test
transformTestsTT libdir = TestLabel "transformTestsTT" $ TestList
  [
   mkTestModChange libdir addLocaLDecl5  "AddLocalDecl5.hs"
  ]

transformTests :: LibDir -> Test
transformTests libdir = TestLabel "transformation tests" $ TestList
  [
    TestLabel "Low level transformations"
       (TestList (transformLowLevelTests libdir))
  , TestLabel "High level transformations"
       (TestList (transformHighLevelTests libdir))
  ]

transformLowLevelTests :: LibDir -> [Test]
transformLowLevelTests libdir = [
    mkTestModChange libdir changeRenameCase1 "RenameCase1.hs"
  , mkTestModChange libdir changeLayoutLet2  "LayoutLet2.hs"
  , mkTestModChange libdir changeLayoutLet3  "LayoutLet3.hs"
  , mkTestModChange libdir changeLayoutLet3  "LayoutLet4.hs"
  , mkTestModChange libdir changeRename1     "Rename1.hs"
  , mkTestModChange libdir changeRename2     "Rename2.hs"
  , mkTestModChange libdir changeLayoutIn1   "LayoutIn1.hs"
  , mkTestModChange libdir changeLayoutIn3   "LayoutIn3.hs"
  , mkTestModChange libdir changeLayoutIn3   "LayoutIn3a.hs"
  , mkTestModChange libdir changeLayoutIn3   "LayoutIn3b.hs"
  , mkTestModChange libdir changeLayoutIn4   "LayoutIn4.hs"
  , mkTestModChange libdir changeLocToName   "LocToName.hs"
  , mkTestModChange libdir changeLetIn1      "LetIn1.hs"
  , mkTestModChange libdir changeWhereIn4    "WhereIn4.hs"
  , mkTestModChange libdir changeAddDecl     "AddDecl.hs"
  , mkTestModChange libdir changeLocalDecls  "LocalDecls.hs"
  , mkTestModChange libdir changeLocalDecls2 "LocalDecls2.hs"
  , mkTestModChange libdir changeWhereIn3a   "WhereIn3a.hs"
--  , mkTestModChange changeCifToCase  "C.hs"          "C"
  ]

mkTestModChange :: LibDir -> Changer -> FilePath -> Test
mkTestModChange libdir f file = mkTestMod libdir "expected" "transform" f file

mkTestModBad :: LibDir -> FilePath -> Test
mkTestModBad libdir file
  = mkTestMod libdir "bad" "failing" noChange file

mkTestModBadMD :: LibDir -> FilePath -> Test
mkTestModBadMD libdir file
  = mkTestMod libdir "bad" "failing" changeMakeDelta file

mkTestMod :: LibDir -> String -> FilePath -> Changer -> FilePath ->  Test
mkTestMod libdir suffix dir f fp =
  let basename       = testPrefix </> dir </> fp
      expected       = basename <.> suffix
      writeFailure   = writeFile (basename <.> "out")
  in
    TestCase (do r <- either (\(ParseFailure s) -> error (s ++ basename)) id
                        <$> genTest libdir f basename expected
                 writeFailure (debugTxt r)
                 assertBool fp (status r == Success))


-- ---------------------------------------------------------------------

-- | Check that balanceCommentsList is idempotent
changeWhereIn3a :: Changer
changeWhereIn3a _libdir (L l p) = do
  let decls0 = hsmodDecls p
      (decls,_,w) = runTransform (balanceCommentsList decls0)
      (_de0:_:de1:_d2:_) = decls
  debugM $ unlines w
  debugM $ "changeWhereIn3a:de1:" ++ showAst de1
  let p2 = p { hsmodDecls = decls}
  return (L l p2)

-- ---------------------------------------------------------------------

changeWhereIn3b :: Changer
changeWhereIn3b _libdir (L l p) = do
  let decls0 = hsmodDecls p
      (decls,_,w) = runTransform (balanceCommentsList decls0)
      (de0:_:de1:d2:_) = decls
      de0' = setEntryDP de0 (DifferentLine 2 0)
      de1' = setEntryDP de1 (DifferentLine 2 0)
      d2' = setEntryDP d2 (DifferentLine 2 0)
      decls' = d2':de1':de0':(tail decls)
  debugM $ unlines w
  debugM $ "changeWhereIn3b:de1':" ++ showAst de1'
  let p2 = p { hsmodDecls = decls'}
  return (L l p2)

-- ---------------------------------------------------------------------

-- | Add a local declaration with signature to LocalDecl, where there was no
-- prior local decl. So it adds a "where" annotation.
changeLocalDecls2 :: Changer
changeLocalDecls2 libdir top = do
  Right d@(L ld (ValD _ decl)) <- withDynFlags libdir (\df -> parseDecl df "decl" "nn = 2")
  Right s@(L ls (SigD _ sig))  <- withDynFlags libdir (\df -> parseDecl df "sig"  "nn :: Int")
  let decl' = setEntryDP (makeDeltaAst (L ld decl)) (DifferentLine 1 0)
  let  sig' = setEntryDP (makeDeltaAst (L ls  sig)) (SameLine 2)
  let (top',_,_w) = runTransform doAddLocal
      doAddLocal = everywhereM (mkM replaceLocalBinds) (makeDeltaAst top)
      replaceLocalBinds :: LMatch GhcPs (LHsExpr GhcPs)
                        -> Transform (LMatch GhcPs (LHsExpr GhcPs))
      replaceLocalBinds (L lm (Match ma mln pats (GRHSs _ rhs EmptyLocalBinds{}))) = do
        newSpan <- uniqueSrcSpanT
        let anc = (Anchor (rs newSpan) (MovedAnchor (DifferentLine 1 2)))
        let anc2 = (Anchor (rs newSpan) (MovedAnchor (DifferentLine 1 4)))
        let an = EpAnn anc
                        (AnnList (Just anc2) Nothing Nothing
                                 [AddEpAnn AnnWhere (EpaDelta (SameLine 0) [])] [])
                        emptyComments
        let decls = [s,d]
        let sortKey = captureOrder decls
        let binds = (HsValBinds an (ValBinds sortKey (listToBag $ [decl'])
                                    [sig']))
        return (L lm (Match ma mln pats (GRHSs emptyComments rhs binds)))
      replaceLocalBinds x = return x
  -- return (L l p')
  return top'

-- ---------------------------------------------------------------------

-- | Add a local declaration with signature to LocalDecl
changeLocalDecls :: Changer
changeLocalDecls libdir top = do
  Right s@(L ls (SigD _ sig))  <- withDynFlags libdir (\df -> parseDecl df "sig"  "nn :: Int")
  Right d@(L ld (ValD _ decl)) <- withDynFlags libdir (\df -> parseDecl df "decl" "nn = 2")
  let decl' = setEntryDP (makeDeltaAst (L ld decl)) (DifferentLine 1 0)
  let  sig' = setEntryDP (makeDeltaAst (L ls sig))  (SameLine 0)
  -- let (p',_,_w) = runTransform doAddLocal
  let (top',_,_w) = runTransform doAddLocal
      doAddLocal = everywhereM (mkM replaceLocalBinds) (makeDeltaAst top)
      replaceLocalBinds :: LMatch GhcPs (LHsExpr GhcPs)
                        -> Transform (LMatch GhcPs (LHsExpr GhcPs))
      replaceLocalBinds (L lm (Match an mln pats (GRHSs _ rhs (HsValBinds van (ValBinds _ binds sigs))))) = do
        let oldDecls = sortLocatedA $ map wrapDecl (bagToList binds) ++ map wrapSig sigs
        let decls = s:d:oldDecls
        let oldDecls' = captureLineSpacing oldDecls
        let oldBinds     = concatMap decl2Bind oldDecls'
            (os:oldSigs) = concatMap decl2Sig  oldDecls'
            os' = setEntryDP os (DifferentLine 2 0)
        let sortKey = captureOrder decls
        -- let (EpAnn anc (AnnList (Just (Anchor anc2 _)) a b c dd) cs) = van
        -- let van' = (EpAnn anc (AnnList (Just (Anchor anc2 (MovedAnchor (DifferentLine 1 5)))) a b c dd) cs)
        let (EpAnn anc (AnnList _ a b c dd) cs) = van
        let van' = (EpAnn anc (AnnList (Just (Anchor (anchor anc) (MovedAnchor (DifferentLine 1 4)))) a b c dd) cs)
        let binds' = (HsValBinds van'
                          (ValBinds sortKey
                                    (listToBag $ decl':oldBinds)
                                    (sig':os':oldSigs)))
        return (L lm (Match an mln pats (GRHSs emptyComments rhs binds')))
      replaceLocalBinds x = return x
  -- return (L l p')
  return top'

-- ---------------------------------------------------------------------

-- | Add a declaration to AddDecl
changeAddDecl :: Changer
changeAddDecl libdir top = do
  Right decl <- withDynFlags libdir (\df -> parseDecl df "<interactive>" "nn = n2")
  -- let decl' = setEntryDP decl (DifferentLine 2 0)
  let decl' = setEntryDP (makeDeltaAst decl) (DifferentLine 2 0)

  let (p',_,_) = runTransform doAddDecl
      -- doAddDecl = everywhereM (mkM replaceTopLevelDecls) (makeDeltaAst top)
      doAddDecl = everywhereM (mkM replaceTopLevelDecls) top
      replaceTopLevelDecls :: ParsedSource -> Transform ParsedSource
      replaceTopLevelDecls m = insertAtStart m decl'
  return p'

-- ---------------------------------------------------------------------

changeRenameCase1 :: Changer
changeRenameCase1 _libdir parsed = return (rename "bazLonger" [((3,15),(3,18))] parsed)

changeRenameCase2 :: Changer
changeRenameCase2 _libdir parsed = return (rename "fooLonger" [((3,1),(3,4))] parsed)

changeLayoutLet2 :: Changer
changeLayoutLet2 _libdir parsed = return (rename "xxxlonger" [((7,5),(7,8)),((8,24),(8,27))] parsed)

changeLocToName :: Changer
changeLocToName _libdir parsed = return (rename "LocToName.newPoint" [((20,1),(20,11)),((20,28),(20,38)),((24,1),(24,11))] parsed)

changeLayoutIn3 :: Changer
changeLayoutIn3 _libdir parsed = return (rename "anotherX" [((7,13),(7,14)),((7,37),(7,38)),((8,37),(8,38))] parsed)

changeLayoutIn4 :: Changer
changeLayoutIn4 _libdir parsed = return (rename "io" [((7,8),(7,13)),((7,28),(7,33))] parsed)

changeLayoutIn1 :: Changer
changeLayoutIn1 _libdir parsed = return (rename "square" [((7,17),(7,19)),((7,24),(7,26))] parsed)

changeRename1 :: Changer
changeRename1 _libdir parsed = return (rename "bar2" [((3,1),(3,4))] parsed)

changeRename2 :: Changer
changeRename2 _libdir parsed = return (rename "joe" [((2,1),(2,5))] parsed)

changeLayoutLet3 :: Changer
changeLayoutLet3 _libdir parsed = return (rename "xxxlonger" [((7,5),(7,8)),((9,14),(9,17))] parsed)

changeLayoutLet5 :: Changer
changeLayoutLet5 _libdir parsed = return (rename "x" [((7,5),(7,8)),((9,14),(9,17))] parsed)


rename :: (ExactPrint a, Data a) => String -> [(Pos, Pos)] -> a -> a
rename newNameStr spans' a
  = everywhere (mkT replaceRdr) (makeDeltaAst a)
  where
    newName = mkRdrUnqual (mkVarOcc newNameStr)

    cond :: SrcSpan -> Bool
    cond ln = ss2range ln `elem` spans'

    replaceRdr :: LocatedN RdrName -> LocatedN RdrName
    replaceRdr (L ln _)
        | cond (locA ln) = L ln newName
    replaceRdr x = x

-- ---------------------------------------------------------------------

changeWhereIn4 :: Changer
changeWhereIn4 _libdir parsed
  = return (everywhere (mkT replace) (makeDeltaAst parsed))
  where
    replace :: LocatedN RdrName -> LocatedN RdrName
    replace (L ln _n)
      | ss2range (locA ln) == ((12,16),(12,17)) = L ln (mkRdrUnqual (mkVarOcc "p_2"))
    replace x = x

-- ---------------------------------------------------------------------

changeLetIn1 :: Changer
changeLetIn1 _libdir parsed
  = return (everywhere (mkT replace) (makeDeltaAst parsed))
  where
    replace :: HsExpr GhcPs -> HsExpr GhcPs
    replace (HsLet an tkLet localDecls _ expr)
      =
         let (HsValBinds x (ValBinds xv bagDecls sigs)) = localDecls
             [l2,_l1] = map wrapDecl $ bagToList bagDecls
             bagDecls' = listToBag $ concatMap decl2Bind [l2]
             (L (SrcSpanAnn _ le) e) = expr
             a = (SrcSpanAnn (EpAnn (Anchor (realSrcSpan le) (MovedAnchor (SameLine 1))) mempty emptyComments) le)
             expr' = L a e
             tkIn' = L (TokenLoc (EpaDelta (DifferentLine 1 0) [])) HsTok
         in (HsLet an tkLet
                (HsValBinds x (ValBinds xv bagDecls' sigs)) tkIn' expr')

    replace x = x

-- ---------------------------------------------------------------------

transformHighLevelTests :: LibDir -> [Test]
transformHighLevelTests libdir =
  [
    mkTestModChange libdir addLocaLDecl1  "AddLocalDecl1.hs"
  , mkTestModChange libdir addLocaLDecl2  "AddLocalDecl2.hs"
  , mkTestModChange libdir addLocaLDecl3  "AddLocalDecl3.hs"
  , mkTestModChange libdir addLocaLDecl4  "AddLocalDecl4.hs"
  , mkTestModChange libdir addLocaLDecl5  "AddLocalDecl5.hs"
  , mkTestModChange libdir addLocaLDecl6  "AddLocalDecl6.hs"
  , mkTestModChange libdir addLocaLDecl7  "AddLocalDecl7.hs"

  , mkTestModChange libdir rmDecl1 "RmDecl1.hs"
  , mkTestModChange libdir rmDecl2 "RmDecl2.hs"
  , mkTestModChange libdir rmDecl3 "RmDecl3.hs"
  , mkTestModChange libdir rmDecl4 "RmDecl4.hs"
  , mkTestModChange libdir rmDecl5 "RmDecl5.hs"
  , mkTestModChange libdir rmDecl6 "RmDecl6.hs"

  -- Currently failing, arguable output
  -- , mkTestModChange libdir rmDecl7 "RmDecl7.hs"

  , mkTestModChange libdir rmTypeSig1 "RmTypeSig1.hs"
  , mkTestModChange libdir rmTypeSig2 "RmTypeSig2.hs"

  , mkTestModChange libdir addHiding1 "AddHiding1.hs"
  , mkTestModChange libdir addHiding2 "AddHiding2.hs"

  , mkTestModChange libdir cloneDecl1 "CloneDecl1.hs"
  ]

-- ---------------------------------------------------------------------

addLocaLDecl1 :: Changer
addLocaLDecl1 libdir top = do
  Right (L ld (ValD _ decl)) <- withDynFlags libdir (\df -> parseDecl df "decl" "nn = 2")
  let decl' = setEntryDP (L ld decl) (DifferentLine 1 5)
      doAddLocal = do
        let lp = makeDeltaAst top
        (de1:d2:d3:_) <- hsDecls lp
        (de1'',d2') <- balanceComments de1 d2
        (de1',_) <- modifyValD (getLocA de1'') de1'' $ \_m d -> do
          return ((wrapDecl decl' : d),Nothing)
        replaceDecls lp [de1', d2', d3]

  (lp',_,w) <- runTransformT doAddLocal
  debugM $ "addLocaLDecl1:" ++ intercalate "\n" w
  return lp'

-- ---------------------------------------------------------------------

addLocaLDecl2 :: Changer
addLocaLDecl2 libdir lp = do
  Right newDecl <- withDynFlags libdir (\df -> parseDecl df "decl" "nn = 2")
  let
      doAddLocal = do
         -- (de1:d2:_) <- hsDecls (makeDeltaAst lp)
         (de1:d2:_) <- hsDecls lp
         (de1'',d2') <- balanceComments de1 d2

         (parent',_) <- modifyValD (getLocA de1) de1'' $ \_m (d:ds) -> do
           newDecl' <- transferEntryDP' d (makeDeltaAst newDecl)
           let d' = setEntryDP d (DifferentLine 1 0)
           return ((newDecl':d':ds),Nothing)

         replaceDecls lp [parent',d2']

  (lp',_,_w) <- runTransformT doAddLocal
  debugM $ "log:[\n" ++ intercalate "\n" _w ++ "]log end\n"
  return lp'

-- ---------------------------------------------------------------------

addLocaLDecl3 :: Changer
addLocaLDecl3 libdir top = do
  Right newDecl <- withDynFlags libdir (\df -> parseDecl df "decl" "nn = 2")
  let
      doAddLocal = do
         let lp = makeDeltaAst top
         (de1:d2:_) <- hsDecls lp
         (de1'',d2') <- balanceComments de1 d2

         (parent',_) <- modifyValD (getLocA de1) de1'' $ \_m (d:ds) -> do
           let newDecl' = setEntryDP (makeDeltaAst newDecl) (DifferentLine 1 0)
           return (((d:ds) ++ [newDecl']),Nothing)

         replaceDecls (anchorEof lp) [parent',d2']

  (lp',_,_w) <- runTransformT doAddLocal
  debugM $ "log:[\n" ++ intercalate "\n" _w ++ "]log end\n"
  return lp'

-- ---------------------------------------------------------------------

addLocaLDecl4 :: Changer
addLocaLDecl4 libdir lp = do
  Right newDecl <- withDynFlags libdir (\df -> parseDecl df "decl" "nn = 2")
  Right newSig  <- withDynFlags libdir (\df -> parseDecl df "sig"  "nn :: Int")
  let
      doAddLocal = do
         (parent:ds) <- hsDecls (makeDeltaAst lp)

         let newDecl' = setEntryDP (makeDeltaAst newDecl) (DifferentLine 1 0)
         let newSig'  = setEntryDP (makeDeltaAst newSig)  (DifferentLine 1 5)

         (parent',_) <- modifyValD (getLocA parent) parent $ \_m decls -> do
           return ((decls++[newSig',newDecl']),Nothing)

         replaceDecls (anchorEof lp) (parent':ds)

  (lp',_,_w) <- runTransformT doAddLocal
  debugM $ "log:[\n" ++ intercalate "\n" _w ++ "]log end\n"
  return lp'

-- ---------------------------------------------------------------------

addLocaLDecl5 :: Changer
addLocaLDecl5 _libdir lp = do
  let
      doAddLocal = do
         decls <- hsDecls (makeDeltaAst lp)
         [s1,de1,d2,d3] <- balanceCommentsList decls

         let d3' = setEntryDP d3 (DifferentLine 2 0)

         (de1',_) <- modifyValD (getLocA de1) de1 $ \_m _decls -> do
           let d2' = setEntryDP d2 (DifferentLine 1 0)
           return ([d2'],Nothing)
         replaceDecls lp [s1,de1',d3']

  (lp',_,_w) <- runTransformT doAddLocal
  debugM $ "log:[\n" ++ intercalate "\n" _w ++ "]log end\n"
  return lp'

-- ---------------------------------------------------------------------

addLocaLDecl6 :: Changer
addLocaLDecl6 libdir lp = do
  Right newDecl <- withDynFlags libdir (\df -> parseDecl df "decl" "x = 3")
  let
      newDecl' = setEntryDP (makeDeltaAst newDecl) (DifferentLine 1 5)
      doAddLocal = do
        -- decls0 <- hsDecls (makeDeltaAst lp)
        decls0 <- hsDecls lp
        [de1'',d2] <- balanceCommentsList decls0

        let de1 = captureMatchLineSpacing de1''
        let L _ (ValD _ (FunBind _ _ (MG _ (L _ ms) _) _)) = de1
        let [ma1,_ma2] = ms

        (de1',_) <- modifyValD (getLocA ma1) de1 $ \_m decls -> do
           return ((newDecl' : decls),Nothing)
        replaceDecls lp [de1', d2]

  (lp',_,_w) <- runTransformT doAddLocal
  debugM $ "log:[\n" ++ intercalate "\n" _w ++ "]log end\n"
  return lp'

addLocaLDecl7 :: Changer
addLocaLDecl7 libdir top = do
  Right (L ld (ValD _ decl)) <- withDynFlags libdir (\df -> parseDecl df "decl" "nn = 2")
  let decl' = setEntryDP (L ld decl) (DifferentLine 1 5)
      doAddLocal = do
        let lp = makeDeltaAst top
        ds <- balanceCommentsList =<< hsDecls lp
        ds' <- flip mapM ds $ \d -> do
          (d',_) <- modifyValD (getLocA d) d $ \_m ds -> do
            pure (prependDecl (wrapDecl decl') ds, Nothing)
          pure d'
        replaceDecls lp ds'
  (lp',_,w) <- runTransformT doAddLocal
  debugM $ "addLocaLDecl7:" ++ intercalate "\n" w
  return lp'

prependDecl :: LHsDecl GhcPs -> [LHsDecl GhcPs] -> [LHsDecl GhcPs]
prependDecl ldecl = \case
  [] -> [setEntryDP ldecl (DifferentLine 1 2)]
  ld1:lds -> ldecl':ld1'':lds
    where
      (ancOp, ld1'') = case ld1 of
        (L (SrcSpanAnn (EpAnn _ _ (EpaComments _)) _) _) ->
          error "Unexpected unbalanced comments"
        (L (SrcSpanAnn (EpAnn d1Anc d1Ann (EpaCommentsBalanced (L (Anchor c1Rss cAnc) c1:restCs) d1AfterCs)) ss) d1) ->
          -- NOTE cannot use setEntryDP to simply assign `DL 1 0` here because when there is no prior decl, the
          --      DeltaPos on the declaration is absolute instead of relative, and so we must manually update the
          --      DeltaPos to be relative (since there is about to be a prior declaration).
          let ld1' = L
                (SrcSpanAnn
                  (EpAnn
                    (setAnchorDp d1Anc $ DifferentLine 1 0)
                    d1Ann
                    (EpaCommentsBalanced (L (Anchor c1Rss $ MovedAnchor $ DifferentLine 1 0) c1:restCs) d1AfterCs))
                  ss)
                d1
          in (cAnc, ld1')
        (L (SrcSpanAnn (EpAnn (Anchor d1Rss d1AncOp) d1Ann epaCs@(EpaCommentsBalanced [] _)) ss) d1) ->
          let ld1' = L (SrcSpanAnn (EpAnn (Anchor d1Rss $ MovedAnchor $ DifferentLine 1 0) d1Ann epaCs) ss) d1
          in (d1AncOp, ld1')
        L (SrcSpanAnn EpAnnNotUsed _) _ -> error "Unexpected EpAnnNotUsed"
      ldecl' = setEntryDP ldecl (getAnchorOpDp ancOp)

getAnchorDp :: Anchor -> DeltaPos
getAnchorDp (Anchor _ (MovedAnchor dp)) = dp
getAnchorDp (Anchor _ UnchangedAnchor) = error "Unexpected UnchangedAnchor"

setAnchorDp :: Anchor -> DeltaPos -> Anchor
setAnchorDp (Anchor rss _) dp = Anchor rss (MovedAnchor dp)

getAnchorOpDp :: AnchorOperation -> DeltaPos
getAnchorOpDp (MovedAnchor dp) = dp
getAnchorOpDp UnchangedAnchor = error "Unexpected UnchangedAnchor"

-- ---------------------------------------------------------------------

rmDecl1 :: Changer
rmDecl1 _libdir lp = do
  let doRmDecl = do
         tlDecs0 <- hsDecls lp
         tlDecs <- balanceCommentsList tlDecs0
         let (de1:_s1:_d2:d3:ds) = tlDecs
         let d3' = setEntryDP d3 (DifferentLine 2 0)

         replaceDecls lp (de1:d3':ds)

  (lp',_,_w) <- runTransformT doRmDecl
  debugM $ "log:[\n" ++ intercalate "\n" _w ++ "]log end\n"
  return lp'

-- ---------------------------------------------------------------------

rmDecl2 :: Changer
rmDecl2 _libdir lp = do
  let
      doRmDecl = do
        let
          go :: GHC.LHsExpr GhcPs -> Transform (GHC.LHsExpr GhcPs)
          go e@(GHC.L _ (GHC.HsLet{})) = do
            decs0 <- hsDecls e
            decs <- balanceCommentsList $ captureLineSpacing decs0
            e' <- replaceDecls e (init decs)
            return e'
          go x = return x

        everywhereM (mkM go) (makeDeltaAst lp)

  let (lp',_,_w) = runTransform doRmDecl
  debugM $ "log:[\n" ++ intercalate "\n" _w ++ "]log end\n"
  return lp'
-- ---------------------------------------------------------------------

rmDecl3 :: Changer
rmDecl3 _libdir lp = do
  let
      doRmDecl = do
         -- [de1,d2] <- hsDecls (makeDeltaAst lp)
         [de1,d2] <- hsDecls lp

         (de1',Just sd1) <- modifyValD (getLocA de1) de1 $ \_m [sd1] -> do
           let sd1' = setEntryDP sd1 (DifferentLine 2 0)
           return ([],Just sd1')

         replaceDecls lp [de1',sd1,d2]

  (lp',_,_w) <- runTransformT doRmDecl
  debugM $ "log:[\n" ++ intercalate "\n" _w ++ "]log end\n"
  return lp'
-- ---------------------------------------------------------------------

rmDecl4 :: Changer
rmDecl4 _libdir lp = do
  let
      doRmDecl = do
         let lpd = (makeDeltaAst lp)
         [de1] <- hsDecls lpd

         (de1',Just sd1) <- modifyValD (getLocA de1) de1 $ \_m [sd1,sd2] -> do
           sd2' <- transferEntryDP' sd1 sd2

           let sd1' = setEntryDP sd1 (DifferentLine 2 0)
           return ([sd2'],Just sd1')

         replaceDecls (anchorEof lpd) [de1',sd1]

  (lp',_,_w) <- runTransformT doRmDecl
  debugM $ "log:[\n" ++ intercalate "\n" _w ++ "]log end\n"
  return lp'

-- ---------------------------------------------------------------------

rmDecl5 :: Changer
rmDecl5 _libdir lp = do
  let
      doRmDecl = do
        let
          go :: HsExpr GhcPs -> Transform (HsExpr GhcPs)
          go (HsLet a tkLet lb tkIn expr) = do
            decs <- hsDeclsValBinds lb
            let dec = last decs
            -- _ <- transferEntryDPT (head decs) dec
            lb' <- replaceDeclsValbinds WithoutWhere lb [dec]
            return (HsLet a tkLet lb' tkIn expr)
          go x = return x

        everywhereM (mkM go) lp

  let (lp',_,_w) = runTransform doRmDecl
  debugM $ "log:[\n" ++ intercalate "\n" _w ++ "]log end\n"
  return lp'

-- ---------------------------------------------------------------------

rmDecl6 :: Changer
rmDecl6 _libdir lp = do
  let
      doRmDecl = do
         [de1] <- hsDecls (makeDeltaAst lp)

         (de1',_) <- modifyValD (getLocA de1) de1 $ \_m subDecs -> do
           let (ss1:_sd1:sd2:sds) = subDecs
           sd2' <- transferEntryDP' ss1 sd2

           return (sd2':sds,Nothing)

         replaceDecls lp [de1']

  (lp',_,_w) <- runTransformT doRmDecl
  debugM $ "log:[\n" ++ intercalate "\n" _w ++ "]log end\n"
  return lp'
-- ---------------------------------------------------------------------

rmDecl7 :: Changer
rmDecl7 _libdir lp = do
  let
      doRmDecl = do
         -- tlDecs <- hsDecls (makeDeltaAst lp)
         tlDecs <- hsDecls lp
         [s1,de1,d2,d3] <- balanceCommentsList tlDecs

         d3' <- transferEntryDP' d2 d3

         replaceDecls lp [s1,de1,d3']

  (lp',_,_w) <- runTransformT doRmDecl
  debugM $ "log:[\n" ++ intercalate "\n" _w ++ "]log end\n"
  return lp'
-- ---------------------------------------------------------------------

rmTypeSig1 :: Changer
rmTypeSig1 _libdir lp = do
  let doRmDecl = do
         -- tlDecs <- hsDecls (makeDeltaAst lp)
         tlDecs <- hsDecls lp
         let (s0:de1:d2) = tlDecs
             s1 = captureTypeSigSpacing s0
             (L l (SigD x1 (TypeSig x2 [n1,n2] typ))) = s1
         n2' <- transferEntryDP n1 n2
         let s1' = (L l (SigD x1 (TypeSig x2 [n2'] typ)))
         replaceDecls lp (s1':de1:d2)

  let (lp',_,_w) = runTransform doRmDecl
  debugM $ "log:[\n" ++ intercalate "\n" _w ++ "]log end\n"
  return lp'

-- ---------------------------------------------------------------------

rmTypeSig2 :: Changer
rmTypeSig2 _libdir lp = do
  let doRmDecl = do
         -- tlDecs <- hsDecls (makeDeltaAst lp)
         tlDecs <- hsDecls lp
         let [de1] = tlDecs

         (de1',_) <- modifyValD (getLocA de1) de1 $ \_m [_s,d] -> do
           return ([d],Nothing)
         replaceDecls lp [de1']

  let (lp',_,_w) = runTransform doRmDecl
  debugM $ "log:[\n" ++ intercalate "\n" _w ++ "]log end\n"
  return lp'

-- ---------------------------------------------------------------------

addHiding1 :: Changer
addHiding1 _libdir (L l p) = do
  let doTransform = do
        l0 <- uniqueSrcSpanT
        l1 <- uniqueSrcSpanT
        l2 <- uniqueSrcSpanT
        let
          [L li imp1,imp2] = hsmodImports p
          n1 = L (noAnnSrcSpanDP0 l1) (mkVarUnqual (mkFastString "n1"))
          n2 = L (noAnnSrcSpanDP0 l2) (mkVarUnqual (mkFastString "n2"))
          v1 = L (addComma $ noAnnSrcSpanDP0 l1) (IEVar noExtField (L (noAnnSrcSpanDP0 l1) (IEName n1)))
          v2 = L (           noAnnSrcSpanDP0 l2) (IEVar noExtField (L (noAnnSrcSpanDP0 l2) (IEName n2)))
          impHiding = L (SrcSpanAnn (EpAnn (Anchor (realSrcSpan l0) m0)
                                     (AnnList Nothing
                                              (Just (AddEpAnn AnnOpenP  d1))
                                              (Just (AddEpAnn AnnCloseP d0))
                                              [(AddEpAnn AnnHiding d1)]
                                              [])
                                       emptyComments) l0) [v1,v2]
          imp1' = imp1 { ideclHiding = Just (True,impHiding)}
          p' = p { hsmodImports = [L li imp1',imp2]}
        return (L l p')

  let (lp',_,_w) = runTransform doTransform
  debugM $ "log:[\n" ++ intercalate "\n" _w ++ "]log end\n"
  return lp'
-- ---------------------------------------------------------------------

addHiding2 :: Changer
addHiding2 _libdir (L l p) = do
  let doTransform = do
        l1 <- uniqueSrcSpanT
        l2 <- uniqueSrcSpanT
        let
          [L li imp1] = hsmodImports p
          Just (_,L lh ns) = ideclHiding imp1
          lh' = (SrcSpanAnn (EpAnn (Anchor (realSrcSpan (locA lh)) m1)
                                     (AnnList Nothing
                                              (Just (AddEpAnn AnnOpenP  d1))
                                              (Just (AddEpAnn AnnCloseP d0))
                                              [(AddEpAnn AnnHiding d0)]
                                              [])
                                       emptyComments) (locA lh))
          n1 = L (noAnnSrcSpanDP0 l1) (mkVarUnqual (mkFastString "n1"))
          n2 = L (noAnnSrcSpanDP0 l2) (mkVarUnqual (mkFastString "n2"))
          v1 = L (addComma $ noAnnSrcSpanDP0 l1) (IEVar noExtField (L (noAnnSrcSpanDP0 l1) (IEName n1)))
          v2 = L (           noAnnSrcSpanDP0 l2) (IEVar noExtField (L (noAnnSrcSpanDP0 l2) (IEName n2)))
          L ln n = last ns
          n' = L (addComma ln) n
          imp1' = imp1 { ideclHiding = Just (True,L lh' (init ns ++ [n',v1,v2]))}
          p' = p { hsmodImports = [L li imp1']}
        return (L l p')

  let (lp',_,_w) = runTransform doTransform
  debugM $ "log:[\n" ++ intercalate "\n" _w ++ "]log end\n"
  return lp'

-- ---------------------------------------------------------------------

cloneDecl1 :: Changer
cloneDecl1 _libdir lp = do
  let doChange = do
         tlDecs <- hsDecls (makeDeltaAst lp)
         let (d1':d2:ds) = tlDecs
         -- d2' <- fst <$> cloneT d2
         let d2' = d2
         let d2'' = setEntryDP d2' (DifferentLine 2 0)
         replaceDecls lp (d1':d2:d2'':ds)

  let (lp',_,_w) = runTransform doChange
  return lp'

-- ---------------------------------------------------------------------
