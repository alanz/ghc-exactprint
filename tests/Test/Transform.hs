{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
module Test.Transform where

import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Parsers
import Language.Haskell.GHC.ExactPrint.Utils

import GHC                       as GHC
import GHC.Data.Bag              as GHC
import GHC.Data.FastString       as GHC
import GHC.Types.Name.Occurrence as GHC
import GHC.Types.Name.Reader     as GHC
-- import GHC.Types.SrcLoc          as GHC

import Data.Generics as SYB

import System.FilePath
-- import qualified Data.Map as Map
import Data.List
-- import Data.Maybe

-- import Data.Data

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

-- changeWhereIn3a :: Changer
-- changeWhereIn3a ans (GHC.L l p) = do
--   let decls = GHC.hsmodDecls p
--          -- (GHC.L _ (GHC.SigD sig))    = head $ drop 1 decls
--       d1 = head $ drop 2 decls
--       d2 = head $ drop 3 decls
--   let (_p1,(ans',_),_w) = runTransform ans (balanceComments d1 d2)
--   let p2 = p { GHC.hsmodDecls = d2:d1:decls}
--   return (ans',GHC.L l p2)

-- | Check that balanceCommentsList is idempotent
changeWhereIn3a :: Changer
changeWhereIn3a _libdir (L l p) = do
  let decls0 = hsmodDecls p
      (decls,(_,_),w) = runTransform mempty (balanceCommentsList decls0)
      (_de0:_:de1:_d2:_) = decls
  debugM $ unlines w
  debugM $ "changeWhereIn3a:de1:" ++ showAst de1
  let p2 = p { hsmodDecls = decls}
  return (L l p2)

-- ---------------------------------------------------------------------

changeWhereIn3b :: Changer
changeWhereIn3b _libdir (L l p) = do
  let decls0 = hsmodDecls p
      (decls,(_,_),w) = runTransform mempty (balanceCommentsList decls0)
      (de0:_:de1:d2:_) = decls
      de0' = setEntryDP' de0 (DifferentLine 2 0)
      de1' = setEntryDP' de1 (DifferentLine 2 0)
      d2' = setEntryDP' d2 (DifferentLine 2 0)
      decls' = d2':de1':de0':(tail decls)
  debugM $ unlines w
  debugM $ "changeWhereIn3b:de1':" ++ showAst de1'
  let p2 = p { hsmodDecls = decls'}
  return (L l p2)

-- ---------------------------------------------------------------------

-- -- | Add a local declaration with signature to LocalDecl, where there was no
-- -- prior local decl. So it adds a "where" annotation.
-- changeLocalDecls2 :: Changer
-- changeLocalDecls2 ans (GHC.L l p) = do
--   Right (declAnns, d@(GHC.L ld (GHC.ValD _ decl))) <- withDynFlags (\df -> parseDecl df "decl" "nn = 2")
--   Right (sigAnns, s@(GHC.L ls (GHC.SigD _ sig)))   <- withDynFlags (\df -> parseDecl df "sig"  "nn :: Int")
--   let declAnns' = setPrecedingLines (GHC.L ld decl) 1 0 declAnns
--   let  sigAnns' = setPrecedingLines (GHC.L ls  sig) 1 4 sigAnns
--   -- putStrLn $ "changeLocalDecls:sigAnns=" ++ show sigAnns
--   -- putStrLn $ "changeLocalDecls:declAnns=" ++ show declAnns
--   -- putStrLn $ "\nchangeLocalDecls:sigAnns'=" ++ show sigAnns'
--   let (p',(ans',_),_w) = runTransform ans doAddLocal
--       doAddLocal = SYB.everywhereM (SYB.mkM replaceLocalBinds) p
--       replaceLocalBinds :: GHC.LMatch GHC.GhcPs (GHC.LHsExpr GHC.GhcPs)
--                         -> Transform (GHC.LMatch GHC.GhcPs (GHC.LHsExpr GHC.GhcPs))
--       replaceLocalBinds m@(GHC.L lm (GHC.Match _ mln pats (GHC.GRHSs _ rhs (GHC.L _ GHC.EmptyLocalBinds{})))) = do
--         newSpan <- uniqueSrcSpanT
--         let
--           newAnnKey = AnnKey (rs newSpan) (CN "HsValBinds")
--           addWhere mkds =
--             case Map.lookup (mkAnnKey m) mkds of
--               Nothing -> error "wtf"
--               Just ann -> Map.insert newAnnKey ann2 mkds2
--                 where
--                   ann1 = ann { annsDP = annsDP ann ++ [(G GHC.AnnWhere,DP (1,2))]
--                              , annCapturedSpan = Just newAnnKey
--                              , annSortKey = Just [rs ls, rs ld]
--                              }
--                   mkds2 = Map.insert (mkAnnKey m) ann1 mkds
--                   ann2 = annNone
--                              { annEntryDelta     = DP (1,0) }
--         modifyAnnsT addWhere
--         let decls = [s,d]
--         -- logTr $ "(m,decls)=" ++ show (mkAnnKey m,map mkAnnKey decls)
--         modifyAnnsT (captureOrderAnnKey newAnnKey decls)
--         let binds = (GHC.HsValBinds noExt (GHC.ValBinds noExt (GHC.listToBag $ [GHC.L ld decl])
--                                     [GHC.L ls sig]))
--         bindSpan <- uniqueSrcSpanT
--         return (GHC.L lm (GHC.Match noExt mln pats (GHC.GRHSs noExt rhs (GHC.L bindSpan binds))))
--       replaceLocalBinds x = return x
--   -- putStrLn $ "log:" ++ intercalate "\n" w
--   return (mergeAnnList [declAnns',sigAnns',ans'],GHC.L l p')


-- | Add a local declaration with signature to LocalDecl, where there was no
-- prior local decl. So it adds a "where" annotation.
changeLocalDecls2 :: Changer
changeLocalDecls2 libdir (L l p) = do
  Right d@(L ld (ValD _ decl)) <- withDynFlags libdir (\df -> parseDecl df "decl" "nn = 2")
  Right s@(L ls (SigD _ sig))  <- withDynFlags libdir (\df -> parseDecl df "sig"  "nn :: Int")
  let decl' = setEntryDP' (L ld decl) (DifferentLine 1 0)
  let  sig' = setEntryDP' (L ls  sig) (SameLine 2)
  let (p',(_,_),_w) = runTransform mempty doAddLocal
      doAddLocal = everywhereM (mkM replaceLocalBinds) p
      replaceLocalBinds :: LMatch GhcPs (LHsExpr GhcPs)
                        -> Transform (LMatch GhcPs (LHsExpr GhcPs))
      replaceLocalBinds (L lm (Match ma mln pats (GRHSs _ rhs EmptyLocalBinds{}))) = do
        newSpan <- uniqueSrcSpanT
        let anc = (Anchor (rs newSpan) (MovedAnchor (DifferentLine 1 3)))
        let anc2 = (Anchor (rs newSpan) (MovedAnchor (DifferentLine 1 5)))
        let an = EpAnn anc
                        (AnnList (Just anc2) Nothing Nothing
                                 [(undeltaSpan (rs newSpan) AnnWhere (SameLine 0))] [])
                        emptyComments
        let decls = [s,d]
        let sortKey = captureOrder decls
        let binds = (HsValBinds an (ValBinds sortKey (listToBag $ [decl'])
                                    [sig']))
        return (L lm (Match ma mln pats (GRHSs noExtField rhs binds)))
      replaceLocalBinds x = return x
  return (L l p')

-- ---------------------------------------------------------------------

-- -- | Add a local declaration with signature to LocalDecl
-- changeLocalDecls :: Changer
-- changeLocalDecls ans (GHC.L l p) = do
--   Right (declAnns, d@(GHC.L ld (GHC.ValD _ decl))) <- withDynFlags (\df -> parseDecl df "decl" "nn = 2")
--   Right (sigAnns, s@(GHC.L ls (GHC.SigD _ sig)))   <- withDynFlags (\df -> parseDecl df "sig"  "nn :: Int")
--   let declAnns' = setPrecedingLines (GHC.L ld decl) 1 0 declAnns
--   let  sigAnns' = setPrecedingLines (GHC.L ls  sig) 1 4 sigAnns
--   -- putStrLn $ "changeLocalDecls:sigAnns=" ++ show sigAnns
--   -- putStrLn $ "changeLocalDecls:declAnns=" ++ show declAnns
--   -- putStrLn $ "\nchangeLocalDecls:sigAnns'=" ++ show sigAnns'
--   let (p',(ans',_),_w) = runTransform ans doAddLocal
--       doAddLocal = SYB.everywhereM (SYB.mkM replaceLocalBinds) p
--       replaceLocalBinds :: GHC.LMatch GHC.GhcPs (GHC.LHsExpr GHC.GhcPs)
--                         -> Transform (GHC.LMatch GHC.GhcPs (GHC.LHsExpr GHC.GhcPs))
--       replaceLocalBinds m@(GHC.L lm (GHC.Match _ mln pats (GHC.GRHSs _ rhs (GHC.L lb (GHC.HsValBinds _ (GHC.ValBinds _ binds sigs)))))) = do
--         a1 <- getAnnsT
--         a' <- case sigs of
--               []    -> return a1
--               (s1:_) -> do
--                 let a2 = setPrecedingLines s1 2 0 a1
--                 return a2
--         putAnnsT a'
--         let oldDecls = GHC.sortLocated $ map wrapDecl (GHC.bagToList binds) ++ map wrapSig sigs
--         let decls = s:d:oldDecls
--         -- logTr $ "(m,decls)=" ++ show (mkAnnKey m,map mkAnnKey decls)
--         modifyAnnsT (captureOrder m decls)
--         let binds' = (GHC.HsValBinds noExt
--                           (GHC.ValBinds noExt (GHC.listToBag $ (GHC.L ld decl):GHC.bagToList binds)
--                                           (GHC.L ls sig:sigs)))
--         return (GHC.L lm (GHC.Match noExt mln pats (GHC.GRHSs noExt rhs (GHC.L lb binds'))))
--       replaceLocalBinds x = return x
--   -- putStrLn $ "log:" ++ intercalate "\n" w
--   return (mergeAnnList [declAnns',sigAnns',ans'],GHC.L l p')


-- | Add a local declaration with signature to LocalDecl
changeLocalDecls :: Changer
changeLocalDecls libdir (L l p) = do
  Right s@(L ls (SigD _ sig))  <- withDynFlags libdir (\df -> parseDecl df "sig"  "nn :: Int")
  Right d@(L ld (ValD _ decl)) <- withDynFlags libdir (\df -> parseDecl df "decl" "nn = 2")
  let decl' = setEntryDP' (L ld decl) (DifferentLine 1 0)
  let  sig' = setEntryDP' (L ls sig)  (SameLine 0)
  let (p',(_,_),_w) = runTransform mempty doAddLocal
      doAddLocal = everywhereM (mkM replaceLocalBinds) p
      replaceLocalBinds :: LMatch GhcPs (LHsExpr GhcPs)
                        -> Transform (LMatch GhcPs (LHsExpr GhcPs))
      replaceLocalBinds (L lm (Match an mln pats (GRHSs _ rhs (HsValBinds van (ValBinds _ binds sigs))))) = do
        let oldDecls = sortLocatedA $ map wrapDecl (bagToList binds) ++ map wrapSig sigs
        let decls = s:d:oldDecls
        let oldDecls' = captureLineSpacing oldDecls
        let oldBinds     = concatMap decl2Bind oldDecls'
            (os:oldSigs) = concatMap decl2Sig  oldDecls'
            os' = setEntryDP' os (DifferentLine 2 0)
        let sortKey = captureOrder decls
        let (EpAnn anc (AnnList (Just (Anchor anc2 _)) a b c dd) cs) = van
        let van' = (EpAnn anc (AnnList (Just (Anchor anc2 (MovedAnchor (DifferentLine 1 5)))) a b c dd) cs)
        let binds' = (HsValBinds van'
                          (ValBinds sortKey
                                    (listToBag $ decl':oldBinds)
                                    (sig':os':oldSigs)))
        return (L lm (Match an mln pats (GRHSs noExtField rhs binds')))
      replaceLocalBinds x = return x
  return (L l p')

-- ---------------------------------------------------------------------

-- | Add a declaration to AddDecl
-- changeAddDecl :: Changer
-- changeAddDecl ans top = do
--   Right (declAnns, decl) <- withDynFlags (\df -> parseDecl df "<interactive>" "nn = n2")
--   -- putStrLn $ "changeDecl:(declAnns,decl)=" ++ showGhc (declAnns,decl)
--   let declAnns' = setPrecedingLines decl 2 0 declAnns
--   -- putStrLn $ "changeDecl:(declAnns',decl)=" ++ showGhc (declAnns',decl)

--   let (p',(ans',_),_) = runTransform ans doAddDecl
--       doAddDecl = SYB.everywhereM (SYB.mkM replaceTopLevelDecls) top
--       replaceTopLevelDecls :: GHC.ParsedSource -> Transform (GHC.ParsedSource)
--       replaceTopLevelDecls m = insertAtStart m decl
--   return (mergeAnns declAnns' ans',p')

-- | Add a declaration to AddDecl
changeAddDecl :: Changer
changeAddDecl libdir top = do
  Right decl <- withDynFlags libdir (\df -> parseDecl df "<interactive>" "nn = n2")
  let decl' = setEntryDP' decl (DifferentLine 2 0)

  let (p',(_,_),_) = runTransform mempty doAddDecl
      doAddDecl = everywhereM (mkM replaceTopLevelDecls) top
      replaceTopLevelDecls :: ParsedSource -> Transform ParsedSource
      replaceTopLevelDecls m = insertAtStart m decl'
  return p'
-- ---------------------------------------------------------------------

-- -- |Remove a decl with a trailing comment, and remove the trailing comment too
-- changeWhereIn3 :: Int -> Changer
-- changeWhereIn3 declIndex ans p = return (ans',p')
--   where
--     (p',(ans',_),_) = runTransform ans doTransform
--     doTransform = doRmDecl p

--     doRmDecl (GHC.L l (GHC.HsModule lo mmn mexp imps decls mdepr haddock)) = do
--       let
--         -- declIndex = 2 -- zero based
--         decls1 = take declIndex decls
--         decls2 = drop (declIndex + 1) decls
--         decls' = decls1 ++ decls2
--       return (GHC.L l (GHC.HsModule lo mmn mexp imps decls' mdepr haddock))
--       -- error $ "doRmDecl:decls2=" ++ showGhc (length decls,decls1,decls2)

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
-- changeLayoutIn3 parsed = rename "anotherX" [((7,13),(7,14)),((7,37),(7,38))] parsed

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


rename :: (Data a) => String -> [(Pos, Pos)] -> a -> a
rename newNameStr spans' a
  = everywhere (mkT replaceRdr) a
  where
    newName = mkRdrUnqual (mkVarOcc newNameStr)

    cond :: SrcSpan -> Bool
    cond ln = ss2range ln `elem` spans'

    replaceRdr :: LocatedN RdrName -> LocatedN RdrName
    replaceRdr (L ln _)
        | cond (locA ln) = L ln newName
    replaceRdr x = x


-- ---------------------------------------------------------------------

-- changeWhereIn4 :: Changer
-- changeWhereIn4 ans parsed
--   = return (ans,SYB.everywhere (SYB.mkT replace) parsed)
--   where
--     replace :: GHC.Located GHC.RdrName -> GHC.Located GHC.RdrName
--     replace (GHC.L ln _n)
--       | ss2range ln == ((12,16),(12,17)) = GHC.L ln (GHC.mkRdrUnqual (GHC.mkVarOcc "p_2"))
--     replace x = x

changeWhereIn4 :: Changer
changeWhereIn4 _libdir parsed
  = return (everywhere (mkT replace) parsed)
  where
    replace :: LocatedN RdrName -> LocatedN RdrName
    replace (L ln _n)
      | ss2range (locA ln) == ((12,16),(12,17)) = L ln (mkRdrUnqual (mkVarOcc "p_2"))
    replace x = x
-- ---------------------------------------------------------------------

-- changeLetIn1 :: Changer
-- changeLetIn1 ans parsed
--   = return (ans,SYB.everywhere (SYB.mkT replace) parsed)
--   where
--     replace :: GHC.HsExpr GHC.GhcPs -> GHC.HsExpr GHC.GhcPs
--     replace (GHC.HsLet _ (GHC.L lb localDecls) expr@(GHC.L _ _))
--       =
--          let (GHC.HsValBinds x (GHC.ValBinds xv bagDecls sigs)) = localDecls
--              bagDecls' = GHC.listToBag $ init $ GHC.bagToList bagDecls
--          in (GHC.HsLet noExt (GHC.L lb (GHC.HsValBinds x (GHC.ValBinds xv bagDecls' sigs))) expr)

--     replace x = x

changeLetIn1 :: Changer
changeLetIn1 _libdir parsed
  = return (everywhere (mkT replace) parsed)
  where
    replace :: HsExpr GhcPs -> HsExpr GhcPs
    replace (HsLet (EpAnn anc (AnnsLet l _i) cs) localDecls expr)
      =
         let (HsValBinds x (ValBinds xv bagDecls sigs)) = localDecls
             [l2,_l1] = map wrapDecl $ bagToList bagDecls
             bagDecls' = listToBag $ concatMap decl2Bind [l2]
             (L (SrcSpanAnn _ le) e) = expr
             a = (SrcSpanAnn (EpAnn (Anchor (realSrcSpan le) (MovedAnchor (SameLine 1))) mempty emptyComments) le)
             expr' = L a e
         in (HsLet (EpAnn anc (AnnsLet l (EpaDelta (DifferentLine 1 0))) cs)
                (HsValBinds x (ValBinds xv bagDecls' sigs)) expr')

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

  , mkTestModChange libdir rmDecl1 "RmDecl1.hs"
  , mkTestModChange libdir rmDecl2 "RmDecl2.hs"
  , mkTestModChange libdir rmDecl3 "RmDecl3.hs"
  , mkTestModChange libdir rmDecl4 "RmDecl4.hs"
  , mkTestModChange libdir rmDecl5 "RmDecl5.hs"
  , mkTestModChange libdir rmDecl6 "RmDecl6.hs"
  , mkTestModChange libdir rmDecl7 "RmDecl7.hs"

  , mkTestModChange libdir rmTypeSig1 "RmTypeSig1.hs"
  , mkTestModChange libdir rmTypeSig2 "RmTypeSig2.hs"

  , mkTestModChange libdir addHiding1 "AddHiding1.hs"
  , mkTestModChange libdir addHiding2 "AddHiding2.hs"

  , mkTestModChange libdir cloneDecl1 "CloneDecl1.hs"
  ]

-- ---------------------------------------------------------------------

-- addLocaLDecl1 :: Changer
-- addLocaLDecl1 ans lp = do
--   Right (declAnns, newDecl) <- withDynFlags (\df -> parseDecl df "decl" "nn = 2")
--   let declAnns' = setPrecedingLines newDecl 1 4 declAnns
--       doAddLocal = do
--         (d1:d2:_) <- hsDecls lp
--         balanceComments d1 d2
--         (d1',_) <- modifyValD (GHC.getLoc d1) d1 $ \_m d -> do
--           return ((newDecl : d),Nothing)
--         replaceDecls lp [d1', d2]

--   (lp',(ans',_),_w) <- runTransformT (mergeAnns ans declAnns') doAddLocal
--   -- putStrLn $ "log:\n" ++ intercalate "\n" _w
--   return (ans',lp')

addLocaLDecl1 :: Changer
addLocaLDecl1 libdir lp = do
  Right (L ld (ValD _ decl)) <- withDynFlags libdir (\df -> parseDecl df "decl" "nn = 2")
  let decl' = setEntryDP' (L ld decl) (DifferentLine 1 5)
      doAddLocal = do
        (de1:d2:d3:_) <- hsDecls lp
        (de1'',d2') <- balanceComments de1 d2
        (de1',_) <- modifyValD (getLocA de1'') de1'' $ \_m d -> do
          return ((wrapDecl decl' : d),Nothing)
        replaceDecls lp [de1', d2', d3]

  (lp',(_,_),w) <- runTransformT mempty doAddLocal
  debugM $ "addLocaLDecl1:" ++ intercalate "\n" w
  return lp'
-- ---------------------------------------------------------------------

-- addLocaLDecl2 :: Changer
-- addLocaLDecl2 ans lp = do
--   Right (declAnns, newDecl) <- withDynFlags (\df -> parseDecl df "decl" "nn = 2")
--   let
--       doAddLocal = do
--          tlDecs <- hsDecls lp
--          let parent = head tlDecs
--          balanceComments parent (head $ tail tlDecs)

--          (parent',_) <- modifyValD (GHC.getLoc parent) parent $ \_m decls -> do
--            transferEntryDPT (head decls) newDecl
--            setEntryDPT (head decls) (DP (1, 0))
--            return ((newDecl:decls),Nothing)

--          replaceDecls lp (parent':tail tlDecs)

--   let (lp',(ans',_),_w) = runTransform (mergeAnns ans declAnns) doAddLocal
--   -- putStrLn $ "log:\n" ++ intercalate "\n" _w
--   return (ans',lp')

addLocaLDecl2 :: Changer
addLocaLDecl2 libdir lp = do
  Right newDecl <- withDynFlags libdir (\df -> parseDecl df "decl" "nn = 2")
  let
      doAddLocal = do
         (de1:d2:_) <- hsDecls lp
         (de1'',d2') <- balanceComments de1 d2

         (parent',_) <- modifyValD (getLocA de1) de1'' $ \_m (d:ds) -> do
           newDecl' <- transferEntryDP' d newDecl
           let d' = setEntryDP' d (DifferentLine 1 0)
           return ((newDecl':d':ds),Nothing)

         replaceDecls lp [parent',d2']

  (lp',(_,_),_w) <- runTransformT mempty doAddLocal
  debugM $ "log:[\n" ++ intercalate "\n" _w ++ "]log end\n"
  return lp'

-- ---------------------------------------------------------------------

-- addLocaLDecl3 :: Changer
-- addLocaLDecl3 ans lp = do
--   Right (declAnns, newDecl) <- withDynFlags (\df -> parseDecl df "decl" "nn = 2")
--   let
--       doAddLocal = do
--          -- logDataWithAnnsTr "parsed:" lp
--          logDataWithAnnsTr "newDecl:" newDecl
--          tlDecs <- hsDecls lp
--          let parent = head tlDecs
--          balanceComments parent (head $ tail tlDecs)

--          (parent',_) <- modifyValD (GHC.getLoc parent) parent $ \m decls -> do
--            setPrecedingLinesT newDecl 1 0
--            moveTrailingComments m (last decls)
--            return ((decls++[newDecl]),Nothing)

--          replaceDecls lp (parent':tail tlDecs)

--   let (lp',(ans',_),_w) = runTransform (mergeAnns ans declAnns) doAddLocal
--   -- putStrLn $ "log\n" ++ intercalate "\n" _w
--   return (ans',lp')

addLocaLDecl3 :: Changer
addLocaLDecl3 libdir lp = do
  Right newDecl <- withDynFlags libdir (\df -> parseDecl df "decl" "nn = 2")
  let
      doAddLocal = do
         (de1:d2:_) <- hsDecls lp
         (de1'',d2') <- balanceComments de1 d2

         (parent',_) <- modifyValD (getLocA de1) de1'' $ \_m (d:ds) -> do
           let newDecl' = setEntryDP' newDecl (DifferentLine 1 0)
           return (((d:ds) ++ [newDecl']),Nothing)

         replaceDecls (anchorEof lp) [parent',d2']

  (lp',(_,_),_w) <- runTransformT mempty doAddLocal
  debugM $ "log:[\n" ++ intercalate "\n" _w ++ "]log end\n"
  return lp'

-- ---------------------------------------------------------------------

-- addLocaLDecl4 :: Changer
-- addLocaLDecl4 ans lp = do
--   Right (declAnns, newDecl) <- withDynFlags (\df -> parseDecl df "decl" "nn = 2")
--   Right (sigAnns, newSig)   <- withDynFlags (\df -> parseDecl df "sig"  "nn :: Int")
--   -- putStrLn $ "addLocaLDecl4:lp=" ++ showGhc lp
--   let
--       doAddLocal = do
--          tlDecs <- hsDecls lp
--          let parent = head tlDecs

--          setPrecedingLinesT newSig  1 0
--          setPrecedingLinesT newDecl 1 0

--          (parent',_) <- modifyValD (GHC.getLoc parent) parent $ \_m decls -> do
--            return ((decls++[newSig,newDecl]),Nothing)

--          replaceDecls lp (parent':tail tlDecs)

--   let (lp',(ans',_),_w) = runTransform (mergeAnnList [ans,declAnns,sigAnns]) doAddLocal
--   -- putStrLn $ "log\n" ++ intercalate "\n" _w
--   return (ans',lp')


addLocaLDecl4 :: Changer
addLocaLDecl4 libdir lp = do
  Right newDecl <- withDynFlags libdir (\df -> parseDecl df "decl" "nn = 2")
  Right newSig  <- withDynFlags libdir (\df -> parseDecl df "sig"  "nn :: Int")
  let
      doAddLocal = do
         (parent:ds) <- hsDecls lp

         let newDecl' = setEntryDP' newDecl (DifferentLine 1 0)
         let newSig'  = setEntryDP' newSig  (DifferentLine 1 5)

         (parent',_) <- modifyValD (getLocA parent) parent $ \_m decls -> do
           return ((decls++[newSig',newDecl']),Nothing)

         replaceDecls (anchorEof lp) (parent':ds)

  (lp',(_,_),_w) <- runTransformT mempty doAddLocal
  debugM $ "log:[\n" ++ intercalate "\n" _w ++ "]log end\n"
  return lp'

-- ---------------------------------------------------------------------

-- addLocaLDecl5 :: Changer
-- addLocaLDecl5 ans lp = do
--   let
--       doAddLocal = do
--          [s1,d1,d2,d3] <- hsDecls lp

--          transferEntryDPT d2 d3

--          (d1',_) <- modifyValD (GHC.getLoc d1) d1 $ \_m _decls -> do
--            return ([d2],Nothing)
--          replaceDecls lp [s1,d1',d3]

--   (lp',(ans',_),_w) <- runTransformT ans doAddLocal
--   -- putStrLn $ "log\n" ++ intercalate "\n" _w
--   return (ans',lp')

addLocaLDecl5 :: Changer
addLocaLDecl5 _libdir lp = do
  let
      doAddLocal = do
         decls <- hsDecls lp
         [s1,de1,d2,d3] <- balanceCommentsList decls

         let d3' = setEntryDP' d3 (DifferentLine 2 0)

         (de1',_) <- modifyValD (getLocA de1) de1 $ \_m _decls -> do
           let d2' = setEntryDP' d2 (DifferentLine 1 0)
           return ([d2'],Nothing)
         replaceDecls lp [s1,de1',d3']

  (lp',(_,_),_w) <- runTransformT mempty doAddLocal
  debugM $ "log:[\n" ++ intercalate "\n" _w ++ "]log end\n"
  return lp'

-- ---------------------------------------------------------------------

-- addLocaLDecl6 :: Changer
-- addLocaLDecl6 ans lp = do
--   Right (declAnns, newDecl) <- withDynFlags (\df -> parseDecl df "decl" "x = 3")
--   let declAnns' = setPrecedingLines newDecl 1 4 declAnns
--       doAddLocal = do
--         [d1,d2] <- hsDecls lp
--         balanceComments d1 d2

--         let GHC.L _ (GHC.ValD _ (GHC.FunBind _ _ (GHC.MG _ (GHC.L _ [m1,m2]) _) _)) = d1
--         balanceComments m1 m2

--         (d1',_) <- modifyValD (GHC.getLoc m1) d1 $ \_m decls -> do
--            return ((newDecl : decls),Nothing)
--         replaceDecls lp [d1', d2]

--   (lp',(ans',_),_w) <- runTransformT (mergeAnns ans declAnns') doAddLocal
--   -- putStrLn $ "log:\n" ++ intercalate "\n" _w
--   return (ans',lp')


addLocaLDecl6 :: Changer
addLocaLDecl6 libdir lp = do
  Right newDecl <- withDynFlags libdir (\df -> parseDecl df "decl" "x = 3")
  let
      newDecl' = setEntryDP' newDecl (DifferentLine 1 5)
      doAddLocal = do
        decls0 <- hsDecls lp
        [de1'',d2] <- balanceCommentsList decls0

        let de1 = captureMatchLineSpacing de1''
        let L _ (ValD _ (FunBind _ _ (MG _ (L _ ms) _) _)) = de1
        let [ma1,_ma2] = ms

        (de1',_) <- modifyValD (getLocA ma1) de1 $ \_m decls -> do
           return ((newDecl' : decls),Nothing)
        replaceDecls lp [de1', d2]

  (lp',(_,_),_w) <- runTransformT mempty doAddLocal
  debugM $ "log:[\n" ++ intercalate "\n" _w ++ "]log end\n"
  return lp'
-- ---------------------------------------------------------------------

-- rmDecl1 :: Changer
-- rmDecl1 ans lp = do
--   let doRmDecl = do
--          tlDecs <- hsDecls lp
--          let (d1:s1:d2:ds) = tlDecs

--          -- First delete the decl (d2) only
--          balanceComments s1 d2 -- ++AZ++
--          balanceComments d2 (head ds)
--          lp1 <- replaceDecls lp (d1:s1:ds)
--          -- return lp1

--          -- Then delete the sig separately
--          tlDecs1 <- hsDecls lp1
--          let (d1':s1':ds') = tlDecs1
--          -- transferEntryDPT s1' (head ds')  -- required in HaRe.
--          balanceComments d1' s1'
--          balanceComments s1' (head ds')
--          transferEntryDPT s1' (head ds')  -- required in HaRe.
--          replaceDecls lp (d1':ds')

--   (lp',(ans',_),_w) <- runTransformT ans doRmDecl
--   return (ans',lp')

rmDecl1 :: Changer
rmDecl1 _libdir lp = do
  let doRmDecl = do
         tlDecs0 <- hsDecls lp
         tlDecs <- balanceCommentsList $ captureLineSpacing tlDecs0
         let (de1:_s1:_d2:ds) = tlDecs

         replaceDecls lp (de1:ds)

  (lp',(_,_),_w) <- runTransformT mempty doRmDecl
  debugM $ "log:[\n" ++ intercalate "\n" _w ++ "]log end\n"
  return lp'

-- ---------------------------------------------------------------------

-- rmDecl2 :: Changer
-- rmDecl2 ans lp = do
--   let
--       doRmDecl = do
--         let
--           go :: GHC.LHsExpr GHC.GhcPs -> Transform (GHC.LHsExpr GHC.GhcPs)
--           go e@(GHC.L _ (GHC.HsLet{})) = do
--             decs <- hsDecls e
--             e' <- replaceDecls e (init decs)
--             return e'
--           go x = return x

--         SYB.everywhereM (SYB.mkM go) lp

--   let (lp',(ans',_),_w) = runTransform ans doRmDecl
--   -- putStrLn $ "log:\n" ++ intercalate "\n" _w
--   return (ans',lp')

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

        everywhereM (mkM go) lp

  let (lp',(_,_),_w) = runTransform mempty doRmDecl
  debugM $ "log:[\n" ++ intercalate "\n" _w ++ "]log end\n"
  return lp'
-- ---------------------------------------------------------------------

-- rmDecl3 :: Changer
-- rmDecl3 ans lp = do
--   let
--       doRmDecl = do
--          [d1,d2] <- hsDecls lp

--          (d1',Just sd1) <- modifyValD (GHC.getLoc d1) d1 $ \_m [sd1] -> do
--            setPrecedingLinesDeclT sd1 2 0
--            return ([],Just sd1)

--          replaceDecls lp [d1',sd1,d2]

--   (lp',(ans',_),_w) <- runTransformT ans doRmDecl
--   -- putStrLn $ "log:\n" ++ intercalate "\n" _w
--   return (ans',lp')

rmDecl3 :: Changer
rmDecl3 _libdir lp = do
  let
      doRmDecl = do
         [de1,d2] <- hsDecls lp

         (de1',Just sd1) <- modifyValD (getLocA de1) de1 $ \_m [sd1] -> do
           let sd1' = setEntryDP' sd1 (DifferentLine 2 0)
           return ([],Just sd1')

         replaceDecls lp [de1',sd1,d2]

  (lp',(_,_),_w) <- runTransformT mempty doRmDecl
  debugM $ "log:[\n" ++ intercalate "\n" _w ++ "]log end\n"
  return lp'
-- ---------------------------------------------------------------------

-- rmDecl4 :: Changer
-- rmDecl4 ans lp = do
--   let
--       doRmDecl = do
--          [d1] <- hsDecls lp

--          (d1',Just sd1) <- modifyValD (GHC.getLoc d1) d1 $ \_m [sd1,sd2] -> do
--            -- [sd1,sd2] <- hsDecls d1
--            transferEntryDPT sd1 sd2

--            setPrecedingLinesDeclT sd1 2 0
--            -- d1' <- replaceDecls d1 [sd2]
--            return ([sd2],Just sd1)

--          replaceDecls lp [d1',sd1]

--   (lp',(ans',_),_w) <- runTransformT ans doRmDecl
--   return (ans',lp')

rmDecl4 :: Changer
rmDecl4 _libdir lp = do
  let
      doRmDecl = do
         [de1] <- hsDecls lp

         (de1',Just sd1) <- modifyValD (getLocA de1) de1 $ \_m [sd1,sd2] -> do
           sd2' <- transferEntryDP' sd1 sd2

           let sd1' = setEntryDP' sd1 (DifferentLine 2 0)
           return ([sd2'],Just sd1')

         replaceDecls (anchorEof lp) [de1',sd1]

  (lp',(_,_),_w) <- runTransformT mempty doRmDecl
  debugM $ "log:[\n" ++ intercalate "\n" _w ++ "]log end\n"
  return lp'
-- ---------------------------------------------------------------------

-- rmDecl5 :: Changer
-- rmDecl5 ans lp = do
--   let
--       doRmDecl = do
--         let
--           go :: GHC.HsExpr GHC.GhcPs -> Transform (GHC.HsExpr GHC.GhcPs)
--           go (GHC.HsLet _ (GHC.L l lb) expr) = do
--             decs <- hsDeclsValBinds lb
--             let dec = last decs
--             transferEntryDPT (head decs) dec
--             lb' <- replaceDeclsValbinds lb [dec]
--             return (GHC.HsLet noExt (GHC.L l lb') expr)
--           go x = return x

--         SYB.everywhereM (SYB.mkM go) lp

--   let (lp',(ans',_),_w) = runTransform ans doRmDecl
--   -- putStrLn $ "log:" ++ intercalate "\n" _w
--   return (ans',lp')

rmDecl5 :: Changer
rmDecl5 _libdir lp = do
  let
      doRmDecl = do
        let
          go :: HsExpr GhcPs -> Transform (HsExpr GhcPs)
          go (HsLet a lb expr) = do
            decs <- hsDeclsValBinds lb
            let dec = last decs
            _ <- transferEntryDPT (head decs) dec
            lb' <- replaceDeclsValbinds WithoutWhere lb [dec]
            return (HsLet a lb' expr)
          go x = return x

        everywhereM (mkM go) lp

  let (lp',(_,_),_w) = runTransform mempty doRmDecl
  debugM $ "log:[\n" ++ intercalate "\n" _w ++ "]log end\n"
  return lp'

-- ---------------------------------------------------------------------

-- rmDecl6 :: Changer
-- rmDecl6 ans lp = do
--   let
--       doRmDecl = do
--          [d1] <- hsDecls lp

--          (d1',_) <- modifyValD (GHC.getLoc d1) d1 $ \_m subDecs -> do
--            let (ss1:_sd1:sd2:sds) = subDecs
--            transferEntryDPT ss1 sd2

--            return (sd2:sds,Nothing)

--          replaceDecls lp [d1']

--   (lp',(ans',_),_w) <- runTransformT ans doRmDecl
--   -- putStrLn $ "log:" ++ intercalate "\n" _w
--   return (ans',lp')

rmDecl6 :: Changer
rmDecl6 _libdir lp = do
  let
      doRmDecl = do
         [de1] <- hsDecls lp

         (de1',_) <- modifyValD (getLocA de1) de1 $ \_m subDecs -> do
           let (ss1:_sd1:sd2:sds) = subDecs
           sd2' <- transferEntryDP' ss1 sd2

           return (sd2':sds,Nothing)

         replaceDecls lp [de1']

  (lp',(_,_),_w) <- runTransformT mempty doRmDecl
  debugM $ "log:[\n" ++ intercalate "\n" _w ++ "]log end\n"
  return lp'
-- ---------------------------------------------------------------------

-- rmDecl7 :: Changer
-- rmDecl7 ans lp = do
--   let
--       doRmDecl = do
--          tlDecs <- hsDecls lp
--          let [s1,d1,d2,d3] = tlDecs

--          balanceComments d1 d2
--          balanceComments d2 d3

--          transferEntryDPT d2 d3

--          replaceDecls lp [s1,d1,d3]

--   let (lp',(ans',_),_w) = runTransform ans doRmDecl
--   -- putStrLn $ "log:" ++ intercalate "\n" _w
--   return (ans',lp')

rmDecl7 :: Changer
rmDecl7 _libdir lp = do
  let
      doRmDecl = do
         tlDecs <- hsDecls lp
         [s1,de1,d2,d3] <- balanceCommentsList tlDecs

         d3' <- transferEntryDP' d2 d3

         replaceDecls lp [s1,de1,d3']

  (lp',(_,_),_w) <- runTransformT mempty doRmDecl
  debugM $ "log:[\n" ++ intercalate "\n" _w ++ "]log end\n"
  return lp'
-- ---------------------------------------------------------------------

-- rmTypeSig1 :: Changer
-- rmTypeSig1 ans lp = do
--   let doRmDecl = do
--          tlDecs <- hsDecls lp
--          let (s1:d1:d2) = tlDecs
--              (GHC.L l (GHC.SigD x1 (GHC.TypeSig x2 names typ))) = s1
--              s1' = (GHC.L l (GHC.SigD x1 (GHC.TypeSig x2 (tail names) typ)))
--          replaceDecls lp (s1':d1:d2)

--   let (lp',(ans',_),_w) = runTransform ans doRmDecl
--   return (ans',lp')
rmTypeSig1 :: Changer
rmTypeSig1 _libdir lp = do
  let doRmDecl = do
         tlDecs <- hsDecls lp
         let (s0:de1:d2) = tlDecs
             s1 = captureTypeSigSpacing s0
             (L l (SigD x1 (TypeSig x2 [n1,n2] typ))) = s1
         n2' <- transferEntryDP n1 n2
         let s1' = (L l (SigD x1 (TypeSig x2 [n2'] typ)))
         replaceDecls lp (s1':de1:d2)

  let (lp',(_,_),_w) = runTransform mempty doRmDecl
  debugM $ "log:[\n" ++ intercalate "\n" _w ++ "]log end\n"
  return lp'

-- ---------------------------------------------------------------------

-- rmTypeSig2 :: Changer
-- rmTypeSig2 ans lp = do
--   let doRmDecl = do
--          tlDecs <- hsDecls lp
--          let [d1] = tlDecs

--          (d1',_) <- modifyValD (GHC.getLoc d1) d1 $ \_m [s,d] -> do
--            transferEntryDPT s d
--            return ([d],Nothing)
--          replaceDecls lp [d1']

--   let (lp',(ans',_),_w) = runTransform ans doRmDecl
--   -- putStrLn $ "log:" ++ intercalate "\n" _w
--   return (ans',lp')

rmTypeSig2 :: Changer
rmTypeSig2 _libdir lp = do
  let doRmDecl = do
         tlDecs <- hsDecls lp
         let [de1] = tlDecs

         (de1',_) <- modifyValD (getLocA de1) de1 $ \_m [s,d] -> do
           d' <- transferEntryDPT s d
           return ([d'],Nothing)
         replaceDecls lp [de1']

  let (lp',(_,_),_w) = runTransform mempty doRmDecl
  debugM $ "log:[\n" ++ intercalate "\n" _w ++ "]log end\n"
  return lp'

-- ---------------------------------------------------------------------

-- addHiding1 :: Changer
-- addHiding1 ans (GHC.L l p) = do
--   let doTransform = do
--         l0 <- uniqueSrcSpanT
--         l1 <- uniqueSrcSpanT
--         l2 <- uniqueSrcSpanT
--         let
--           [GHC.L li imp1,imp2] = GHC.hsmodImports p
--           n1 = GHC.L l1 (GHC.mkVarUnqual (GHC.mkFastString "n1"))
--           n2 = GHC.L l2 (GHC.mkVarUnqual (GHC.mkFastString "n2"))
--           v1 = GHC.L l1 (GHC.IEVar noExt (GHC.L l1 (GHC.IEName n1)))
--           v2 = GHC.L l2 (GHC.IEVar noExt (GHC.L l2 (GHC.IEName n2)))
--           impHiding = GHC.L l0 [v1,v2]
--           imp1' = imp1 { GHC.ideclHiding = Just (True,impHiding)}
--           p' = p { GHC.hsmodImports = [GHC.L li imp1',imp2]}
--         addSimpleAnnT impHiding (DP (0,1)) [((G GHC.AnnHiding),DP (0,0)),((G GHC.AnnOpenP),DP (0,1)),((G GHC.AnnCloseP),DP (0,0))]
--         addSimpleAnnT n1        (DP (0,0)) [((G GHC.AnnVal),DP (0,0))]
--         addSimpleAnnT v1        (DP (0,0)) [((G GHC.AnnComma),DP (0,0))]
--         addSimpleAnnT n2        (DP (0,0)) [((G GHC.AnnVal),DP (0,0))]
--         return (GHC.L l p')

--   let (lp',(ans',_),_w) = runTransform ans doTransform
--   return (ans',lp')

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

  let (lp',(_ans',_),_w) = runTransform mempty doTransform
  debugM $ "log:[\n" ++ intercalate "\n" _w ++ "]log end\n"
  return lp'
-- ---------------------------------------------------------------------

-- addHiding2 :: Changer
-- addHiding2 ans (GHC.L l p) = do
--   let doTransform = do
--         l1 <- uniqueSrcSpanT
--         l2 <- uniqueSrcSpanT
--         let
--           [GHC.L li imp1] = GHC.hsmodImports p
--           Just (_,GHC.L lh ns) = GHC.ideclHiding imp1
--           n1 = GHC.L l1 (GHC.mkVarUnqual (GHC.mkFastString "n1"))
--           n2 = GHC.L l2 (GHC.mkVarUnqual (GHC.mkFastString "n2"))
--           v1 = GHC.L l1 (GHC.IEVar noExt (GHC.L l1 (GHC.IEName n1)))
--           v2 = GHC.L l2 (GHC.IEVar noExt (GHC.L l2 (GHC.IEName n2)))
--           imp1' = imp1 { GHC.ideclHiding = Just (True,GHC.L lh (ns ++ [v1,v2]))}
--           p' = p { GHC.hsmodImports = [GHC.L li imp1']}
--         addSimpleAnnT n1        (DP (0,0)) [((G GHC.AnnVal),DP (0,0))]
--         addSimpleAnnT v1        (DP (0,0)) [((G GHC.AnnComma),DP (0,0))]
--         addSimpleAnnT n2        (DP (0,0)) [((G GHC.AnnVal),DP (0,0))]
--         addTrailingCommaT (last ns)
--         return (GHC.L l p')

--   let (lp',(ans',_),_w) = runTransform ans doTransform
--   return (ans',lp')

addHiding2 :: Changer
addHiding2 _libdir (L l p) = do
  let doTransform = do
        l1 <- uniqueSrcSpanT
        l2 <- uniqueSrcSpanT
        let
          [L li imp1] = hsmodImports p
          Just (_,L lh ns) = ideclHiding imp1
          lh' = (SrcSpanAnn (EpAnn (Anchor (realSrcSpan (locA lh)) m0)
                                     (AnnList Nothing
                                              (Just (AddEpAnn AnnOpenP  d1))
                                              (Just (AddEpAnn AnnCloseP d0))
                                              [(AddEpAnn AnnHiding d1)]
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

  let (lp',(_ans',_),_w) = runTransform mempty doTransform
  debugM $ "log:[\n" ++ intercalate "\n" _w ++ "]log end\n"
  return lp'
-- ---------------------------------------------------------------------

cloneDecl1 :: Changer
cloneDecl1 _libdir lp = do
  let doChange = do
         tlDecs <- hsDecls lp
         let (d1':d2:ds) = tlDecs
         d2' <- fst <$> cloneT d2
         let d2'' = setEntryDP' d2' (DifferentLine 2 0)
         replaceDecls lp (d1':d2:d2'':ds)

  let (lp',(_ans',_),_w) = runTransform mempty doChange
  return lp'

-- ---------------------------------------------------------------------
