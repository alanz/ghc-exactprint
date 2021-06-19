{-# LANGUAGE CPP #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
module Test.Transform where

import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Parsers
import Language.Haskell.GHC.ExactPrint.Utils

#if __GLASGOW_HASKELL__ >= 808
import qualified GHC                       as GHC
import qualified GHC.Data.Bag              as GHC
import qualified GHC.Data.FastString       as GHC
import qualified GHC.Types.Name.Occurrence as GHC
import qualified GHC.Types.Name.Reader     as GHC
import qualified GHC.Types.SrcLoc          as GHC
#else
import qualified Bag            as GHC
import qualified GHC            as GHC
import qualified OccName        as GHC
import qualified RdrName        as GHC
import qualified SrcLoc         as GHC
import qualified FastString     as GHC
#endif

import qualified Data.Generics as SYB

import System.FilePath
import qualified Data.Map as Map
-- import Data.List
-- import Data.Maybe

import Test.Common

import Test.HUnit

transformTests :: Test
transformTests = TestLabel "transformation tests" $ TestList
  [
    TestLabel "Low level transformations"
       (TestList transformLowLevelTests)
  , TestLabel "High level transformations"
       (TestList transformHighLevelTests)
  ]

transformLowLevelTests :: [Test]
transformLowLevelTests = [
    mkTestModChange changeRenameCase1 "RenameCase1.hs"
  , mkTestModChange changeLayoutLet2  "LayoutLet2.hs"
  , mkTestModChange changeLayoutLet3  "LayoutLet3.hs"
  , mkTestModChange changeLayoutLet3  "LayoutLet4.hs"
  , mkTestModChange changeRename1     "Rename1.hs"
  , mkTestModChange changeRename2     "Rename2.hs"
  , mkTestModChange changeLayoutIn1   "LayoutIn1.hs"
  , mkTestModChange changeLayoutIn3   "LayoutIn3.hs"
  , mkTestModChange changeLayoutIn3   "LayoutIn3a.hs"
  , mkTestModChange changeLayoutIn3   "LayoutIn3b.hs"
  , mkTestModChange changeLayoutIn4   "LayoutIn4.hs"
  , mkTestModChange changeLocToName   "LocToName.hs"
  , mkTestModChange changeLetIn1      "LetIn1.hs"
  , mkTestModChange changeWhereIn4    "WhereIn4.hs"
  , mkTestModChange changeAddDecl     "AddDecl.hs"
  , mkTestModChange changeLocalDecls  "LocalDecls.hs"
  , mkTestModChange changeLocalDecls2 "LocalDecls2.hs"
  , mkTestModChange changeWhereIn3a   "WhereIn3a.hs"
--  , mkTestModChange changeCifToCase  "C.hs"          "C"
  ]

mkTestModChange :: Changer -> FilePath -> Test
mkTestModChange = mkTestMod "expected" "transform"

mkTestModBad :: FilePath -> Test
mkTestModBad = mkTestMod "bad" "failing" noChange


mkTestMod :: String -> FilePath -> Changer -> FilePath ->  Test
mkTestMod suffix dir f fp =
  let basename       = testPrefix </> dir </> fp
      expected       = basename <.> suffix
      writeFailure   = writeFile (basename <.> "out")
  in
    TestCase (do r <- either (\(ParseFailure s) -> error (s ++ basename)) id
                        <$> genTest f basename expected
                 writeFailure (debugTxt r)
                 assertBool fp (status r == Success))


-- ---------------------------------------------------------------------

changeWhereIn3a :: Changer
changeWhereIn3a ans (GHC.L l p) = do
  let decls = GHC.hsmodDecls p
         -- (GHC.L _ (GHC.SigD sig))    = head $ drop 1 decls
      d1 = head $ drop 2 decls
      d2 = head $ drop 3 decls
  let (_p1,(ans',_),_w) = runTransform ans (balanceComments d1 d2)
  let p2 = p { GHC.hsmodDecls = d2:d1:decls}
  return (ans',GHC.L l p2)

-- ---------------------------------------------------------------------

-- | Add a local declaration with signature to LocalDecl, where there was no
-- prior local decl. So it adds a "where" annotation.
changeLocalDecls2 :: Changer
changeLocalDecls2 ans (GHC.L l p) = do
#if __GLASGOW_HASKELL__ > 804
  Right (declAnns, d@(GHC.L ld (GHC.ValD _ decl))) <- withDynFlags (\df -> parseDecl df "decl" "nn = 2")
  Right (sigAnns, s@(GHC.L ls (GHC.SigD _ sig)))   <- withDynFlags (\df -> parseDecl df "sig"  "nn :: Int")
#else
  Right (declAnns, d@(GHC.L ld (GHC.ValD decl))) <- withDynFlags (\df -> parseDecl df "decl" "nn = 2")
  Right (sigAnns, s@(GHC.L ls (GHC.SigD sig)))   <- withDynFlags (\df -> parseDecl df "sig"  "nn :: Int")
#endif
  let declAnns' = setPrecedingLines (GHC.L ld decl) 1 0 declAnns
  let  sigAnns' = setPrecedingLines (GHC.L ls  sig) 1 4 sigAnns
  -- putStrLn $ "changeLocalDecls:sigAnns=" ++ show sigAnns
  -- putStrLn $ "changeLocalDecls:declAnns=" ++ show declAnns
  -- putStrLn $ "\nchangeLocalDecls:sigAnns'=" ++ show sigAnns'
  let (p',(ans',_),_w) = runTransform ans doAddLocal
      doAddLocal = SYB.everywhereM (SYB.mkM replaceLocalBinds) p
      replaceLocalBinds :: GHC.LMatch GhcPs (GHC.LHsExpr GhcPs)
                        -> Transform (GHC.LMatch GhcPs (GHC.LHsExpr GhcPs))
#if __GLASGOW_HASKELL__ <= 710
      replaceLocalBinds m@(GHC.L lm (GHC.Match mln pats typ (GHC.GRHSs rhs (GHC.EmptyLocalBinds)))) = do
#elif __GLASGOW_HASKELL__ <= 802
      replaceLocalBinds m@(GHC.L lm (GHC.Match mln pats typ (GHC.GRHSs rhs (GHC.L _ GHC.EmptyLocalBinds)))) = do
#elif __GLASGOW_HASKELL__ <= 804
      replaceLocalBinds m@(GHC.L lm (GHC.Match mln pats (GHC.GRHSs rhs (GHC.L _ GHC.EmptyLocalBinds)))) = do
#else
      replaceLocalBinds m@(GHC.L lm (GHC.Match _ mln pats (GHC.GRHSs _ rhs (GHC.L _ GHC.EmptyLocalBinds{})))) = do
#endif
        newSpan <- uniqueSrcSpanT
        let
          newAnnKey = AnnKey (rs newSpan) (CN "HsValBinds")
          addWhere mkds =
            case Map.lookup (mkAnnKey m) mkds of
              Nothing -> error "wtf"
              Just ann -> Map.insert newAnnKey ann2 mkds2
                where
                  ann1 = ann { annsDP = annsDP ann ++ [(G GHC.AnnWhere,DP (1,2))]
                             , annCapturedSpan = Just newAnnKey
                             , annSortKey = Just [rs ls, rs ld]
                             }
                  mkds2 = Map.insert (mkAnnKey m) ann1 mkds
                  ann2 = annNone
                             { annEntryDelta     = DP (1,0) }
        modifyAnnsT addWhere
        let decls = [s,d]
        -- logTr $ "(m,decls)=" ++ show (mkAnnKey m,map mkAnnKey decls)
        modifyAnnsT (captureOrderAnnKey newAnnKey decls)
#if __GLASGOW_HASKELL__ > 804
        let binds = (GHC.HsValBinds noExt (GHC.ValBinds noExt (GHC.listToBag $ [GHC.L ld decl])
                                    [GHC.L ls sig]))
#else
        let binds = (GHC.HsValBinds (GHC.ValBindsIn (GHC.listToBag $ [GHC.L ld decl])
                                    [GHC.L ls sig]))
#endif
#if __GLASGOW_HASKELL__ <= 710
        return (GHC.L lm (GHC.Match mln pats typ (GHC.GRHSs rhs binds)))
#elif __GLASGOW_HASKELL__ <= 802
        bindSpan <- uniqueSrcSpanT
        return (GHC.L lm (GHC.Match mln pats typ (GHC.GRHSs rhs (GHC.L bindSpan binds))))
#elif __GLASGOW_HASKELL__ <= 804
        bindSpan <- uniqueSrcSpanT
        return (GHC.L lm (GHC.Match mln pats (GHC.GRHSs rhs (GHC.L bindSpan binds))))
#else
        bindSpan <- uniqueSrcSpanT
        return (GHC.L lm (GHC.Match noExt mln pats (GHC.GRHSs noExt rhs (GHC.L bindSpan binds))))
#endif
      replaceLocalBinds x = return x
  -- putStrLn $ "log:" ++ intercalate "\n" w
  return (mergeAnnList [declAnns',sigAnns',ans'],GHC.L l p')

-- ---------------------------------------------------------------------

-- | Add a local declaration with signature to LocalDecl
changeLocalDecls :: Changer
changeLocalDecls ans (GHC.L l p) = do
#if __GLASGOW_HASKELL__ > 804
  Right (declAnns, d@(GHC.L ld (GHC.ValD _ decl))) <- withDynFlags (\df -> parseDecl df "decl" "nn = 2")
  Right (sigAnns, s@(GHC.L ls (GHC.SigD _ sig)))   <- withDynFlags (\df -> parseDecl df "sig"  "nn :: Int")
#else
  Right (declAnns, d@(GHC.L ld (GHC.ValD decl))) <- withDynFlags (\df -> parseDecl df "decl" "nn = 2")
  Right (sigAnns, s@(GHC.L ls (GHC.SigD sig)))   <- withDynFlags (\df -> parseDecl df "sig"  "nn :: Int")
#endif
  let declAnns' = setPrecedingLines (GHC.L ld decl) 1 0 declAnns
  let  sigAnns' = setPrecedingLines (GHC.L ls  sig) 1 4 sigAnns
  -- putStrLn $ "changeLocalDecls:sigAnns=" ++ show sigAnns
  -- putStrLn $ "changeLocalDecls:declAnns=" ++ show declAnns
  -- putStrLn $ "\nchangeLocalDecls:sigAnns'=" ++ show sigAnns'
  let (p',(ans',_),_w) = runTransform ans doAddLocal
      doAddLocal = SYB.everywhereM (SYB.mkM replaceLocalBinds) p
      replaceLocalBinds :: GHC.LMatch GhcPs (GHC.LHsExpr GhcPs)
                        -> Transform (GHC.LMatch GhcPs (GHC.LHsExpr GhcPs))
#if __GLASGOW_HASKELL__ <= 710
      replaceLocalBinds m@(GHC.L lm (GHC.Match mln pats typ (GHC.GRHSs rhs (GHC.HsValBinds (GHC.ValBindsIn binds sigs))))) = do
#elif __GLASGOW_HASKELL__ <= 802
      replaceLocalBinds m@(GHC.L lm (GHC.Match mln pats typ (GHC.GRHSs rhs (GHC.L lb (GHC.HsValBinds (GHC.ValBindsIn binds sigs)))))) = do
#elif __GLASGOW_HASKELL__ <= 804
      replaceLocalBinds m@(GHC.L lm (GHC.Match mln pats (GHC.GRHSs rhs (GHC.L lb (GHC.HsValBinds (GHC.ValBindsIn binds sigs)))))) = do
#else
      replaceLocalBinds m@(GHC.L lm (GHC.Match _ mln pats (GHC.GRHSs _ rhs (GHC.L lb (GHC.HsValBinds _ (GHC.ValBinds _ binds sigs)))))) = do
#endif
        a1 <- getAnnsT
        a' <- case sigs of
              []    -> return a1
              (s1:_) -> do
                let a2 = setPrecedingLines s1 2 0 a1
                return a2
        putAnnsT a'
        let oldDecls = GHC.sortLocated $ map wrapDecl (GHC.bagToList binds) ++ map wrapSig sigs
        let decls = s:d:oldDecls
        -- logTr $ "(m,decls)=" ++ show (mkAnnKey m,map mkAnnKey decls)
        modifyAnnsT (captureOrder m decls)
#if __GLASGOW_HASKELL__ > 804
        let binds' = (GHC.HsValBinds noExt
                          (GHC.ValBinds noExt (GHC.listToBag $ (GHC.L ld decl):GHC.bagToList binds)
                                          (GHC.L ls sig:sigs)))
#else
        let binds' = (GHC.HsValBinds
                          (GHC.ValBindsIn (GHC.listToBag $ (GHC.L ld decl):GHC.bagToList binds)
                                          (GHC.L ls sig:sigs)))
#endif
#if __GLASGOW_HASKELL__ <= 710
        return (GHC.L lm (GHC.Match mln pats typ (GHC.GRHSs rhs binds')))
#elif __GLASGOW_HASKELL__ <= 802
        return (GHC.L lm (GHC.Match mln pats typ (GHC.GRHSs rhs (GHC.L lb binds'))))
#elif __GLASGOW_HASKELL__ <= 804
        return (GHC.L lm (GHC.Match mln pats (GHC.GRHSs rhs (GHC.L lb binds'))))
#else
        return (GHC.L lm (GHC.Match noExt mln pats (GHC.GRHSs noExt rhs (GHC.L lb binds'))))
#endif
      replaceLocalBinds x = return x
  -- putStrLn $ "log:" ++ intercalate "\n" w
  return (mergeAnnList [declAnns',sigAnns',ans'],GHC.L l p')

-- ---------------------------------------------------------------------

-- | Add a declaration to AddDecl
changeAddDecl :: Changer
changeAddDecl ans top = do
  Right (declAnns, decl) <- withDynFlags (\df -> parseDecl df "<interactive>" "nn = n2")
  -- putStrLn $ "changeDecl:(declAnns,decl)=" ++ showGhc (declAnns,decl)
  let declAnns' = setPrecedingLines decl 2 0 declAnns
  -- putStrLn $ "changeDecl:(declAnns',decl)=" ++ showGhc (declAnns',decl)

  let (p',(ans',_),_) = runTransform ans doAddDecl
      doAddDecl = SYB.everywhereM (SYB.mkM replaceTopLevelDecls) top
      replaceTopLevelDecls :: GHC.ParsedSource -> Transform (GHC.ParsedSource)
      replaceTopLevelDecls m = insertAtStart m decl
  return (mergeAnns declAnns' ans',p')

-- ---------------------------------------------------------------------

-- |Remove a decl with a trailing comment, and remove the trailing comment too
changeWhereIn3 :: Int -> Changer
changeWhereIn3 declIndex ans p = return (ans',p')
  where
    (p',(ans',_),_) = runTransform ans doTransform
    doTransform = doRmDecl p

#if __GLASGOW_HASKELL__ >= 808
    doRmDecl (GHC.L l (GHC.HsModule lo mmn mexp imps decls mdepr haddock)) = do
#else
    doRmDecl (GHC.L l (GHC.HsModule    mmn mexp imps decls mdepr haddock)) = do
#endif
      let
        -- declIndex = 2 -- zero based
        decls1 = take declIndex decls
        decls2 = drop (declIndex + 1) decls
        decls' = decls1 ++ decls2
#if __GLASGOW_HASKELL__ >= 808
      return (GHC.L l (GHC.HsModule lo mmn mexp imps decls' mdepr haddock))
#else
      return (GHC.L l (GHC.HsModule    mmn mexp imps decls' mdepr haddock))
#endif
      -- error $ "doRmDecl:decls2=" ++ showGhc (length decls,decls1,decls2)

-- ---------------------------------------------------------------------

changeRenameCase1 :: Changer
changeRenameCase1 ans parsed = return (ans,rename "bazLonger" [((3,15),(3,18))] parsed)
-- changeRenameCase1 ans parsed = return (ans,rename "bazLonger" [((3,15),(3,17))] parsed)

changeRenameCase2 :: Changer
changeRenameCase2 ans parsed = return (ans,rename "fooLonger" [((3,1),(3,4))] parsed)

changeLayoutLet2 :: Changer
changeLayoutLet2 ans parsed = return (ans,rename "xxxlonger" [((7,5),(7,8)),((8,24),(8,27))] parsed)

changeLocToName :: Changer
changeLocToName ans parsed = return (ans,rename "LocToName.newPoint" [((20,1),(20,11)),((20,28),(20,38)),((24,1),(24,11))] parsed)

changeLayoutIn3 :: Changer
changeLayoutIn3 ans parsed = return (ans,rename "anotherX" [((7,13),(7,14)),((7,37),(7,38)),((8,37),(8,38))] parsed)
-- changeLayoutIn3 parsed = rename "anotherX" [((7,13),(7,14)),((7,37),(7,38))] parsed

changeLayoutIn4 :: Changer
changeLayoutIn4 ans parsed = return (ans,rename "io" [((7,8),(7,13)),((7,28),(7,33))] parsed)

changeLayoutIn1 :: Changer
changeLayoutIn1 ans parsed = return (ans,rename "square" [((7,17),(7,19)),((7,24),(7,26))] parsed)

changeRename1 :: Changer
changeRename1 ans parsed = return (ans,rename "bar2" [((3,1),(3,4))] parsed)

changeRename2 :: Changer
changeRename2 ans parsed = return (ans,rename "joe" [((2,1),(2,5))] parsed)

changeLayoutLet3 :: Changer
changeLayoutLet3 ans parsed = return (ans,rename "xxxlonger" [((7,5),(7,8)),((9,14),(9,17))] parsed)

changeLayoutLet5 :: Changer
changeLayoutLet5 ans parsed = return (ans,rename "x" [((7,5),(7,8)),((9,14),(9,17))] parsed)

-- AZ:TODO: the GHC 8 version only needs to consider Located RdrName
rename :: (SYB.Data a) => String -> [(Pos, Pos)] -> a -> a
rename newNameStr spans a
  = SYB.everywhere ( SYB.mkT   replaceRdr
                    `SYB.extT` replaceHsVar
                    `SYB.extT` replacePat
                   ) a
  where
    newName = GHC.mkRdrUnqual (GHC.mkVarOcc newNameStr)

    cond :: GHC.SrcSpan -> Bool
    cond ln = ss2range ln `elem` spans

    replaceRdr :: GHC.Located GHC.RdrName -> GHC.Located GHC.RdrName
    replaceRdr (GHC.L ln _)
        | cond ln = GHC.L ln newName
    replaceRdr x = x

    replaceHsVar :: GHC.LHsExpr GhcPs -> GHC.LHsExpr GhcPs
    replaceHsVar (GHC.L ln (GHC.HsVar{}))
#if __GLASGOW_HASKELL__ <= 710
        | cond ln = GHC.L ln (GHC.HsVar newName)
#elif __GLASGOW_HASKELL__ <= 804
        | cond ln = GHC.L ln (GHC.HsVar (GHC.L ln newName))
#else
        | cond ln = GHC.L ln (GHC.HsVar noExt (GHC.L ln newName))
#endif
    replaceHsVar x = x

#if __GLASGOW_HASKELL__ > 804
    replacePat :: GHC.LPat GhcPs -> GHC.LPat GhcPs
    replacePat (GHC.L ln (GHC.VarPat {}))
        | cond ln = GHC.L ln (GHC.VarPat noExt (GHC.L ln newName))
#elif __GLASGOW_HASKELL__ > 802
    replacePat :: GHC.LPat GhcPs -> GHC.LPat GhcPs
    replacePat (GHC.L ln (GHC.VarPat {}))
        | cond ln = GHC.L ln (GHC.VarPat (GHC.L ln newName))
#elif __GLASGOW_HASKELL__ >= 800
    replacePat (GHC.L ln (GHC.VarPat {}))
        | cond ln = GHC.L ln (GHC.VarPat (GHC.L ln newName))
#else
    replacePat (GHC.L ln (GHC.VarPat {}))
        | cond ln = GHC.L ln (GHC.VarPat newName)
#endif
    replacePat x = x

-- ---------------------------------------------------------------------

changeWhereIn4 :: Changer
changeWhereIn4 ans parsed
  = return (ans,SYB.everywhere (SYB.mkT replace) parsed)
  where
    replace :: GHC.Located GHC.RdrName -> GHC.Located GHC.RdrName
    replace (GHC.L ln _n)
      | ss2range ln == ((12,16),(12,17)) = GHC.L ln (GHC.mkRdrUnqual (GHC.mkVarOcc "p_2"))
    replace x = x

-- ---------------------------------------------------------------------

changeLetIn1 :: Changer
changeLetIn1 ans parsed
  = return (ans,SYB.everywhere (SYB.mkT replace) parsed)
  where
    replace :: GHC.HsExpr GhcPs -> GHC.HsExpr GhcPs
#if __GLASGOW_HASKELL__ <= 710
    replace (GHC.HsLet localDecls expr@(GHC.L _ _))
#elif __GLASGOW_HASKELL__ <= 804
    replace (GHC.HsLet (GHC.L lb localDecls) expr@(GHC.L _ _))
#else
    replace (GHC.HsLet _ (GHC.L lb localDecls) expr@(GHC.L _ _))
#endif
      =
#if __GLASGOW_HASKELL__ > 804
         let (GHC.HsValBinds x (GHC.ValBinds xv bagDecls sigs)) = localDecls
             bagDecls' = GHC.listToBag $ init $ GHC.bagToList bagDecls
#else
         let (GHC.HsValBinds (GHC.ValBindsIn bagDecls sigs)) = localDecls
             bagDecls' = GHC.listToBag $ init $ GHC.bagToList bagDecls
#endif
#if __GLASGOW_HASKELL__ <= 710
         in (GHC.HsLet (GHC.HsValBinds (GHC.ValBindsIn bagDecls' sigs)) expr)
#elif __GLASGOW_HASKELL__ <= 802
         in (GHC.HsLet (GHC.L lb (GHC.HsValBinds (GHC.ValBindsIn bagDecls' sigs))) expr)
#elif __GLASGOW_HASKELL__ <= 804
         in (GHC.HsLet (GHC.L lb (GHC.HsValBinds (GHC.ValBindsIn bagDecls' sigs))) expr)
#else
         in (GHC.HsLet noExt (GHC.L lb (GHC.HsValBinds x (GHC.ValBinds xv bagDecls' sigs))) expr)
#endif

    replace x = x

-- ---------------------------------------------------------------------

transformHighLevelTests :: [Test]
transformHighLevelTests =
  [
    mkTestModChange addLocaLDecl1  "AddLocalDecl1.hs"
  , mkTestModChange addLocaLDecl2  "AddLocalDecl2.hs"
  , mkTestModChange addLocaLDecl3  "AddLocalDecl3.hs"
  , mkTestModChange addLocaLDecl4  "AddLocalDecl4.hs"
  , mkTestModChange addLocaLDecl5  "AddLocalDecl5.hs"
  , mkTestModChange addLocaLDecl6  "AddLocalDecl6.hs"

  , mkTestModChange rmDecl1 "RmDecl1.hs"
  , mkTestModChange rmDecl2 "RmDecl2.hs"
  , mkTestModChange rmDecl3 "RmDecl3.hs"
  , mkTestModChange rmDecl4 "RmDecl4.hs"
  , mkTestModChange rmDecl5 "RmDecl5.hs"
  , mkTestModChange rmDecl6 "RmDecl6.hs"
  , mkTestModChange rmDecl7 "RmDecl7.hs"

  , mkTestModChange rmTypeSig1 "RmTypeSig1.hs"
  , mkTestModChange rmTypeSig2 "RmTypeSig2.hs"

  , mkTestModChange addHiding1 "AddHiding1.hs"
  , mkTestModChange addHiding2 "AddHiding2.hs"

  , mkTestModChange cloneDecl1 "CloneDecl1.hs"
  ]

-- ---------------------------------------------------------------------

addLocaLDecl1 :: Changer
addLocaLDecl1 ans lp = do
  Right (declAnns, newDecl) <- withDynFlags (\df -> parseDecl df "decl" "nn = 2")
  let declAnns' = setPrecedingLines newDecl 1 4 declAnns
      doAddLocal = do
        (d1:d2:_) <- hsDecls lp
        balanceComments d1 d2
        (d1',_) <- modifyValD (GHC.getLoc d1) d1 $ \_m d -> do
          return ((newDecl : d),Nothing)
        replaceDecls lp [d1', d2]

  (lp',(ans',_),_w) <- runTransformT (mergeAnns ans declAnns') doAddLocal
  -- putStrLn $ "log:\n" ++ intercalate "\n" _w
  return (ans',lp')

-- ---------------------------------------------------------------------

addLocaLDecl2 :: Changer
addLocaLDecl2 ans lp = do
  Right (declAnns, newDecl) <- withDynFlags (\df -> parseDecl df "decl" "nn = 2")
  let
      doAddLocal = do
         tlDecs <- hsDecls lp
         let parent = head tlDecs
         balanceComments parent (head $ tail tlDecs)

         (parent',_) <- modifyValD (GHC.getLoc parent) parent $ \_m decls -> do
           transferEntryDPT (head decls) newDecl
           setEntryDPT (head decls) (DP (1, 0))
           return ((newDecl:decls),Nothing)

         replaceDecls lp (parent':tail tlDecs)

  let (lp',(ans',_),_w) = runTransform (mergeAnns ans declAnns) doAddLocal
  -- putStrLn $ "log:\n" ++ intercalate "\n" _w
  return (ans',lp')

-- ---------------------------------------------------------------------

addLocaLDecl3 :: Changer
addLocaLDecl3 ans lp = do
  Right (declAnns, newDecl) <- withDynFlags (\df -> parseDecl df "decl" "nn = 2")
  let
      doAddLocal = do
         -- logDataWithAnnsTr "parsed:" lp
         logDataWithAnnsTr "newDecl:" newDecl
         tlDecs <- hsDecls lp
         let parent = head tlDecs
         balanceComments parent (head $ tail tlDecs)

         (parent',_) <- modifyValD (GHC.getLoc parent) parent $ \m decls -> do
           setPrecedingLinesT newDecl 1 0
           moveTrailingComments m (last decls)
           return ((decls++[newDecl]),Nothing)

         replaceDecls lp (parent':tail tlDecs)

  let (lp',(ans',_),_w) = runTransform (mergeAnns ans declAnns) doAddLocal
  -- putStrLn $ "log\n" ++ intercalate "\n" _w
  return (ans',lp')

-- ---------------------------------------------------------------------

addLocaLDecl4 :: Changer
addLocaLDecl4 ans lp = do
  Right (declAnns, newDecl) <- withDynFlags (\df -> parseDecl df "decl" "nn = 2")
  Right (sigAnns, newSig)   <- withDynFlags (\df -> parseDecl df "sig"  "nn :: Int")
  -- putStrLn $ "addLocaLDecl4:lp=" ++ showGhc lp
  let
      doAddLocal = do
         tlDecs <- hsDecls lp
         let parent = head tlDecs

         setPrecedingLinesT newSig  1 0
         setPrecedingLinesT newDecl 1 0

         (parent',_) <- modifyValD (GHC.getLoc parent) parent $ \_m decls -> do
           return ((decls++[newSig,newDecl]),Nothing)

         replaceDecls lp (parent':tail tlDecs)

  let (lp',(ans',_),_w) = runTransform (mergeAnnList [ans,declAnns,sigAnns]) doAddLocal
  -- putStrLn $ "log\n" ++ intercalate "\n" _w
  return (ans',lp')

-- ---------------------------------------------------------------------

addLocaLDecl5 :: Changer
addLocaLDecl5 ans lp = do
  let
      doAddLocal = do
         [s1,d1,d2,d3] <- hsDecls lp

         transferEntryDPT d2 d3

         (d1',_) <- modifyValD (GHC.getLoc d1) d1 $ \_m _decls -> do
           return ([d2],Nothing)
         replaceDecls lp [s1,d1',d3]

  (lp',(ans',_),_w) <- runTransformT ans doAddLocal
  -- putStrLn $ "log\n" ++ intercalate "\n" _w
  return (ans',lp')

-- ---------------------------------------------------------------------

addLocaLDecl6 :: Changer
addLocaLDecl6 ans lp = do
  Right (declAnns, newDecl) <- withDynFlags (\df -> parseDecl df "decl" "x = 3")
  let declAnns' = setPrecedingLines newDecl 1 4 declAnns
      doAddLocal = do
        [d1,d2] <- hsDecls lp
        balanceComments d1 d2

#if __GLASGOW_HASKELL__ >= 808
        let GHC.L _ (GHC.ValD _ (GHC.FunBind _ _ (GHC.MG _ (GHC.L _ [m1,m2]) _) _)) = d1
#elif __GLASGOW_HASKELL__ > 804
        let GHC.L _ (GHC.ValD _ (GHC.FunBind _ _ (GHC.MG _ (GHC.L _ [m1,m2]) _) _ _)) = d1
#elif __GLASGOW_HASKELL__ > 710
        let GHC.L _ (GHC.ValD (GHC.FunBind  _ (GHC.MG (GHC.L _ [m1,m2]) _ _ _) _ _ _)) = d1
#else
        let GHC.L _ (GHC.ValD (GHC.FunBind  _ _ (GHC.MG [m1,m2] _ _ _) _ _ _)) = d1
#endif
        balanceComments m1 m2

        (d1',_) <- modifyValD (GHC.getLoc m1) d1 $ \_m decls -> do
           return ((newDecl : decls),Nothing)
        replaceDecls lp [d1', d2]

  (lp',(ans',_),_w) <- runTransformT (mergeAnns ans declAnns') doAddLocal
  -- putStrLn $ "log:\n" ++ intercalate "\n" _w
  return (ans',lp')
-- ---------------------------------------------------------------------

rmDecl1 :: Changer
rmDecl1 ans lp = do
  let doRmDecl = do
         tlDecs <- hsDecls lp
         let (d1:s1:d2:ds) = tlDecs

         -- First delete the decl (d2) only
         balanceComments s1 d2 -- ++AZ++
         balanceComments d2 (head ds)
         lp1 <- replaceDecls lp (d1:s1:ds)
         -- return lp1

         -- Then delete the sig separately
         tlDecs1 <- hsDecls lp1
         let (d1':s1':ds') = tlDecs1
         -- transferEntryDPT s1' (head ds')  -- required in HaRe.
         balanceComments d1' s1'
         balanceComments s1' (head ds')
         transferEntryDPT s1' (head ds')  -- required in HaRe.
         replaceDecls lp (d1':ds')

  (lp',(ans',_),_w) <- runTransformT ans doRmDecl
  return (ans',lp')

-- ---------------------------------------------------------------------

rmDecl2 :: Changer
rmDecl2 ans lp = do
  let
      doRmDecl = do
        let
          go :: GHC.LHsExpr GhcPs -> Transform (GHC.LHsExpr GhcPs)
          go e@(GHC.L _ (GHC.HsLet{})) = do
            decs <- hsDecls e
            e' <- replaceDecls e (init decs)
            return e'
          go x = return x

        SYB.everywhereM (SYB.mkM go) lp

  let (lp',(ans',_),_w) = runTransform ans doRmDecl
  -- putStrLn $ "log:\n" ++ intercalate "\n" _w
  return (ans',lp')

-- ---------------------------------------------------------------------

rmDecl3 :: Changer
rmDecl3 ans lp = do
  let
      doRmDecl = do
         [d1,d2] <- hsDecls lp

         (d1',Just sd1) <- modifyValD (GHC.getLoc d1) d1 $ \_m [sd1] -> do
           setPrecedingLinesDeclT sd1 2 0
           return ([],Just sd1)

         replaceDecls lp [d1',sd1,d2]

  (lp',(ans',_),_w) <- runTransformT ans doRmDecl
  -- putStrLn $ "log:\n" ++ intercalate "\n" _w
  return (ans',lp')

-- ---------------------------------------------------------------------

rmDecl4 :: Changer
rmDecl4 ans lp = do
  let
      doRmDecl = do
         [d1] <- hsDecls lp

         (d1',Just sd1) <- modifyValD (GHC.getLoc d1) d1 $ \_m [sd1,sd2] -> do
           -- [sd1,sd2] <- hsDecls d1
           transferEntryDPT sd1 sd2

           setPrecedingLinesDeclT sd1 2 0
           -- d1' <- replaceDecls d1 [sd2]
           return ([sd2],Just sd1)

         replaceDecls lp [d1',sd1]

  (lp',(ans',_),_w) <- runTransformT ans doRmDecl
  return (ans',lp')

-- ---------------------------------------------------------------------

rmDecl5 :: Changer
rmDecl5 ans lp = do
  let
      doRmDecl = do
        let
          go :: GHC.HsExpr GhcPs -> Transform (GHC.HsExpr GhcPs)
#if __GLASGOW_HASKELL__ <= 710
          go (GHC.HsLet lb expr) = do
#elif __GLASGOW_HASKELL__ <= 804
          go (GHC.HsLet (GHC.L l lb) expr) = do
#else
          go (GHC.HsLet _ (GHC.L l lb) expr) = do
#endif
            decs <- hsDeclsValBinds lb
            let dec = last decs
            transferEntryDPT (head decs) dec
            lb' <- replaceDeclsValbinds lb [dec]
#if __GLASGOW_HASKELL__ <= 710
            return (GHC.HsLet lb' expr)
#elif __GLASGOW_HASKELL__ <= 804
            return (GHC.HsLet (GHC.L l lb') expr)
#else
            return (GHC.HsLet noExt (GHC.L l lb') expr)
#endif
          go x = return x

        SYB.everywhereM (SYB.mkM go) lp

  let (lp',(ans',_),_w) = runTransform ans doRmDecl
  -- putStrLn $ "log:" ++ intercalate "\n" _w
  return (ans',lp')

-- ---------------------------------------------------------------------

rmDecl6 :: Changer
rmDecl6 ans lp = do
  let
      doRmDecl = do
         [d1] <- hsDecls lp

         (d1',_) <- modifyValD (GHC.getLoc d1) d1 $ \_m subDecs -> do
           let (ss1:_sd1:sd2:sds) = subDecs
           transferEntryDPT ss1 sd2

           return (sd2:sds,Nothing)

         replaceDecls lp [d1']

  (lp',(ans',_),_w) <- runTransformT ans doRmDecl
  -- putStrLn $ "log:" ++ intercalate "\n" _w
  return (ans',lp')

-- ---------------------------------------------------------------------

rmDecl7 :: Changer
rmDecl7 ans lp = do
  let
      doRmDecl = do
         tlDecs <- hsDecls lp
         let [s1,d1,d2,d3] = tlDecs

         balanceComments d1 d2
         balanceComments d2 d3

         transferEntryDPT d2 d3

         replaceDecls lp [s1,d1,d3]

  let (lp',(ans',_),_w) = runTransform ans doRmDecl
  -- putStrLn $ "log:" ++ intercalate "\n" _w
  return (ans',lp')

-- ---------------------------------------------------------------------

rmTypeSig1 :: Changer
rmTypeSig1 ans lp = do
  let doRmDecl = do
         tlDecs <- hsDecls lp
         let (s1:d1:d2) = tlDecs
#if __GLASGOW_HASKELL__ <= 710
             (GHC.L l (GHC.SigD (GHC.TypeSig names typ p))) = s1
             s1' = (GHC.L l (GHC.SigD (GHC.TypeSig (tail names) typ p)))
#elif __GLASGOW_HASKELL__ <= 804
             (GHC.L l (GHC.SigD (GHC.TypeSig names typ))) = s1
             s1' = (GHC.L l (GHC.SigD (GHC.TypeSig (tail names) typ)))
#else
             (GHC.L l (GHC.SigD x1 (GHC.TypeSig x2 names typ))) = s1
             s1' = (GHC.L l (GHC.SigD x1 (GHC.TypeSig x2 (tail names) typ)))
#endif
         replaceDecls lp (s1':d1:d2)

  let (lp',(ans',_),_w) = runTransform ans doRmDecl
  return (ans',lp')

-- ---------------------------------------------------------------------

rmTypeSig2 :: Changer
rmTypeSig2 ans lp = do
  let doRmDecl = do
         tlDecs <- hsDecls lp
         let [d1] = tlDecs

         (d1',_) <- modifyValD (GHC.getLoc d1) d1 $ \_m [s,d] -> do
           transferEntryDPT s d
           return ([d],Nothing)
         replaceDecls lp [d1']

  let (lp',(ans',_),_w) = runTransform ans doRmDecl
  -- putStrLn $ "log:" ++ intercalate "\n" _w
  return (ans',lp')

-- ---------------------------------------------------------------------

addHiding1 :: Changer
addHiding1 ans (GHC.L l p) = do
  let doTransform = do
        l0 <- uniqueSrcSpanT
        l1 <- uniqueSrcSpanT
        l2 <- uniqueSrcSpanT
        let
          [GHC.L li imp1,imp2] = GHC.hsmodImports p
          n1 = GHC.L l1 (GHC.mkVarUnqual (GHC.mkFastString "n1"))
          n2 = GHC.L l2 (GHC.mkVarUnqual (GHC.mkFastString "n2"))
#if __GLASGOW_HASKELL__ > 804
          v1 = GHC.L l1 (GHC.IEVar noExt (GHC.L l1 (GHC.IEName n1)))
          v2 = GHC.L l2 (GHC.IEVar noExt (GHC.L l2 (GHC.IEName n2)))
#elif __GLASGOW_HASKELL__ > 800
          v1 = GHC.L l1 (GHC.IEVar (GHC.L l1 (GHC.IEName n1)))
          v2 = GHC.L l2 (GHC.IEVar (GHC.L l2 (GHC.IEName n2)))
#else
          v1 = GHC.L l1 (GHC.IEVar n1)
          v2 = GHC.L l2 (GHC.IEVar n2)
#endif
          impHiding = GHC.L l0 [v1,v2]
          imp1' = imp1 { GHC.ideclHiding = Just (True,impHiding)}
          p' = p { GHC.hsmodImports = [GHC.L li imp1',imp2]}
        addSimpleAnnT impHiding (DP (0,1)) [((G GHC.AnnHiding),DP (0,0)),((G GHC.AnnOpenP),DP (0,1)),((G GHC.AnnCloseP),DP (0,0))]
        addSimpleAnnT n1        (DP (0,0)) [((G GHC.AnnVal),DP (0,0))]
        addSimpleAnnT v1        (DP (0,0)) [((G GHC.AnnComma),DP (0,0))]
        addSimpleAnnT n2        (DP (0,0)) [((G GHC.AnnVal),DP (0,0))]
        return (GHC.L l p')

  let (lp',(ans',_),_w) = runTransform ans doTransform
  return (ans',lp')

-- ---------------------------------------------------------------------

addHiding2 :: Changer
addHiding2 ans (GHC.L l p) = do
  let doTransform = do
        l1 <- uniqueSrcSpanT
        l2 <- uniqueSrcSpanT
        let
          [GHC.L li imp1] = GHC.hsmodImports p
          Just (_,GHC.L lh ns) = GHC.ideclHiding imp1
          n1 = GHC.L l1 (GHC.mkVarUnqual (GHC.mkFastString "n1"))
          n2 = GHC.L l2 (GHC.mkVarUnqual (GHC.mkFastString "n2"))
#if __GLASGOW_HASKELL__ > 804
          v1 = GHC.L l1 (GHC.IEVar noExt (GHC.L l1 (GHC.IEName n1)))
          v2 = GHC.L l2 (GHC.IEVar noExt (GHC.L l2 (GHC.IEName n2)))
#elif __GLASGOW_HASKELL__ > 800
          v1 = GHC.L l1 (GHC.IEVar (GHC.L l1 (GHC.IEName n1)))
          v2 = GHC.L l2 (GHC.IEVar (GHC.L l2 (GHC.IEName n2)))
#else
          v1 = GHC.L l1 (GHC.IEVar n1)
          v2 = GHC.L l2 (GHC.IEVar n2)
#endif
          imp1' = imp1 { GHC.ideclHiding = Just (True,GHC.L lh (ns ++ [v1,v2]))}
          p' = p { GHC.hsmodImports = [GHC.L li imp1']}
        addSimpleAnnT n1        (DP (0,0)) [((G GHC.AnnVal),DP (0,0))]
        addSimpleAnnT v1        (DP (0,0)) [((G GHC.AnnComma),DP (0,0))]
        addSimpleAnnT n2        (DP (0,0)) [((G GHC.AnnVal),DP (0,0))]
        addTrailingCommaT (last ns)
        return (GHC.L l p')

  let (lp',(ans',_),_w) = runTransform ans doTransform
  return (ans',lp')

-- ---------------------------------------------------------------------

cloneDecl1 :: Changer
cloneDecl1 ans lp = do
  let doChange = do
         tlDecs <- hsDecls lp
         let (d1:d2:ds) = tlDecs
         d2' <- fst <$> cloneT d2
         replaceDecls lp (d1:d2:d2':ds)

  let (lp',(ans',_),_w) = runTransform ans doChange
  return (ans',lp')

-- ---------------------------------------------------------------------
