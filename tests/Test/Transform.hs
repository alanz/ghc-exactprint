{-# LANGUAGE TupleSections #-}
module Test.Transform where

import Language.Haskell.GHC.ExactPrint
import Language.Haskell.GHC.ExactPrint.Preprocess
import Language.Haskell.GHC.ExactPrint.Types
import Language.Haskell.GHC.ExactPrint.Utils
import Language.Haskell.GHC.ExactPrint.Parsers

import GHC.Paths ( libdir )

import qualified Bag            as GHC
import qualified DynFlags       as GHC
import qualified GHC            as GHC
import qualified OccName        as GHC
import qualified RdrName        as GHC
import qualified SrcLoc         as GHC
import qualified FastString     as GHC

import qualified Data.Generics as SYB
-- import qualified GHC.SYB.Utils as SYB

import Control.Monad
import System.FilePath
import System.IO
import qualified Data.Map as Map
import Data.List
import Data.Maybe

import System.IO.Silently

import Test.Common

import Test.HUnit

transformTests :: [Test]
transformTests =
  [
    TestLabel "Low level transformations"
       (TestList transformLowLevelTests)
  , TestLabel "High level transformations"
       (TestList transformHighLevelTests)
  ]

transformLowLevelTests :: [Test]
transformLowLevelTests = [
    mkTestModChange changeLayoutLet2  "LayoutLet2.hs"  "LayoutLet2"
  , mkTestModChange changeLayoutLet3  "LayoutLet3.hs"  "LayoutLet3"
  , mkTestModChange changeLayoutLet3  "LayoutLet4.hs"  "LayoutLet4"
  , mkTestModChange changeRename1     "Rename1.hs"     "Main"
  , mkTestModChange changeRename2     "Rename2.hs"     "Main"
  , mkTestModChange changeLayoutIn1   "LayoutIn1.hs"   "LayoutIn1"
  , mkTestModChange changeLayoutIn3   "LayoutIn3.hs"   "LayoutIn3"
  , mkTestModChange changeLayoutIn3   "LayoutIn3a.hs"  "LayoutIn3a"
  , mkTestModChange changeLayoutIn3   "LayoutIn3b.hs"  "LayoutIn3b"
  , mkTestModChange changeLayoutIn4   "LayoutIn4.hs"   "LayoutIn4"
  , mkTestModChange changeLocToName   "LocToName.hs"   "LocToName"
  , mkTestModChange changeLetIn1      "LetIn1.hs"      "LetIn1"
  , mkTestModChange changeWhereIn4    "WhereIn4.hs"    "WhereIn4"
  , mkTestModChange changeAddDecl     "AddDecl.hs"     "AddDecl"
  , mkTestModChange changeLocalDecls  "LocalDecls.hs"  "LocalDecls"
  , mkTestModChange changeLocalDecls2 "LocalDecls2.hs" "LocalDecls2"
  , mkTestModChange changeWhereIn3a   "WhereIn3a.hs"   "WhereIn3a"
--  , mkTestModChange changeCifToCase  "C.hs"          "C"
  ]

mkTestModChange :: Changer -> FilePath -> String -> Test
mkTestModChange change fileName modName
  = TestCase (do r <- manipulateAstTestWithMod change "expected" fileName modName
                 assertBool fileName r )

type Changer = (Anns -> GHC.ParsedSource -> IO (Anns,GHC.ParsedSource))

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
  Right (declAnns, d@(GHC.L ld (GHC.ValD decl))) <- withDynFlags (\df -> parseDecl df "decl" "nn = 2")
  Right (sigAnns, s@(GHC.L ls (GHC.SigD sig)))   <- withDynFlags (\df -> parseDecl df "sig"  "nn :: Int")
  let declAnns' = setPrecedingLines (GHC.L ld decl) 1 0 declAnns
  let  sigAnns' = setPrecedingLines (GHC.L ls  sig) 1 4 sigAnns
  -- putStrLn $ "changeLocalDecls:sigAnns=" ++ show sigAnns
  -- putStrLn $ "changeLocalDecls:declAnns=" ++ show declAnns
  -- putStrLn $ "\nchangeLocalDecls:sigAnns'=" ++ show sigAnns'
  let (p',(ans',_),_w) = runTransform ans doAddLocal
      doAddLocal = SYB.everywhereM (SYB.mkM replaceLocalBinds) p
      replaceLocalBinds :: GHC.LMatch GHC.RdrName (GHC.LHsExpr GHC.RdrName)
                        -> Transform (GHC.LMatch GHC.RdrName (GHC.LHsExpr GHC.RdrName))
      replaceLocalBinds m@(GHC.L lm (GHC.Match mln pats typ (GHC.GRHSs rhs (GHC.EmptyLocalBinds)))) = do
        newSpan <- uniqueSrcSpanT
        let
          newAnnKey = AnnKey newSpan (CN "HsValBinds")
          addWhere mkds =
            case Map.lookup (mkAnnKey m) mkds of
              Nothing -> error "wtf"
              Just ann -> Map.insert newAnnKey ann2 mkds2
                where
                  ann1 = ann { annsDP = annsDP ann ++ [(G GHC.AnnWhere,DP (1,2))]
                             , annCapturedSpan = Just newAnnKey
                             , annSortKey = Just [ls, ld]
                             }
                  mkds2 = Map.insert (mkAnnKey m) ann1 mkds
                  ann2 = annNone
                             { annEntryDelta     = DP (1,0) }
        modifyAnnsT addWhere
        let decls = [s,d]
        -- logTr $ "(m,decls)=" ++ show (mkAnnKey m,map mkAnnKey decls)
        modifyAnnsT (captureOrderAnnKey newAnnKey decls)
        return (GHC.L lm (GHC.Match mln pats typ (GHC.GRHSs rhs
                        (GHC.HsValBinds
                          (GHC.ValBindsIn (GHC.listToBag $ [GHC.L ld decl])
                                          [GHC.L ls sig])))))
      replaceLocalBinds x = return x
  -- putStrLn $ "log:" ++ intercalate "\n" w
  return (mergeAnnList [declAnns',sigAnns',ans'],GHC.L l p')

-- ---------------------------------------------------------------------

-- | Add a local declaration with signature to LocalDecl
changeLocalDecls :: Changer
changeLocalDecls ans (GHC.L l p) = do
  Right (declAnns, d@(GHC.L ld (GHC.ValD decl))) <- withDynFlags (\df -> parseDecl df "decl" "nn = 2")
  Right (sigAnns, s@(GHC.L ls (GHC.SigD sig)))   <- withDynFlags (\df -> parseDecl df "sig"  "nn :: Int")
  let declAnns' = setPrecedingLines (GHC.L ld decl) 1 0 declAnns
  let  sigAnns' = setPrecedingLines (GHC.L ls  sig) 1 4 sigAnns
  -- putStrLn $ "changeLocalDecls:sigAnns=" ++ show sigAnns
  -- putStrLn $ "changeLocalDecls:declAnns=" ++ show declAnns
  -- putStrLn $ "\nchangeLocalDecls:sigAnns'=" ++ show sigAnns'
  let (p',(ans',_),_w) = runTransform ans doAddLocal
      doAddLocal = SYB.everywhereM (SYB.mkM replaceLocalBinds) p
      replaceLocalBinds :: GHC.LMatch GHC.RdrName (GHC.LHsExpr GHC.RdrName)
                        -> Transform (GHC.LMatch GHC.RdrName (GHC.LHsExpr GHC.RdrName))
      replaceLocalBinds m@(GHC.L lm (GHC.Match mln pats typ (GHC.GRHSs rhs (GHC.HsValBinds (GHC.ValBindsIn binds sigs))))) = do
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
        return (GHC.L lm (GHC.Match mln pats typ (GHC.GRHSs rhs
                        (GHC.HsValBinds
                          (GHC.ValBindsIn (GHC.listToBag $ (GHC.L ld decl):GHC.bagToList binds)
                                          (GHC.L ls sig:sigs))))))
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

    doRmDecl (GHC.L l (GHC.HsModule mmn mexp imps decls mdepr haddock)) = do
      let
        -- declIndex = 2 -- zero based
        decls1 = take declIndex decls
        decls2 = drop (declIndex + 1) decls
        decls' = decls1 ++ decls2
      return (GHC.L l (GHC.HsModule mmn mexp imps decls' mdepr haddock))
      -- error $ "doRmDecl:decls2=" ++ showGhc (length decls,decls1,decls2)

-- ---------------------------------------------------------------------
{-
-- |Convert the if statement in C.hs to a case, adjusting layout appropriately.
changeCifToCase :: Changer
changeCifToCase ans p = return (ans',p')
  where
    (p',(ans',_),_) = runTransform ans doTransform
    doTransform = SYB.everywhereM (SYB.mkM ifToCaseTransform) p

    ifToCaseTransform :: GHC.Located (GHC.HsExpr GHC.RdrName)
                      -> Transform (GHC.Located (GHC.HsExpr GHC.RdrName))
    ifToCaseTransform li@(GHC.L l (GHC.HsIf _se e1 e2 e3)) = do
      caseLoc        <- uniqueSrcSpanT -- HaRe:-1:1
      trueMatchLoc   <- uniqueSrcSpanT -- HaRe:-1:2
      trueLoc1       <- uniqueSrcSpanT -- HaRe:-1:3
      trueLoc        <- uniqueSrcSpanT -- HaRe:-1:4
      trueRhsLoc     <- uniqueSrcSpanT -- HaRe:-1:5
      falseLoc1      <- uniqueSrcSpanT -- HaRe:-1:6
      falseLoc       <- uniqueSrcSpanT -- HaRe:-1:7
      falseMatchLoc  <- uniqueSrcSpanT -- HaRe:-1:8
      falseRhsLoc    <- uniqueSrcSpanT -- HaRe:-1:9
      caseVirtualLoc <- uniqueSrcSpanT -- HaRe:-1:10
      let trueName  = mkRdrName "True"
      let falseName = mkRdrName "False"
      let ret = GHC.L caseLoc (GHC.HsCase e1
                 (GHC.MG
                  [
                    (GHC.L trueMatchLoc $ GHC.Match
                     Nothing
                     [
                       GHC.L trueLoc1 $ GHC.ConPatIn (GHC.L trueLoc trueName) (GHC.PrefixCon [])
                     ]
                     Nothing
                     (GHC.GRHSs
                       [
                         GHC.L trueRhsLoc $ GHC.GRHS [] e2
                       ] GHC.EmptyLocalBinds)
                    )
                  , (GHC.L falseMatchLoc $ GHC.Match
                     Nothing
                     [
                       GHC.L falseLoc1 $ GHC.ConPatIn (GHC.L falseLoc falseName) (GHC.PrefixCon [])
                     ]
                     Nothing
                     (GHC.GRHSs
                       [
                         GHC.L falseRhsLoc $ GHC.GRHS [] e3
                       ] GHC.EmptyLocalBinds)
                    )
                  ] [] GHC.placeHolderType GHC.FromSource))

      oldAnns <- getAnnsT
      let annIf   = gfromJust "Case.annIf"   $ getAnnotationEP li NotNeeded oldAnns
      let annCond = gfromJust "Case.annCond" $ getAnnotationEP e1 NotNeeded oldAnns
      let annThen = gfromJust "Case.annThen" $ getAnnotationEP e2 NotNeeded oldAnns
      let annElse = gfromJust "Case.annElse" $ getAnnotationEP e3 NotNeeded oldAnns
      logTr $ "Case:annIf="   ++ show annIf
      logTr $ "Case:annThen=" ++ show annThen
      logTr $ "Case:annElse=" ++ show annElse

      -- let ((_ifr,    ifc),  ifDP) = getOriginalPos oldAnns li (G GHC.AnnIf)
      -- let ((_thenr,thenc),thenDP) = getOriginalPos oldAnns li (G GHC.AnnThen)
      -- let ((_elser,elsec),elseDP) = getOriginalPos oldAnns li (G GHC.AnnElse)
      -- let newCol = ifc + 2
      let newCol = 6

      -- AZ:TODO: under some circumstances the GRHS annotations need LineSame, in others LineChanged.
      let ifDelta     = gfromJust "Case.ifDelta"     $ lookup (G GHC.AnnIf) (annsDP annIf)
      -- let ifSpanEntry = gfromJust "Case.ifSpanEntry" $ lookup AnnSpanEntry (annsDP annIf)
      -- let ifSpanEntry = annEntryDelta annIf
      let anne2' =
            [ ( AnnKey caseLoc       (CN "HsCase") NotNeeded,   annIf { annsDP = [ (G GHC.AnnCase, ifDelta)
                                                                     , (G GHC.AnnOf,     DP (0,1))]
                                                                     , annCapturedSpan = Just (AnnKey caseVirtualLoc (CN "(:)") NotNeeded)
                                                                     } )
            , ( AnnKey caseVirtualLoc (CN "(:)") NotNeeded,     Ann (DP (1,newCol)) (ColDelta newCol) (DP (1,newCol)) [] [] [(AnnSpanEntry,DP (1,0))] Nothing Nothing)
            , ( AnnKey trueMatchLoc  (CN "Match") NotNeeded,   annNone )
            , ( AnnKey trueLoc1      (CN "ConPatIn") NotNeeded, annNone )
            , ( AnnKey trueLoc       (CN "Unqual") NotNeeded,  annNone )
            , ( AnnKey trueRhsLoc    (CN "GRHS") NotNeeded,     Ann (DP (0,2)) 6 (DP (0,0)) [] [] [(AnnSpanEntry,DP (0,2)),(G GHC.AnnRarrow, DP (0,0))] Nothing Nothing )

            , ( AnnKey falseMatchLoc (CN "Match") NotNeeded,    Ann (DP (1,0)) 0 (DP (0,0)) [] [] [(AnnSpanEntry,DP (1,0))] Nothing Nothing )
            , ( AnnKey falseLoc1     (CN "ConPatIn") NotNeeded, annNone )
            , ( AnnKey falseLoc      (CN "Unqual") NotNeeded, annNone )
            , ( AnnKey falseRhsLoc   (CN "GRHS") NotNeeded,     Ann (DP (0,1)) 6 (DP (0,0)) [] [] [(AnnSpanEntry,DP (0,1)),(G GHC.AnnRarrow, DP (0,0))] Nothing Nothing )
            ]

      let annThen' = adjustAnnOffset (ColDelta 6) annThen
      let anne1 = modifyKeywordDeltas (Map.delete (AnnKey l (CN "HsIf") NotNeeded)) oldAnns
          final = modifyKeywordDeltas (\s -> Map.union s (Map.fromList anne2')) anne1
          anne3 = setLocatedAnns final
                    [ (e1, annCond)
                    , (e2, annThen')
                    , (e3, annElse)
                    ]
      putAnnsT anne3
      return ret
    ifToCaseTransform x = return x

    mkRdrName :: String -> GHC.RdrName
    mkRdrName s = GHC.mkVarUnqual (GHC.mkFastString s)
-}
-- ---------------------------------------------------------------------

noChange :: Changer
noChange ans parsed = return (ans,parsed)

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

rename :: (SYB.Data a) => String -> [(Pos, Pos)] -> a -> a
rename newNameStr spans a
  = SYB.everywhere ( SYB.mkT   replaceRdr
                    `SYB.extT` replaceHsVar
                    `SYB.extT` replacePat
                   ) a
  where
    newName = GHC.mkRdrUnqual (GHC.mkVarOcc newNameStr)

    cond :: GHC.SrcSpan -> Bool
    cond ln = ln `elem` srcSpans
      where
        srcSpans  = map (\(start, end) -> GHC.mkSrcSpan (f start) (f end)) spans
        fname = fromMaybe (GHC.mkFastString "f") (GHC.srcSpanFileName_maybe ln)
        f = uncurry (GHC.mkSrcLoc fname)


    replaceRdr :: GHC.Located GHC.RdrName -> GHC.Located GHC.RdrName
    replaceRdr (GHC.L ln _)
        | cond ln = GHC.L ln newName
    replaceRdr x = x

    replaceHsVar :: GHC.LHsExpr GHC.RdrName -> GHC.LHsExpr GHC.RdrName
    replaceHsVar (GHC.L ln (GHC.HsVar _))
        | cond ln = GHC.L ln (GHC.HsVar newName)
    replaceHsVar x = x

    replacePat (GHC.L ln (GHC.VarPat _))
        | cond ln = GHC.L ln (GHC.VarPat newName)
    replacePat x = x



-- ---------------------------------------------------------------------

changeWhereIn4 :: Changer
changeWhereIn4 ans parsed
  = return (ans,SYB.everywhere (SYB.mkT replace) parsed)
  where
    replace :: GHC.Located GHC.RdrName -> GHC.Located GHC.RdrName
    replace (GHC.L ln _n)
      | ln == (g (12,16) (12,17)) = GHC.L ln (GHC.mkRdrUnqual (GHC.mkVarOcc "p_2"))
      where
        g start end = GHC.mkSrcSpan (f start) (f end)
        fname = fromMaybe (GHC.mkFastString "f") (GHC.srcSpanFileName_maybe ln)
        f = uncurry (GHC.mkSrcLoc fname)
    replace x = x

-- ---------------------------------------------------------------------

changeLetIn1 :: Changer
changeLetIn1 ans parsed
  = return (ans,SYB.everywhere (SYB.mkT replace) parsed)
  where
    replace :: GHC.HsExpr GHC.RdrName -> GHC.HsExpr GHC.RdrName
    replace (GHC.HsLet localDecls expr@(GHC.L _ _))
      =
         let (GHC.HsValBinds (GHC.ValBindsIn bagDecls sigs)) = localDecls
             bagDecls' = GHC.listToBag $ init $ GHC.bagToList bagDecls
         in (GHC.HsLet (GHC.HsValBinds (GHC.ValBindsIn bagDecls' sigs)) expr)

    replace x = x

-- ---------------------------------------------------------------------


manipulateAstTestWithMod :: Changer -> String -> FilePath -> String -> IO Bool
manipulateAstTestWithMod change suffix file modname = manipulateAstTest' (Just (change, suffix)) False file modname

manipulateAstTestWFnameMod :: Changer -> FilePath -> String -> IO (FilePath,Bool)
manipulateAstTestWFnameMod change fileName modname
  = do r <- manipulateAstTestWithMod change "expected" fileName modname
       return (fileName,r)

manipulateAstTestWFnameBad :: FilePath -> String -> IO (FilePath,Bool)
manipulateAstTestWFnameBad fileName modname
  = do r <- manipulateAstTestWithMod noChange "bad" fileName modname
       return (fileName,r)

manipulateAstTest :: FilePath -> String -> IO Bool
manipulateAstTest file modname = manipulateAstTest' Nothing False file modname

manipulateAstTestWFname :: FilePath -> String -> IO (FilePath, Bool)
manipulateAstTestWFname file modname = (file,) <$> manipulateAstTest file modname


mkTestModBad :: FilePath -> String -> Test
mkTestModBad fileName modName
  = TestCase (do r <- manipulateAstTestWithMod noChange "bad" fileName modName
                 assertBool fileName r )

manipulateAstTest' :: Maybe (Changer, String)
                   -> Bool -> FilePath -> String -> IO Bool
manipulateAstTest' mchange useTH file' modname = do
  let testpath = "./tests/examples/"
      file     = testpath </> file'
      out      = file <.> "out"

  contents <- case mchange of
                   Nothing                 -> readFile file
                   Just (_,expectedSuffix) -> readFile (file <.> expectedSuffix)
  (ghcAnns',p,cppComments) <- hSilence [stderr] $  parsedFileGhc file modname useTH
  -- (ghcAnns',p,cppComments) <-                      parsedFileGhc file modname useTH
  let
    parsedOrig = GHC.pm_parsed_source $ p
    (ghcAnns,parsed) = (ghcAnns', parsedOrig)
    parsedAST = showAnnData emptyAnns 0 parsed
    -- cppComments = map (tokComment . commentToAnnotation . fst) cppCommentToks
    -- parsedAST = showGhc parsed
       -- `debug` ("getAnn:=" ++ (show (getAnnotationValue (snd ann) (GHC.getLoc parsed) :: Maybe AnnHsModule)))
    -- try to pretty-print; summarize the test result
    ann = relativiseApiAnnsWithComments cppComments parsedOrig ghcAnns'
      `debug` ("ghcAnns:" ++ showGhc ghcAnns)

  (ann',parsed') <- case mchange of
                   Nothing         -> return (ann,parsed)
                   Just (change,_) -> change ann parsed

  let
    printed = exactPrint parsed' ann' -- `debug` ("ann=" ++ (show $ map (\(s,a) -> (ss2span s, a)) $ Map.toList ann))
    outcome = if printed == contents
                then "Match\n"
                else "Fail\n"
    result = printed ++ "\n==============\n"
             ++ outcome ++ "\n==============\n"
             ++ "lengths:" ++ show (length printed,length contents) ++ "\n"
             ++ showAnnData ann' 0 parsed'
             ++ "\n========================\n"
             ++ showGhc ann'
             ++ "\n========================\n"
             ++ showGhc ghcAnns
             ++ "\n========================\n"
             ++ parsedAST
             ++ "\n========================\n"
             ++ showGhc ann
  -- putStrLn $ "Test:ann :" ++ showGhc ann
  writeFile out $ result
  -- putStrLn $ "Test:contents' :" ++ contents
  -- putStrLn $ "Test:parsed=" ++ parsedAST
  -- putStrLn $ "Test:showdata:parsedOrig" ++ SYB.showData SYB.Parser 0 parsedOrig
  -- putStrLn $ "Test:ann :" ++ showGhc ann
  -- putStrLn $ "Test:ghcAnns :" ++ showGhc ghcAnns
  -- putStrLn $ "Test:ghcAnns' :" ++ showGhc ghcAnns'
  -- putStrLn $ "Test:showdata:" ++ showAnnData ann 0 parsed
  -- putStrLn $ "Test:showdata:parsed'" ++ SYB.showData SYB.Parser 0 parsed'
  -- putStrLn $ "Test:showdata:parsed'" ++ showAnnData ann 0 parsed'
  -- putStrLn $ "Test:outcome' :" ++ outcome
  return (printed == contents)


-- ---------------------------------------------------------------------
-- |Result of parsing a Haskell source file. It is simply the
-- TypeCheckedModule produced by GHC.
type ParseResult = GHC.ParsedModule

parsedFileGhc :: String -> String -> Bool -> IO (GHC.ApiAnns,ParseResult,[Comment])
parsedFileGhc fileName _modname useTH = do
    -- putStrLn $ "parsedFileGhc:" ++ show fileName
    GHC.defaultErrorHandler GHC.defaultFatalMessager GHC.defaultFlushOut $ do
      GHC.runGhc (Just libdir) $ do
        dflags <- GHC.getSessionDynFlags
        let dflags2 = dflags { GHC.importPaths = ["./tests/examples/","../tests/examples/",
                                                  "./src/","../src/"] }
            tgt = if useTH then GHC.HscInterpreted
                           else GHC.HscNothing -- allows FFI
            dflags3 = dflags2 { GHC.hscTarget = tgt
                              , GHC.ghcLink =  GHC.LinkInMemory
                              }

            dflags4 = GHC.gopt_set dflags3 GHC.Opt_KeepRawTokenStream

        (dflags5,_args,_warns) <- GHC.parseDynamicFlagsCmdLine dflags4 [GHC.noLoc "-package ghc"]
        -- GHC.liftIO $ putStrLn $ "dflags set:(args,warns)" ++ show (map GHC.unLoc _args,map GHC.unLoc _warns)
        void $ GHC.setSessionDynFlags dflags5
        -- GHC.liftIO $ putStrLn $ "dflags set"

        -- hsc_env <- GHC.getSession
        -- (dflags6,fn_pp) <- GHC.liftIO $ GHC.preprocess hsc_env (fileName,Nothing)
        -- GHC.liftIO $ putStrLn $ "preprocess got:" ++ show fn_pp


        target <- GHC.guessTarget fileName Nothing
        GHC.setTargets [target]
        -- GHC.liftIO $ putStrLn $ "target set:" ++ showGhc (GHC.targetId target)
        void $ GHC.load GHC.LoadAllTargets -- Loads and compiles, much as calling make
        -- GHC.liftIO $ putStrLn $ "targets loaded"
        -- g <- GHC.getModuleGraph
        -- let showStuff ms = show (GHC.moduleNameString $ GHC.moduleName $ GHC.ms_mod ms,GHC.ms_location ms)
        -- GHC.liftIO $ putStrLn $ "module graph:" ++ (intercalate "," (map showStuff g))

        -- modSum <- GHC.getModSummary $ GHC.mkModuleName modname
        Just modSum <- getModSummaryForFile fileName
        -- GHC.liftIO $ putStrLn $ "got modSum"
        -- let modSum = head g
        cppComments <-  if (GHC.xopt GHC.Opt_Cpp dflags5)
                        then getCppTokensAsComments defaultCppOptions fileName
                        else return []
        -- let cppComments = [] :: [(GHC.Located GHC.Token, String)]
--        GHC.liftIO $ putStrLn $ "\ncppTokensAsComments for:"  ++ fileName ++ "=========\n"
--                              ++ showGhc cppComments ++ "\n================\n"
{-
        (sourceFile, source, flags) <- getModuleSourceAndFlags (GHC.ms_mod modSum)
        strSrcBuf <- getPreprocessedSrc sourceFile
        GHC.liftIO $ putStrLn $ "preprocessedSrc====\n" ++ strSrcBuf ++ "\n================\n"
-}
        p <- GHC.parseModule modSum
        -- GHC.liftIO $ putStrLn $ "got parsedModule"
--        t <- GHC.typecheckModule p
        -- GHC.liftIO $ putStrLn $ "typechecked"
        -- toks <- GHC.getRichTokenStream (GHC.ms_mod modSum)
        -- GHC.liftIO $ putStrLn $ "toks" ++ show toks
        let anns = GHC.pm_annotations p
        -- GHC.liftIO $ putStrLn $ "anns"
        return (anns,p,cppComments)

-- ---------------------------------------------------------------------

transformHighLevelTests :: [Test]
transformHighLevelTests =
  [
    mkTestModChange addLocaLDecl1  "AddLocalDecl1.hs"  "AddLocalDecl1"
  , mkTestModChange addLocaLDecl2  "AddLocalDecl2.hs"  "AddLocalDecl2"
  , mkTestModChange addLocaLDecl3  "AddLocalDecl3.hs"  "AddLocalDecl3"
  , mkTestModChange addLocaLDecl4  "AddLocalDecl4.hs"  "AddLocalDecl4"
  , mkTestModChange addLocaLDecl5  "AddLocalDecl5.hs"  "AddLocalDecl5"
  , mkTestModChange addLocaLDecl6  "AddLocalDecl6.hs"  "AddLocalDecl6"

  , mkTestModChange rmDecl1 "RmDecl1.hs" "RmDecl1"
  , mkTestModChange rmDecl2 "RmDecl2.hs" "RmDecl2"
  , mkTestModChange rmDecl3 "RmDecl3.hs" "RmDecl3"
  , mkTestModChange rmDecl4 "RmDecl4.hs" "RmDecl4"
  , mkTestModChange rmDecl5 "RmDecl5.hs" "RmDecl5"
  , mkTestModChange rmDecl6 "RmDecl6.hs" "RmDecl6"
  , mkTestModChange rmDecl7 "RmDecl7.hs" "RmDecl7"

  , mkTestModChange rmTypeSig1 "RmTypeSig1.hs" "RmTypeSig1"
  , mkTestModChange rmTypeSig2 "RmTypeSig2.hs" "RmTypeSig2"

  , mkTestModChange addHiding1 "AddHiding1.hs" "AddHiding1"
  , mkTestModChange addHiding2 "AddHiding2.hs" "AddHiding2"

  , mkTestModChange cloneDecl1 "CloneDecl1.hs" "CloneDecl1"
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

  let (lp',(ans',_),_w) = runTransform (mergeAnns ans declAnns') doAddLocal
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

  let (lp',(ans',_),_w) = runTransform ans doAddLocal
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

        let GHC.L _ (GHC.ValD (GHC.FunBind  _ _ (GHC.MG [m1,m2] _ _ _) _ _ _)) = d1
        balanceComments m1 m2

        (d1',_) <- modifyValD (GHC.getLoc m1) d1 $ \_m decls -> do
           return ((newDecl : decls),Nothing)
        replaceDecls lp [d1', d2]

  let (lp',(ans',_),_w) = runTransform (mergeAnns ans declAnns') doAddLocal
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

  let (lp',(ans',_),_w) = runTransform ans doRmDecl
  return (ans',lp')

-- ---------------------------------------------------------------------

rmDecl2 :: Changer
rmDecl2 ans lp = do
  let
      doRmDecl = do
        let
          go :: GHC.LHsExpr GHC.RdrName -> Transform (GHC.LHsExpr GHC.RdrName)
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

  let (lp',(ans',_),_w) = runTransform ans doRmDecl
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

  let (lp',(ans',_),_w) = runTransform ans doRmDecl
  return (ans',lp')

-- ---------------------------------------------------------------------

rmDecl5 :: Changer
rmDecl5 ans lp = do
  let
      doRmDecl = do
        let
          go :: GHC.HsExpr GHC.RdrName -> Transform (GHC.HsExpr GHC.RdrName)
          go (GHC.HsLet lb expr) = do
            decs <- hsDeclsValBinds lb
            let dec = last decs
            transferEntryDPT (head decs) dec
            lb' <- replaceDeclsValbinds lb [dec]
            return (GHC.HsLet lb' expr)
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

  let (lp',(ans',_),_w) = runTransform ans doRmDecl
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
             (GHC.L l (GHC.SigD (GHC.TypeSig names typ p))) = s1
             s1' = (GHC.L l (GHC.SigD (GHC.TypeSig (tail names) typ p)))
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
          v1 = GHC.L l1 (GHC.IEVar n1)
          v2 = GHC.L l2 (GHC.IEVar n2)
          impHiding = GHC.L l0 [v1,v2]
          imp1' = imp1 { GHC.ideclHiding = Just (True,impHiding)}
          p' = p { GHC.hsmodImports = [GHC.L li imp1',imp2]}
        addSimpleAnnT impHiding (DP (0,1)) [((G GHC.AnnHiding),DP (0,0)),((G GHC.AnnOpenP),DP (0,1)),((G GHC.AnnCloseP),DP (0,0))]
        addSimpleAnnT n1        (DP (0,0)) [((G GHC.AnnVal),DP (0,0)),((G GHC.AnnComma),DP (0,0))]
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
          (GHC.L _ (GHC.IEVar ln)) = last ns
          n1 = GHC.L l1 (GHC.mkVarUnqual (GHC.mkFastString "n1"))
          n2 = GHC.L l2 (GHC.mkVarUnqual (GHC.mkFastString "n2"))
          v1 = GHC.L l1 (GHC.IEVar n1)
          v2 = GHC.L l2 (GHC.IEVar n2)
          imp1' = imp1 { GHC.ideclHiding = Just (True,GHC.L lh (ns ++ [v1,v2]))}
          p' = p { GHC.hsmodImports = [GHC.L li imp1']}
        addSimpleAnnT n1        (DP (0,0)) [((G GHC.AnnVal),DP (0,0)),((G GHC.AnnComma),DP (0,0))]
        addSimpleAnnT n2        (DP (0,0)) [((G GHC.AnnVal),DP (0,0))]
        addTrailingCommaT ln
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
