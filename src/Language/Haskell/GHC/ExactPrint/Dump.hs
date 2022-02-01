
{-
Temporary copy of the GHC.Hs.Dump module, modified to show DeltaPos
hacked into a SrcSpan.
-}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Haskell.GHC.ExactPrint.Dump (
        -- * Dumping ASTs
        showAstData,
        BlankSrcSpan(..),
        BlankEpAnnotations(..),
    ) where

import Prelude ()
import GHC.Prelude

import GHC.Hs

import GHC.Core.DataCon

import GHC.Data.Bag
import GHC.Data.FastString
import GHC.Types.Name.Set
import GHC.Types.Name
import GHC.Types.SrcLoc
import GHC.Types.Var
import GHC.Types.SourceText
import GHC.Unit.Module
import GHC.Utils.Outputable

import Data.Data hiding (Fixity)
import qualified Data.ByteString as B

import Data.Generics (extQ, ext1Q, ext2Q)
import Language.Haskell.GHC.ExactPrint.Utils

data BlankSrcSpan = BlankSrcSpan | BlankSrcSpanFile | NoBlankSrcSpan
                  deriving (Eq,Show)

data BlankEpAnnotations = BlankEpAnnotations | NoBlankEpAnnotations
                  deriving (Eq,Show)

-- | Show a GHC syntax tree. This parameterised because it is also used for
-- comparing ASTs in ppr roundtripping tests, where the SrcSpan's are blanked
-- out, to avoid comparing locations, only structure
showAstData :: Data a => BlankSrcSpan -> BlankEpAnnotations -> a -> SDoc
showAstData bs ba a0 = blankLine $$ showAstData' a0
  where
    showAstData' :: Data a => a -> SDoc
    showAstData' =
      generic
              `ext1Q` list
              `extQ` string `extQ` fastString `extQ` srcSpan `extQ` realSrcSpan
              `extQ` annotation
              `extQ` annotationModule
              `extQ` annotationAddEpAnn
              `extQ` annotationGrhsAnn
              `extQ` annotationEpAnnHsCase
              `extQ` annotationEpAnnHsLet
              `extQ` annotationAnnList
              `extQ` annotationEpAnnImportDecl
              `extQ` annotationAnnParen
              `extQ` annotationTrailingAnn
              `extQ` annotationEpaLocation
              `extQ` addEpAnn
              `extQ` lit `extQ` litr `extQ` litt
              `extQ` sourceText
              `extQ` deltaPos
              `extQ` epaAnchor
              `extQ` bytestring
              `extQ` name `extQ` occName `extQ` moduleName `extQ` var
              `extQ` dataCon
              `extQ` bagName `extQ` bagRdrName `extQ` bagVar `extQ` nameSet
              `extQ` fixity
              `ext2Q` located
              `extQ` srcSpanAnnA
              `extQ` srcSpanAnnL
              `extQ` srcSpanAnnP
              `extQ` srcSpanAnnC
              `extQ` srcSpanAnnN

      where generic :: Data a => a -> SDoc
            generic t = parens $ text (showConstr (toConstr t))
                                  $$ vcat (gmapQ showAstData' t)

            string :: String -> SDoc
            string     = text . normalize_newlines . show

            fastString :: FastString -> SDoc
            fastString s = braces $
                            text "FastString:"
                        <+> text (normalize_newlines . show $ s)

            bytestring :: B.ByteString -> SDoc
            bytestring = text . normalize_newlines . show

            list []    = brackets empty
            list [x]   = brackets (showAstData' x)
            list (x1 : x2 : xs) =  (text "[" <> showAstData' x1)
                                $$ go x2 xs
              where
                go y [] = text "," <> showAstData' y <> text "]"
                go y1 (y2 : ys) = (text "," <> showAstData' y1) $$ go y2 ys

            -- Eliminate word-size dependence
            lit :: HsLit GhcPs -> SDoc
            lit (HsWordPrim   s x) = numericLit "HsWord{64}Prim" x s
            lit (HsWord64Prim s x) = numericLit "HsWord{64}Prim" x s
            lit (HsIntPrim    s x) = numericLit "HsInt{64}Prim"  x s
            lit (HsInt64Prim  s x) = numericLit "HsInt{64}Prim"  x s
            lit l                  = generic l

            litr :: HsLit GhcRn -> SDoc
            litr (HsWordPrim   s x) = numericLit "HsWord{64}Prim" x s
            litr (HsWord64Prim s x) = numericLit "HsWord{64}Prim" x s
            litr (HsIntPrim    s x) = numericLit "HsInt{64}Prim"  x s
            litr (HsInt64Prim  s x) = numericLit "HsInt{64}Prim"  x s
            litr l                  = generic l

            litt :: HsLit GhcTc -> SDoc
            litt (HsWordPrim   s x) = numericLit "HsWord{64}Prim" x s
            litt (HsWord64Prim s x) = numericLit "HsWord{64}Prim" x s
            litt (HsIntPrim    s x) = numericLit "HsInt{64}Prim"  x s
            litt (HsInt64Prim  s x) = numericLit "HsInt{64}Prim"  x s
            litt l                  = generic l

            numericLit :: String -> Integer -> SourceText -> SDoc
            numericLit tag x s = braces $ hsep [ text tag
                                               , generic x
                                               , generic s ]

            sourceText :: SourceText -> SDoc
            sourceText NoSourceText = parens $ text "NoSourceText"
            sourceText (SourceText src) = case bs of
              NoBlankSrcSpan   -> parens $ text "SourceText" <+> text src
              BlankSrcSpanFile -> parens $ text "SourceText" <+> text src
              _                -> parens $ text "SourceText" <+> text "blanked"

            epaAnchor :: EpaLocation -> SDoc
            epaAnchor (EpaSpan r)  = parens $ text "EpaSpan" <+> realSrcSpan r
            epaAnchor (EpaDelta d cs) = case ba of
              NoBlankEpAnnotations -> parens $ text "EpaDelta" <+> deltaPos d <+> showAstData' cs
              BlankEpAnnotations -> parens $ text "EpaDelta" <+> deltaPos d <+> text "blanked"

            deltaPos :: DeltaPos -> SDoc
            deltaPos (SameLine c) = parens $ text "SameLine" <+> ppr c
            deltaPos (DifferentLine l c) = parens $ text "DifferentLine" <+> ppr l <+> ppr c

            name :: Name -> SDoc
            name nm    = braces $ text "Name:" <+> ppr nm

            occName n  =  braces $
                          text "OccName:"
                      <+> text (occNameString n)

            moduleName :: ModuleName -> SDoc
            moduleName m = braces $ text "ModuleName:" <+> ppr m

            srcSpan :: SrcSpan -> SDoc
            srcSpan ss = case bs of
             BlankSrcSpan -> text "{ ss }"
             NoBlankSrcSpan -> braces $ char ' ' <>
                             (hang (pprSrcSpanWithAnchor ss) 1
                                   (text ""))
             BlankSrcSpanFile -> braces $ char ' ' <>
                             (hang (pprUserSpan False ss) 1
                                   (text ""))

            realSrcSpan :: RealSrcSpan -> SDoc
            realSrcSpan ss = case bs of
             BlankSrcSpan -> text "{ ss }"
             NoBlankSrcSpan -> braces $ char ' ' <>
                             (hang (ppr ss) 1
                                   (text ""))
             BlankSrcSpanFile -> braces $ char ' ' <>
                             (hang (pprUserRealSpan False ss) 1
                                   (text ""))


            addEpAnn :: AddEpAnn -> SDoc
            addEpAnn (AddEpAnn a s) = case ba of
             BlankEpAnnotations -> parens
                                      $ text "blanked:" <+> text "AddEpAnn"
             NoBlankEpAnnotations ->
              parens $ text "AddEpAnn" <+> ppr a <+> epaAnchor s

            var  :: Var -> SDoc
            var v      = braces $ text "Var:" <+> ppr v

            dataCon :: DataCon -> SDoc
            dataCon c  = braces $ text "DataCon:" <+> ppr c

            bagRdrName:: Bag (LocatedA (HsBind GhcPs)) -> SDoc
            bagRdrName bg =  braces $
                             text "Bag(LocatedA (HsBind GhcPs)):"
                          $$ (list . bagToList $ bg)

            bagName   :: Bag (LocatedA (HsBind GhcRn)) -> SDoc
            bagName bg  =  braces $
                           text "Bag(LocatedA (HsBind Name)):"
                        $$ (list . bagToList $ bg)

            bagVar    :: Bag (LocatedA (HsBind GhcTc)) -> SDoc
            bagVar bg  =  braces $
                          text "Bag(LocatedA (HsBind Var)):"
                       $$ (list . bagToList $ bg)

            nameSet ns =  braces $
                          text "NameSet:"
                       $$ (list . nameSetElemsStable $ ns)

            fixity :: Fixity -> SDoc
            fixity fx =  braces $
                         text "Fixity:"
                     <+> ppr fx

            located :: (Data a, Data b) => GenLocated a b -> SDoc
            located (L ss a)
              = parens (text "L"
                        $$ vcat [showAstData' ss, showAstData' a])


            -- -------------------------

            annotation :: EpAnn [AddEpAnn] -> SDoc
            annotation = annotation' (text "EpAnn [AddEpAnn]")

            annotationModule :: EpAnn AnnsModule -> SDoc
            annotationModule = annotation' (text "EpAnn AnnsModule")

            annotationAddEpAnn :: EpAnn AddEpAnn -> SDoc
            annotationAddEpAnn = annotation' (text "EpAnn AddEpAnn")

            annotationGrhsAnn :: EpAnn GrhsAnn -> SDoc
            annotationGrhsAnn = annotation' (text "EpAnn GrhsAnn")

            annotationEpAnnHsCase :: EpAnn EpAnnHsCase -> SDoc
            annotationEpAnnHsCase = annotation' (text "EpAnn EpAnnHsCase")

            annotationEpAnnHsLet :: EpAnn AnnsLet -> SDoc
            annotationEpAnnHsLet = annotation' (text "EpAnn AnnsLet")

            annotationAnnList :: EpAnn AnnList -> SDoc
            annotationAnnList = annotation' (text "EpAnn AnnList")

            annotationEpAnnImportDecl :: EpAnn EpAnnImportDecl -> SDoc
            annotationEpAnnImportDecl = annotation' (text "EpAnn EpAnnImportDecl")

            annotationAnnParen :: EpAnn AnnParen -> SDoc
            annotationAnnParen = annotation' (text "EpAnn AnnParen")

            annotationTrailingAnn :: EpAnn TrailingAnn -> SDoc
            annotationTrailingAnn = annotation' (text "EpAnn TrailingAnn")

            annotationEpaLocation :: EpAnn EpaLocation -> SDoc
            annotationEpaLocation = annotation' (text "EpAnn EpaLocation")

            annotation' :: forall a .(Data a)
                       => SDoc -> EpAnn a -> SDoc
            annotation' tag anns = case ba of
             BlankEpAnnotations -> parens (text "blanked:" <+> tag)
             NoBlankEpAnnotations -> parens $ text (showConstr (toConstr anns))
                                               $$ vcat (gmapQ showAstData' anns)

            -- -------------------------

            srcSpanAnnA :: SrcSpanAnn' (EpAnn AnnListItem) -> SDoc
            srcSpanAnnA = locatedAnn'' (text "SrcSpanAnnA")

            srcSpanAnnL :: SrcSpanAnn' (EpAnn AnnList) -> SDoc
            srcSpanAnnL = locatedAnn'' (text "SrcSpanAnnL")

            srcSpanAnnP :: SrcSpanAnn' (EpAnn AnnPragma) -> SDoc
            srcSpanAnnP = locatedAnn'' (text "SrcSpanAnnP")

            srcSpanAnnC :: SrcSpanAnn' (EpAnn AnnContext) -> SDoc
            srcSpanAnnC = locatedAnn'' (text "SrcSpanAnnC")

            srcSpanAnnN :: SrcSpanAnn' (EpAnn NameAnn) -> SDoc
            srcSpanAnnN = locatedAnn'' (text "SrcSpanAnnN")

            locatedAnn'' :: forall a. (Data a)
              => SDoc -> SrcSpanAnn' a -> SDoc
            locatedAnn'' tag ss = parens $
              case cast ss of
                Just ((SrcSpanAnn ann s) :: SrcSpanAnn' a) ->
                  case ba of
                    BlankEpAnnotations
                      -> parens (text "blanked:" <+> tag)
                    NoBlankEpAnnotations
                      -> text "SrcSpanAnn" <+> showAstData' ann
                              <+> srcSpan s
                Nothing -> text "locatedAnn:unmatched" <+> tag
                           <+> (parens $ text (showConstr (toConstr ss)))


normalize_newlines :: String -> String
normalize_newlines ('\\':'r':'\\':'n':xs) = '\\':'n':normalize_newlines xs
normalize_newlines (x:xs)                 = x:normalize_newlines xs
normalize_newlines []                     = []

pprSrcSpanWithAnchor :: SrcSpan -> SDoc
pprSrcSpanWithAnchor ss@(UnhelpfulSpan _) = ppr ss
pprSrcSpanWithAnchor ss = ppr ss <+> parens (ppr (hackSrcSpanToAnchor ss))
