{-# LANGUAGE CPP #-}
module Language.Haskell.GHC.ExactPrint.Lookup
  (
    keywordToString
#if __GLASGOW_HASKELL__ <= 710
  , unicodeString
#endif
  ) where

import Language.Haskell.GHC.ExactPrint.Types
import qualified GHC (AnnKeywordId(..))
#if __GLASGOW_HASKELL__ <= 710
import Data.Maybe
#endif

-- | Maps `AnnKeywordId` to the corresponding String representation.
-- There is no specific mapping for the following constructors.
-- `AnnOpen`, `AnnClose`, `AnnVal`, `AnnPackageName`, `AnnHeader`, `AnnFunId`,
-- `AnnInfix`
keywordToString :: KeywordId -> String
keywordToString kw =
  let mkErr x = error $ "keywordToString: missing case for:" ++ show x
  in
  case kw of
      -- Specifically handle all cases so that there are pattern match
      -- warnings if new constructors are added.
      AnnComment _      -> mkErr kw
      AnnString _       -> mkErr kw
#if __GLASGOW_HASKELL__ >= 808
      (AnnEofPos       ) -> mkErr kw
#endif
#if __GLASGOW_HASKELL__ <= 710
      AnnUnicode kw'    -> keywordToString (G kw')
#endif
      AnnSemiSep        -> ";"
#if __GLASGOW_HASKELL__ >= 801
      (G GHC.AnnAnyclass) -> "anyclass"
#endif
      (G GHC.AnnOpen  ) -> mkErr kw
      (G GHC.AnnClose ) -> mkErr kw
      (G GHC.AnnVal   ) -> mkErr kw
      (G GHC.AnnPackageName) -> mkErr kw
      (G GHC.AnnHeader ) -> mkErr kw
      (G GHC.AnnFunId  ) -> mkErr kw
      (G GHC.AnnInfix  ) -> mkErr kw
      (G GHC.AnnValStr ) -> mkErr kw
      (G GHC.AnnName   ) -> mkErr kw
      (G GHC.AnnAs     ) -> "as"
      (G GHC.AnnAt     ) -> "@"
      (G GHC.AnnBang   ) -> "!"
      (G GHC.AnnBackquote ) -> "`"
      (G GHC.AnnBy     ) -> "by"
      (G GHC.AnnCase   ) -> "case"
      (G GHC.AnnClass   ) -> "class"
#if __GLASGOW_HASKELL__ >= 801
      (G GHC.AnnCloseB  ) -> "|)"
      (G GHC.AnnCloseBU ) -> "⦈"
#endif
      (G GHC.AnnCloseC  ) -> "}"
      (G GHC.AnnCloseP  ) -> ")"
#if __GLASGOW_HASKELL__ >= 801
      (G GHC.AnnCloseQ  ) -> "|]"
      (G GHC.AnnCloseQU ) -> "⟧"
#endif
      (G GHC.AnnCloseS  ) -> "]"
      (G GHC.AnnColon   ) -> ":"
      (G GHC.AnnComma   ) -> ","
      (G GHC.AnnCommaTuple ) -> ","
      (G GHC.AnnDarrow  ) -> "=>"
      (G GHC.AnnData    ) -> "data"
      (G GHC.AnnDcolon  ) -> "::"
      (G GHC.AnnDefault ) -> "default"
      (G GHC.AnnDeriving ) -> "deriving"
      (G GHC.AnnDo       ) -> "do"
      (G GHC.AnnDot      ) -> "."
      (G GHC.AnnDotdot   ) -> ".."
      (G GHC.AnnElse     ) -> "else"
      (G GHC.AnnEqual    ) -> "="
      (G GHC.AnnExport   ) -> "export"
      (G GHC.AnnFamily   ) -> "family"
      (G GHC.AnnForall   ) -> "forall"
      (G GHC.AnnForeign  ) -> "foreign"
      (G GHC.AnnGroup    ) -> "group"
      (G GHC.AnnHiding   ) -> "hiding"
      (G GHC.AnnIf       ) -> "if"
      (G GHC.AnnImport   ) -> "import"
      (G GHC.AnnIn       ) -> "in"
      (G GHC.AnnInstance ) -> "instance"
      (G GHC.AnnLam      ) -> "\\"
      (G GHC.AnnLarrow   ) -> "<-"
      (G GHC.AnnLet      ) -> "let"
#if __GLASGOW_HASKELL__ >= 808
      -- (G GHC.AnnLolly    ) -> "#->"
      (G GHC.AnnLollyU    ) -> "⊸"
#endif
      (G GHC.AnnMdo      ) -> "mdo"
      (G GHC.AnnMinus    ) -> "-"
      (G GHC.AnnModule   ) -> "module"
#if __GLASGOW_HASKELL__ >= 808
      (G GHC.AnnPercent   ) -> "%"
      (G GHC.AnnPercentOne) -> "%1"
#endif
      (G GHC.AnnNewtype  ) -> "newtype"
      (G GHC.AnnOf       ) -> "of"
#if __GLASGOW_HASKELL__ >= 801
      (G GHC.AnnOpenB    ) -> "(|"
      (G GHC.AnnOpenBU   ) ->  "⦇"
#endif
      (G GHC.AnnOpenC    ) -> "{"
#if __GLASGOW_HASKELL__ > 710
      (G GHC.AnnOpenE    ) -> "[e|"
#endif
#if __GLASGOW_HASKELL__ >= 801
      (G GHC.AnnOpenEQ   ) -> "[|"
      (G GHC.AnnOpenEQU  ) ->  "⟦"
#endif
      (G GHC.AnnOpenP    ) -> "("
#if __GLASGOW_HASKELL__ < 808
      (G GHC.AnnOpenPE   ) -> "$("
      (G GHC.AnnOpenPTE  ) -> "$$("
#endif
      (G GHC.AnnOpenS    ) -> "["
      (G GHC.AnnPattern  ) -> "pattern"
      (G GHC.AnnProc     ) -> "proc"
      (G GHC.AnnQualified ) -> "qualified"
      (G GHC.AnnRarrow   ) -> "->"
      (G GHC.AnnRec      ) -> "rec"
      (G GHC.AnnRole     ) -> "role"
      (G GHC.AnnSafe     ) -> "safe"
      (G GHC.AnnSemi     ) -> ";"
#if __GLASGOW_HASKELL__ >= 801
      (G GHC.AnnSignature) -> "signature"
      (G GHC.AnnStock    ) -> "stock"
#endif
      (G GHC.AnnStatic   ) -> "static"
      (G GHC.AnnThen     ) -> "then"
      (G GHC.AnnTilde    ) -> "~"
#if __GLASGOW_HASKELL__ <= 804
      (G GHC.AnnTildehsh ) -> "~#"
#endif
      (G GHC.AnnType     ) -> "type"
      (G GHC.AnnUnit     ) -> "()"
      (G GHC.AnnUsing    ) -> "using"
      (G GHC.AnnVbar     ) -> "|"
      (G GHC.AnnWhere    ) -> "where"
      (G GHC.Annlarrowtail ) -> "-<"
      (G GHC.Annrarrowtail ) -> ">-"
      (G GHC.AnnLarrowtail ) -> "-<<"
      (G GHC.AnnRarrowtail ) -> ">>-"
      (G GHC.AnnSimpleQuote ) -> "'"
      (G GHC.AnnThTyQuote   ) -> "''"
#if __GLASGOW_HASKELL__ >= 808
      (G GHC.AnnDollar       ) -> "$"
      (G GHC.AnnDollarDollar ) -> "$$"
#else
      (G GHC.AnnThIdSplice  ) -> "$"
      (G GHC.AnnThIdTySplice ) -> "$$"
#endif
#if __GLASGOW_HASKELL__ < 808
      (G GHC.AnnEofPos       ) -> ""
#endif
#if __GLASGOW_HASKELL__ > 710
      (G GHC.AnnDarrowU) -> "⇒"
      (G GHC.AnnDcolonU) -> "∷"
      (G GHC.AnnForallU) -> "∀"
      (G GHC.AnnLarrowU) -> "←"
      (G GHC.AnnLarrowtailU) -> "⤛"
      (G GHC.AnnRarrowU) -> "→"
      (G GHC.AnnRarrowtailU) -> "⤜"
      (G GHC.AnnlarrowtailU) -> "⤙"
      (G GHC.AnnrarrowtailU) -> "⤚"
#endif
#if __GLASGOW_HASKELL__ >= 800
      AnnTypeApp             -> "@"
#endif
#if __GLASGOW_HASKELL__ > 804
      (G GHC.AnnVia) -> "via"
#endif

#if __GLASGOW_HASKELL__ <= 710
-- | Tries to find a unicode equivalent to a 'KeywordId'.
-- If none exists then fall back to find the ASCII version.
unicodeString :: KeywordId -> String
unicodeString kw =
  fromMaybe (keywordToString kw) (lookup kw unicodeChars)

unicodeChars :: [(KeywordId, String)]
unicodeChars =
    -- AZ:TODO:make this a Data.Map, doing linear scan each time
      [ (G GHC.AnnDarrow, "⇒")
      , (G GHC.AnnDcolon, "∷")
      , (G GHC.AnnForall, "∀")
      , (G GHC.AnnLarrow, "←")
      , (G GHC.AnnLarrowtail, "⤛")
      , (G GHC.AnnRarrow, "→")
      , (G GHC.AnnRarrowtail, "⤜")
      , (G GHC.Annlarrowtail, "⤙")
      , (G GHC.Annrarrowtail, "⤚")
      ]

{-
From Lexer.x

       ,("∷",   ITdcolon, unicodeSyntaxEnabled)
       ,("⇒",   ITdarrow, unicodeSyntaxEnabled)
       ,("∀",   ITforall, unicodeSyntaxEnabled)
       ,("→",   ITrarrow, unicodeSyntaxEnabled)
       ,("←",   ITlarrow, unicodeSyntaxEnabled)

       ,("⤙",   ITlarrowtail, \i -> unicodeSyntaxEnabled i && arrowsEnabled i)
       ,("⤚",   ITrarrowtail, \i -> unicodeSyntaxEnabled i && arrowsEnabled i)
       ,("⤛",   ITLarrowtail, \i -> unicodeSyntaxEnabled i && arrowsEnabled i)
       ,("⤜",   ITRarrowtail, \i -> unicodeSyntaxEnabled i && arrowsEnabled i)

       ,("★", ITstar, unicodeSyntaxEnabled)

-}

#endif
