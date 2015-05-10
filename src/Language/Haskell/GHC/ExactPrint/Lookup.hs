module Language.Haskell.GHC.ExactPrint.Lookup
  (
    keywordToString
  , unicodeString
  ) where

import Language.Haskell.GHC.ExactPrint.Types
import qualified GHC (AnnKeywordId(..))
import Data.Maybe

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
      AnnSpanEntry      -> mkErr kw
      AnnComment _      -> mkErr kw
      AnnList _ _       -> mkErr kw
      AnnString _       -> mkErr kw
      AnnUnicode kw'    -> keywordToString (G kw')
      AnnSemiSep        -> ";"
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
      (G GHC.AnnCloseC  ) -> "}"
      (G GHC.AnnCloseP  ) -> ")"
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
      (G GHC.AnnMdo      ) -> "mdo"
      (G GHC.AnnMinus    ) -> "-"
      (G GHC.AnnModule   ) -> "module"
      (G GHC.AnnNewtype  ) -> "newtype"
      (G GHC.AnnOf       ) -> "of"
      (G GHC.AnnOpenC    ) -> "{"
      (G GHC.AnnOpenP    ) -> "("
      (G GHC.AnnOpenS    ) -> "["
      (G GHC.AnnPattern  ) -> "pattern"
      (G GHC.AnnProc     ) -> "proc"
      (G GHC.AnnQualified ) -> "qualified"
      (G GHC.AnnRarrow   ) -> "->"
      (G GHC.AnnRec      ) -> "rec"
      (G GHC.AnnRole     ) -> "role"
      (G GHC.AnnSafe     ) -> "safe"
      (G GHC.AnnSemi     ) -> ";"
      (G GHC.AnnStatic   ) -> "static"
      (G GHC.AnnThen     ) -> "then"
      (G GHC.AnnTilde    ) -> "~"
      (G GHC.AnnTildehsh ) -> "~#"
      (G GHC.AnnType     ) -> "type"
      (G GHC.AnnUnit     ) -> "()"
      (G GHC.AnnUsing    ) -> "using"
      (G GHC.AnnVbar     ) -> "|"
      (G GHC.AnnWhere    ) -> "where"
      (G GHC.Annlarrowtail ) -> "-<"
      (G GHC.Annrarrowtail ) -> "->"
      (G GHC.AnnLarrowtail ) -> "-<<"
      (G GHC.AnnRarrowtail ) -> ">>-"
      (G GHC.AnnSimpleQuote ) -> "'"
      (G GHC.AnnThIdSplice  ) -> "''"
      (G GHC.AnnThIdTySplice ) -> "$$"
      (G GHC.AnnEofPos       ) -> ""

unicodeString :: KeywordId -> String
unicodeString kw =
  fromMaybe (keywordToString kw) (lookup kw unicodeChars)

unicodeChars :: [(KeywordId, String)]
unicodeChars =
      [ (G GHC.AnnDcolon, "∷")
      , (G GHC.AnnDarrow, "⇒")
      , (G GHC.AnnForall, "∀")
      , (G GHC.AnnRarrow, "→")
      , (G GHC.AnnLarrow, "←")
      , (G GHC.Annlarrowtail, "⤙")
      , (G GHC.Annrarrowtail, "⤚")
      , (G GHC.AnnLarrowtail, "⤛")
      , (G GHC.AnnRarrowtail, "⤜")]
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
