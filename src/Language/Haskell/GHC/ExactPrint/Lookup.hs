module Language.Haskell.GHC.ExactPrint.Lookup (keywordToString, unicodeString) where

import GHC (AnnKeywordId(..))
import Data.Maybe

-- | Maps `AnnKeywordId` to the corresponding String representation.
-- There is no specific mapping for the following constructors.
-- `AnnOpen`, `AnnClose`, `AnnVal`, `AnnPackageName`, `AnnHeader`, `AnnFunId`,
-- `AnnInfix`
keywordToString :: AnnKeywordId -> String
keywordToString kw =
  let mkErr x = error $ "keywordToString: missing case for:" ++ show x
  in
  case kw of
      -- Specifically handle all cases so that there are pattern match
      -- warnings if new constructors are added.
      AnnOpen  -> mkErr kw
      AnnClose -> mkErr kw
      AnnVal   -> mkErr kw
      AnnPackageName -> mkErr kw
      AnnHeader -> mkErr kw
      AnnFunId -> mkErr kw
      AnnInfix  -> mkErr kw
      AnnValStr -> mkErr kw
      AnnAs    -> "as"
      AnnAt   -> "@"
      AnnBang  -> "!"
      AnnBackquote   -> "`"
      AnnBy   -> "by"
      AnnCase  -> "case"
      AnnClass    -> "class"
      AnnCloseC  -> "}"
      AnnCloseP  -> ")"
      AnnCloseS  -> "]"
      AnnColon    -> ":"
      AnnComma   -> ","
      AnnCommaTuple  -> ","
      AnnDarrow  -> "=>"
      AnnData   -> "data"
      AnnDcolon  -> "::"
      AnnDefault    -> "default"
      AnnDeriving   -> "deriving"
      AnnDo   -> "do"
      AnnDot   -> "."
      AnnDotdot  -> ".."
      AnnElse   -> "else"
      AnnEqual    -> "="
      AnnExport   -> "export"
      AnnFamily   -> "family"
      AnnForall   -> "forall"
      AnnForeign    -> "foreign"
      AnnGroup    -> "group"
      AnnHiding   -> "hiding"
      AnnIf   -> "if"
      AnnImport   -> "import"
      AnnIn   -> "in"
      AnnInstance   -> "instance"
      AnnLam    -> "\\"
      AnnLarrow  -> "<-"
      AnnLet    -> "let"
      AnnMdo    -> "mdo"
      AnnMinus   -> "-"
      AnnModule   -> "module"
      AnnNewtype  -> "newtype"
      AnnOf   -> "of"
      AnnOpenC   -> "{"
      AnnOpenP   -> "("
      AnnOpenS   -> "["
      AnnPattern -> "pattern"
      AnnProc    -> "proc"
      AnnQualified -> "qualified"
      AnnRarrow -> "->"
      AnnRec    -> "rec"
      AnnRole   -> "role"
      AnnSafe   -> "safe"
      AnnSemi   -> ";"
      AnnStatic -> "static"
      AnnThen   -> "then"
      AnnTilde  -> "~"
      AnnTildehsh -> "~#"
      AnnType  -> "type"
      AnnUnit  -> "()"
      AnnUsing -> "using"
      AnnVbar  -> "|"
      AnnWhere -> "where"
      Annlarrowtail  -> "-<"
      Annrarrowtail  -> "->"
      AnnLarrowtail  -> "-<<"
      AnnRarrowtail  -> ">>-"
      AnnSimpleQuote -> "'"
      AnnThIdSplice  -> "''"
      AnnThIdTySplice -> "$$"
      AnnEofPos -> ""

unicodeString :: AnnKeywordId -> String
unicodeString kw =
  fromMaybe (keywordToString kw) (lookup kw unicodeChars)

unicodeChars :: [(AnnKeywordId, String)]
unicodeChars =
      [(GHC.AnnDcolon, "∷")
      , (GHC.AnnDarrow, "⇒")
      , (GHC.AnnForall, "∀")
      , (GHC.AnnRarrow, "→")
      , (GHC.AnnLarrow, "←")
      , (GHC.Annlarrowtail, "↢")
      , (GHC.Annrarrowtail, "↣")
      , (GHC.AnnLarrowtail, "⤛")
      , (GHC.AnnRarrowtail, "⤜")]

