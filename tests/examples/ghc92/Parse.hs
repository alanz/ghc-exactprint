
module Language.Python.Internal.Parse where

compoundStatement pIndent indentBefore =
      do; a <- doAsync; fundef
