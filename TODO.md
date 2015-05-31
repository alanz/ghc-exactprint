
To do items, in no particular order
-----------------------------------

## Process CPP files.

Bring getRichTokenStreamWA over from HaRe (if feasible), and include
the CPP / preprocessed comments in the global comment stream in the
GHC.ApiAnns, so they are interleaved.

## Process LHS files

Should be possible to do something similar to CPP above

## Consider splitting both ExactPrint.hs and Utils.hs in two

Break them into an engine part, and a GHC AST specific part. This
opens the way to reuse in HSE or similar.

## Capture sort order in an independent way

At the moment lists of decls etc are exlicitly sorted according to SrcSpan.

The goal is that a SrcSpan is simply an index from the AST to the annotations,
so the delta phase needs to capture this sort order in a way that can be
manipulated in transform and used in print.

## More mundane things

There are currently warnings for missing pattern matches in both Utils
and ExactPrint. Add the missing cases.

## Future : 7.10.3 and beyond

https://phabricator.haskell.org/D907 enables the following tests to pass

    - Deprecation.hs
    - MultiLineWarningPragma.hs
    - UnicodeRules.hs

Changes are required in the lexer to allow the following to pass

    - UnicodeSyntax.hs
      we have no way of detecting a unicode *

