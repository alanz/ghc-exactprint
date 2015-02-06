
To do items, in no particular order
-----------------------------------

## Sort out the test for "ParensAroundContext.hs"

The extra set of parens is lost, mainly through mangling in GHC
adjustments for the wild card value.

## Process CPP files.

Bring getRichTokenStreamWA over from HaRe (if feasible), and include
the CPP / preprocessed comments in the global comment stream in the
GHC.ApiAnns, so they are interleaved.

## Process LHS files

Should be possible to do something similar to CPP above

## Consider splitting both ExactPrint.hs and Utils.hs in two

Break them into an engine part, and a GHC AST specific part. This
opens the way to reuse in HSE or similar.

## Use a table of strings for the fixed annotations

At the moment, ExactPrint.hs is littered with statements of the form

    printStringAtMaybeAnn (G GHC.AnnComma) ","

There should be no reason to supply the string.

## Create DSL to harmonise delta capture and delta output

At the moment, the delta creation happens in the AP monad, and output
is generated via the EP monad.

However, the instances for AnnotateP and ExactP are structurally
almost identical. There must be some way to harmonise this, so that a
single instance of something can be written, and used in both.

Perhaps a free monad.

# More mundane things

There are currently warnings for missing pattern matches in both Utils
and ExactPrint. Add the missing cases.

