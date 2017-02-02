


````
<alanz> I can see us needing a "default indent step" parameter in HaRe for e.g. ifToCase to say how far to indent the case alternatives
<mpickering> I see that kind of thing being resolved in an exactprint module
<mpickering> a user can supply a "formatting config" and a AST fragment and then we fill in the annotations based on that
<alanz> that sounds good.
<alanz> And at some future date we can put some heuristics in to extract it from the existing code. See what the majority of indents are, or something
````

## For GHC 7.12

Make sure that all names are located, so e.g. HsTyVar will take a Located name


Make BooleanFormula properly Located, or provide original source representation

Show instance for BooleanFormula

Sort out GADT parsing, as first class process.

## For roundtrip

Explicitly filter out files with

  - hashbang at the top
  - #LINE pragma
  - CPP and trailing backslash
  - any #define
  - any #include. But, use filename in SrcSpan?

GHC: Capture ^L (formfeed) as a specific kind of comment?
