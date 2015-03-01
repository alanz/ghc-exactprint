
Principles
----------

## Robustness

There are two ways of approaching the annotations. The one, as used
in HSE, is to define the positions so that the original source can be
reconstructed.

The more useful/difficult option is to defined the annotations so that
they can be used to reconstruct a properly formatted source file even
when some major manipulations have been done to the AST. The means
everything has to be relative, similar to the pretty-print library.


## Mechanics

Each AST element/annotation must be self-contained, in that it can be
rendered correctly relative to a (positive) starting (row,col)
position at the top left, regardless of what these are.

Positions are all stored as instances of DeltaPos, which gives the row
and column offset for the next element relative to the *current
position*.

The *current position* is stored in the rendering state, and may be
updated by either explicitly moving forward, or by printing at an
*absolute position*.

The existing HSE ExactPrint rendering engine is used, which makes use
of absolute positions.

Utility functions exist to print at a DeltaPos, which simply combine
the *current position* with the delta to provide an *absolute
position*.

This combining takes place in the exactP/exactPC instances, and it is
up to the annotation generator and using instance to define the
reference point.

Two options exist

a) Relative to the SrcSpan defining the Located AST element.
b) Relative to the last output position

Option (a) makes sense for specific layout items that need vertical
alignment, while option (b) makes sense for spacing withing say an
expression where the name of one of the variables may eventually be
changed.

Rule of thumb: use option (b) unless option (a) is required, it should
result in a more resilient annotation when tools need to change
things.

## Comments

Comments should be pushed as low as possible down the AST.

Each annotation maintains a list of DComments, which are comments
defined as being relative to the start of the containing AST element
SrcSpan.

The rendering engine has an ordered list of comments which it inserts
into the output stream as it advances.

Every time a new AST element/annotation is entered, the current
position is taken as the start of the SrcSpan, and so the absolute
positions of the comment can be determined. These are then merged into
the list of comments for rendering.

Question: what happens if a sub-element changes size from the
original? Later comments will then be in a different position.

Perhaps provide a list of lists, relative to the sub-items.


### Mechanics of comments

Option 1.

When generating the annotation, pass the remaining list of comments to
the sub elements. Each sub-element (recursively) grabs the comments it
takes responsibility for, and passes the balance back. Any comments
fitting into the span but not allocated to a sub-element become
allocated at this level, interspersed between the sub elements.

Option 2.

Do a hard separation of comments according to SrcSpan. So the parent
element takes all comments in the parent SrcSpan that do not fit into
any of the sub-element SrcSpans.

This has the disadvantage that a syntax element can become
disconnected from its comments, e.g. a pre or post comment for a
declaration.


Note: Because the position always moves forward when emitting source,
and syntax element start points are used by exactPC to advance, there
is a problem with preceding comments.

## Delta Annotations and modifications

Assume the following (LayoutLet2.hs from the tests)


```
         1         2
123456789012345678901234567890
foo xxx = let a = 1
              b = 2 in xxx + a + b
```

We end up with the following annotations for the chain from `foo` to
`let`, ignoring the ValD/FunBind part, as it is not actually
processed, the Match is all we need.

```
 ((tests/examples/LayoutLet2.hs:(7,1)-(8,34), CN "Match"),
  (Ann {ann_entry_delta = DP (6,0), ann_delta = 0},
   [(G AnnEqual, DP (0,1))])),
```


The ann_entry_delta says this span starts 6 lines and 0 columns offset
from the prior output (getPriorSrcSpanAP), which puts us from location
(1,1) at (7,1).

It starts against the left margin, so ann_delta is 0.

```
((tests/examples/LayoutLet2.hs:7:1-3, CN "Unqual"),
  (Ann {ann_entry_delta = DP (6,0), ann_delta = 0},
    (G AnnVal, DP (6,0))])),
```

The "foo" appears next, there was no output from the start of the
Match, so the ann_entry_delta and ann_delta are unchanged.

Once the name "foo" is output, the prior output position is at (7,4),
one character past the last output, as per the GHC SrcSpan convention.

Next is the "xxx", a VarPat

```
 ((tests/examples/LayoutLet2.hs:7:5-7, CN "VarPat"),
  (Ann {ann_entry_delta = DP (0,1), ann_delta = 4},
   [(G AnnVal, DP (0,1))])),
```

The ann_entry_delta DP is (0,1), because the "xxx" starts on the same
line and one space over from "foo". So we can get the start of the
SrcSpan as

    prior output + ann_entry_delta
    (7,4)        + (0,1)           = (7,5)

At this point the column offet is 4, (1 + 4 == 5).

Once "xxx" is printed, the last output pos is (7,8)

The next part of the Match is the GRHSs, the SrcSpan is popped and a
new one entered

```
 ((tests/examples/LayoutLet2.hs:7:9, CN "GRHS"),
  (Ann {ann_entry_delta = DP (0,-1), ann_delta = 8}, [])),
```

AZ:Not sure why we have a -1 here.

The HsLet runs from (7,11) to (8,35) (GHC reports one less for the end)

```
 ((tests/examples/LayoutLet2.hs:(7,11)-(8,34), CN "HsLet"),
  (Ann {ann_entry_delta = DP (0,1), ann_delta = 2},
   [(G AnnLet, DP (0,1)), (G AnnIn, DP (0,1))])),
```

The delta from previous output (the "=" on col 8, endinf on 9) is 1,
for a total indent of 10. We already have an indent of 8 from the
 (CN "GRHS") entry higher in the stack, so this ann_delta is 2.

The "let" occurs one space over from prior output. It is the first
output item for this span so it has the same DeltaPos as
ann_entry_delta.






More to come ...


## Column offsets / indentation

Things we know

1. The SrcSpan captures the original scope of the item being annotated.

2. But, there may be commas or semis after the item being annotated.

3. When converting to a delta, the two fundamental variants are
   spacing in the same line, which is straightforward, and managing
   the indent when moving to a new line. These are independent.

4. The trailing commas, semis, or comments may spill on to the next
   line, as leaders to the next SrcSpan.

   e.g

     ls = [ 1
          , 2
          ]

   In this case the SrcSpan for "2" will be preceded by a comma
   captured in the SrcSpan for "1".

   So, we need to capture the starting offset for a given line, but
   cannot necessarily rely on the SrcSpan that originally starts the
   line.

5. When moving into a SrcSpan, on the same line, we have the concept
   of a current indentation offset. This is the additional offset
   applied to indentation for this particular AST item.

   e.g.

     f x = let a = 3
               b = 4
           in x + a + b

   The let statement as a whole has an indentation offset, based on
   the SrcSpan of the GRHS containing it. The offset for "b=4" depends
   on the position of "a=3" in the HsLocalBinds, and has an additional
   offset of 4.

   So the offset for a new line is the sum of the offsets in force on
   the previous line (or rather the ones captured in the SrcSpan
   stack)

   SO

   Moving to a new line is indicated by a span entry offset with
   lineOffset > 0. We use it to trigger the new line only, and do not
   process it further.

   In addition, every time we hit a new line we need to start a new
   sub-stack of nested offsets, based on the ruling offset of the
   prior sub-stack.
