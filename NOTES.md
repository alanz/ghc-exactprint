
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

Assume the following


```
         1         2
123456789012345678901
foo xxx = let
            a = 3
          in aaa + xx
```

Currently, when moving from the "let" to "a = 3" we have an offset
specified as DP (1,2), ie one line down

Output stage in exactPC

printStringAtMaybeAnn gets the offset and calls
printStringAtLsDelta which does

  cl = the DP to use
  p <- getPos -- to get the current output (r,c)
  colOffset < getOffset -- gets fst $ head dps, the second AP var.

Then
  undelta p cl colOffset

undelta :: Pos -> DeltaPos -> Int -> Pos
undelta (l,c) (DP (dl,dc)) co = (fl,fc)
  where
    fl = l + dl
    fc = if dl == 0 then c  + dc
                    else co + dc

So undelta treats co as the baseline offset when moving to a new line.

co is set on entry to a new srcspan as follows

  The SrcSpan has (Ann lcs ec nd dp)
     where lcs = commsnts
           ec  = original column offset at the point the dp was captured.
           nd  = nested delta position, no longer used.
           dp = the delta pos associated with the span.

This gets fed into pushOffset dp ec

this does
    (co,_) = ghead "pushOffset" dps -- second param of EP
    co' = dc + co
 and then sets dps to
   (co',dc):dps

Currently the second value in the tuple is not used. We will use it to
store the current difference that is embodied in the co. Initially
this is zero, as there have been no changes in the offset. If the
current col and the ec value differ, this diff is calculated and
compared with the stored diff. If different, the difference is added
to the co, and the stored diff updated. This was an offset will only
be applied once.

Problem is, how do we unwind? i.e., regardless of where the prior line
ended, a new fn needs to be at col 1. Can't use prior pos for that.

As we pop up the stack of annotations, we need to undo the deltas. Or
perhaps only compare deltas at equivalent positions before. It is
captured using a stack, based on pos on entry, do the same on use. TODO.
