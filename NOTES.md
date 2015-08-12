
Principles
----------

## Goals

1. The SrcSpan's can be used during the annotation phase for
   positional information, in the output phase they are purely indices
   into the annotation store.

2. It must be possible to edit the annotated AST and still get
   "correct" output. For example, if an element is indented, all the
   children should be too, if a definition in a let statement is
   removed, the remaining ones should fill in the gap.

3. Only the minimal additional indentation should be done, which is
   driven by the layout rules in haskell. Code should only change its
   column if it has to.


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




Worked example
--------------

Working through the `LayoutLet2.hs` file in `tests/examples`

```haskell
module LayoutLet2 where

-- Simple let expression, rename xxx to something longer or shorter
-- and the let/in layout should adjust accordingly
-- In this case the tokens for xxx + a + b should also shift out

foo xxx = let a = 1
              b = 2 in xxx + a + b

```

The AST for this is as follows

```
(L {tests/examples/LayoutLet2.hs:1:1} 
 (HsModule 
  (Just 
   (L {tests/examples/LayoutLet2.hs:1:8-17} {ModuleName: LayoutLet2})) 
  (Nothing) 
  [] 
  [
   (L {tests/examples/LayoutLet2.hs:(7,1)-(8,34)} 
    (ValD 
     (FunBind 
      (L {tests/examples/LayoutLet2.hs:7:1-3} 
       (Unqual {OccName: foo})) 
      (False) 
      (MG 
       [
        (L {tests/examples/LayoutLet2.hs:(7,1)-(8,34)} 
         (Match 
          (Just 
           ((,) 
            (L {tests/examples/LayoutLet2.hs:7:1-3} 
             (Unqual {OccName: foo})) 
            (False))) 
          [
           (L {tests/examples/LayoutLet2.hs:7:5-7} 
            (VarPat 
             (Unqual {OccName: xxx})))] 
          (Nothing) 
          (GRHSs 
           [
            (L {tests/examples/LayoutLet2.hs:(7,9)-(8,34)} 
             (GRHS 
              [] 
              (L {tests/examples/LayoutLet2.hs:(7,11)-(8,34)} 
               (HsLet 
                (HsValBinds 
                 (ValBindsIn {Bag(Located (HsBind RdrName)): 
                  [
                   (L {tests/examples/LayoutLet2.hs:7:15-19} 
                    (FunBind 
                     (L {tests/examples/LayoutLet2.hs:7:15} 
                      (Unqual {OccName: a})) 
                     (False) 
                     (MG 
                      [
                       (L {tests/examples/LayoutLet2.hs:7:15-19} 
                        (Match 
                         (Just 
                          ((,) 
                           (L {tests/examples/LayoutLet2.hs:7:15} 
                            (Unqual {OccName: a})) 
                           (False))) 
                         [] 
                         (Nothing) 
                         (GRHSs 
                          [
                           (L {tests/examples/LayoutLet2.hs:7:17-19} 
                            (GRHS 
                             [] 
                             (L {tests/examples/LayoutLet2.hs:7:19} 
                              (HsOverLit {HsOverLit:1}))))] 
                          (EmptyLocalBinds))))] 
                      [] 
                      (PlaceHolder) 
                      (FromSource)) 
                     (WpHole) 
                     (PlaceHolder) 
                     [])),
                   (L {tests/examples/LayoutLet2.hs:8:15-19} 
                    (FunBind 
                     (L {tests/examples/LayoutLet2.hs:8:15} 
                      (Unqual {OccName: b})) 
                     (False) 
                     (MG 
                      [
                       (L {tests/examples/LayoutLet2.hs:8:15-19} 
                        (Match 
                         (Just 
                          ((,) 
                           (L {tests/examples/LayoutLet2.hs:8:15} 
                            (Unqual {OccName: b})) 
                           (False))) 
                         [] 
                         (Nothing) 
                         (GRHSs 
                          [
                           (L {tests/examples/LayoutLet2.hs:8:17-19} 
                            (GRHS 
                             [] 
                             (L {tests/examples/LayoutLet2.hs:8:19} 
                              (HsOverLit {HsOverLit:2}))))] 
                          (EmptyLocalBinds))))] 
                      [] 
                      (PlaceHolder) 
                      (FromSource)) 
                     (WpHole) 
                     (PlaceHolder) 
                     []))]} 
                  [])) 
                (L {tests/examples/LayoutLet2.hs:8:24-34} 
                 (OpApp 
                  (L {tests/examples/LayoutLet2.hs:8:24-30} 
                   (OpApp 
                    (L {tests/examples/LayoutLet2.hs:8:24-26} 
                     (HsVar 
                      (Unqual {OccName: xxx}))) 
                    (L {tests/examples/LayoutLet2.hs:8:28} 
                     (HsVar 
                      (Unqual {OccName: +}))) 
                    (PlaceHolder) 
                    (L {tests/examples/LayoutLet2.hs:8:30} 
                     (HsVar 
                      (Unqual {OccName: a}))))) 
                  (L {tests/examples/LayoutLet2.hs:8:32} 
                   (HsVar 
                    (Unqual {OccName: +}))) 
                  (PlaceHolder) 
                  (L {tests/examples/LayoutLet2.hs:8:34} 
                   (HsVar 
                    (Unqual {OccName: b})))))))))] 
           (EmptyLocalBinds))))] 
       [] 
       (PlaceHolder) 
       (FromSource)) 
      (WpHole) 
      (PlaceHolder) 
      [])))] 
  (Nothing) 
  (Nothing)))
```

When this is broken down into its properly nested structure, and duplicates removed, we have

````
   (L {LayoutLet2.hs:(7,1)-(8,34)} 
      (L {LayoutLet2.hs:7:1-3}                  foo
      (L {LayoutLet2.hs:7:5-7}                  |   xxx
      (L {LayoutLet2.hs:(7,9)-(8,34)}           |   |   =
         (L {LayoutLet2.hs:(7,11)-(8,34)}       |   |     let        in
 ===SYN=====(L {LayoutLet2.hs:(7,15)-(8,19)}    |   |     ^
               (L {LayoutLet2.hs:7:15-19}       |   |     |          |
                  (L {LayoutLet2.hs:7:15}       |   |     |   a      |
                  (L {LayoutLet2.hs:7:17-19}    |   |     |   ^ =    |
                     (L {LayoutLet2.hs:7:19}    |   |     |   |   1  |
                                              --------------------------------------------
               (L {LayoutLet2.hs:8:15-19}       |   |     |   |      |
                  (L {LayoutLet2.hs:8:15}       |   |     |   b      |
                  (L {LayoutLet2.hs:8:17-19}    |   |     |   | =    |
                     (L {LayoutLet2.hs:8:19}    |   |     |   |    2 |
            (L {LayoutLet2.hs:8:24-34}          |   |     |          |
              (L {LayoutLet2.hs:8:24-30}        |   |     |          |
                (L {LayoutLet2.hs:8:24-26}      |   |     |          |   xxx
                (L {LayoutLet2.hs:8:28}         |   |     |          |       +
                (L {LayoutLet2.hs:8:30}         |   |     |          |         a
              (L {LayoutLet2.hs:8:32}           |   |     |          |           +
              (L {LayoutLet2.hs:8:34}           |   |     |          |             b
````

The ^ at the top of a vertical line indicates points where layout is flagged in
the above span, and the horizontal line is where the source text moves on to the
next line.

Note the SYN span, this is synthesised during the delta phase to provide an
appropriate SrcSpan to capture the layout annotation

If we now replace `xxx` with `xxxlonger`, we expect the following output

````
   (L {LayoutLet2.hs:(7,1)-(8,34)} 
      (L {LayoutLet2.hs:7:1-3}                  foo
      (L {LayoutLet2.hs:7:5-7}                  |   xxxlonger
      (L {LayoutLet2.hs:(7,9)-(8,34)}           |   |  ...... =
         (L {LayoutLet2.hs:(7,11)-(8,34)}       |   |  ......   let        in
 ===SYN=====(L {LayoutLet2.hs:(7,15)-(8,19)}    |   |  ......   ^
               (L {LayoutLet2.hs:7:15-19}       |   |  ......   |          |
                  (L {LayoutLet2.hs:7:15}       |   |  ......   |   a      |
                  (L {LayoutLet2.hs:7:17-19}    |   |  ......   |   ^ =    |
                     (L {LayoutLet2.hs:7:19}    |   |  ......   |   |   1  |
                                                         --------------------------------------------
               (L {LayoutLet2.hs:8:15-19}       |   |  ......   |   |      |
                  (L {LayoutLet2.hs:8:15}       |   |  ......   |   b      |
                  (L {LayoutLet2.hs:8:17-19}    |   |  ......   |   | =    |
                     (L {LayoutLet2.hs:8:19}    |   |  ......   |   |    2 |
            (L {LayoutLet2.hs:8:24-34}          |   |  ......   |          |
              (L {LayoutLet2.hs:8:24-30}        |   |  ......   |          |
                (L {LayoutLet2.hs:8:24-26}      |   |  ......   |          |   xxxlonger
                (L {LayoutLet2.hs:8:28}         |   |  ......   |          |      ...... +
                (L {LayoutLet2.hs:8:30}         |   |  ......   |          |      ......   a
              (L {LayoutLet2.hs:8:32}           |   |  ......   |          |      ......     +
              (L {LayoutLet2.hs:8:34}           |   |  ......   |          |      ......       b
````

The changes within line 7 are all handled naturally by the DP values, since they
are on the same line so the spacing just flows.

The start of line 8 needs to be moved in by six spaces, and this is a block
indent, signified by `......`. If xxx was even longer, or shorter, this distance
would change.

No further indentation change is required for the layout starting at `a = 1`, as
the change in indentation can only be captured once per line, and there is no
additional change between the `let` and the `a`

The second instance of `xxx`, on line 8, also flows naturally because it is on
the same line.





Example 2
---------

```haskell
module LayoutIn3 where

--Layout rule applies after 'where','let','do' and 'of'

--In this Example: rename 'x' after 'let'  to 'anotherX'.

foo x = let x = 12 in (let y = 3
                           z = 2 in x * y * z * w) where   y = 2
                                                           --there is a comment.
                                                           w = x
                                                             where
                                                               x = let y = 5 in y + 3

```

This results in the following tree structure

```
(L {LayoutIn3.hs:(7,1)-(12,85)} 
   (L {LayoutIn3.hs:7:1-3}                              foo 
   (L {LayoutIn3.hs:7:5}                                |   x
   (L {LayoutIn3.hs:(7,7)-(12,85)}                      |     =
      (L {LayoutIn3.hs:(7,9)-(8,50)}                    |       let        in                              where 
=SYN=====(L {LayoutIn3.hs:7:13-18}                      |                                                |
      |  (L {LayoutIn3.hs:7:13-18}                      |                                                |
      |     (L {LayoutIn3.hs:7:13}                      |           x                                    |
      |     (L {LayoutIn3.hs:7:17-18}                   |           ^ = 12                               |
      |   (L {LayoutIn3.hs:(7,23)-(8,50)}               |           |         (                          )
      |      (L {LayoutIn3.hs:(7,24)-(8,49)}            |                      let       in              |
=SYN=========   (L {LayoutIn3.hs:(7,28)-(8,32)}         |                                                |
      |            (L {LayoutIn3.hs:7:28-32}            |                          y                     |
      |               (L {LayoutIn3.hs:7:28}            |                          ^ =                   |
      |               (L {LayoutIn3.hs:7:30-32}         |                          |   3                 |
      |               (L {LayoutIn3.hs:7:32}            |                          |                     |
      |--------------------------------------------------------------------------------            
      |            (L {LayoutIn3.hs:8:28-32}            |                          |                     |
      |               (L {LayoutIn3.hs:8:28}            |                          z                     |
      |               (L {LayoutIn3.hs:8:30-32}         |                          | =                   |
      |               (L {LayoutIn3.hs:8:32}            |                          |   2                 |
      |         (L {LayoutIn3.hs:8:37-49}               |                                                |
      |            (L {LayoutIn3.hs:8:37-45}            |                                                |
      |               (L {LayoutIn3.hs:8:37-41}         |                                                |
      |                  (L {LayoutIn3.hs:8:37}         |                                   x            |
      |                  (L {LayoutIn3.hs:8:39}         |                                     *          |
      |                  (L {LayoutIn3.hs:8:41}         |                                       y        |
      |               (L {LayoutIn3.hs:8:43}            |                                         *      |
      |               (L {LayoutIn3.hs:8:45}            |                                           z    |
      |            (L {LayoutIn3.hs:8:47}               |                                             *  |
      |            (L {LayoutIn3.hs:8:49}               |                                               w|
=SYN==(L {LayoutIn3.hs:(8,60)-(12,85)}=======           |
         (L {LayoutIn3.hs:8:60-64}                      |
         |  (L {LayoutIn3.hs:8:60}                      |                                                        y
         |  (L {LayoutIn3.hs:8:62-64}                   |                                                        ^ =
         |  (L {LayoutIn3.hs:8:64}                      |                                                        |   2
          --------------------------------------------------------------------------------------------
         (L {LayoutIn3.hs:(10,60)-(12,85)}              |                                                        |
         |  (L {LayoutIn3.hs:10:60}                     |                                                        w
         |  (L {LayoutIn3.hs:(10,62)-(12,85)}           |                                                          =
         |     (L {LayoutIn3.hs:10:64}                  |                                                            x
         --------------------------------------------------------------------------------------
         |     (L {LayoutIn3.hs:12:64-85}               |
         |        (L {LayoutIn3.hs:12:64-85}            |
         |           (L {LayoutIn3.hs:12:64}            |
         |           (L {LayoutIn3.hs:12:66-85}         |
         |              (L {LayoutIn3.hs:12:68-85}      |
         |              (L {LayoutIn3.hs:12:72-76}      |
         |                 (L {LayoutIn3.hs:12:72}      |
         |                 (L {LayoutIn3.hs:12:74-76}   |
         |                 (L {LayoutIn3.hs:12:76}      |
         |              (L {LayoutIn3.hs:12:81-85}      |
         |                 (L {LayoutIn3.hs:12:81}      |
         |                 (L {LayoutIn3.hs:12:83}      |
         |                 (L {LayoutIn3.hs:12:85}      |
```

## Principles

The following principles should apply to the annotated AST, to simplify Transform operations.

### Locality

