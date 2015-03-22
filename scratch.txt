Annotation values from the Delta phase.

edp : entry delta.
  This is the offset from the previous output item and the current item.
  Initially just the straight (deltaLine,deltaCol) value
  Then corrected via adjustDeltaForOffsetM
      currentColOffset
  So if it is on the same line it is unchanged, else
  it is calculated relative to the existing colOffset

Stored as offset in the withSrcSpanDelta calculation
and then retrieved to store in Annotation as `ann_entry_delta`

nl : Did the original span start on a new line wrt to the prior one?
     prior meaning higher up the stack

     This encodes Line same/changed, as well as Layout on/off

getCurrentDP
  ss = span we are annotating
  ps = previous (higher up stack) span

  colOffset is the start col of ss if ss and ps have different start lines,
       else the difference in start columns between them.

  The LineSame/LineChanged part is set according to the colOffset logic
  The Layout On/Off is set according to the layout flag passed in
      The flag is set iff layout needs to be captured for an item.

original_col is set to the original column of the SrcSpan being annotated.

The dp is set to colOffset from getCurrentDP


Annotation usage in the Print Phase
-----------------------------------
  (in withOffset)

epStack has a (colOffset,colIndent) for each SrcSpan.

The current output position is stored in the Monad, retrieved as
(_l, currentColumn)

A new indentation value is calculated only iff
  1) Layout is flagged to be captured
  2) LineSame is set.


The process is to calculate a newStartColumn, based on the original edp value
  If the original edp was on the same line, this simply
      the current output position + the edp col.
  Otherwise it is the current colOffset, as the indentation cannot change going on to a new line.

The new colOffset value is calculated depending on the LineChanged value.
  If the line has changed, the line is started at
     the existing colIndent value + the dp from Ann
     The new colOffset is calculated as the old colOffset + the dp from Ann
     if layout is being captured
        the new colOffset is adjusted by the difference between the new and old indent values. 


