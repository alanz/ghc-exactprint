-- From ./hackage-roundtrip-work/asil-1.2/dist/build/CFG.hs
-- UUAGC 0.9.36 (src/CFG.ag)
module CFG where

{-# LINE 6 "src/CFG.ag" #-}

import Data.Word
import ByteCode
{-# LINE 11 "dist/src/sdist.27680/asil-1.2/dist/build/CFG.hs" #-}
-- CFG ---------------------------------------------------------
data CFG  = CFG_CFG !(Int) !(Segments )
-- CFGs --------------------------------------------------------
type CFGs  = [CFG ]
-- Code --------------------------------------------------------
data Code  = Code_Jump !(Int) !(JumpCond )
           | Code_Label !(Int)
           | Code_Nop
           | Code_Switch !(Int) !(([Int]))
-- Codes -------------------------------------------------------
type Codes  = [Code ]
-- JumpCond ----------------------------------------------------
data JumpCond  = JumpCond_IfEq
               | JumpCond_IfFalse
               | JumpCond_IfGe
               | JumpCond_IfGt
               | JumpCond_IfLe
               | JumpCond_IfLt
               | JumpCond_IfNGe
               | JumpCond_IfNGt
               | JumpCond_IfNLe
               | JumpCond_IfNLt
               | JumpCond_IfNe
               | JumpCond_IfStrictEq
               | JumpCond_IfStrictNe
               | JumpCond_IfTrue
               | JumpCond_Jump
               | JumpCond_None
-- Node --------------------------------------------------------
data Node  = Node_Opaque !(Instruction)
           | Node_Pseudo !(Int) !(Pseudo ) !(Codes )
-- Nodes -------------------------------------------------------
type Nodes  = [Node ]
-- Program -----------------------------------------------------
data Program  = Program_Program !(CFGs )
-- Programs ----------------------------------------------------
type Programs  = [Program ]
-- Pseudo ------------------------------------------------------
data Pseudo  = Pseudo_BeginCall
             | Pseudo_BeginCoerce
             | Pseudo_DoneCall
             | Pseudo_DoneCoerce
             | Pseudo_EnterBlock
             | Pseudo_EnterFun
             | Pseudo_FailBlock
             | Pseudo_FailFun
             | Pseudo_FailedCall
             | Pseudo_FailedCoerce
             | Pseudo_LeaveBlock
             | Pseudo_LeaveFun
             | Pseudo_Nop
-- Segment -----------------------------------------------------
data Segment  = Segment_Segment !(Int) !(Nodes ) !(Code )
-- Segments ----------------------------------------------------
type Segments  = [Segment ]
