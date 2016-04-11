module Synthesizer.MIDI.CausalIO.Process1 where

gateFromNoteOffs=
   let dur = 1
   in (d, 3
          {-
            AllNotesOff -> VoiceMsg.normalVelocity -} )
