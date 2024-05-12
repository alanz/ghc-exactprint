{-# LANGUAGE ExistentialQuantification #-}
module Synthesizer.MIDI.CausalIO.Process (
   Events,

   slice,
   controllerLinear,
   controllerExponential,
   pitchBend,
   channelPressure,
   bendWheelPressure,
   constant,

   Instrument,
   Bank,
   GateChunk,
   noteEvents,
   embedPrograms,
   applyInstrument,
   applyModulatedInstrument,
   flattenControlSchedule,
   applyModulation,
   arrangeStorable,
   sequenceCore,
   sequenceModulated,
   sequenceModulatedMultiProgram,
   sequenceModulatedMultiProgramVelocityPitch,
   sequenceStorable,

   -- auxiliary function
   initWith,
   mapMaybe,
   ) where

import qualified Synthesizer.CausalIO.Gate as Gate
import qualified Synthesizer.CausalIO.Process as PIO

import qualified Synthesizer.MIDI.Value.BendModulation as BM
import qualified Synthesizer.MIDI.Value.BendWheelPressure as BWP
import qualified Synthesizer.MIDI.Value as MV

import qualified Synthesizer.MIDI.EventList as MIDIEv
import Synthesizer.MIDI.EventList (StrictTime, )

import qualified Synthesizer.PiecewiseConstant.Signal as PC
import qualified Synthesizer.Storable.Cut as CutSt
import qualified Synthesizer.Generic.Cut as CutG
import qualified Synthesizer.Zip as Zip

import qualified Sound.MIDI.Message.Class.Check as Check
import qualified Sound.MIDI.Message.Channel.Voice as VoiceMsg

import Control.DeepSeq (rnf, )

import qualified Data.EventList.Relative.TimeBody  as EventList
import qualified Data.EventList.Relative.BodyTime  as EventListBT
import qualified Data.EventList.Relative.TimeTime  as EventListTT
import qualified Data.EventList.Relative.TimeMixed as EventListTM
import qualified Data.EventList.Relative.MixedTime as EventListMT
import qualified Data.EventList.Absolute.TimeBody  as AbsEventList

import qualified Numeric.NonNegative.Wrapper as NonNegW
import qualified Numeric.NonNegative.Class as NonNeg

import qualified Algebra.Transcendental as Trans
import qualified Algebra.RealRing       as RealRing
import qualified Algebra.Field          as Field
import qualified Algebra.Additive       as Additive
import qualified Algebra.ToInteger      as ToInteger

import qualified Data.StorableVector as SV
import qualified Data.StorableVector.ST.Strict as SVST
import Foreign.Storable (Storable, )

import qualified Control.Monad.Trans.Writer as MW
import qualified Control.Monad.Trans.State as MS
import qualified Control.Monad.Trans.Class as MT
import Control.Monad.IO.Class (liftIO, )

import qualified Data.Traversable as Trav
import Data.Traversable (Traversable, )
import Data.Foldable (traverse_, )

import Control.Arrow (Arrow, arr, (^<<), (<<^), )
import Control.Category ((.), )

import qualified Data.Map as Map

import qualified Data.List.HT as ListHT
import qualified Data.Maybe as Maybe
import Data.Monoid (Monoid, mempty, mappend, )
import Data.Maybe (maybeToList, )
import Data.Tuple.HT (mapFst, mapPair, )

import NumericPrelude.Numeric
import NumericPrelude.Base hiding ((.), sequence, )
import Prelude ()


type Events event = EventListTT.T StrictTime [event]


initWith ::
   (y -> c) ->
   c ->
   PIO.T
      (Events y)
      (EventListBT.T PC.ShortStrictTime c)
initWith f initial =
   PIO.traverse initial $
      \evs0 -> do
         y0 <- MS.get
         fmap (PC.subdivideLongStrict . EventListMT.consBody y0) $
            Trav.traverse (\ys -> traverse_ (MS.put . f) ys >> MS.get) evs0

slice ::
   (Check.C event) =>
   (event -> Maybe Int) ->
   (Int -> y) -> y ->
   PIO.T
      (Events event)
      (EventListBT.T PC.ShortStrictTime y)
slice select f initial =
   initWith f initial . mapMaybe select



mapMaybe ::
   (Arrow arrow, Functor f) =>
   (a -> Maybe b) ->
   arrow (f [a]) (f [b])
mapMaybe f =
   arr $ fmap $ Maybe.mapMaybe f

catMaybes ::
   (Arrow arrow, Functor f) =>
   arrow (f [Maybe a]) (f [a])
catMaybes =
   arr $ fmap Maybe.catMaybes

traverse ::
   (Traversable f) =>
   s -> (a -> MS.State s b) ->
   PIO.T (f [a]) (f [b])
traverse initial f =
   PIO.traverse initial (Trav.traverse (Trav.traverse f))


controllerLinear ::
   (Check.C event, Field.C y) =>
   MIDIEv.Channel ->
   MIDIEv.Controller ->
   (y,y) -> y ->
   PIO.T
      (Events event)
      (EventListBT.T PC.ShortStrictTime y)
controllerLinear chan ctrl bnd initial =
   slice (Check.controller chan ctrl)
      (MV.controllerLinear bnd) initial

controllerExponential ::
   (Check.C event, Trans.C y) =>
   MIDIEv.Channel ->
   MIDIEv.Controller ->
   (y,y) -> y ->
   PIO.T
      (Events event)
      (EventListBT.T PC.ShortStrictTime y)
controllerExponential chan ctrl bnd initial =
   slice (Check.controller chan ctrl)
      (MV.controllerExponential bnd) initial

pitchBend ::
   (Check.C event, Trans.C y) =>
   MIDIEv.Channel ->
   y -> y ->
   PIO.T
      (Events event)
      (EventListBT.T PC.ShortStrictTime y)
pitchBend chan range center =
   slice (Check.pitchBend chan)
      (MV.pitchBend range center) center

channelPressure ::
   (Check.C event, Trans.C y) =>
   MIDIEv.Channel ->
   y -> y ->
   PIO.T
      (Events event)
      (EventListBT.T PC.ShortStrictTime y)
channelPressure chan maxVal initial =
   slice (Check.channelPressure chan)
      (MV.controllerLinear (zero,maxVal)) initial

bendWheelPressure ::
   (Check.C event, RealRing.C y, Trans.C y) =>
   MIDIEv.Channel ->
   Int -> y -> y ->
   PIO.T
      (Events event)
      (EventListBT.T PC.ShortStrictTime (BM.T y))
bendWheelPressure chan
      pitchRange wheelDepth pressDepth =
   let toBM = BM.fromBendWheelPressure pitchRange wheelDepth pressDepth
   in  initWith toBM (toBM BWP.deflt)
       .
       catMaybes
       .
       traverse BWP.deflt (BWP.check chan)


-- might be moved to synthesizer-core
constant ::
   (Arrow arrow) =>
   y -> arrow (Events event) (EventListBT.T PC.ShortStrictTime y)
constant y = arr $
   EventListBT.singleton y .
   NonNegW.fromNumberMsg "MIDI.CausalIO.constant" .
   fromIntegral .
   EventListTT.duration

_constant ::
   (Arrow arrow, CutG.Read input) =>
   y -> arrow input (EventListBT.T PC.ShortStrictTime y)
_constant y = arr $
   EventListBT.singleton y .
   NonNegW.fromNumberMsg "MIDI.CausalIO.constant" .
   CutG.length



noteEvents ::
   (Check.C event, Arrow arrow) =>
   MIDIEv.Channel ->
   arrow
      (Events event)
      (Events (Either MIDIEv.Program (MIDIEv.NoteBoundary Bool)))
noteEvents chan =
   mapMaybe $ MIDIEv.checkNoteEvent chan


embedPrograms ::
   MIDIEv.Program ->
   PIO.T
      (Events (Either MIDIEv.Program (MIDIEv.NoteBoundary Bool)))
      (Events (MIDIEv.NoteBoundary (Maybe MIDIEv.Program)))
embedPrograms initPgm =
   catMaybes .
   traverse initPgm MIDIEv.embedProgramState


type GateChunk = Gate.Chunk MIDIEv.Velocity
type Instrument y chunk = y -> y -> PIO.T GateChunk chunk
type Bank y chunk = MIDIEv.Program -> Instrument y chunk



{-
for distinction of notes with the same pitch

We must use Integer instead of Int, in order to avoid an overflow
that would invalidate the check for unmatched NoteOffs
that is based on comparison of the NoteIds.
We cannot re-use NoteIds easily,
since the events at one time point are handled out of order.
-}
newtype NoteId = NoteId Integer
   deriving (Show, Eq, Ord)

succNoteId :: NoteId -> NoteId
succNoteId (NoteId n) = NoteId (n+1)

flattenNoteIdRange :: (NoteId,NoteId) -> [NoteId]
flattenNoteIdRange (start,afterEnd) =
   takeWhile (<afterEnd) $ iterate succNoteId start


newtype NoteOffList =
   NoteOffList {
      unwrapNoteOffList :: Events (NoteBoundary NoteId)
   }


instance CutG.Read NoteOffList where
   null (NoteOffList evs) =
      EventListTT.isPause evs && EventListTT.duration evs == 0
   length = fromIntegral . EventListTT.duration . unwrapNoteOffList

instance CutG.NormalForm NoteOffList where
   evaluateHead =
      EventListMT.switchTimeL (\t _ -> rnf (NonNegW.toNumber t)) .
      unwrapNoteOffList

instance Monoid NoteOffList where
   mempty = NoteOffList (EventListTT.pause mempty)
   mappend (NoteOffList xs) (NoteOffList ys) =
      NoteOffList (mappend xs ys)

{- |
The function defined here are based on the interpretation
of event lists as piecewise constant signals.
They do not fit to the interpretation of atomic events.
Because e.g. it makes no sense to split an atomic event into two instances by splitAt,
and it is also not clear, whether dropping the first chunk
shall leave a chunk of length zero
or remove that chunk completely.
-}
instance CutG.Transform NoteOffList where
   take n (NoteOffList xs) =
      NoteOffList $
      EventListTT.takeTime
         (NonNegW.fromNumberMsg "NoteOffList.take" $ fromIntegral n) xs
   drop n (NoteOffList xs) =
      NoteOffList $
      EventListTT.dropTime
         (NonNegW.fromNumberMsg "NoteOffList.drop" $ fromIntegral n) xs
   splitAt n (NoteOffList xs) =
      mapPair (NoteOffList, NoteOffList) $
      EventListTT.splitAtTime
         (NonNegW.fromNumberMsg "NoteOffList.splitAtTime" $ fromIntegral n) xs

   -- cf. ChunkySize.dropMarginRem
   dropMarginRem =
      CutG.dropMarginRemChunky
         (fmap fromIntegral . EventListTT.getTimes . unwrapNoteOffList)

   reverse (NoteOffList xs) =
      NoteOffList . EventListTT.reverse $ xs


findEvent ::
   (a -> Bool) ->
   Events a -> (Events a, Maybe a)
findEvent p =
   EventListTT.foldr
      (\t -> mapFst (EventListMT.consTime t))
      (\evs rest ->
         case ListHT.break p evs of
            (prefix, suffix) ->
               mapFst (EventListMT.consBody prefix) $
               case suffix of
                  [] -> rest
                  ev:_ -> (EventListTT.pause mempty, Just ev))
      (EventListBT.empty, Nothing)

gateFromNoteOffs ::
   (MIDIEv.Pitch, NoteId) ->
   NoteOffList ->
   GateChunk
gateFromNoteOffs pitchNoteId (NoteOffList noteOffs) =
   let dur = EventListTT.duration noteOffs
       (sustain, mEnd) =
          findEvent
             (\bnd ->
                case bnd of
                   -- AllNotesOff -> True
                   NoteBoundary endPitch _ noteId ->
                      pitchNoteId == (endPitch, noteId))
          noteOffs
   in  Gate.chunk dur $
       flip fmap mEnd $ \end ->
       (EventListTT.duration sustain,
        case end of
           NoteBoundary _ endVel _ -> endVel
           {-
           AllNotesOff -> VoiceMsg.normalVelocity -} )


data NoteBoundary a =
     NoteBoundary VoiceMsg.Pitch VoiceMsg.Velocity a
--   | AllSoundOff
   deriving (Eq, Show)

{- |
We count NoteIds per pitch,
such that the pair (pitch,noteId) identifies a note.
We treat nested notes in a first-in-first-out order (FIFO).
E.g.

> On, On, On, Off, Off, Off

is interpreted as

> On 0, On 1, On 2, Off 0, Off 1, Off 2

NoteOffs without previous NoteOns are thrown away.
-}
assignNoteIds ::
   (Traversable f) =>
   PIO.T
      (f [MIDIEv.NoteBoundary (Maybe MIDIEv.Program)])
      (f [NoteBoundary (NoteId, Maybe MIDIEv.Program)])
assignNoteIds =
   fmap concat
   ^<<
   traverse Map.empty (\bnd ->
      case bnd of
         MIDIEv.AllNotesOff -> do
            notes <- MS.get
            MS.put Map.empty
            return $
               concatMap (\(pitch, range) ->
                  map
                     (\noteId ->
                        NoteBoundary pitch VoiceMsg.normalVelocity
                           (noteId, Nothing))
                     (flattenNoteIdRange range)) $
               Map.toList notes
         MIDIEv.NoteBoundary pitch vel mpgm ->
            fmap (fmap (\noteId ->
               NoteBoundary pitch vel (noteId,mpgm))) $
            case mpgm of
               Nothing -> do
                  mNoteId <- MS.gets (Map.lookup pitch)
                  case mNoteId of
                     Nothing -> return []
                     Just (nextNoteOffId, nextNoteOnId) ->
                        if nextNoteOffId >= nextNoteOnId
                          then return []
                          else do
                             MS.modify (Map.insert pitch (succNoteId nextNoteOffId, nextNoteOnId))
                             return [nextNoteOffId]
               Just _ -> do
                  mNoteId <- MS.gets (Map.lookup pitch)
                  let (nextNoteOffId, nextNoteOnId) =
                         case mNoteId of
                            Nothing -> (NoteId 0, NoteId 0)
                            Just ids -> ids

                  MS.modify (Map.insert pitch (nextNoteOffId, succNoteId nextNoteOnId))
                  return [nextNoteOnId])

{-# INLINE velFreqBank #-}
velFreqBank ::
   (Trans.C y) =>
   (MIDIEv.Program -> y -> y -> process) ->
   (MIDIEv.Program -> MIDIEv.Velocity -> MIDIEv.Pitch -> process)
velFreqBank bank pgm vel pitch =
   bank pgm (MV.velocity vel) (MV.frequencyFromPitch pitch)

applyInstrumentCore ::
   (Arrow arrow) =>
   ((MIDIEv.Pitch, NoteId) -> noteOffListCtrl -> gateCtrl) ->
   (MIDIEv.Program -> MIDIEv.Velocity -> MIDIEv.Pitch ->
    PIO.T gateCtrl chunk) ->
   arrow
      (Events (NoteBoundary (NoteId, Maybe MIDIEv.Program)))
      (Zip.T
         NoteOffList
         (Events (PIO.T noteOffListCtrl chunk)))
applyInstrumentCore makeGate bank = arr $
   uncurry Zip.Cons .
   mapFst NoteOffList .
   EventListTT.unzip .
   fmap (ListHT.unzipEithers . fmap (\ev ->
      case ev of
--         MIDIEv.AllNotesOff -> Left MIDIEv.AllNotesOff
         NoteBoundary pitch vel (noteId, mpgm) ->
            case mpgm of
               Nothing -> Left $ NoteBoundary pitch vel noteId
               Just pgm ->
                  Right $
                     bank pgm vel pitch
                     <<^
                     makeGate (pitch, noteId)))

applyInstrument ::
   (Arrow arrow) =>
   (MIDIEv.Program -> MIDIEv.Velocity -> MIDIEv.Pitch ->
    PIO.T GateChunk chunk) ->
   arrow
      (Events (NoteBoundary (NoteId, Maybe MIDIEv.Program)))
      (Zip.T
         NoteOffList
         (Events (PIO.T NoteOffList chunk)))
applyInstrument bank =
   applyInstrumentCore gateFromNoteOffs bank


type ModulatedBank y ctrl chunk =
        MIDIEv.Program -> y -> y ->
        PIO.T (Zip.T GateChunk ctrl) chunk

applyModulatedInstrument ::
   (Arrow arrow, CutG.Read ctrl) =>
   (MIDIEv.Program -> MIDIEv.Velocity -> MIDIEv.Pitch ->
    PIO.T (Zip.T GateChunk ctrl) chunk) ->
   arrow
      (Zip.T
         (Events (NoteBoundary (NoteId, Maybe MIDIEv.Program)))
         ctrl)
      (Zip.T
         (Zip.T NoteOffList ctrl)
         (Events (PIO.T (Zip.T NoteOffList ctrl) chunk)))
applyModulatedInstrument bank =
   (\(Zip.Cons (Zip.Cons noteOffs events) ctrl) ->
      Zip.Cons (Zip.Cons noteOffs ctrl) events)
   ^<<
   Zip.arrowFirst
      (applyInstrumentCore
         (Zip.arrowFirst . gateFromNoteOffs) bank)


{- |
Turn an event list with bundles of elements
into an event list with single events.
ToDo: Move to event-list package?
-}
flatten ::
   (NonNeg.C time) =>
   a ->
   EventListTT.T time [a] ->
   EventListTT.T time a
flatten empty =
   EventListTT.foldr
      EventListMT.consTime
      (\bt xs ->
         uncurry EventListMT.consBody $
         case bt of
            [] -> (empty, xs)
            b:bs -> (b, foldr (\c rest -> EventListTT.cons NonNeg.zero c rest) xs bs))
      EventListBT.empty


flattenControlSchedule ::
   (Monoid chunk, Arrow arrow) =>
   arrow
      (Zip.T ctrl
         (EventListTT.T StrictTime [PIO.T ctrl chunk]))
      (Zip.T ctrl
         (EventListTT.T StrictTime (PIO.T ctrl chunk)))
flattenControlSchedule = arr $
   \(Zip.Cons ctrl evs) ->
      -- Zip.consChecked "flattenControlSchedule" ctrl $
      Zip.Cons ctrl $
      flatten (arr (const mempty)) evs



data CausalState a b =
   forall state.
   CausalState
      (a -> state -> IO (b, state))
      (state -> IO ())
      state

_applyChunkSimple :: CausalState a b -> a -> IO (b, CausalState a b)
_applyChunkSimple (CausalState next delete state0) input = do
   (output, state1) <- next input state0
   return (output, CausalState next delete state1)

applyChunk ::
   (CutG.Read a, CutG.Read b) =>
   CausalState a b -> a -> IO (b, Maybe (CausalState a b))
applyChunk (CausalState next delete state0) input = do
   (output, state1) <- next input state0
   cs <-
      if CutG.length output < CutG.length input
        then do
           delete state1
           return Nothing
        else return $ Just $ CausalState next delete state1
   return (output, cs)

-- could be moved to synthesizer-core
applyModulation ::
   (CutG.Transform ctrl, CutG.NormalForm ctrl,
    CutG.Read chunk,
    Monoid time, ToInteger.C time) =>
   PIO.T
      (Zip.T ctrl (EventListTT.T time (PIO.T ctrl chunk)))
      (EventListTT.T time chunk)
applyModulation = PIO.Cons
   (\(Zip.Cons ctrl evs) acc0 -> do
      acc1 <- mapM (flip applyChunk ctrl) acc0
      let (accChunks, acc2) = unzip acc1

      (newChunks, newAcc) <-
         MW.runWriterT $
         flip MS.evalStateT ctrl $
         EventListTT.mapM
            (\time -> do
               ctrl_ <- MS.gets (CutG.drop (fromIntegral time))
               MS.put ctrl_
               return (case CutG.evaluateHead ctrl_ of () -> time))
            (\(PIO.Cons next create delete) -> do
               state0 <- liftIO create
               (chunk, state1) <-
                  liftIO . applyChunk (CausalState next delete state0)
                  =<< MS.get
               MT.lift $ MW.tell $ maybeToList state1
               return chunk)
            evs

      return
         (EventListTM.prependBodyEnd
             (EventList.fromPairList $
              map ((,) mempty) accChunks)
             newChunks,
          Maybe.catMaybes acc2 ++ newAcc))

   (return [])
   (mapM_ (\(CausalState _ close state) -> close state))

-- move synthesizer-core:CausalIO
arrangeStorable ::
   (Arrow arrow, Storable a, Additive.C a) =>
   arrow
      (EventListTT.T StrictTime (SV.Vector a))
      (SV.Vector a)
arrangeStorable =
   arr $ \evs ->
   SVST.runSTVector (do
      v <- SVST.new (fromIntegral $ EventListTT.duration evs) zero
      mapM_ (uncurry $ CutSt.addChunkToBuffer v) $
         AbsEventList.toPairList $
         AbsEventList.mapTime fromIntegral $
         EventList.toAbsoluteEventList 0 $
         EventListTM.switchTimeR const evs
      return v)



sequenceCore ::
   (Check.C event, Monoid chunk, CutG.Read chunk, Trans.C y) =>
   MIDIEv.Channel ->
   Bank y chunk ->
   PIO.T (Events event) (EventListTT.T StrictTime chunk)
sequenceCore channel bank =
   applyModulation
   .
   flattenControlSchedule
   .
   applyInstrument (velFreqBank bank)
   .
   assignNoteIds
   .
   embedPrograms (VoiceMsg.toProgram 0)
   .
   noteEvents channel


sequenceModulated ::
   (Check.C event, Monoid chunk, CutG.Read chunk,
    CutG.Transform ctrl, CutG.NormalForm ctrl, Trans.C y) =>
   MIDIEv.Channel ->
   ModulatedBank y ctrl chunk ->
   PIO.T (Zip.T (Events event) ctrl) (EventListTT.T StrictTime chunk)
sequenceModulated channel bank =
   applyModulation
   .
   flattenControlSchedule
   .
   applyModulatedInstrument (velFreqBank bank)
   .
   Zip.arrowFirst
      (assignNoteIds
       .
       embedPrograms (VoiceMsg.toProgram 0)
       .
       noteEvents channel)


sequenceModulatedMultiProgram ::
   (Check.C event, Monoid chunk, CutG.Read chunk,
    CutG.Transform ctrl, CutG.NormalForm ctrl, Trans.C y) =>
   MIDIEv.Channel ->
   MIDIEv.Program ->
   ModulatedBank y ctrl chunk ->
   PIO.T (Zip.T (Events event) ctrl) (EventListTT.T StrictTime chunk)
sequenceModulatedMultiProgram channel initPgm bank =
   applyModulation
   .
   flattenControlSchedule
   .
   applyModulatedInstrument (velFreqBank bank)
   .
   Zip.arrowFirst
      (assignNoteIds
       .
       embedPrograms initPgm
       .
       noteEvents channel)


-- | may replace the other functions
sequenceModulatedMultiProgramVelocityPitch ::
   (Check.C event, Monoid chunk, CutG.Read chunk,
    CutG.Transform ctrl, CutG.NormalForm ctrl) =>
   MIDIEv.Channel ->
   MIDIEv.Program ->
   (MIDIEv.Program -> MIDIEv.Velocity -> MIDIEv.Pitch ->
    PIO.T (Zip.T GateChunk ctrl) chunk) ->
   PIO.T (Zip.T (Events event) ctrl) (EventListTT.T StrictTime chunk)
sequenceModulatedMultiProgramVelocityPitch channel initPgm bank =
   applyModulation
   .
   flattenControlSchedule
   .
   applyModulatedInstrument bank
   .
   Zip.arrowFirst
      (assignNoteIds
       .
       embedPrograms initPgm
       .
       noteEvents channel)


sequenceStorable ::
   (Check.C event, Storable a, Additive.C a, Trans.C y) =>
   MIDIEv.Channel ->
   Bank y (SV.Vector a) ->
   PIO.T (Events event) (SV.Vector a)
sequenceStorable channel bank =
   arrangeStorable
   .
   sequenceCore channel bank
