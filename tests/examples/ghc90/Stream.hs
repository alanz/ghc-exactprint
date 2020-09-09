{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module     : Simulation.Aivika.Trans.Stream
-- Copyright  : Copyright (c) 2009-2017, David Sorokin <david.sorokin@gmail.com>
-- License    : BSD3
-- Maintainer : David Sorokin <david.sorokin@gmail.com>
-- Stability  : experimental
-- Tested with: GHC 8.0.1
--
-- The infinite stream of data in time.
--
module Simulation.Aivika.Trans.Stream
       (-- * Stream Type
        Stream(..),
        -- * Merging and Splitting Stream
        emptyStream,
        mergeStreams,
        mergeQueuedStreams,
        mergePriorityStreams,
        concatStreams,
        concatQueuedStreams,
        concatPriorityStreams,
        splitStream,
        splitStreamQueueing,
        splitStreamPrioritising,
        splitStreamFiltering,
        splitStreamFilteringQueueing,
        -- * Specifying Identifier
        streamUsingId,
        -- * Prefetching and Delaying Stream
        prefetchStream,
        delayStream,
        -- * Stream Arriving
        arrivalStream,
        -- * Memoizing, Zipping and Uzipping Stream
        memoStream,
        zipStreamSeq,
        zipStreamParallel,
        zip3StreamSeq,
        zip3StreamParallel,
        unzipStream,
        streamSeq,
        streamParallel,
        -- * Consuming and Sinking Stream
        consumeStream,
        sinkStream,
        -- * Useful Combinators
        repeatProcess,
        mapStream,
        mapStreamM,
        accumStream,
        apStream,
        apStreamM,
        filterStream,
        filterStreamM,
        takeStream,
        takeStreamWhile,
        takeStreamWhileM,
        dropStream,
        dropStreamWhile,
        dropStreamWhileM,
        singletonStream,
        joinStream,
        -- * Failover
        failoverStream,
        -- * Integrating with Signals
        signalStream,
        streamSignal,
        queuedSignalStream,
        -- * Utilities
        leftStream,
        rightStream,
        replaceLeftStream,
        replaceRightStream,
        partitionEitherStream,
        -- * Assemblying Streams
        cloneStream,
        firstArrivalStream,
        lastArrivalStream,
        assembleAccumStream,
        -- * Debugging
        traceStream) where

import Data.Maybe
import Data.Monoid hiding ((<>))
import Data.Semigroup (Semigroup(..))
import Data.List.NonEmpty (NonEmpty((:|)))

import Control.Applicative
import Control.Monad
import Control.Monad.Trans

import Simulation.Aivika.Trans.Ref.Base
import Simulation.Aivika.Trans.DES
import Simulation.Aivika.Trans.Parameter
import Simulation.Aivika.Trans.Simulation
import Simulation.Aivika.Trans.Dynamics
import Simulation.Aivika.Trans.Event
import Simulation.Aivika.Trans.Composite
import Simulation.Aivika.Trans.Cont
import Simulation.Aivika.Trans.Process
import Simulation.Aivika.Trans.Signal
import Simulation.Aivika.Trans.Resource.Base
import Simulation.Aivika.Trans.QueueStrategy
import qualified Simulation.Aivika.Trans.Queue.Infinite.Base as IQ
import Simulation.Aivika.Arrival (Arrival(..))

-- | Represents an infinite stream of data in time,
-- some kind of never-ending cons cell.
newtype Stream m a = Cons { runStream :: Process m (a, Stream m a)
                            -- ^ Run the stream.
                          }

instance MonadDES m => Functor (Stream m) where

  {-# INLINE fmap #-}
  fmap = mapStream

instance MonadDES m => Applicative (Stream m) where

  {-# INLINE pure #-}
  pure a = let y = Cons (return (a, y)) in y

  {-# INLINE (<*>) #-}
  (<*>) = apStream

instance MonadDES m => Alternative (Stream m) where

  {-# INLINE empty #-}
  empty = emptyStream

  {-# INLINE (<|>) #-}
  (<|>) = mergeStreams

instance MonadDES m => Semigroup (Stream m a) where

  {-# INLINE (<>) #-}
  (<>) = mergeStreams

  {-# INLINE sconcat #-}
  sconcat (h :| t) = concatStreams (h : t)

instance MonadDES m => Monoid (Stream m a) where

  {-# INLINE mempty #-}
  mempty  = emptyStream

  {-# INLINE mappend #-}
  mappend = (<>)

  {-# INLINE mconcat #-}
  mconcat = concatStreams

-- | Create a stream that will use the specified process identifier.
-- It can be useful to refer to the underlying 'Process' computation which
-- can be passivated, interrupted, canceled and so on. See also the
-- 'processUsingId' function for more details.
streamUsingId :: MonadDES m => ProcessId m -> Stream m a -> Stream m a
{-# INLINABLE streamUsingId #-}
streamUsingId pid (Cons s) =
  Cons $ processUsingId pid s

-- | Memoize the stream so that it would always return the same data
-- within the simulation run.
memoStream :: MonadDES m => Stream m a -> Simulation m (Stream m a)
{-# INLINABLE memoStream #-}
memoStream (Cons s) =
  do p <- memoProcess $
          do ~(x, xs) <- s
             xs' <- liftSimulation $ memoStream xs
             return (x, xs')
     return (Cons p)

-- | Zip two streams trying to get data sequentially.
zipStreamSeq :: MonadDES m => Stream m a -> Stream m b -> Stream m (a, b)
{-# INLINABLE zipStreamSeq #-}
zipStreamSeq (Cons sa) (Cons sb) = Cons y where
  y = do ~(x, xs) <- sa
         ~(y, ys) <- sb
         return ((x, y), zipStreamSeq xs ys)

-- | Zip two streams trying to get data as soon as possible,
-- launching the sub-processes in parallel.
zipStreamParallel :: MonadDES m => Stream m a -> Stream m b -> Stream m (a, b)
{-# INLINABLE zipStreamParallel #-}
zipStreamParallel (Cons sa) (Cons sb) = Cons y where
  y = do ~((x, xs), (y, ys)) <- zipProcessParallel sa sb
         return ((x, y), zipStreamParallel xs ys)

-- | Zip three streams trying to get data sequentially.
zip3StreamSeq :: MonadDES m => Stream m a -> Stream m b -> Stream m c -> Stream m (a, b, c)
{-# INLINABLE zip3StreamSeq #-}
zip3StreamSeq (Cons sa) (Cons sb) (Cons sc) = Cons y where
  y = do ~(x, xs) <- sa
         ~(y, ys) <- sb
         ~(z, zs) <- sc
         return ((x, y, z), zip3StreamSeq xs ys zs)

-- | Zip three streams trying to get data as soon as possible,
-- launching the sub-processes in parallel.
zip3StreamParallel :: MonadDES m => Stream m a -> Stream m b -> Stream m c -> Stream m (a, b, c)
{-# INLINABLE zip3StreamParallel #-}
zip3StreamParallel (Cons sa) (Cons sb) (Cons sc) = Cons y where
  y = do ~((x, xs), (y, ys), (z, zs)) <- zip3ProcessParallel sa sb sc
         return ((x, y, z), zip3StreamParallel xs ys zs)

-- | Unzip the stream.
unzipStream :: MonadDES m => Stream m (a, b) -> Simulation m (Stream m a, Stream m b)
{-# INLINABLE unzipStream #-}
unzipStream s =
  do s' <- memoStream s
     let sa = mapStream fst s'
         sb = mapStream snd s'
     return (sa, sb)

-- | To form each new portion of data for the output stream,
-- read data sequentially from the input streams.
--
-- This is a generalization of 'zipStreamSeq'.
streamSeq :: MonadDES m => [Stream m a] -> Stream m [a]
{-# INLINABLE streamSeq #-}
streamSeq xs = Cons y where
  y = do ps <- forM xs runStream
         return (map fst ps, streamSeq $ map snd ps)

-- | To form each new portion of data for the output stream,
-- read data from the input streams in parallel.
--
-- This is a generalization of 'zipStreamParallel'.
streamParallel :: MonadDES m => [Stream m a] -> Stream m [a]
{-# INLINABLE streamParallel #-}
streamParallel xs = Cons y where
  y = do ps <- processParallel $ map runStream xs
         return (map fst ps, streamParallel $ map snd ps)

-- | Return a stream of values generated by the specified process.
repeatProcess :: MonadDES m => Process m a -> Stream m a
{-# INLINABLE repeatProcess #-}
repeatProcess p = Cons y where
  y = do a <- p
         return (a, repeatProcess p)

-- | Map the stream according the specified function.
mapStream :: MonadDES m => (a -> b) -> Stream m a -> Stream m b
{-# INLINABLE mapStream #-}
mapStream f (Cons s) = Cons y where
  y = do (a, xs) <- s
         return (f a, mapStream f xs)

-- | Compose the stream.
mapStreamM :: MonadDES m => (a -> Process m b) -> Stream m a -> Stream m b
{-# INLINABLE mapStreamM #-}
mapStreamM f (Cons s) = Cons y where
  y = do (a, xs) <- s
         b <- f a
         return (b, mapStreamM f xs)

-- | Accumulator that outputs a value determined by the supplied function.
accumStream :: MonadDES m => (acc -> a -> Process m (acc, b)) -> acc -> Stream m a -> Stream m b
{-# INLINABLE accumStream #-}
accumStream f acc xs = Cons $ loop xs acc where
  loop (Cons s) acc =
    do (a, xs) <- s
       (acc', b) <- f acc a
       return (b, Cons $ loop xs acc')

-- | Sequential application.
apStream :: MonadDES m => Stream m (a -> b) -> Stream m a -> Stream m b
{-# INLINABLE apStream #-}
apStream (Cons sf) (Cons sa) = Cons y where
  y = do (f, sf') <- sf
         (a, sa') <- sa
         return (f a, apStream sf' sa')

-- | Sequential application.
apStreamM :: MonadDES m => Stream m (a -> Process m b) -> Stream m a -> Stream m b
{-# INLINABLE apStreamM #-}
apStreamM (Cons sf) (Cons sa) = Cons y where
  y = do (f, sf') <- sf
         (a, sa') <- sa
         x <- f a
         return (x, apStreamM sf' sa')

-- | Filter only those data values that satisfy to the specified predicate.
filterStream :: MonadDES m => (a -> Bool) -> Stream m a -> Stream m a
{-# INLINABLE filterStream #-}
filterStream p (Cons s) = Cons y where
  y = do (a, xs) <- s
         if p a
           then return (a, filterStream p xs)
           else let Cons z = filterStream p xs in z

-- | Filter only those data values that satisfy to the specified predicate.
filterStreamM :: MonadDES m => (a -> Process m Bool) -> Stream m a -> Stream m a
{-# INLINABLE filterStreamM #-}
filterStreamM p (Cons s) = Cons y where
  y = do (a, xs) <- s
         b <- p a
         if b
           then return (a, filterStreamM p xs)
           else let Cons z = filterStreamM p xs in z

-- | The stream of 'Left' values.
leftStream :: MonadDES m => Stream m (Either a b) -> Stream m a
{-# INLINABLE leftStream #-}
leftStream (Cons s) = Cons y where
  y = do (a, xs) <- s
         case a of
           Left a  -> return (a, leftStream xs)
           Right _ -> let Cons z = leftStream xs in z

-- | The stream of 'Right' values.
rightStream :: MonadDES m => Stream m (Either a b) -> Stream m b
{-# INLINABLE rightStream #-}
rightStream (Cons s) = Cons y where
  y = do (a, xs) <- s
         case a of
           Left _  -> let Cons z = rightStream xs in z
           Right a -> return (a, rightStream xs)

-- | Replace the 'Left' values.
replaceLeftStream :: MonadDES m => Stream m (Either a b) -> Stream m c -> Stream m (Either c b)
{-# INLINABLE replaceLeftStream #-}
replaceLeftStream (Cons sab) (ys0 @ ~(Cons sc)) = Cons z where
  z = do (a, xs) <- sab
         case a of
           Left _ ->
             do (b, ys) <- sc
                return (Left b, replaceLeftStream xs ys)
           Right a ->
             return (Right a, replaceLeftStream xs ys0)

-- | Replace the 'Right' values.
replaceRightStream :: MonadDES m => Stream m (Either a b) -> Stream m c -> Stream m (Either a c)
{-# INLINABLE replaceRightStream #-}
replaceRightStream (Cons sab) (ys0 @ ~(Cons sc)) = Cons z where
  z = do (a, xs) <- sab
         case a of
           Right _ ->
             do (b, ys) <- sc
                return (Right b, replaceRightStream xs ys)
           Left a ->
             return (Left a, replaceRightStream xs ys0)

-- | Partition the stream of 'Either' values into two streams.
partitionEitherStream :: MonadDES m => Stream m (Either a b) -> Simulation m (Stream m a, Stream m b)
{-# INLINABLE partitionEitherStream #-}
partitionEitherStream s =
  do s' <- memoStream s
     return (leftStream s', rightStream s')

-- | Split the input stream into the specified number of output streams
-- after applying the 'FCFS' strategy for enqueuing the output requests.
splitStream :: MonadDES m => Int -> Stream m a -> Simulation m [Stream m a]
{-# INLINABLE splitStream #-}
splitStream = splitStreamQueueing FCFS

-- | Split the input stream into the specified number of output streams.
--
-- If you don't know what the strategy to apply, then you probably
-- need the 'FCFS' strategy, or function 'splitStream' that
-- does namely this.
splitStreamQueueing :: (MonadDES m, EnqueueStrategy m s)
                       => s
                       -- ^ the strategy applied for enqueuing the output requests
                       -> Int
                       -- ^ the number of output streams
                       -> Stream m a
                       -- ^ the input stream
                       -> Simulation m [Stream m a]
                       -- ^ the splitted output streams
{-# INLINABLE splitStreamQueueing #-}
splitStreamQueueing s n x =
  do ref <- newRef x
     res <- newResource s 1
     let reader =
           usingResource res $
           do p <- liftEvent $ readRef ref
              (a, xs) <- runStream p
              liftEvent $ writeRef ref xs
              return a
     return $ map (\i -> repeatProcess reader) [1..n]

-- | Split the input stream into a list of output streams
-- using the specified priorities.
splitStreamPrioritising :: (MonadDES m, PriorityQueueStrategy m s p)
                           => s
                           -- ^ the strategy applied for enqueuing the output requests
                           -> [Stream m p]
                           -- ^ the streams of priorities
                           -> Stream m a
                           -- ^ the input stream
                           -> Simulation m [Stream m a]
                           -- ^ the splitted output streams
{-# INLINABLE splitStreamPrioritising #-}
splitStreamPrioritising s ps x =
  do ref <- newRef x
     res <- newResource s 1
     let stream (Cons p) = Cons z where
           z = do (p', ps) <- p
                  a <- usingResourceWithPriority res p' $
                       do p <- liftEvent $ readRef ref
                          (a, xs) <- runStream p
                          liftEvent $ writeRef ref xs
                          return a
                  return (a, stream ps)
     return $ map stream ps

-- | Split the input stream into the specified number of output streams
-- after filtering and applying the 'FCFS' strategy for enqueuing the output requests.
splitStreamFiltering :: MonadDES m => [a -> Event m Bool] -> Stream m a -> Simulation m [Stream m a]
{-# INLINABLE splitStreamFiltering #-}
splitStreamFiltering = splitStreamFilteringQueueing FCFS

-- | Split the input stream into the specified number of output streams after filtering.
--
-- If you don't know what the strategy to apply, then you probably
-- need the 'FCFS' strategy, or function 'splitStreamFiltering' that
-- does namely this.
splitStreamFilteringQueueing :: (MonadDES m, EnqueueStrategy m s)
                                => s
                                -- ^ the strategy applied for enqueuing the output requests
                                -> [a -> Event m Bool]
                                -- ^ the filters for output streams
                                -> Stream m a
                                -- ^ the input stream
                                -> Simulation m [Stream m a]
                                -- ^ the splitted output streams
{-# INLINABLE splitStreamFilteringQueueing #-}
splitStreamFilteringQueueing s preds x =
  do ref <- liftSimulation $ newRef x
     res <- newResource s 1
     let reader pred =
           do a <-
                usingResource res $
                do p <- liftEvent $ readRef ref
                   (a, xs) <- runStream p
                   liftEvent $
                     do f <- pred a
                        if f
                          then do writeRef ref xs
                                  return $ Just a
                          else do writeRef ref $ Cons (return (a, xs))
                                  return Nothing
              case a of
                Just a  -> return a
                Nothing -> reader pred
     return $ map (repeatProcess . reader) preds

-- | Concatenate the input streams applying the 'FCFS' strategy and
-- producing one output stream.
concatStreams :: MonadDES m => [Stream m a] -> Stream m a
{-# INLINABLE concatStreams #-}
concatStreams = concatQueuedStreams FCFS

-- | Concatenate the input streams producing one output stream.
--
-- If you don't know what the strategy to apply, then you probably
-- need the 'FCFS' strategy, or function 'concatStreams' that
-- does namely this.
concatQueuedStreams :: (MonadDES m, EnqueueStrategy m s)
                       => s
                       -- ^ the strategy applied for enqueuing the input data
                       -> [Stream m a]
                       -- ^ the input stream
                       -> Stream m a
                       -- ^ the combined output stream
{-# INLINABLE concatQueuedStreams #-}
concatQueuedStreams s streams = Cons z where
  z = do reading <- liftSimulation $ newResourceWithMaxCount FCFS 0 (Just 1)
         writing <- liftSimulation $ newResourceWithMaxCount s 1 (Just 1)
         conting <- liftSimulation $ newResourceWithMaxCount FCFS 0 (Just 1)
         ref <- liftSimulation $ newRef Nothing
         let writer p =
               do (a, xs) <- runStream p
                  requestResource writing
                  liftEvent $ writeRef ref (Just a)
                  releaseResource reading
                  requestResource conting
                  writer xs
             reader =
               do requestResource reading
                  Just a <- liftEvent $ readRef ref
                  liftEvent $ writeRef ref Nothing
                  releaseResource writing
                  return a
         forM_ streams $ spawnProcess . writer
         a <- reader
         let xs = repeatProcess (releaseResource conting >> reader)
         return (a, xs)

-- | Concatenate the input priority streams producing one output stream.
concatPriorityStreams :: (MonadDES m, PriorityQueueStrategy m s p)
                         => s
                         -- ^ the strategy applied for enqueuing the input data
                         -> [Stream m (p, a)]
                         -- ^ the input stream
                         -> Stream m a
                         -- ^ the combined output stream
{-# INLINABLE concatPriorityStreams #-}
concatPriorityStreams s streams = Cons z where
  z = do reading <- liftSimulation $ newResourceWithMaxCount FCFS 0 (Just 1)
         writing <- liftSimulation $ newResourceWithMaxCount s 1 (Just 1)
         conting <- liftSimulation $ newResourceWithMaxCount FCFS 0 (Just 1)
         ref <- liftSimulation $ newRef Nothing
         let writer p =
               do ((priority, a), xs) <- runStream p
                  requestResourceWithPriority writing priority
                  liftEvent $ writeRef ref (Just a)
                  releaseResource reading
                  requestResource conting
                  writer xs
             reader =
               do requestResource reading
                  Just a <- liftEvent $ readRef ref
                  liftEvent $ writeRef ref Nothing
                  releaseResource writing
                  return a
         forM_ streams $ spawnProcess . writer
         a <- reader
         let xs = repeatProcess (releaseResource conting >> reader)
         return (a, xs)

-- | Merge two streams applying the 'FCFS' strategy for enqueuing the input data.
mergeStreams :: MonadDES m => Stream m a -> Stream m a -> Stream m a
{-# INLINABLE mergeStreams #-}
mergeStreams = mergeQueuedStreams FCFS

-- | Merge two streams.
--
-- If you don't know what the strategy to apply, then you probably
-- need the 'FCFS' strategy, or function 'mergeStreams' that
-- does namely this.
mergeQueuedStreams :: (MonadDES m, EnqueueStrategy m s)
                      => s
                      -- ^ the strategy applied for enqueuing the input data
                      -> Stream m a
                      -- ^ the fist input stream
                      -> Stream m a
                      -- ^ the second input stream
                      -> Stream m a
                      -- ^ the output combined stream
{-# INLINABLE mergeQueuedStreams #-}
mergeQueuedStreams s x y = concatQueuedStreams s [x, y]

-- | Merge two priority streams.
mergePriorityStreams :: (MonadDES m, PriorityQueueStrategy m s p)
                        => s
                        -- ^ the strategy applied for enqueuing the input data
                        -> Stream m (p, a)
                        -- ^ the fist input stream
                        -> Stream m (p, a)
                        -- ^ the second input stream
                        -> Stream m a
                        -- ^ the output combined stream
{-# INLINABLE mergePriorityStreams #-}
mergePriorityStreams s x y = concatPriorityStreams s [x, y]

-- | An empty stream that never returns data.
emptyStream :: MonadDES m => Stream m a
{-# INLINABLE emptyStream #-}
emptyStream = Cons neverProcess

-- | Consume the stream. It returns a process that infinitely reads data
-- from the stream and then redirects them to the provided function.
-- It is useful for modeling the process of enqueueing data in the queue
-- from the input stream.
consumeStream :: MonadDES m => (a -> Process m ()) -> Stream m a -> Process m ()
{-# INLINABLE consumeStream #-}
consumeStream f (Cons s) =
  do (a, xs) <- s
     f a
     consumeStream f xs

-- | Sink the stream. It returns a process that infinitely reads data
-- from the stream. The resulting computation can be a moving force
-- to simulate the whole system of the interconnected streams and
-- processors.
sinkStream :: MonadDES m => Stream m a -> Process m ()
{-# INLINABLE sinkStream #-}
sinkStream (Cons s) =
  do (a, xs) <- s
     sinkStream xs

-- | Prefetch the input stream requesting for one more data item in advance
-- while the last received item is not yet fully processed in the chain of
-- streams, usually by the processors.
--
-- You can think of this as the prefetched stream could place its latest
-- data item in some temporary space for later use, which is very useful
-- for modeling a sequence of separate and independent work places.
prefetchStream :: MonadDES m => Stream m a -> Stream m a
{-# INLINABLE prefetchStream #-}
prefetchStream s = Cons z where
  z = do reading <- liftSimulation $ newResourceWithMaxCount FCFS 0 (Just 1)
         writing <- liftSimulation $ newResourceWithMaxCount FCFS 1 (Just 1)
         ref <- liftSimulation $ newRef Nothing
         let writer p =
               do (a, xs) <- runStream p
                  requestResource writing
                  liftEvent $ writeRef ref (Just a)
                  releaseResource reading
                  writer xs
             reader =
               do requestResource reading
                  Just a <- liftEvent $ readRef ref
                  liftEvent $ writeRef ref Nothing
                  releaseResource writing
                  return a
         spawnProcess $ writer s
         runStream $ repeatProcess reader

-- | Like 'signalStream' but allows specifying an arbitrary queue instead of the unbounded queue.
queuedSignalStream :: MonadDES m
                      => (a -> Event m ())
                      -- ^ enqueue
                      -> Process m a
                      -- ^ dequeue
                      -> Signal m a
                      -- ^ the input signal
                      -> Composite m (Stream m a)
                      -- ^ the output stream
{-# INLINABLE queuedSignalStream #-}
queuedSignalStream enqueue dequeue s =
  do h <- liftEvent $
          handleSignal s enqueue
     disposableComposite h
     return $ repeatProcess dequeue

-- | Return a stream of values triggered by the specified signal.
--
-- Since the time at which the values of the stream are requested for may differ from
-- the time at which the signal is triggered, it can be useful to apply the 'arrivalSignal'
-- function to add the information about the time points at which the signal was
-- actually received.
--
-- The point is that the 'Stream' is requested outside, while the 'Signal' is triggered
-- inside. They are different by nature. The former is passive, while the latter is active.
--
-- The resulting stream may be a root of space leak as it uses an internal unbounded queue to store
-- the values received from the signal. The oldest value is dequeued each time we request
-- the stream and it is returned within the computation. Consider using 'queuedSignalStream' that
-- allows specifying the bounded queue in case of need.
signalStream :: MonadDES m => Signal m a -> Composite m (Stream m a)
{-# INLINABLE signalStream #-}
signalStream s =
  do q <- liftSimulation IQ.newFCFSQueue
     queuedSignalStream (IQ.enqueue q) (IQ.dequeue q) s

-- | Return a computation of the signal that triggers values from the specified stream,
-- each time the next value of the stream is received within the underlying 'Process'
-- computation.
streamSignal :: MonadDES m => Stream m a -> Composite m (Signal m a)
{-# INLINABLE streamSignal #-}
streamSignal z =
  do s <- liftSimulation newSignalSource
     pid <- liftSimulation newProcessId
     liftEvent $
       runProcessUsingId pid $
       consumeStream (liftEvent . triggerSignal s) z
     disposableComposite $
       DisposableEvent $
       cancelProcessWithId pid
     return $ publishSignal s

-- | Transform a stream so that the resulting stream returns a sequence of arrivals
-- saving the information about the time points at which the original stream items
-- were received by demand.
arrivalStream :: MonadDES m => Stream m a -> Stream m (Arrival a)
{-# INLINABLE arrivalStream #-}
arrivalStream s = Cons $ loop s Nothing where
  loop s t0 = do (a, xs) <- runStream s
                 t <- liftDynamics time
                 let b = Arrival { arrivalValue = a,
                                   arrivalTime  = t,
                                   arrivalDelay =
                                     case t0 of
                                       Nothing -> Nothing
                                       Just t0 -> Just (t - t0) }
                 return (b, Cons $ loop xs (Just t))

-- | Delay the stream by one step using the specified initial value.
delayStream :: MonadDES m => a -> Stream m a -> Stream m a
{-# INLINABLE delayStream #-}
delayStream a0 s = Cons $ return (a0, s)

-- | Return a stream consisting of exactly one element and inifinite tail.
singletonStream :: MonadDES m => a -> Stream m a
{-# INLINABLE singletonStream #-}
singletonStream a = Cons $ return (a, emptyStream)

-- | Removes one level of the computation, projecting its bound stream into the outer level.
joinStream :: MonadDES m => Process m (Stream m a) -> Stream m a
{-# INLINABLE joinStream #-}
joinStream m = Cons $ m >>= runStream

-- | Takes the next stream from the list after the current stream fails because of cancelling the underlying process.
failoverStream :: MonadDES m => [Stream m a] -> Stream m a
{-# INLINABLE failoverStream #-}
failoverStream ps = Cons z where
  z = do reading <- liftSimulation $ newResourceWithMaxCount FCFS 0 (Just 1)
         writing <- liftSimulation $ newResourceWithMaxCount FCFS 0 (Just 1)
         ref <- liftSimulation $ newRef Nothing
         pid <- processId
         let writer p =
               do requestResource writing
                  pid' <- processId
                  (a, xs) <-
                    finallyProcess (runStream p) $
                    liftEvent $
                    do cancelled' <- processCancelled pid'
                       when cancelled' $
                         releaseResourceWithinEvent writing
                  liftEvent $ writeRef ref (Just a)
                  releaseResource reading
                  writer xs
             reader =
               do releaseResource writing
                  requestResource reading
                  Just a <- liftEvent $ readRef ref
                  liftEvent $ writeRef ref Nothing
                  return a
             loop [] = return ()
             loop (p: ps) =
               do pid' <- processId
                  h' <- liftEvent $
                        handleSignal (processCancelling pid) $ \() ->
                        cancelProcessWithId pid'
                  finallyProcess (writer p) $
                    liftEvent $
                    do disposeEvent h'
                       cancelled <- processCancelled pid
                       unless cancelled $
                         do cancelled' <- processCancelled pid'
                            unless cancelled' $
                              error "Expected the sub-process to be cancelled: failoverStream"
                            runProcess $ loop ps
         liftEvent $ runProcess $ loop ps
         runStream $ repeatProcess reader

-- | Return the prefix of the stream of the specified length.
takeStream :: MonadDES m => Int -> Stream m a -> Stream m a
{-# INLINABLE takeStream #-}
takeStream n s
  | n <= 0    = emptyStream
  | otherwise =
    Cons $
    do (a, xs) <- runStream s
       return (a, takeStream (n - 1) xs)

-- | Return the longest prefix of the stream of elements that satisfy the predicate.
takeStreamWhile :: MonadDES m => (a -> Bool) -> Stream m a -> Stream m a
{-# INLINABLE takeStreamWhile #-}
takeStreamWhile p s =
  Cons $
  do (a, xs) <- runStream s
     if p a
       then return (a, takeStreamWhile p xs)
       else neverProcess

-- | Return the longest prefix of the stream of elements that satisfy the computation.
takeStreamWhileM :: MonadDES m => (a -> Process m Bool) -> Stream m a -> Stream m a
{-# INLINABLE takeStreamWhileM #-}
takeStreamWhileM p s =
  Cons $
  do (a, xs) <- runStream s
     f <- p a
     if f
       then return (a, takeStreamWhileM p xs)
       else neverProcess

-- | Return the suffix of the stream after the specified first elements.
dropStream :: MonadDES m => Int -> Stream m a -> Stream m a
{-# INLINABLE dropStream #-}
dropStream n s
  | n <= 0    = s
  | otherwise =
    Cons $
    do (a, xs) <- runStream s
       runStream $ dropStream (n - 1) xs

-- | Return the suffix of the stream of elements remaining after 'takeStreamWhile'.
dropStreamWhile :: MonadDES m => (a -> Bool) -> Stream m a -> Stream m a
{-# INLINABLE dropStreamWhile #-}
dropStreamWhile p s =
  Cons $
  do (a, xs) <- runStream s
     if p a
       then runStream $ dropStreamWhile p xs
       else return (a, xs)

-- | Return the suffix of the stream of elements remaining after 'takeStreamWhileM'.
dropStreamWhileM :: MonadDES m => (a -> Process m Bool) -> Stream m a -> Stream m a
{-# INLINABLE dropStreamWhileM #-}
dropStreamWhileM p s =
  Cons $
  do (a, xs) <- runStream s
     f <- p a
     if f
       then runStream $ dropStreamWhileM p xs
       else return (a, xs)

-- | Create the specified number of equivalent clones of the input stream.
cloneStream :: MonadDES m => Int -> Stream m a -> Simulation m [Stream m a]
{-# INLINABLE cloneStream #-}
cloneStream n s =
  do qs  <- forM [1..n] $ \i -> IQ.newFCFSQueue
     rs  <- newFCFSResource 1
     ref <- newRef s
     let reader m q =
           do a <- liftEvent $ IQ.tryDequeue q
              case a of
                Just a  -> return a
                Nothing ->
                  usingResource rs $
                  do a <- liftEvent $ IQ.tryDequeue q
                     case a of
                       Just a  -> return a
                       Nothing ->
                         do s <- liftEvent $ readRef ref
                            (a, xs) <- runStream s
                            liftEvent $ writeRef ref xs
                            forM_ (zip [1..] qs) $ \(i, q) ->
                              unless (i == m) $
                              liftEvent $ IQ.enqueue q a
                            return a
     forM (zip [1..] qs) $ \(i, q) ->
       return $ repeatProcess $ reader i q

-- | Return a stream of first arrivals after assembling the specified number of elements.
firstArrivalStream :: MonadDES m => Int -> Stream m a -> Stream m a
{-# INLINABLE firstArrivalStream #-}
firstArrivalStream n s = assembleAccumStream f (1, Nothing) s
  where f (i, a0) a =
          let a0' = Just $ fromMaybe a a0
          in if i `mod` n == 0
             then return ((1, Nothing), a0')
             else return ((i + 1, a0'), Nothing)

-- | Return a stream of last arrivals after assembling the specified number of elements.
lastArrivalStream :: MonadDES m => Int -> Stream m a -> Stream m a
{-# INLINABLE lastArrivalStream #-}
lastArrivalStream n s = assembleAccumStream f 1 s
  where f i a =
          if i `mod` n == 0
          then return (1, Just a)
          else return (i + 1, Nothing)

-- | Assemble an accumulated stream using the supplied function.
assembleAccumStream :: MonadDES m => (acc -> a -> Process m (acc, Maybe b)) -> acc -> Stream m a -> Stream m b
{-# INLINABLE assembleAccumStream #-}
assembleAccumStream f acc s =
  mapStream fromJust $
  filterStream isJust $
  accumStream f acc s

-- | Show the debug messages with the current simulation time.
traceStream :: MonadDES m
               => Maybe String
               -- ^ the request message
               -> Maybe String
               -- ^ the response message
               -> Stream m a
               -- ^ a stream
               -> Stream m a
{-# INLINABLE traceStream #-}
traceStream request response s = Cons $ loop s where
  loop s = do (a, xs) <-
                case request of
                  Nothing -> runStream s
                  Just message ->
                    traceProcess message $
                    runStream s
              case response of
                Nothing -> return (a, Cons $ loop xs)
                Just message ->
                  traceProcess message $
                  return (a, Cons $ loop xs)

