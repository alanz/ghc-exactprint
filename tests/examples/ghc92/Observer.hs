-- | Observer Effects
--
-- This module supports the implementation of observerRegistry and observables. Expected use
-- case is event propagation.
--
-- The observable event sources and the observers are usually server processes for a
-- protocol that embeds the 'ObserverRegistry' and 'Observer' 'Pdu's respectively.
--
-- A generic FIFO queue based observer can be found in "Control.Eff.Concurrent.Protocol.Observer.Queue".
--
-- @since 0.16.0
module Control.Eff.Concurrent.Protocol.Observer
  ( Observer (..),
    ObservationSink (),
    IsObservable,
    CanObserve,
    Pdu (RegisterObserver, ForgetObserver, Observed),
    registerObserver,
    forgetObserver,
    forgetObserverUnsafe,
    ObserverRegistry (..),
    ObserverRegistryState,
    observerRegistryNotify,
    evalObserverRegistryState,
    emptyObserverRegistry,
    observerRegistryHandlePdu,
    observerRegistryRemoveProcess,
  )
where

import Control.DeepSeq (NFData (rnf))
import Control.Eff
import Control.Eff.Concurrent.Process
import Control.Eff.Concurrent.Protocol
import Control.Eff.Concurrent.Protocol.Client
import Control.Eff.Concurrent.Protocol.Wrapper (Request (Cast))
import Control.Eff.Log
import Control.Eff.State.Strict
import Control.Lens
import Control.Monad
import Data.Dynamic
import Data.Kind
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Proxy
import Data.Semigroup
import GHC.Generics
import GHC.Stack

-- * Observers

-- ** Observables

-- | A /protocol/ to communicate 'Observed' events from a sources to many sinks.
--
-- A sink is any process that serves a protocol with a 'Pdu' instance that embeds
-- the 'Observer' Pdu via an 'HasPduPrism' instance.
--
-- This type has /dual use/, for one it serves as type-index for 'Pdu', i.e.
-- 'HasPdu' respectively, and secondly it contains an 'ObservationSink' and
-- a 'MonitorReference'.
--
-- The 'ObservationSink' is used to serialize and send the 'Observed' events,
-- while the 'ProcessId' serves as key for internal maps.
--
-- @since 0.28.0
newtype Observer event
  = MkObserver (Arg ProcessId (ObservationSink event))
  deriving (Eq, Ord, Typeable)

instance ToTypeLogMsg event => ToLogMsg (Observer event) where
  toLogMsg (MkObserver (Arg t _)) = toTypeLogMsg (Proxy @(Observer event)) <> toLogMsg t

instance ToTypeLogMsg event => ToTypeLogMsg (Observer event) where
  toTypeLogMsg _ = toTypeLogMsg (Proxy @event) <> packLogMsg "_observer"

instance NFData (Observer event) where
  rnf (MkObserver (Arg x y)) = rnf x `seq` rnf y

instance (Tangible event) => HasPdu (Observer event) where
  data Pdu (Observer event) r where
    Observed :: event -> Pdu (Observer event) 'Asynchronous
    deriving (Typeable)

instance NFData event => NFData (Pdu (Observer event) r) where
  rnf (Observed event) = rnf event

instance ToLogMsg event => ToLogMsg (Pdu (Observer event) r) where
  toLogMsg (Observed event) =
    packLogMsg "observed: " <> toLogMsg event

-- | The Information necessary to wrap an 'Observed' event to a process specific
-- message, e.g. the embedded 'Observer' 'Pdu' instance, and the 'MonitorReference' of
-- the destination process.
--
-- @since 0.28.0
data ObservationSink event = MkObservationSink
  { _observerSerializer :: Serializer (Pdu (Observer event) 'Asynchronous),
    _observerMonitorReference :: MonitorReference
  }
  deriving (Generic, Typeable)

instance NFData (ObservationSink event) where
  rnf (MkObservationSink s p) = s `seq` rnf p

-- | Convenience type alias.
--
-- @since 0.28.0
type IsObservable eventSource event =
  ( Tangible event,
    Embeds eventSource (ObserverRegistry event),
    HasPdu eventSource,
    ToTypeLogMsg event,
    ToLogMsg event
  )

-- | Convenience type alias.
--
-- @since 0.28.0
type CanObserve eventSink event =
  ( Tangible event,
    Embeds eventSink (Observer event),
    HasPdu eventSink
  )

-- | And an 'Observer' to the set of recipients for all observations reported by 'observerRegistryNotify'.
--   Note that the observerRegistry are keyed by the observing process, i.e. a previous entry for the process
--   contained in the 'Observer' is overwritten. If you want multiple entries for a single process, just
--   combine several filter functions.
--
-- @since 0.16.0
registerObserver ::
  forall event eventSink eventSource r q.
  ( HasProcesses r q,
    TangiblePdu eventSource 'Asynchronous,
    IsObservable eventSource event,
    TangiblePdu eventSink 'Asynchronous,
    CanObserve eventSink event
  ) =>
  Endpoint eventSource ->
  Endpoint eventSink ->
  Eff r ()
registerObserver eventSource eventSink =
  cast eventSource (RegisterObserver serializer (eventSink ^. fromEndpoint))
  where
    serializer =
      MkSerializer
        ( toMessage
            . Cast
            . embedPdu @eventSink @(Observer event) @('Asynchronous)
        )

-- | Send the 'ForgetObserver' message
--
-- @since 0.16.0
forgetObserver ::
  forall event eventSink eventSource r q.
  ( HasProcesses r q,
    TangiblePdu eventSource 'Asynchronous,
    IsObservable eventSource event
  ) =>
  Endpoint eventSource ->
  Endpoint eventSink ->
  Eff r ()
forgetObserver eventSource eventSink =
  forgetObserverUnsafe @event @eventSource eventSource (eventSink ^. fromEndpoint)

-- | Send the 'ForgetObserver' message, use a raw 'ProcessId' as parameter.
--
-- @since 0.28.0
forgetObserverUnsafe ::
  forall event eventSource r q.
  ( HasProcesses r q,
    TangiblePdu eventSource 'Asynchronous,
    IsObservable eventSource event
  ) =>
  Endpoint eventSource ->
  ProcessId ->
  Eff r ()
forgetObserverUnsafe eventSource eventSink =
  cast eventSource (ForgetObserver @event eventSink)

-- ** Observer Support Functions

-- * Managing Observers

-- | A protocol for managing 'Observer's, encompassing  registration and de-registration of
-- 'Observer's.
--
-- @since 0.28.0
data ObserverRegistry (event :: Type) = MkObserverRegistry
  {_observerRegistry :: Map ProcessId (ObservationSink event)}
  deriving (Typeable)

instance ToTypeLogMsg event => ToTypeLogMsg (ObserverRegistry event) where
  toTypeLogMsg _ = toTypeLogMsg (Proxy @event) <> packLogMsg "_observer_registry_event"

instance (Tangible event) => HasPdu (ObserverRegistry event) where
  data Pdu (ObserverRegistry event) r where
    -- | This message denotes that the given 'Observer' should receive observations until 'ForgetObserver' is
    --   received.
    --
    -- @since 0.28.0
    RegisterObserver :: Serializer (Pdu (Observer event) 'Asynchronous) -> ProcessId -> Pdu (ObserverRegistry event) 'Asynchronous
    -- | This message denotes that the given 'Observer' should not receive observations anymore.
    --
    -- @since 0.16.1
    ForgetObserver :: ProcessId -> Pdu (ObserverRegistry event) 'Asynchronous
    --    -- | This message denotes that a monitored process died
    --    --
    --    -- @since 0.28.0
    --    ObserverMightBeDown :: MonitorReference -> Pdu (ObserverRegistry event) ( 'Synchronous Bool)
    deriving (Typeable)

instance NFData (Pdu (ObserverRegistry event) r) where
  rnf (RegisterObserver ser pid) = rnf ser `seq` rnf pid
  rnf (ForgetObserver pid) = rnf pid

instance ToTypeLogMsg event => ToLogMsg (Pdu (ObserverRegistry event) r) where
  toLogMsg (RegisterObserver _ser pid) = packLogMsg "register " <> toTypeLogMsg (Proxy @event) <> packLogMsg " observer " <> toLogMsg pid
  toLogMsg (ForgetObserver pid) = packLogMsg "forget " <> toTypeLogMsg (Proxy @event) <> packLogMsg " observer " <> toLogMsg pid

-- ** Protocol for integrating 'ObserverRegistry' into processes.

-- | Provide the implementation for the 'ObserverRegistry' Protocol, this handled 'RegisterObserver' and 'ForgetObserver'
-- messages. It also adds the 'ObserverRegistryState' constraint to the effect list.
--
-- @since 0.28.0
observerRegistryHandlePdu ::
  forall event q r.
  ( HasCallStack,
    ToTypeLogMsg event,
    HasProcesses r q,
    Member (ObserverRegistryState event) r,
    Member Logs r
  ) =>
  Pdu (ObserverRegistry event) 'Asynchronous ->
  Eff r ()
observerRegistryHandlePdu = \case
  RegisterObserver ser pid -> do
    monRef <- monitor pid
    let sink = MkObservationSink ser monRef
        observer = MkObserver (Arg pid sink)
    modify @(ObserverRegistry event) (over observerRegistry (Map.insert pid sink))
    os <- get @(ObserverRegistry event)
    logDebug
      (LABEL "registered" observer)
      ( LABEL
          "current number of observers" -- TODO put this info into the process details
          (Map.size (view observerRegistry os))
      )
  ForgetObserver ob -> do
    wasRemoved <- observerRegistryRemoveProcess @event ob
    unless wasRemoved $
      logDebug (LABEL "unknown observer " ob)

-- | Remove the entry in the 'ObserverRegistry' for the 'ProcessId'
-- and return 'True' if there was an entry, 'False' otherwise.
--
-- @since 0.28.0
observerRegistryRemoveProcess ::
  forall event q r.
  ( HasCallStack,
    ToTypeLogMsg event,
    HasProcesses r q,
    Member (ObserverRegistryState event) r,
    Member Logs r
  ) =>
  ProcessId ->
  Eff r Bool
observerRegistryRemoveProcess ob = do
  mSink <- view (observerRegistry . at ob) <$> get @(ObserverRegistry event)
  modify @(ObserverRegistry event) (observerRegistry . at ob .~ Nothing)
  os <- get @(ObserverRegistry event)
  maybe
    (pure False)
    (foundIt os)
    mSink
  where
    foundIt os sink@(MkObservationSink _ monRef) = do
      demonitor monRef
      logDebug
        (LABEL "removed" (MkObserver $ Arg ob sink))
        (LABEL "current number of observers" (Map.size (view observerRegistry os)))
      pure True

-- | Keep track of registered 'Observer's.
--
-- Handle the 'ObserverRegistryState' effect, i.e. run 'evalState' on an 'emptyObserverRegistry'.
--
-- @since 0.28.0
evalObserverRegistryState :: Eff (ObserverRegistryState event ': r) a -> Eff r a
evalObserverRegistryState = evalState emptyObserverRegistry

-- | The empty 'ObserverRegistryState'
--
-- @since 0.28.0
emptyObserverRegistry :: ObserverRegistry event
emptyObserverRegistry = MkObserverRegistry Map.empty

-- | Alias for the effect that contains the observers managed by 'evalObserverRegistryState'
type ObserverRegistryState event = State (ObserverRegistry event)

-- | An 'Iso' for the 'Map' used internally.
observerRegistry :: Iso' (ObserverRegistry event) (Map ProcessId (ObservationSink event))
observerRegistry = iso _observerRegistry MkObserverRegistry

-- | Report an observation to all observers.
-- The process needs to 'evalObserverRegistryState' and to 'observerRegistryHandlePdu'.
--
-- @since 0.28.0
observerRegistryNotify ::
  forall event r q.
  ( HasProcesses r q,
    Member (ObserverRegistryState event) r
  ) =>
  event ->
  Eff r ()
observerRegistryNotify observation = do
  os <- view observerRegistry <$> get
  mapM_ notifySomeObserver (Map.assocs os)
  where
    notifySomeObserver (destination, (MkObservationSink serializer _)) =
      sendAnyMessage destination (runSerializer serializer (Observed observation))

