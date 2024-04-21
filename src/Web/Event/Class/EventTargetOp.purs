-- | Type classes of convenience.

module Web.Event.Class.EventTargetOp
  ( EventOptions
  , allOff
  , allOffM
  , class EventTargetOp
  , getAllListeners
  , newEvent
  , off
  , offM
  , offOptions
  , offOptionsM
  , on
  , onM
  , onOptions
  , onOptionsM
  , registerListener
  , toEventTarget
  , trigger
  , triggerM
  , triggerOptions
  , triggerOptionsM
  , typeOff
  , typeOffM
  , typeOffOptions
  , typeOffOptionsM
  , unregisterListener
  ) where

import Prelude

import Control.Monad.ST (run)
import Data.Array ((!!))
import Data.Array.ST (splice, unsafeThaw)
import Data.Foldable (traverse_)
import Data.Maybe (maybe)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Unsafe.Coerce (unsafeCoerce)
import Unsafe.Reference (reallyUnsafeRefEq)
import Web.DOM (CharacterData, Document, Text)
import Web.DOM.CharacterData as C
import Web.DOM.Document as D
import Web.DOM.Element as E
import Web.DOM.Node as N
import Web.DOM.Text as T
import Web.Event.Event (Event, EventType(EventType))
import Web.Event.EventTarget (EventTarget, addEventListenerWithOptions, dispatchEvent, eventListener)
import Web.HTML (Window)
import Web.HTML.HTMLButtonElement as HB
import Web.HTML.HTMLDivElement as HDv
import Web.HTML.HTMLDocument as HD
import Web.HTML.HTMLElement as HE
import Web.HTML.HTMLInputElement as HI
import Web.HTML.HTMLTableCellElement as HTd
import Web.HTML.HTMLTableRowElement as HTr

class EventTargetOp et where
  toEventTarget ∷ et → EventTarget

--------------------------------------------------------------------------------
instance EventTargetOp EventTarget where
  toEventTarget = identity

--------------------------------------------------------------------------------
-- children
instance EventTargetOp N.Node where
  toEventTarget = N.toEventTarget

--------------------------------------------------------------------------------
-- grandchildren
instance EventTargetOp CharacterData where
  toEventTarget = C.toEventTarget

instance EventTargetOp Document where
  toEventTarget = D.toEventTarget

instance EventTargetOp E.Element where
  toEventTarget = E.toEventTarget

instance EventTargetOp Window where
  toEventTarget = unsafeCoerce

--------------------------------------------------------------------------------
-- great-grandchildren
instance EventTargetOp HD.HTMLDocument where
  toEventTarget = HD.toEventTarget

instance EventTargetOp HE.HTMLElement where
  toEventTarget = HE.toEventTarget

instance EventTargetOp Text where
  toEventTarget = T.toEventTarget

--------------------------------------------------------------------------------
-- great-great-grandchildren
instance EventTargetOp HB.HTMLButtonElement where
  toEventTarget = HB.toEventTarget

instance EventTargetOp HDv.HTMLDivElement where
  toEventTarget = HDv.toEventTarget

instance EventTargetOp HI.HTMLInputElement where
  toEventTarget = HI.toEventTarget

instance EventTargetOp HTd.HTMLTableCellElement where
  toEventTarget = HTd.toEventTarget

instance EventTargetOp HTr.HTMLTableRowElement where
  toEventTarget = HTr.toEventTarget

type EventOptions = { bubbles ∷ Boolean, cancelable ∷ Boolean, composed ∷ Boolean }

-- | Creates an event.
foreign import _newEvent ∷ EventType → EventOptions → Event

newEvent ∷ EventType → EventOptions → Event
newEvent = _newEvent

type ListenerOptions = { capture ∷ Boolean, once ∷ Boolean, passive ∷ Boolean }

type ListenerDetails a = { eventType ∷ String, listener ∷ Event → Effect a, options ∷ ListenerOptions }

foreign import _registerListener ∷ ∀ a. ListenerDetails a → EventTarget → Effect Unit

toggleOptions ∷ ListenerOptions
toggleOptions = { capture: false, once: false, passive: false }

-- | Attach an event handler function to the event target. The target is
-- | returned.
registerListener ∷ ∀ a m t. MonadEffect m ⇒ EventTargetOp t ⇒ String → (Event → Effect a) → ListenerOptions → t → m Unit
registerListener typeStr f listenOptions target = liftEffect do
  let eventTarget = toEventTarget target
  _registerListener { eventType: typeStr, listener: f, options: listenOptions } eventTarget
  listener ← eventListener f
  addEventListenerWithOptions (EventType typeStr) listener listenOptions eventTarget

-- | Attach an event handler function to the event target. The target is
-- | returned.
onOptions ∷ ∀ a m t. MonadEffect m ⇒ EventTargetOp t ⇒ String → (Event → Effect a) → ListenerOptions → t → m t
onOptions typeStr f listenOptions target = registerListener typeStr f listenOptions target *> pure target

onOptionsM ∷ ∀ a m t. MonadEffect m ⇒ EventTargetOp t ⇒ String → (Event → Effect a) → ListenerOptions → m t → m t
onOptionsM typeStr f = (<<<) (=<<) $ onOptions typeStr f

-- | Attach an event handler function to the event target. The target is
-- | returned.
on ∷ ∀ a m t. MonadEffect m ⇒ EventTargetOp t ⇒ String → (Event → Effect a) → t → m t
on typeStr f = onOptions typeStr f toggleOptions

-- | Attach an event handler function to the event target. The target is
-- | returned.
onM ∷ ∀ a m t. MonadEffect m ⇒ EventTargetOp t ⇒ String → (Event → Effect a) → m t → m t
onM a f = (=<<) $ on a f

-- | Gets all listeners tied to an event target. Each value returned is in the
-- | form of a continuation that expects an function argument that takes four
-- | arguments: event type, event listener, Boolean (indicating whether events
-- | of this type will be dispatched to the registered listener before being
-- | dispatched to any EventTarget beneath it in the DOM tree), and an event
-- | target.
foreign import _getAllListeners ∷ ∀ a. EventTarget → Effect (Array (ListenerDetails a))

foreign import _rmListener ∷ ∀ a. ListenerDetails a → EventTarget → Effect Unit

unregisterListener ∷ ∀ a m t. MonadEffect m ⇒ EventTargetOp t ⇒ ListenerDetails a → t → m Boolean
unregisterListener details target = liftEffect do
  let eventTarget = toEventTarget target
  detailss ← _getAllListeners eventTarget
  let
    loop ndx =
      maybe (pure false)
        ( \details' →
            if
              details.eventType == details'.eventType
                && reallyUnsafeRefEq details.listener details'.listener
                && details.options.capture == details'.options.capture
                && details.options.once == details'.options.once
                &&
                  details.options.passive == details'.options.once then pure (run (unsafeThaw detailss >>= splice ndx 1 []))
              *> _rmListener details eventTarget
              *>
                pure true
            else loop $ ndx + 1
        ) $ detailss !! ndx
  loop 0

getAllListeners ∷ ∀ a m t. MonadEffect m ⇒ EventTargetOp t ⇒ t → m (Array (ListenerDetails a))
getAllListeners = liftEffect <<< _getAllListeners <<< toEventTarget

-- | Removes an event handler function from an event target. The target is
-- | returned.
offOptions ∷ ∀ a m t. MonadEffect m ⇒ EventTargetOp t ⇒ String → (Event → Effect a) → ListenerOptions → t → m t
offOptions typeStr f options target = do
  _ ← unregisterListener { eventType: typeStr, listener: f, options: options } target
  pure target

-- | Removes an event handler function from an event target. The target is
-- | returned.
offOptionsM ∷ ∀ a m t. MonadEffect m ⇒ EventTargetOp t ⇒ String → (Event → Effect a) → ListenerOptions → m t → m t
offOptionsM typeStr f = (<<<) (=<<) $ offOptions typeStr f

-- | Removes an event handler function from an event target. The target is
-- | returned.
off ∷ ∀ a m t. MonadEffect m ⇒ EventTargetOp t ⇒ String → (Event → Effect a) → t → m t
off typeStr f = offOptions typeStr f toggleOptions

-- | Removes an event handler function from an event target. The target is
-- | returned.
offM ∷ ∀ a m t. MonadEffect m ⇒ EventTargetOp t ⇒ String → (Event → Effect a) → m t → m t
offM a f = (=<<) $ off a f

-- | Removes all event handler functions of the given type from an event target.
-- | The target is returned.
typeOffOptions ∷ ∀ t m. MonadEffect m ⇒ EventTargetOp t ⇒ String → ListenerOptions → t → m t
typeOffOptions typeStr options target = do
  getAllListeners target >>=
    traverse_
      ( \listenerDetails →
          when (typeStr == listenerDetails.eventType && listenerDetails.options == options) $
            unregisterListener listenerDetails target *> pure unit
      )
  pure target

-- | Removes all event handler functions of the given type from an event target.
-- | The target is returned.
typeOffOptionsM ∷ ∀ t m. MonadEffect m ⇒ EventTargetOp t ⇒ String → ListenerOptions → m t → m t
typeOffOptionsM = (<<<) (=<<) <<< typeOffOptions

-- | Removes all event handler functions of the given type from an event target.
-- | The target is returned.
typeOff ∷ ∀ t m. MonadEffect m ⇒ EventTargetOp t ⇒ String → t → m t
typeOff typeStr = typeOffOptions typeStr toggleOptions

-- | Removes all event handler functions of the given type from an event target.
-- | The target is returned.
typeOffM ∷ ∀ m t. Bind m ⇒ MonadEffect m ⇒ EventTargetOp t ⇒ String → m t → m t
typeOffM = (=<<) <<< typeOff

-- | Removes all event handler functions from an event target. The target is
-- | returned.
allOff ∷ ∀ m t. Bind m ⇒ MonadEffect m ⇒ EventTargetOp t ⇒ t → m t
allOff target = do
  getAllListeners target >>=
    traverse_ (\listenerDetails → unregisterListener listenerDetails target)
  pure target

-- | Removes all event handler functions from an event target. The target is
-- | returned.
allOffM ∷ ∀ m t. Bind m ⇒ MonadEffect m ⇒ EventTargetOp t ⇒ m t → m t
allOffM = (=<<) allOff

-- | Triggers the event handlers tied to the event type to the event target. The
-- | target is returned.
triggerOptions ∷ ∀ t m. EventTargetOp t ⇒ MonadEffect m ⇒ String → EventOptions → t → m t
triggerOptions evtType options target = liftEffect do
  _ ← dispatchEvent (newEvent (EventType evtType) options) (toEventTarget target)
  pure target

-- | Triggers the event handlers tied to the event type to the event target. The
-- | target is returned.
triggerOptionsM ∷ ∀ t m. EventTargetOp t ⇒ MonadEffect m ⇒ String → EventOptions → m t → m t
triggerOptionsM = (<<<) (=<<) <<< triggerOptions

-- | Triggers the event handlers tied to the event type to the event target. The
-- | target is returned.
trigger ∷ ∀ t m. EventTargetOp t ⇒ MonadEffect m ⇒ String → t → m t
trigger evtType = triggerOptions evtType { bubbles: false, cancelable: false, composed: false }

-- | Triggers the event handlers tied to the event type to the event target. The
-- | target is returned.
triggerM ∷ ∀ t m. EventTargetOp t ⇒ MonadEffect m ⇒ String → m t → m t
triggerM = (=<<) <<< trigger
