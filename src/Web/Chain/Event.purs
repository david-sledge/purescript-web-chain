-- | Chain functions related to events.
module Web.Chain.Event
  ( allOff
  , allOffM
  , change
  , changeM
  , getAllListeners
  , newEvent
  , off
  , offM
  , on
  , onChange
  , onChangeM
  , onM
  , onReady
  , onReady_
  , trigger
  , triggerM
  , typeOff
  , typeOffM
  )
  where

import Prelude

import Data.Foldable (traverse_)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Web.Chain.Class (class IsEventTarget, toEventTarget)
import Web.Event.Event (Event, EventType(..))
import Web.Event.EventTarget (EventListener, EventTarget, addEventListener, dispatchEvent, eventListener, removeEventListener)
import Web.HTML (window)
import Web.HTML.HTMLDocument (HTMLDocument, readyState)
import Web.HTML.HTMLDocument.ReadyState (ReadyState(..))
import Web.HTML.Window (document)

-- | Creates an event.
foreign import _newEvent ∷ EventType → Effect Event

newEvent ∷ ∀ m. MonadEffect m ⇒ EventType → m Event
newEvent = liftEffect <<< _newEvent

foreign import _registerListener ∷ ∀ a. a → EventTarget → Effect Unit
foreign import _unregisterListener ∷ ∀ a. EventType → EventListener → a → (EventType → EventListener → a → EventTarget → Effect Unit) → EventTarget → Effect Unit

unregisterListener ∷ ∀ m a. MonadEffect m ⇒ EventType → EventListener → a → (EventType → EventListener → a → EventTarget → Effect Unit) → EventTarget → m Unit
unregisterListener = (<<<) ((<<<) <<< (<<<) $ (<<<) liftEffect) <<< _unregisterListener

-- | Gets all listeners tied to an event target. Each value returned is in the
-- | form of a continuation that expects an function argument that takes four
-- | arguments: event type, event listener, Boolean (indicating whether events
-- | of this type will be dispatched to the registered listener before being
-- | dispatched to any EventTarget beneath it in the DOM tree), and an event
-- | target.
foreign import _getAllListeners ∷ EventTarget → Effect (Array ((EventType → EventListener → Boolean → EventTarget → Effect Unit) → Effect Unit))
foreign import _clearRegisteredListeners ∷ EventTarget → Effect Unit

getAllListeners ∷ ∀ m. MonadEffect m ⇒ EventTarget → m (Array ((EventType → EventListener → Boolean → EventTarget → Effect Unit) → Effect Unit))
getAllListeners = liftEffect <<< _getAllListeners

-- | Attach an event handler function to the event target. The target is
-- | returned.
on ∷ ∀ m et a. MonadEffect m ⇒ IsEventTarget et ⇒ String → (Event → Effect a) → et → m et
on typeStr listener target = do
  listen ← liftEffect $ eventListener listener
  let target' = toEventTarget target
      applyArgs f = f (EventType typeStr) listen false target'
  liftEffect $ _registerListener applyArgs target' *> applyArgs addEventListener
  pure target

-- | Attach an event handler function to the event target. The target is
-- | returned.
onM ∷ ∀ m et a. MonadEffect m ⇒ IsEventTarget et ⇒ String → (Event → Effect a) → m et → m et
onM = (<<<) (=<<) <<< on

-- | Attach an event handler function to the event target when the target's
-- | value changes. The target is returned.
onChange ∷ ∀ m et a. MonadEffect m ⇒ IsEventTarget et ⇒ (Event → Effect a) → et → m et
onChange = on "change"

-- | Attach an event handler function to the event target when the target's
-- | value changes. The target is returned.
onChangeM ∷ ∀ m et a. MonadEffect m ⇒ IsEventTarget et ⇒ (Event → Effect a) → m et → m et
onChangeM = onM "change"

-- | Attach an event handler function to the HTML document when the DOM is fully
-- | loaded. The document is returned.
onReady :: ∀ m a. MonadEffect m ⇒ (Event → Effect a) → m HTMLDocument
onReady f = do
  htmlDoc <- liftEffect $ document =<< window
  state ← liftEffect $ readyState htmlDoc
  case state of
    Loading → on "DOMContentLoaded" f htmlDoc
    _ → (liftEffect <<< f =<< (newEvent (EventType "DOMContentLoaded"))) *> pure htmlDoc

-- | Attach an event handler function to the HTML document when the DOM is fully
-- | loaded.
onReady_ :: ∀ m a. MonadEffect m ⇒ (Event → Effect a) → m Unit
onReady_ f = onReady f *> pure unit

-- | Removes an event handler function from an event target. The target is
-- | returned.
off ∷ ∀ et a m. MonadEffect m ⇒ IsEventTarget et ⇒ String → (Event → Effect a) → et → m et
off typeStr listener target = do
  listen ← liftEffect $ eventListener listener
  let target' = toEventTarget target
  unregisterListener (EventType typeStr) listen false removeEventListener target'
  pure target

-- | Removes an event handler function from an event target. The target is
-- | returned.
offM ∷ ∀ et a m. MonadEffect m ⇒ IsEventTarget et ⇒ String → (Event → Effect a) → m et → m et
offM = (<<<) (=<<) <<< off

-- | Removes all event handler functions of the given type from an event target.
-- | The target is returned.
typeOff ∷ ∀ et m. MonadEffect m ⇒ IsEventTarget et ⇒ String → et → m et
typeOff typeStr target = do
  let eventType = EventType typeStr
      target' = toEventTarget target
  listeners ← getAllListeners target'
  traverse_
    (\ f → liftEffect $ f
      (\ evtType listener thirdArg _ →
        if evtType == eventType && thirdArg == false
        then unregisterListener evtType listener thirdArg removeEventListener target'
        else pure unit
      )
    ) listeners
  pure target

-- | Removes all event handler functions of the given type from an event target.
-- | The target is returned.
typeOffM ∷ ∀ et m. MonadEffect m ⇒ IsEventTarget et ⇒ String → m et → m et
typeOffM = (=<<) <<< typeOff

-- | Removes all event handler functions from an event target. The target is
-- | returned.
allOff ∷ ∀ m et. IsEventTarget et ⇒ MonadEffect m ⇒ et → m et
allOff target = do
  let target' = toEventTarget target
  listeners ← getAllListeners $ toEventTarget target
  traverse_ (\ f → liftEffect $ f removeEventListener) listeners
  liftEffect $ _clearRegisteredListeners target'
  pure target

-- | Removes all event handler functions from an event target. The target is
-- | returned.
allOffM ∷ ∀ m et. IsEventTarget et ⇒ MonadEffect m ⇒ m et → m et
allOffM = (=<<) allOff

-- | Triggers the event handlers tied to the event type to the event target. The
-- | target is returned.
trigger ∷ ∀ et m. IsEventTarget et ⇒ MonadEffect m ⇒ String → et → m et
trigger evtType target = do
  event ← newEvent $ EventType evtType
  _ <- liftEffect $ dispatchEvent event (toEventTarget target)
  pure target

-- | Triggers the event handlers tied to the event type to the event target. The
-- | target is returned.
triggerM ∷ ∀ et m. IsEventTarget et ⇒ MonadEffect m ⇒ String → m et → m et
triggerM = (=<<) <<< trigger

-- | Triggers the change event handlers tied to the event target. The target is
-- | returned.
change ∷ ∀ et m. IsEventTarget et ⇒ MonadEffect m ⇒ et → m et
change = trigger "change"

-- | Triggers the change event handlers tied to the event target. The target is
-- | returned.
changeM ∷ ∀ et m. IsEventTarget et ⇒ MonadEffect m ⇒ m et → m et
changeM = (=<<) change
