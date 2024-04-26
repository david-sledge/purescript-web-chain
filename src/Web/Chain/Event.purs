-- | Chain functions related to events.
module Web.Chain.Event
  ( change
  , changeM
  , onChange
  , onChangeM
  , onReady
  , onReady_
  ) where

import Prelude

import Control.Bind (bindFlipped)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Web.Chain (doc)
import Web.Event.Class.EventTargetOp (class EventTargetOp, newEvent, on, onM, trigger)
import Web.Event.Event (EventType(EventType))
import Web.Event.Internal.Types (Event)
import Web.HTML (HTMLDocument)
import Web.HTML.HTMLDocument (readyState)
import Web.HTML.HTMLDocument.ReadyState (ReadyState(Loading))

-- | Attach an event handler function to the HTML document when the DOM is fully
-- | loaded. The document is returned.
onReady ∷ ∀ a m. MonadEffect m ⇒ (Event → Effect a) → m HTMLDocument
onReady f = do
  htmlDoc ← doc
  state ← liftEffect $ readyState htmlDoc
  let register = on "DOMContentLoaded" f htmlDoc
  case state of
    Loading → register
    _ → (liftEffect <<< f $ newEvent (EventType "DOMContentLoaded") { bubbles: false, cancelable: false, composed: false }) *> register

-- | Attach an event handler function to the HTML document when the DOM is fully
-- | loaded.
onReady_ ∷ ∀ a m. MonadEffect m ⇒ (Event → Effect a) → m Unit
onReady_ f = onReady f *> pure unit

-- | Attach an event handler function to the event target when the target's
-- | value changes. The target is returned.
onChange ∷ ∀ a m t. MonadEffect m ⇒ EventTargetOp t ⇒ (Event → Effect a) → t → m t
onChange = on "change"

-- | Attach an event handler function to the event target when the target's
-- | value changes. The target is returned.
onChangeM ∷ ∀ a m t. MonadEffect m ⇒ EventTargetOp t ⇒ (Event → Effect a) → m t → m t
onChangeM = onM "change"

-- | Triggers the change event handlers tied to the event target. The target is
-- | returned.
change ∷ ∀ t m. EventTargetOp t ⇒ MonadEffect m ⇒ t → m t
change = trigger "change"

-- | Triggers the change event handlers tied to the event target. The target is
-- | returned.
changeM ∷ ∀ m t. Bind m ⇒ EventTargetOp t ⇒ MonadEffect m ⇒ m t → m t
changeM = bindFlipped change
