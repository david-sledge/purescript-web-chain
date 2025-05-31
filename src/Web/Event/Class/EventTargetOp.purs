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

import Control.Bind (bindFlipped)
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
import Web.HTML.HTMLAnchorElement as HAn
import Web.HTML.HTMLAreaElement as HAr
import Web.HTML.HTMLAudioElement as HAu
import Web.HTML.HTMLBRElement as HBR
import Web.HTML.HTMLBaseElement as HBa
import Web.HTML.HTMLBodyElement as HBo
import Web.HTML.HTMLButtonElement as HBu
import Web.HTML.HTMLCanvasElement as HCa
import Web.HTML.HTMLDListElement as HDL
import Web.HTML.HTMLDataElement as HDE
import Web.HTML.HTMLDataListElement as HDa
import Web.HTML.HTMLDivElement as HDv
import Web.HTML.HTMLDocument as HD
import Web.HTML.HTMLElement as HE
import Web.HTML.HTMLEmbedElement as HEm
import Web.HTML.HTMLFieldSetElement as HFi
import Web.HTML.HTMLFormElement as HFo
import Web.HTML.HTMLHRElement as HHR
import Web.HTML.HTMLHeadElement as HHe
import Web.HTML.HTMLHeadingElement as HHa
import Web.HTML.HTMLHtmlElement as HHt
import Web.HTML.HTMLIFrameElement as HIF
import Web.HTML.HTMLInputElement as HIn
import Web.HTML.HTMLKeygenElement as HKe
import Web.HTML.HTMLLIElement as HLI
import Web.HTML.HTMLLabelElement as HLa
import Web.HTML.HTMLLegendElement as HLe
import Web.HTML.HTMLLinkElement as HLi
import Web.HTML.HTMLMapElement as HMa
import Web.HTML.HTMLMediaElement as HMe
import Web.HTML.HTMLMetaElement as HMt
import Web.HTML.HTMLMeterElement as HMr
import Web.HTML.HTMLModElement as HMo
import Web.HTML.HTMLOListElement as HOL
import Web.HTML.HTMLObjectElement as HOb
import Web.HTML.HTMLOptGroupElement as HOG
import Web.HTML.HTMLOptionElement as HOp
import Web.HTML.HTMLOutputElement as HOu
import Web.HTML.HTMLParagraphElement as HPa
import Web.HTML.HTMLParamElement as HPm
import Web.HTML.HTMLPreElement as HPr
import Web.HTML.HTMLProgressElement as HPo
import Web.HTML.HTMLQuoteElement as HQu
import Web.HTML.HTMLScriptElement as HSc
import Web.HTML.HTMLSelectElement as HSe
import Web.HTML.HTMLSourceElement as HSo
import Web.HTML.HTMLSpanElement as HSp
import Web.HTML.HTMLStyleElement as HSt
import Web.HTML.HTMLTableCaptionElement as HTC
import Web.HTML.HTMLTableCellElement as HTe
import Web.HTML.HTMLTableColElement as HTo
import Web.HTML.HTMLTableDataCellElement as HTd
import Web.HTML.HTMLTableElement as HTE
import Web.HTML.HTMLTableHeaderCellElement as HTH
import Web.HTML.HTMLTableRowElement as HTr
import Web.HTML.HTMLTableSectionElement as HTS
import Web.HTML.HTMLTemplateElement as HTm
import Web.HTML.HTMLTextAreaElement as HTA
import Web.HTML.HTMLTimeElement as HTi
import Web.HTML.HTMLTitleElement as HTt
import Web.HTML.HTMLTrackElement as HTa
import Web.HTML.HTMLUListElement as HUL
import Web.HTML.HTMLVideoElement as HVi

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
instance EventTargetOp HAn.HTMLAnchorElement where
  toEventTarget = HAn.toEventTarget

instance EventTargetOp HAr.HTMLAreaElement where
  toEventTarget = HAr.toEventTarget

instance EventTargetOp HAu.HTMLAudioElement where
  toEventTarget = HAu.toEventTarget

instance EventTargetOp HBR.HTMLBRElement where
  toEventTarget = HBR.toEventTarget

instance EventTargetOp HBa.HTMLBaseElement where
  toEventTarget = HBa.toEventTarget

instance EventTargetOp HBo.HTMLBodyElement where
  toEventTarget = HBo.toEventTarget

instance EventTargetOp HBu.HTMLButtonElement where
  toEventTarget = HBu.toEventTarget

instance EventTargetOp HCa.HTMLCanvasElement where
  toEventTarget = HCa.toEventTarget

instance EventTargetOp HDL.HTMLDListElement where
  toEventTarget = HDL.toEventTarget

instance EventTargetOp HDE.HTMLDataElement where
  toEventTarget = HDE.toEventTarget

instance EventTargetOp HDa.HTMLDataListElement where
  toEventTarget = HDa.toEventTarget

instance EventTargetOp HDv.HTMLDivElement where
  toEventTarget = HDv.toEventTarget

instance EventTargetOp HEm.HTMLEmbedElement where
  toEventTarget = HEm.toEventTarget

instance EventTargetOp HFi.HTMLFieldSetElement where
  toEventTarget = HFi.toEventTarget

instance EventTargetOp HFo.HTMLFormElement where
  toEventTarget = HFo.toEventTarget

instance EventTargetOp HHR.HTMLHRElement where
  toEventTarget = HHR.toEventTarget

instance EventTargetOp HHe.HTMLHeadElement where
  toEventTarget = HHe.toEventTarget

instance EventTargetOp HHa.HTMLHeadingElement where
  toEventTarget = HHa.toEventTarget

instance EventTargetOp HHt.HTMLHtmlElement where
  toEventTarget = HHt.toEventTarget

instance EventTargetOp HIF.HTMLIFrameElement where
  toEventTarget = HIF.toEventTarget

instance EventTargetOp HIn.HTMLInputElement where
  toEventTarget = HIn.toEventTarget

instance EventTargetOp HKe.HTMLKeygenElement where
  toEventTarget = HKe.toEventTarget

instance EventTargetOp HLI.HTMLLIElement where
  toEventTarget = HLI.toEventTarget

instance EventTargetOp HLa.HTMLLabelElement where
  toEventTarget = HLa.toEventTarget

instance EventTargetOp HLe.HTMLLegendElement where
  toEventTarget = HLe.toEventTarget

instance EventTargetOp HLi.HTMLLinkElement where
  toEventTarget = HLi.toEventTarget

instance EventTargetOp HMa.HTMLMapElement where
  toEventTarget = HMa.toEventTarget

instance EventTargetOp HMe.HTMLMediaElement where
  toEventTarget = HMe.toEventTarget

instance EventTargetOp HMt.HTMLMetaElement where
  toEventTarget = HMt.toEventTarget

instance EventTargetOp HMr.HTMLMeterElement where
  toEventTarget = HMr.toEventTarget

instance EventTargetOp HMo.HTMLModElement where
  toEventTarget = HMo.toEventTarget

instance EventTargetOp HOL.HTMLOListElement where
  toEventTarget = HOL.toEventTarget

instance EventTargetOp HOb.HTMLObjectElement where
  toEventTarget = HOb.toEventTarget

instance EventTargetOp HOG.HTMLOptGroupElement where
  toEventTarget = HOG.toEventTarget

instance EventTargetOp HOp.HTMLOptionElement where
  toEventTarget = HOp.toEventTarget

instance EventTargetOp HOu.HTMLOutputElement where
  toEventTarget = HOu.toEventTarget

instance EventTargetOp HPa.HTMLParagraphElement where
  toEventTarget = HPa.toEventTarget

instance EventTargetOp HPm.HTMLParamElement where
  toEventTarget = HPm.toEventTarget

instance EventTargetOp HPr.HTMLPreElement where
  toEventTarget = HPr.toEventTarget

instance EventTargetOp HPo.HTMLProgressElement where
  toEventTarget = HPo.toEventTarget

instance EventTargetOp HQu.HTMLQuoteElement where
  toEventTarget = HQu.toEventTarget

instance EventTargetOp HSc.HTMLScriptElement where
  toEventTarget = HSc.toEventTarget

instance EventTargetOp HSe.HTMLSelectElement where
  toEventTarget = HSe.toEventTarget

instance EventTargetOp HSo.HTMLSourceElement where
  toEventTarget = HSo.toEventTarget

instance EventTargetOp HSp.HTMLSpanElement where
  toEventTarget = HSp.toEventTarget

instance EventTargetOp HSt.HTMLStyleElement where
  toEventTarget = HSt.toEventTarget

instance EventTargetOp HTC.HTMLTableCaptionElement where
  toEventTarget = HTC.toEventTarget

instance EventTargetOp HTe.HTMLTableCellElement where
  toEventTarget = HTe.toEventTarget

instance EventTargetOp HTo.HTMLTableColElement where
  toEventTarget = HTo.toEventTarget

instance EventTargetOp HTd.HTMLTableDataCellElement where
  toEventTarget = HTd.toEventTarget

instance EventTargetOp HTE.HTMLTableElement where
  toEventTarget = HTE.toEventTarget

instance EventTargetOp HTH.HTMLTableHeaderCellElement where
  toEventTarget = HTH.toEventTarget

instance EventTargetOp HTr.HTMLTableRowElement where
  toEventTarget = HTr.toEventTarget

instance EventTargetOp HTS.HTMLTableSectionElement where
  toEventTarget = HTS.toEventTarget

instance EventTargetOp HTm.HTMLTemplateElement where
  toEventTarget = HTm.toEventTarget

instance EventTargetOp HTA.HTMLTextAreaElement where
  toEventTarget = HTA.toEventTarget

instance EventTargetOp HTi.HTMLTimeElement where
  toEventTarget = HTi.toEventTarget

instance EventTargetOp HTt.HTMLTitleElement where
  toEventTarget = HTt.toEventTarget

instance EventTargetOp HTa.HTMLTrackElement where
  toEventTarget = HTa.toEventTarget

instance EventTargetOp HUL.HTMLUListElement where
  toEventTarget = HUL.toEventTarget

instance EventTargetOp HVi.HTMLVideoElement where
  toEventTarget = HVi.toEventTarget

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
onOptionsM typeStr f = compose bindFlipped $ onOptions typeStr f

-- | Attach an event handler function to the event target. The target is
-- | returned.
on ∷ ∀ a m t. MonadEffect m ⇒ EventTargetOp t ⇒ String → (Event → Effect a) → t → m t
on typeStr f = onOptions typeStr f toggleOptions

-- | Attach an event handler function to the event target. The target is
-- | returned.
onM ∷ ∀ a m t. MonadEffect m ⇒ EventTargetOp t ⇒ String → (Event → Effect a) → m t → m t
onM a f = bindFlipped $ on a f

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
                  details.options.passive == details'.options.once
            then pure (run (unsafeThaw detailss >>= splice ndx 1 []))
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
offOptionsM typeStr f = compose bindFlipped $ offOptions typeStr f

-- | Removes an event handler function from an event target. The target is
-- | returned.
off ∷ ∀ a m t. MonadEffect m ⇒ EventTargetOp t ⇒ String → (Event → Effect a) → t → m t
off typeStr f = offOptions typeStr f toggleOptions

-- | Removes an event handler function from an event target. The target is
-- | returned.
offM ∷ ∀ a m t. MonadEffect m ⇒ EventTargetOp t ⇒ String → (Event → Effect a) → m t → m t
offM a f = bindFlipped $ off a f

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
typeOffOptionsM = compose bindFlipped <<< typeOffOptions

-- | Removes all event handler functions of the given type from an event target.
-- | The target is returned.
typeOff ∷ ∀ t m. MonadEffect m ⇒ EventTargetOp t ⇒ String → t → m t
typeOff typeStr = typeOffOptions typeStr toggleOptions

-- | Removes all event handler functions of the given type from an event target.
-- | The target is returned.
typeOffM ∷ ∀ m t. Bind m ⇒ MonadEffect m ⇒ EventTargetOp t ⇒ String → m t → m t
typeOffM = bindFlipped <<< typeOff

-- | Removes all event handler functions from an event target. The target is
-- | returned.
allOff ∷ ∀ m t. Bind m ⇒ MonadEffect m ⇒ EventTargetOp t ⇒ t → m t
allOff target = do
  getAllListeners target >>= traverse_ (flip unregisterListener target)
  pure target

-- | Removes all event handler functions from an event target. The target is
-- | returned.
allOffM ∷ ∀ m t. Bind m ⇒ MonadEffect m ⇒ EventTargetOp t ⇒ m t → m t
allOffM = bindFlipped allOff

-- | Triggers the event handlers tied to the event type to the event target. The
-- | target is returned.
triggerOptions ∷ ∀ t m. EventTargetOp t ⇒ MonadEffect m ⇒ String → EventOptions → t → m t
triggerOptions evtType options target = liftEffect do
  _ ← dispatchEvent (newEvent (EventType evtType) options) (toEventTarget target)
  pure target

-- | Triggers the event handlers tied to the event type to the event target. The
-- | target is returned.
triggerOptionsM ∷ ∀ t m. EventTargetOp t ⇒ MonadEffect m ⇒ String → EventOptions → m t → m t
triggerOptionsM = compose bindFlipped <<< triggerOptions

-- | Triggers the event handlers tied to the event type to the event target. The
-- | target is returned.
trigger ∷ ∀ t m. EventTargetOp t ⇒ MonadEffect m ⇒ String → t → m t
trigger evtType = triggerOptions evtType { bubbles: false, cancelable: false, composed: false }

-- | Triggers the event handlers tied to the event type to the event target. The
-- | target is returned.
triggerM ∷ ∀ t m. EventTargetOp t ⇒ MonadEffect m ⇒ String → m t → m t
triggerM = bindFlipped <<< trigger
