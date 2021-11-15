module Web.Chain.Event where

import Prelude

import Control.Monad.Reader (class MonadAsk, ask)
import Data.Foldable (traverse_)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Web.Chain.Class (class IsEventTarget, toEventTarget)
import Web.Event.Event (Event, EventType(..))
import Web.Event.EventTarget (EventListener, EventTarget, addEventListener, dispatchEvent, eventListener, removeEventListener)
import Web.HTML.HTMLDocument (HTMLDocument, readyState)
import Web.HTML.HTMLDocument.ReadyState (ReadyState(..))

data ListenerArgs = ListenerArgs
  { listener :: EventListener
  , eventType :: EventType
  , someBooleanArg :: Boolean
  }

foreign import newEvent :: EventType -> Effect Event
foreign import registerListener :: forall a. a -> EventTarget -> Effect Unit
foreign import unregisterListener :: forall a. EventType -> EventListener -> a -> (EventType -> EventListener -> a -> EventTarget -> Effect Unit) -> EventTarget -> Effect Unit
foreign import getAllListeners :: EventTarget -> Effect (Array ((EventType -> EventListener -> Boolean -> EventTarget -> Effect Unit) -> Effect Unit))
foreign import clearRegisteredListeners :: EventTarget -> Effect Unit

on :: forall m et a. MonadEffect m => IsEventTarget et => String -> (Event -> Effect a) -> m et -> m et
on typeStr listener mTarget = do
  obj <- mTarget
  listen <- liftEffect $ eventListener listener
  let target = toEventTarget obj
      applyArgs f = f (EventType typeStr) listen false target
  liftEffect $ registerListener applyArgs target *> applyArgs addEventListener
  pure obj

off :: forall et a m. MonadEffect m => IsEventTarget et => String -> (Event -> Effect a) -> m et -> m et
off typeStr listener mTarget = do
  obj <- mTarget
  listen <- liftEffect $ eventListener listener
  let target = toEventTarget obj
  (liftEffect $ unregisterListener (EventType typeStr) listen false removeEventListener target) *> pure obj

typeOff :: forall et m. MonadEffect m => IsEventTarget et => String -> m et -> m et
typeOff typeStr mTarget = do
  obj <- mTarget
  let eventType = EventType typeStr
      target = toEventTarget obj
  listeners <- liftEffect $ getAllListeners target
  traverse_
    (\ f -> liftEffect $ f
      (\ evtType listener thirdArg _ ->
        if evtType == eventType && thirdArg == false
        then liftEffect $ unregisterListener evtType listener thirdArg removeEventListener target
        else pure unit
      )
    ) listeners
  pure obj

allOff :: forall m et. IsEventTarget et => MonadEffect m => m et -> m et
allOff mTarget = do
  obj <- mTarget
  let target = toEventTarget obj
  listeners <- liftEffect <<< getAllListeners $ toEventTarget obj
  traverse_ (\ f -> liftEffect $ f removeEventListener) listeners
  liftEffect $ clearRegisteredListeners target
  pure obj

onChange :: forall m et a. MonadEffect m => IsEventTarget et => (Event -> Effect a) -> m et -> m et
onChange = on "change"

ready :: forall m a. MonadAsk HTMLDocument m => MonadEffect m => (Event -> Effect a) -> m HTMLDocument
ready f = do
  htmlDoc <- ask
  state <- liftEffect $ readyState htmlDoc
  case state of
    Loading -> on "DOMContentLoaded" f <<< pure $ htmlDoc
    _ -> (liftEffect <<< f =<< (liftEffect $ newEvent (EventType "DOMContentLoaded"))) *> pure htmlDoc

trigger :: forall et m. IsEventTarget et => MonadEffect m => String -> m et -> m et
trigger evtType target = do
  obj <- target
  event <- liftEffect <<< newEvent $ EventType evtType
  (liftEffect $ dispatchEvent event (toEventTarget obj)) *> pure obj

change :: forall et m. IsEventTarget et => MonadEffect m => m et -> m et
change = trigger "change"
