module Web.Chain.HTML.Class.HTMLValueContainerOp
  ( class HTMLValueContainerOp
  , setVal
  , setValM
  , val
  , valM
  )
  where

import Prelude

import Control.Bind (bindFlipped)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Web.HTML.HTMLButtonElement as HBu
import Web.HTML.HTMLDataElement as HDE
import Web.HTML.HTMLInputElement as HIn
import Web.HTML.HTMLLIElement as HLI
import Web.HTML.HTMLMeterElement as HMr
import Web.HTML.HTMLOptionElement as HOp
import Web.HTML.HTMLOutputElement as HOu
import Web.HTML.HTMLParamElement as HPm
import Web.HTML.HTMLProgressElement as HPo
import Web.HTML.HTMLSelectElement as HSe
import Web.HTML.HTMLTextAreaElement as HTA

class HTMLValueContainerOp el v where
  val ∷ ∀ m. MonadEffect m ⇒ el → m v
  setVal ∷ ∀ m. MonadEffect m ⇒ v → el → m el

v ∷ ∀ a m b. MonadEffect m ⇒ (a → Effect b) → a → m b
v f el = liftEffect (f el)

sv ∷ ∀ a c b m. Apply m ⇒ MonadEffect m ⇒ (a → b → Effect c) → a → b → m b
sv f str el = liftEffect (f str el) *> pure el

--------------------------------------------------------------------------------
instance HTMLValueContainerOp HBu.HTMLButtonElement String where
  val = v HBu.value
  setVal = sv HBu.setValue

instance HTMLValueContainerOp HDE.HTMLDataElement String where
  val = v HDE.value
  setVal = sv HDE.setValue

instance HTMLValueContainerOp HIn.HTMLInputElement String where
  val = v HIn.value
  setVal = sv HIn.setValue

instance HTMLValueContainerOp HLI.HTMLLIElement Int where
  val = v HLI.value
  setVal = sv HLI.setValue

instance HTMLValueContainerOp HMr.HTMLMeterElement Number where
  val = v HMr.value
  setVal = sv HMr.setValue

instance HTMLValueContainerOp HOp.HTMLOptionElement String where
  val = v HOp.value
  setVal = sv HOp.setValue

instance HTMLValueContainerOp HOu.HTMLOutputElement String where
  val = v HOu.value
  setVal = sv HOu.setValue

instance HTMLValueContainerOp HPm.HTMLParamElement String where
  val = v HPm.value
  setVal = sv HPm.setValue

instance HTMLValueContainerOp HPo.HTMLProgressElement Number where
  val = v HPo.value
  setVal = sv HPo.setValue

instance HTMLValueContainerOp HSe.HTMLSelectElement String where
  val = v HSe.value
  setVal = sv HSe.setValue

instance HTMLValueContainerOp HTA.HTMLTextAreaElement String where
  val = v HTA.value
  setVal = sv HTA.setValue

--------------------------------------------------------------------------------
valM ∷ ∀ m el v. MonadEffect m ⇒ HTMLValueContainerOp el v ⇒ m el → m v
valM = bindFlipped val

setValM ∷ ∀ m el v. MonadEffect m ⇒ HTMLValueContainerOp el v ⇒ v → m el → m el
setValM = bindFlipped <<< setVal
