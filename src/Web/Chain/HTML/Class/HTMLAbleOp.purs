module Web.Chain.HTML.Class.HTMLAbleOp
  ( class HTMLAbleOp
  , disable
  , disableM
  , enable
  , enableM
  , isEnabled
  , isEnabledM
  ) where

import Prelude

import Control.Bind (bindFlipped)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Web.HTML.HTMLButtonElement as HBu
import Web.HTML.HTMLFieldSetElement as HFi
import Web.HTML.HTMLInputElement as HIn
import Web.HTML.HTMLOptGroupElement as HOG
import Web.HTML.HTMLOptionElement as HOp
import Web.HTML.HTMLSelectElement as HSe
import Web.HTML.HTMLTextAreaElement as HTA

class HTMLAbleOp el where
  disable ∷ ∀ m. MonadEffect m ⇒ el → m el
  enable ∷ ∀ m. MonadEffect m ⇒ el → m el
  isEnabled ∷ ∀ m. MonadEffect m ⇒ el → m Boolean

a ∷ ∀ b a el m. MonadEffect m ⇒ (b → el → Effect a) → b → el → m el
a f b el = liftEffect (f b el) *> pure el

d ∷ ∀ a el m. MonadEffect m ⇒ (Boolean → el → Effect a) → el → m el
d f = a f true

e ∷ ∀ a el m. MonadEffect m ⇒ (Boolean → el → Effect a) → el → m el
e f = a f false

ie ∷ ∀ m a el. HeytingAlgebra a ⇒ MonadEffect m ⇒ (el → Effect a) → el → m a
ie f = map not <<< liftEffect <<< f

--------------------------------------------------------------------------------
instance HTMLAbleOp HBu.HTMLButtonElement where
  disable = d HBu.setDisabled
  enable = e HBu.setDisabled
  isEnabled = ie HBu.disabled

instance HTMLAbleOp HFi.HTMLFieldSetElement where
  disable = d HFi.setDisabled
  enable = e HFi.setDisabled
  isEnabled = ie HFi.disabled

instance HTMLAbleOp HIn.HTMLInputElement where
  disable = d HIn.setDisabled
  enable = e HIn.setDisabled
  isEnabled = ie HIn.disabled

instance HTMLAbleOp HOG.HTMLOptGroupElement where
  disable = d HOG.setDisabled
  enable = e HOG.setDisabled
  isEnabled = ie HOG.disabled

instance HTMLAbleOp HOp.HTMLOptionElement where
  disable = d HOp.setDisabled
  enable = e HOp.setDisabled
  isEnabled = ie HOp.disabled

instance HTMLAbleOp HSe.HTMLSelectElement where
  disable = d HSe.setDisabled
  enable = e HSe.setDisabled
  isEnabled = ie HSe.disabled

instance HTMLAbleOp HTA.HTMLTextAreaElement where
  disable = d HTA.setDisabled
  enable = e HTA.setDisabled
  isEnabled = ie HTA.disabled

--------------------------------------------------------------------------------
disableM ∷ ∀ m el. HTMLAbleOp el ⇒ MonadEffect m ⇒ m el → m el
disableM = bindFlipped disable

enableM ∷ ∀ m el. HTMLAbleOp el ⇒ MonadEffect m ⇒ m el → m el
enableM = bindFlipped enable

-- | Is this input enabled?
isEnabledM ∷ ∀ m el. MonadEffect m ⇒ HTMLAbleOp el ⇒ m el → m Boolean
isEnabledM = bindFlipped isEnabled
