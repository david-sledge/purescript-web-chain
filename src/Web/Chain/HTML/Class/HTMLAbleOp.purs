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
import Data.Tuple.Nested ((/\))
import Effect.Class (class MonadEffect)
import Web.Chain.DOM (rmAttr, setAttrs)
import Web.HTML.Class.HTMLElementOp (class HTMLElementOp)
import Web.DOM.Class.ElementOp (class ElementOp, hasAttribute)
import Web.HTML (HTMLButtonElement, HTMLFieldSetElement, HTMLInputElement, HTMLOptGroupElement, HTMLOptionElement, HTMLSelectElement, HTMLTextAreaElement)

class HTMLElementOp el ⇐ HTMLAbleOp el where
  disable ∷ ∀ m. MonadEffect m ⇒ el → m el
  enable ∷ ∀ m. MonadEffect m ⇒ el → m el

disable_ ∷ ∀ el m. ElementOp el ⇒ MonadEffect m ⇒ el → m el
disable_ = setAttrs [ "disabled" /\ "" ]

enable_ ∷ ∀ el m. ElementOp el ⇒ MonadEffect m ⇒ el → m el
enable_ = rmAttr "disabled"

--------------------------------------------------------------------------------
instance HTMLAbleOp HTMLButtonElement where
  disable = disable_
  enable = enable_

instance HTMLAbleOp HTMLFieldSetElement where
  disable = disable_
  enable = enable_

instance HTMLAbleOp HTMLInputElement where
  disable = disable_
  enable = enable_

instance HTMLAbleOp HTMLOptGroupElement where
  disable = disable_
  enable = enable_

instance HTMLAbleOp HTMLOptionElement where
  disable = disable_
  enable = enable_

instance HTMLAbleOp HTMLSelectElement where
  disable = disable_
  enable = enable_

instance HTMLAbleOp HTMLTextAreaElement where
  disable = disable_
  enable = enable_

--------------------------------------------------------------------------------
disableM :: forall m el. Bind m ⇒ HTMLAbleOp el ⇒ MonadEffect m ⇒ m el → m el
disableM = bindFlipped disable

enableM :: forall m el. Bind m ⇒ HTMLAbleOp el ⇒ MonadEffect m ⇒ m el → m el
enableM = bindFlipped enable

-- | Is this input enabled?
isEnabled :: forall m el. MonadEffect m ⇒ HTMLAbleOp el ⇒ el → m Boolean
isEnabled = hasAttribute "disabled"

-- | Is this input enabled?
isEnabledM :: forall m el. MonadEffect m ⇒ HTMLAbleOp el ⇒ m el → m Boolean
isEnabledM = bindFlipped $ hasAttribute "disabled"
