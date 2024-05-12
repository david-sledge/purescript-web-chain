module Web.HTML.Class.HTMLAbleOp
  ( class HTMLAbleOp
  , disable
  , disableM
  , enable
  )
  where

import Prelude

import Control.Bind (bindFlipped)
import Data.Tuple.Nested (type (/\), (/\))
import Effect.Class (class MonadEffect)
import Web.Chain.DOM (rmAttr, setAttrs)
import Web.DOM.Class.ElementOp (class ElementOp)
import Web.HTML (HTMLButtonElement, HTMLFieldSetElement, HTMLInputElement, HTMLOptGroupElement, HTMLOptionElement, HTMLSelectElement, HTMLTextAreaElement)

class HTMLAbleOp el where
  disable :: forall m. MonadEffect m => el -> m el
  enable :: forall m. MonadEffect m => el -> m el

disable_ :: forall el m. ElementOp el => MonadEffect m => el -> m el
disable_ = setAttrs [ "disabled" /\ "" ]

enable_ :: forall el m. ElementOp el => MonadEffect m => el -> m el
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
disableM = bindFlipped disable

enableM = bindFlipped enable
