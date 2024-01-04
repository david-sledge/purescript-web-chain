-- | Type classes of convenience.

module Web.DOM.Class.ElementOp
  ( class ElementOp
  , getAttribute
  , removeAttribute
  , setAttribute
  , toElement
  ) where

import Prelude

import Data.Maybe (Maybe)
import Effect.Class (class MonadEffect, liftEffect)
import Web.DOM as D
import Web.DOM.Class.NodeOp (class NodeOp)
import Web.DOM.Element as E
import Web.HTML.HTMLButtonElement as HB
import Web.HTML.HTMLElement as HE
import Web.HTML.HTMLInputElement as HI

class NodeOp n <= ElementOp n where
  toElement ∷ n → E.Element

instance ElementOp D.Element where
  toElement = identity

instance ElementOp HE.HTMLElement where
  toElement = HE.toElement

instance ElementOp HB.HTMLButtonElement where
  toElement = HB.toElement

instance ElementOp HI.HTMLInputElement where
  toElement = HI.toElement

setAttribute ∷ ∀ m e. MonadEffect m ⇒ ElementOp e ⇒ String → String → e → m Unit
setAttribute name value = liftEffect <<< E.setAttribute name value <<< toElement

getAttribute ∷ ∀ m e. MonadEffect m ⇒ ElementOp e ⇒ String → e → m (Maybe String)
getAttribute name = liftEffect <<< E.getAttribute name <<< toElement

removeAttribute ∷ ∀ m e. MonadEffect m ⇒ ElementOp e ⇒ String → e → m Unit
removeAttribute name = liftEffect <<< E.removeAttribute name <<< toElement
