-- | Type classes of convenience.

module Web.DOM.Class.ElementOp
  ( class ElementOp
  , classList
  , getAttribute
  , removeAttribute
  , setAttribute
  , toElement
  ) where

import Prelude

import Data.Maybe (Maybe)
import Effect.Class (class MonadEffect, liftEffect)
import Web.DOM (DOMTokenList)
import Web.DOM as D
import Web.DOM.Class.NodeOp (class NodeOp)
import Web.DOM.Element as E
import Web.HTML.HTMLButtonElement as HB
import Web.HTML.HTMLDivElement as HD
import Web.HTML.HTMLElement as HE
import Web.HTML.HTMLInputElement as HI
import Web.HTML.HTMLTableCellElement as HTd
import Web.HTML.HTMLTableRowElement as HTr

class NodeOp n <= ElementOp n where
  toElement ∷ n → E.Element

--------------------------------------------------------------------------------
instance ElementOp D.Element where
  toElement = identity

--------------------------------------------------------------------------------
-- children
instance ElementOp HE.HTMLElement where
  toElement = HE.toElement

--------------------------------------------------------------------------------
-- grandchildren
instance ElementOp HB.HTMLButtonElement where
  toElement = HB.toElement

instance ElementOp HD.HTMLDivElement where
  toElement = HD.toElement

instance ElementOp HI.HTMLInputElement where
  toElement = HI.toElement

instance ElementOp HTd.HTMLTableCellElement where
  toElement = HTd.toElement

instance ElementOp HTr.HTMLTableRowElement where
  toElement = HTr.toElement

setAttribute ∷ ∀ m e. MonadEffect m ⇒ ElementOp e ⇒ String → String → e → m Unit
setAttribute name value = liftEffect <<< E.setAttribute name value <<< toElement

getAttribute ∷ ∀ m e. MonadEffect m ⇒ ElementOp e ⇒ String → e → m (Maybe String)
getAttribute name = liftEffect <<< E.getAttribute name <<< toElement

removeAttribute ∷ ∀ m e. MonadEffect m ⇒ ElementOp e ⇒ String → e → m Unit
removeAttribute name = liftEffect <<< E.removeAttribute name <<< toElement

classList ∷ ∀ m e. MonadEffect m ⇒ ElementOp e ⇒ e → m DOMTokenList
classList = liftEffect <<< E.classList <<< toElement
