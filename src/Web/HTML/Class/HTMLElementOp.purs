-- | Type classes of convenience.

module Web.HTML.Class.HTMLElementOp
  ( class HTMLElementOp
  , style
  , toHTMLElement
  )
  where

import Prelude

import Effect.Class (class MonadEffect, liftEffect)
import Web.CSSOM.CSSStyleDeclaration (CSSStyleDeclaration)
import Web.CSSOM.ElementCSSInlineStyle as C
import Web.DOM.Class.ElementOp (class ElementOp)
import Web.HTML.HTMLButtonElement as HB
import Web.HTML.HTMLDivElement as HD
import Web.HTML.HTMLElement as HE
import Web.HTML.HTMLInputElement as HI
import Web.HTML.HTMLTableCellElement as HTd
import Web.HTML.HTMLTableRowElement as HTr

class ElementOp n <= HTMLElementOp n where
  toHTMLElement ∷ n → HE.HTMLElement

--------------------------------------------------------------------------------
instance HTMLElementOp HE.HTMLElement where
  toHTMLElement = identity

--------------------------------------------------------------------------------
-- children
instance HTMLElementOp HB.HTMLButtonElement where
  toHTMLElement = HB.toHTMLElement

instance HTMLElementOp HD.HTMLDivElement where
  toHTMLElement = HD.toHTMLElement

instance HTMLElementOp HI.HTMLInputElement where
  toHTMLElement = HI.toHTMLElement

instance HTMLElementOp HTd.HTMLTableCellElement where
  toHTMLElement = HTd.toHTMLElement

instance HTMLElementOp HTr.HTMLTableRowElement where
  toHTMLElement = HTr.toHTMLElement

style ∷ ∀ m n. MonadEffect m ⇒ HTMLElementOp n ⇒ n → m CSSStyleDeclaration
style n = liftEffect <<< C.style <<< C.fromHTMLElement $ toHTMLElement n
