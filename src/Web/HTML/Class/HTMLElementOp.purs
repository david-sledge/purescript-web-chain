-- | Type classes of convenience.

module Web.HTML.Class.HTMLElementOp
  ( class HTMLElementOp
  , toHTMLElement
  ) where

import Prelude

import Web.DOM.Class.ElementOp (class ElementOp)
import Web.HTML.HTMLButtonElement as HB
import Web.HTML.HTMLElement as HE
import Web.HTML.HTMLInputElement as HI

--------------------------------------------------------------------------------
class ElementOp n <= HTMLElementOp n where
  toHTMLElement ∷ n → HE.HTMLElement

--------------------------------------------------------------------------------
instance HTMLElementOp HE.HTMLElement where
  toHTMLElement = identity

instance HTMLElementOp HB.HTMLButtonElement where
  toHTMLElement = HB.toHTMLElement

instance HTMLElementOp HI.HTMLInputElement where
  toHTMLElement = HI.toHTMLElement
