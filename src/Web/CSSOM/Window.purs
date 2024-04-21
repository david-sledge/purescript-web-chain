module Web.CSSOM.Window
  ( getComputedStyle
  , getDefaultComputedStyle
  )
  where

import Prelude

import Data.Maybe (Maybe, maybe)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Web.CSSOM.CSSStyleDeclaration (CSSStyleDeclaration)
import Web.DOM.Class.ElementOp (class ElementOp, toElement)
import Web.DOM.Element (Element)
import Web.HTML.Window (Window)

foreign import _getComputedStyleNothing ∷ Element → Window → Effect CSSStyleDeclaration
foreign import _getComputedStyleJust ∷ Element → String → Window → Effect CSSStyleDeclaration

getComputedStyle ∷ ∀ n m. ElementOp n ⇒ MonadEffect m ⇒ n → Maybe String → Window → m CSSStyleDeclaration
getComputedStyle n mS w =
  let element = toElement n in
  liftEffect $ maybe (_getComputedStyleNothing element) (_getComputedStyleJust element) mS w

foreign import _getDefaultComputedStyleNothing ∷ Element → Window → Effect CSSStyleDeclaration
foreign import _getDefaultComputedStyleJust ∷ Element → String → Window → Effect CSSStyleDeclaration

getDefaultComputedStyle ∷ ∀ n m. ElementOp n ⇒ MonadEffect m ⇒ n → Maybe String → Window → m CSSStyleDeclaration
getDefaultComputedStyle n mS w =
  let element = toElement n in
  liftEffect $ maybe (_getDefaultComputedStyleNothing element) (_getDefaultComputedStyleJust element) mS w
