module Web.CSSOM.Lifted.CSSStyleDeclaration
  ( length
  , getPropertyValue
  , removeProperty
  , setProperty
  ) where

import Prelude

import Data.Int (fromNumber)
import Data.Maybe (fromMaybe)
import Effect.Class (class MonadEffect, liftEffect)
import Web.CSSOM.CSSStyleDeclaration as C

length ∷ ∀ m. MonadEffect m ⇒ C.CSSStyleDeclaration → m Int
length = map (fromMaybe 0 <<< fromNumber) <<< liftEffect <<< C.length

getPropertyValue ∷ ∀ m. MonadEffect m ⇒ String → C.CSSStyleDeclaration → m String
getPropertyValue = (compose liftEffect) <<< C.getPropertyValue

removeProperty ∷ ∀ m. MonadEffect m ⇒ String → C.CSSStyleDeclaration → m Unit
removeProperty = (compose liftEffect) <<< C.removeProperty

setProperty ∷ ∀ m. MonadEffect m ⇒ String → String → C.CSSStyleDeclaration → m Unit
setProperty = (compose (compose liftEffect)) <<< C.setProperty
