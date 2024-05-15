module Web.Chain.HTML.Util
  ( testCoercion
  )
  where

import Prelude

import Data.Maybe (Maybe, maybe)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Exception (error, throwException)

testCoercion ∷ ∀ m el. MonadEffect m ⇒ String → String → Maybe el → m el
testCoercion tag typeName = maybe (liftEffect <<< throwException $ error $ "'Web.Chain.DOM.el \"" <> tag <> "\"' did not produce " <> typeName) pure
