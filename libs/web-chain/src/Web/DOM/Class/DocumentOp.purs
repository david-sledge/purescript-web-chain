module Web.DOM.Class.DocumentOp
  ( class DocumentOp
  , createElement
  , createTextNode
  , toDocument
  ) where

import Prelude

import Effect.Class (class MonadEffect, liftEffect)
import Web.DOM (Document, Element, Text)
import Web.DOM.Class.NodeOp (class NodeOp)
import Web.DOM.Document as D
import Web.HTML.HTMLDocument (HTMLDocument)
import Web.HTML.HTMLDocument as HD

class NodeOp d <= DocumentOp d where
  toDocument ∷ d → Document

instance DocumentOp Document where
  toDocument = identity

instance DocumentOp HTMLDocument where
  toDocument = HD.toDocument

createTextNode ∷ ∀ d m. DocumentOp d ⇒ MonadEffect m ⇒ String → d → m Text
createTextNode text = liftEffect <<< D.createTextNode text <<< toDocument

createElement ∷ ∀ m d. MonadEffect m ⇒ DocumentOp d ⇒ String → d → m Element
createElement tagName = liftEffect <<< D.createElement tagName <<< toDocument
