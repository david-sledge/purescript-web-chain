-- | Type classes of convenience.

module Web.DOM.Class.NodeOp
  ( appendChild
  , baseURI
  , childNodes
  , class NodeOp
  , clone
  , contains
  , deepClone
  , firstChild
  , hasChildNodes
  , isEqualNode
  , lastChild
  , nextSibling
  , nodeName
  , nodeType
  , nodeTypeIndex
  , nodeValue
  , normalize
  , ownerDocument
  , parentElement
  , parentNode
  , previousSibling
  , removeChild
  , setNodeValue
  , setTextContent
  , textContent
  , toNode
  ) where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Web.DOM (CharacterData, Document, Element, Node, NodeList, NodeType, Text)
import Web.DOM.CharacterData as C
import Web.DOM.Document as D
import Web.DOM.Element as E
import Web.DOM.Node as N
import Web.DOM.Text as T
import Web.Event.Class.EventTargetOp (class EventTargetOp)
import Web.HTML.HTMLButtonElement (HTMLButtonElement)
import Web.HTML.HTMLButtonElement as HB
import Web.HTML.HTMLDivElement (HTMLDivElement)
import Web.HTML.HTMLDivElement as HDv
import Web.HTML.HTMLDocument (HTMLDocument)
import Web.HTML.HTMLDocument as HD
import Web.HTML.HTMLElement (HTMLElement)
import Web.HTML.HTMLElement as HE
import Web.HTML.HTMLInputElement (HTMLInputElement)
import Web.HTML.HTMLInputElement as HI
import Web.HTML.HTMLTableCellElement as HTd
import Web.HTML.HTMLTableRowElement as HTr

class EventTargetOp n <= NodeOp n where
  toNode ∷ n → Node

--------------------------------------------------------------------------------
instance NodeOp Node where
  toNode = identity

--------------------------------------------------------------------------------
-- children
instance NodeOp CharacterData where
  toNode = C.toNode

instance NodeOp Document where
  toNode = D.toNode

instance NodeOp Element where
  toNode = E.toNode

--------------------------------------------------------------------------------
-- grandchildren
instance NodeOp HTMLElement where
  toNode = HE.toNode

instance NodeOp HTMLDocument where
  toNode = HD.toNode

instance NodeOp Text where
  toNode = T.toNode

--------------------------------------------------------------------------------
-- great-grandchildren
instance NodeOp HTMLButtonElement where
  toNode = HB.toNode

instance NodeOp HTMLDivElement where
  toNode = HDv.toNode

instance NodeOp HTMLInputElement where
  toNode = HI.toNode

instance NodeOp HTd.HTMLTableCellElement where
  toNode = HTd.toNode

instance NodeOp HTr.HTMLTableRowElement where
  toNode = HTr.toNode

meh ∷ ∀ n a. NodeOp n ⇒ (Node → a) → n → a
meh n = n <<< toNode

nodeType ∷ ∀ n. NodeOp n ⇒ Partial ⇒ n → NodeType
nodeType = meh N.nodeType

nodeTypeIndex ∷ ∀ n. NodeOp n ⇒ n → Int
nodeTypeIndex = meh N.nodeTypeIndex

nodeName ∷ ∀ n. NodeOp n ⇒ n → String
nodeName = meh N.nodeName

baseURI ∷ ∀ n. NodeOp n ⇒ n → Effect String
baseURI = meh N.baseURI

mehM ∷ ∀ (m ∷ Type → Type) (a ∷ Type) (n ∷ Type). MonadEffect m ⇒ NodeOp n ⇒ (Node → Effect a) → n → m a
mehM = compose liftEffect <<< meh

ownerDocument ∷ ∀ m n. MonadEffect m ⇒ NodeOp n ⇒ n → m (Maybe Document)
ownerDocument = mehM N.ownerDocument

parentNode ∷ ∀ m n. MonadEffect m ⇒ NodeOp n ⇒ n → m (Maybe Node)
parentNode = mehM N.parentNode

parentElement ∷ ∀ m n. MonadEffect m ⇒ NodeOp n ⇒ n → m (Maybe Element)
parentElement = mehM N.parentElement

hasChildNodes ∷ ∀ m n. MonadEffect m ⇒ NodeOp n ⇒ n → m Boolean
hasChildNodes = mehM N.hasChildNodes

childNodes ∷ ∀ m n. MonadEffect m ⇒ NodeOp n ⇒ n → m NodeList
childNodes = mehM N.childNodes

firstChild ∷ ∀ m n. MonadEffect m ⇒ NodeOp n ⇒ n → m (Maybe Node)
firstChild = mehM N.firstChild

lastChild ∷ ∀ m n. MonadEffect m ⇒ NodeOp n ⇒ n → m (Maybe Node)
lastChild = mehM N.lastChild

previousSibling ∷ ∀ m n. MonadEffect m ⇒ NodeOp n ⇒ n → m (Maybe Node)
previousSibling = mehM N.previousSibling

nextSibling ∷ ∀ m n. MonadEffect m ⇒ NodeOp n ⇒ n → m (Maybe Node)
nextSibling = mehM N.nextSibling

nodeValue ∷ ∀ m n. MonadEffect m ⇒ NodeOp n ⇒ n → m (Maybe String)
nodeValue = mehM N.nodeValue

mehM1 ∷ ∀ b m a n. MonadEffect m ⇒ NodeOp n ⇒ (b → Node → Effect a) → b → n → m a
mehM1 = compose mehM

setNodeValue ∷ ∀ m n. MonadEffect m ⇒ NodeOp n ⇒ String → n → m Unit
setNodeValue = mehM1 N.setNodeValue

textContent ∷ ∀ m n. MonadEffect m ⇒ NodeOp n ⇒ n → m String
textContent = mehM N.textContent

setTextContent ∷ ∀ m n. MonadEffect m ⇒ NodeOp n ⇒ String → n → m Unit
setTextContent = mehM1 N.setTextContent

normalize ∷ ∀ m n. MonadEffect m ⇒ NodeOp n ⇒ n → m Unit
normalize = mehM N.normalize

clone ∷ ∀ m n. MonadEffect m ⇒ NodeOp n ⇒ n → m Node
clone = mehM N.clone

deepClone ∷ ∀ m n. MonadEffect m ⇒ NodeOp n ⇒ n → m Node
deepClone = mehM N.deepClone

isEqualNode ∷ ∀ m n1 n2. MonadEffect m ⇒ NodeOp n1 ⇒ NodeOp n2 ⇒ n1 → n2 → m Boolean
isEqualNode = mehM <<< N.isEqualNode <<< toNode

appendChild ∷ ∀ m p c. MonadEffect m ⇒ NodeOp c ⇒ NodeOp p ⇒ c → p → m Unit
appendChild = mehM <<< N.appendChild <<< toNode

removeChild ∷ ∀ m p c. MonadEffect m ⇒ NodeOp c ⇒ NodeOp p ⇒ c → p → m Unit
removeChild = mehM <<< N.removeChild <<< toNode

contains ∷ ∀ m p c. MonadEffect m ⇒ NodeOp c ⇒ NodeOp p ⇒ c → p → m Boolean
contains = mehM <<< N.contains <<< toNode
