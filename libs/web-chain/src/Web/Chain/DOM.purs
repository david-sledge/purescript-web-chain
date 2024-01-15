-- | DOM manipulation tools that can be chained together.

module Web.Chain.DOM
  ( (+<)
  , (+<<)
  , (>+)
  , (>>+)
  , appendNodes
  , appendNodesM
  , appendsNodes
  , appendsNodesM
  , attr
  , attrM
  , detach
  , detachM
  , doc
  , el
  , eln
  , empty
  , emptyM
  , nd
  , ndM
  , remove
  , removeM
  , rmAttr
  , rmAttrM
  , setAttrs
  , setAttrsM
  , tx
  , txn
  ) where

import Prelude

import Data.Foldable (class Foldable, traverse_)
import Data.Maybe (Maybe, maybe)
import Data.Tuple (Tuple(Tuple))
import Effect.Class (class MonadEffect, liftEffect)
import Web.DOM (Element, Node, Text)
import Web.DOM.Class.DocumentOp (createElement, createTextNode)
import Web.DOM.Class.ElementOp (class ElementOp, getAttribute, removeAttribute, setAttribute)
import Web.DOM.Class.NodeOp (class NodeOp, appendChild, firstChild, parentNode, removeChild, toNode)
import Web.Event.Class.EventTargetOp (allOff)
import Web.HTML (HTMLDocument, window)
import Web.HTML.Window (document)

-- | Put an instance of a `NodeOp` in a continuation.
nd ∷ ∀ n m. Functor m => NodeOp n ⇒ m n → m Node
nd mn = toNode <$> mn

ndM ∷ ∀ n m. NodeOp n ⇒ Applicative m ⇒ n → m Node
ndM = nd <<< pure

doc ∷ ∀ m. MonadEffect m ⇒ m HTMLDocument
doc = liftEffect $ document =<< window

-- | Creates a text node from a `String`.
tx ∷ ∀ m. MonadEffect m ⇒ String → m Text
tx string = createTextNode string =<< doc

-- | Calls `tx` and applies the result to `nd`: `nd <<< tx`.
txn ∷ ∀ m. MonadEffect m ⇒ String → m Node
txn = nd <<< tx

-- | Extracts child nodes from a `Foldable` of continuations and appends them to
-- | a parent node. Returns the given parent node.
appendNodes ∷ ∀ m p f. Foldable f ⇒ MonadEffect m ⇒ NodeOp p ⇒ f (m Node) → p → m p
appendNodes childrenM parent = do
  traverse_ ((=<<) (flip appendChild parent)) childrenM
  pure parent

infix 9 appendNodes as >+

appendsNodes ∷ ∀ m p f. NodeOp p ⇒ Foldable f ⇒ MonadEffect m ⇒ p → f (m Node) → m p
appendsNodes = flip (>+)

infix 9 appendsNodes as +<

-- | Extracts child nodes from a `Foldable` of continuations and appends them to
-- | a monadic parent node. Returns the given parent node.
appendNodesM ∷ ∀ m p f. NodeOp p ⇒ Foldable f ⇒ MonadEffect m ⇒ f (m Node) → m p → m p
appendNodesM = (=<<) <<< appendNodes

infix 9 appendNodesM as >>+

appendsNodesM ∷ ∀ m p f. NodeOp p ⇒ Foldable f ⇒ MonadEffect m ⇒ m p → f (m Node) → m p
appendsNodesM = flip (>>+)

infix 9 appendsNodesM as +<<

-- | Detatches a node from its parent node and returns the detached node.
-- | Descendant nodes and event listeners are not affected. The detatched node
-- | is returned.
detach ∷ ∀ c m. MonadEffect m ⇒ NodeOp c ⇒ c → m c
detach c = parentNode c >>= maybe (pure c)
  ( \parent → do
      removeChild c parent
      pure c
  )

-- | Detatches a node from its parent node and returns the detached node.
-- | Descendant nodes and event listeners are not affected. The detatched node
-- | is returned.
detachM ∷ ∀ c m. MonadEffect m ⇒ NodeOp c ⇒ m c → m c
detachM = (=<<) detach

-- | Calls `detach` on the node and its descendants, and in addition removes all
-- | event listeners from the detached node and descendants. The event listners
-- | are removed provided that they were added using `Web.Event.Class.EventTargetOp.on` or
-- | derivation thereof such as `Web.Event.Class.EventTargetOp.change` or
-- | `Web.Event.Class.EventTargetOp.ready`. The removed node is returned.
remove ∷ ∀ c m. MonadEffect m ⇒ NodeOp c ⇒ c → m c
remove childNode = do
  _ ← empty childNode
  detachM $ allOff childNode

-- | Calls `remove` on the all of the node's children. The emptied node is
-- | returned.
empty ∷ ∀ m p. MonadEffect m ⇒ NodeOp p ⇒ p → m p
empty parentNode =
  maybe
    (pure parentNode)
    (\child → remove child *> empty parentNode) =<<
    firstChild parentNode

-- | Calls `detach` on the node and its descendants, and in addition removes all
-- | event listeners from the detached node and descendants. The event listners
-- | are removed provided that they were added using `Web.Event.Class.EventTargetOp.on` or
-- | derivation thereof such as `Web.Event.Class.EventTargetOp.change` or
-- | `Web.Event.Class.EventTargetOp.ready`. The removed node is returned.
removeM ∷ ∀ c m. MonadEffect m ⇒ NodeOp c ⇒ m c → m c
removeM = (=<<) remove

-- | Calls `remove` on the all of the node's children. The emptied node is
-- | returned.
emptyM ∷ ∀ m p. MonadEffect m ⇒ NodeOp p ⇒ m p → m p
emptyM = (=<<) empty

-- | Sets the attributes of an element. Existing attributes of the same names
-- | are overwritten. New names create new attributes. The element is returned.
setAttrs ∷ ∀ e f m. Foldable f ⇒ ElementOp e ⇒ MonadEffect m ⇒ f (Tuple String String) → e → m e
setAttrs attributes element = do
  traverse_
    ( \(Tuple name value) → setAttribute name value element
    )
    attributes
  pure element

-- | Sets the attributes of an element. Existing attributes of the same names
-- | are overwritten. New names create new attributes. The element is returned.
setAttrsM ∷ ∀ e f m. Foldable f ⇒ ElementOp e ⇒ MonadEffect m ⇒ f (Tuple String String) → m e → m e
setAttrsM = (=<<) <<< setAttrs

-- | Gets the value of the named attibute.
attr ∷ ∀ m e. MonadEffect m ⇒ ElementOp e ⇒ String → e → m (Maybe String)
attr = getAttribute

-- | Gets the value of the named attibute.
attrM ∷ ∀ m e. MonadEffect m ⇒ ElementOp e ⇒ String → m e → m (Maybe String)
attrM = (=<<) <<< attr

-- | Removes an attribute from an element. The element is returned.
rmAttr ∷ ∀ m e. MonadEffect m ⇒ ElementOp e ⇒ String → e → m e
rmAttr name element = do
  removeAttribute name element
  pure element

-- | Removes an attribute from an element. The element is returned.
rmAttrM ∷ ∀ m e. MonadEffect m ⇒ ElementOp e ⇒ String → m e → m e
rmAttrM = (=<<) <<< rmAttr

-- | Creates an element, set attributes, and appends child nodes.
el ∷ ∀ m f1 f2. Bind m ⇒ MonadEffect m ⇒ Foldable f1 ⇒ Foldable f2 ⇒ String → f1 (Tuple String String) → f2 (m Node) → m Element
el tagName attributes children = do
  elem ← createElement tagName =<< doc
  (setAttrs attributes elem) # appendNodesM children

-- -- | Calls `el` and applies the result to `nd`: `nd <<< el tagName attributes`.
eln ∷ ∀ m f1 f2. Bind m ⇒ MonadEffect m ⇒ Foldable f1 ⇒ Foldable f2 ⇒ String → f1 (Tuple String String) → f2 (m Node) → m Node
eln = (<<<) ((<<<) nd) <<< el
