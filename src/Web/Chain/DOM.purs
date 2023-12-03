-- | DOM manipulation tools that can be chained together.

module Web.Chain.DOM
  ( (+<)
  , (+<<)
  , (>+)
  , (>>+)
  , N
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
  , ndp
  , remove
  , removeM
  , rmAttr
  , rmAttrM
  , setAttrs
  , setAttrsM
  , tx
  , txn
  )
  where

import Prelude

import Data.Foldable (class Foldable, traverse_)
import Data.Maybe (Maybe, maybe)
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect, liftEffect)
import Unsafe.Coerce (unsafeCoerce)
import Web.Chain.Class (class IsChildNode, class IsElement, class IsParentNode, createElement, createTextNode, toChildNode, toElement, toNode)
import Web.Chain.Event (allOff)
import Web.DOM (Element)
import Web.DOM as D
import Web.DOM.ChildNode as C
import Web.DOM.Element (getAttribute, removeAttribute, setAttribute)
import Web.DOM.Node (appendChild, firstChild)
import Web.DOM.ParentNode as P
import Web.HTML (HTMLDocument, window)
import Web.HTML.Window (document)

-- | Heterogeneous continuation element type for child nodes.
-- |
-- | This type is used to pass a `Foldable` of heterogeneous data types where
-- | all types are instances of `IsChildNode` typically for the purpose of
-- | appending to an instance of `IsParentNode`.
newtype N m =
  N (∀ r. (∀ n. IsChildNode n ⇒ m n → r) → r)

-- | Put an instance of a `IsChildNode` in a continuation.
nd ∷ ∀ n m. IsChildNode n ⇒ m n → N m
nd mn = N \f → f mn

ndp ∷ ∀ n m. IsChildNode n ⇒ Applicative m ⇒ n → N m
ndp = nd <<< pure

-- | Applies a function to a continuation and returns the result.
runN ∷ ∀ r m.
  N m →
  (∀ n. IsChildNode n ⇒ m n → r) →
  r
runN (N f) = f

doc ∷ ∀ m. MonadEffect m ⇒ m HTMLDocument
doc = liftEffect $ document =<< window

-- | Creates a text node from a `String`.
tx ∷ ∀ m. MonadEffect m ⇒ String → m D.Text
tx string = liftEffect $ createTextNode string =<< doc

-- | Calls `tx` and applies the result to `nd`: `nd <<< tx`.
txn ∷ ∀ m. MonadEffect m ⇒ String → N m
txn = nd <<< tx

-- | Extracts child nodes from a `Foldable` of continuations and appends them to
-- | a parent node. Returns the given parent node.
appendNodes ∷ ∀ m p f. IsParentNode p ⇒ Foldable f ⇒ MonadEffect m ⇒ f (N m) → p → m p
appendNodes childrenM parent = do
  let parentNode = toNode parent
  traverse_
    ( \ mChild → do
      child ← runN mChild (map toNode)
      liftEffect $ appendChild child parentNode
    ) childrenM
  pure parent

infix 9 appendNodes as >+

appendsNodes ∷ ∀ m p f. IsParentNode p ⇒ Foldable f ⇒ MonadEffect m ⇒ p → f (N m) → m p
appendsNodes = flip (>+)

infix 9 appendsNodes as +<

-- | Extracts child nodes from a `Foldable` of continuations and appends them to
-- | a monadic parent node. Returns the given parent node.
appendNodesM ∷ ∀ m p f. IsParentNode p ⇒ Foldable f ⇒ MonadEffect m ⇒ f (N m) → m p → m p
appendNodesM = (=<<) <<< appendNodes

infix 9 appendNodesM as >>+

appendsNodesM ∷ ∀ m p f. IsParentNode p ⇒ Foldable f ⇒ MonadEffect m ⇒ m p → f (N m) → m p
appendsNodesM = flip (>>+)

infix 9 appendsNodesM as +<<

-- | Detatches a node from its parent node and returns the detached node.
-- | Descendant nodes and event listeners are not affected. The detatched node
-- | is returned.
detach ∷ ∀ c m. MonadEffect m ⇒ IsChildNode c ⇒ c → m c
detach c = do
  liftEffect <<< C.remove $ toChildNode c
  pure c

-- | Detatches a node from its parent node and returns the detached node.
-- | Descendant nodes and event listeners are not affected. The detatched node
-- | is returned.
detachM ∷ ∀ c m. MonadEffect m ⇒ IsChildNode c ⇒ m c → m c
detachM = (=<<) detach

-- | Calls `detach` on the node and its descendants, and in addition removes all
-- | event listeners from the detached node and descendants. The event listners
-- | are removed provided that they were added using `Web.Chain.Event.on` or
-- | derivation thereof such as `Web.Chain.Event.change` or
-- | `Web.Chain.Event.ready`. The removed node is returned.
remove ∷ ∀ c m. MonadEffect m ⇒ IsChildNode c ⇒ c → m c
remove childNode = do
  _ <- empty (unsafeCoerce childNode :: P.ParentNode)
  detachM $ allOff childNode

-- | Calls `remove` on the all of the node's children. The emptied node is
-- | returned.
empty ∷ ∀ m p. MonadEffect m ⇒ IsParentNode p ⇒ p → m p
empty parentNode = maybe
    (pure parentNode)
    (\ child → (remove (unsafeCoerce child ∷ C.ChildNode)) *> empty parentNode) =<<
    liftEffect (firstChild $ toNode parentNode)

-- | Calls `detach` on the node and its descendants, and in addition removes all
-- | event listeners from the detached node and descendants. The event listners
-- | are removed provided that they were added using `Web.Chain.Event.on` or
-- | derivation thereof such as `Web.Chain.Event.change` or
-- | `Web.Chain.Event.ready`. The removed node is returned.
removeM ∷ ∀ c m. MonadEffect m ⇒ IsChildNode c ⇒ m c → m c
removeM = (=<<) remove

-- | Calls `remove` on the all of the node's children. The emptied node is
-- | returned.
emptyM ∷ ∀ m p. MonadEffect m ⇒ IsParentNode p ⇒ m p → m p
emptyM = (=<<) empty

-- | Sets the attributes of an element. Existing attributes of the same names
-- | are overwritten. New names create new attributes. The element is returned.
setAttrs ∷ ∀ e f m. Foldable f ⇒ IsElement e ⇒ MonadEffect m ⇒ f (Tuple String String) → e → m e
setAttrs attributes element = do
  traverse_
    ( \ (Tuple name value) → liftEffect <<< setAttribute name value $ toElement element
    ) attributes
  pure element

-- | Sets the attributes of an element. Existing attributes of the same names
-- | are overwritten. New names create new attributes. The element is returned.
setAttrsM ∷ ∀ e f m. Foldable f ⇒ IsElement e ⇒ MonadEffect m ⇒ f (Tuple String String) → m e → m e
setAttrsM = (=<<) <<< setAttrs

-- | Gets the value of the named attibute.
attr ∷ ∀ m e. MonadEffect m ⇒ IsElement e ⇒ String → e → m (Maybe String)
attr name element = liftEffect <<< getAttribute name $ toElement element

-- | Gets the value of the named attibute.
attrM ∷ ∀ m e. MonadEffect m ⇒ IsElement e ⇒ String → m e → m (Maybe String)
attrM = (=<<) <<< attr

-- | Removes an attribute from an element. The element is returned.
rmAttr ∷ ∀ m e. MonadEffect m ⇒ IsElement e ⇒ String → e → m e
rmAttr name element = do
  liftEffect <<< removeAttribute name $ toElement element
  pure element

-- | Removes an attribute from an element. The element is returned.
rmAttrM ∷ ∀ m e. MonadEffect m ⇒ IsElement e ⇒ String → m e → m e
rmAttrM = (=<<) <<< rmAttr

-- | Creates an element, set attributes, and appends child nodes.
el ∷ ∀ m f1 f2. Bind m ⇒ MonadEffect m ⇒ Foldable f1 ⇒ Foldable f2 ⇒ String → f1 (Tuple String String) → f2 (N m) → m Element
el tagName attributes children = do
  elem <- liftEffect $ createElement tagName =<< doc
  (setAttrs attributes elem) # appendNodesM children

-- -- | Calls `el` and applies the result to `nd`: `nd <<< el tagName attributes`.
-- eln ∷ ∀ m d f1 f2. MonadAsk d m ⇒ MonadEffect m ⇒ IsDocument d ⇒ Foldable f1 ⇒ Foldable f2 ⇒ String → f1 (Tuple String String) → f2 (N m) → N m
eln ∷ ∀ m f1 f2. Bind m ⇒ MonadEffect m ⇒ Foldable f1 ⇒ Foldable f2 ⇒ String → f1 (Tuple String String) → f2 (N m) → N m
eln = (<<<) ((<<<) nd) <<< el
