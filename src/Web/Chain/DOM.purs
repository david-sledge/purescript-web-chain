-- | DOM manipulation tools that can be chained together.

module Web.Chain.DOM
( N
, appendNodes
, attr
, detach
, el
, eln
, empty
, nd
, ndp
, remove
, rmAttr
, setAttrs
, tx
, txn
, (>+)
) where

import Prelude

import Control.Monad.Reader (class MonadAsk, asks)
import Data.Foldable (class Foldable, traverse_)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Class (class MonadEffect, liftEffect)
import Unsafe.Coerce (unsafeCoerce)
import Web.Chain.Class (class IsChildNode, class IsDocument, class IsElement, class IsParentNode, toChildNode, toDocument, toElement, toNode)
import Web.Chain.Event (allOff)
import Web.DOM as D
import Web.DOM.ChildNode as C
import Web.DOM.Document (createElement, createTextNode)
import Web.DOM.Element (getAttribute, removeAttribute, setAttribute)
import Web.DOM.Node (appendChild, firstChild)
import Web.DOM.ParentNode as P

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

-- | Creates a text node from a `String`.
tx ∷ ∀ m d. MonadAsk d m ⇒ MonadEffect m ⇒ IsDocument d ⇒ String → m D.Text
tx string = liftEffect <<< createTextNode string =<< asks toDocument

-- | Calls `tx` and applies the result to `nd`: `nd <<< tx`.
txn ∷ ∀ m d. MonadAsk d m ⇒ MonadEffect m ⇒ IsDocument d ⇒ String → N m
txn = nd <<< tx

-- | Extracts child nodes from a `Foldable` of continuations and appends them to
-- | a parent node. Returns the given parent node.
appendNodes ∷ ∀ m p f. IsParentNode p ⇒ Foldable f ⇒ MonadEffect m ⇒ f (N m) → m p → m p
appendNodes childrenM mParent = do
  parent ← mParent
  let parentNode = toNode parent
  traverse_
    ( \ mChild → do
      child ← runN mChild (map toNode)
      liftEffect $ appendChild child parentNode
    ) childrenM *> pure parent

infix 9 appendNodes as >+

-- | Detatches a node from its parent node and returns the detached node.
-- | Descendant nodes and event listeners are not affected. The detatched node
-- | is returned.
detach ∷ ∀ c m. MonadEffect m ⇒ IsChildNode c ⇒ m c → m c
detach mChildNode = do
  c ← mChildNode
  let childNode = toChildNode c
  liftEffect $ C.remove childNode
  pure c

-- | Calls `detach` on the node and its descendants, and in addition removes all
-- | event listeners from the detached node and descendants. The event listners
-- | are removed provided that they were added using `Web.Chain.Event.on` or
-- | derivation thereof such as `Web.Chain.Event.change` or
-- | `Web.Chain.Event.ready`. The removed node is returned.
remove ∷ ∀ c m. MonadEffect m ⇒ IsChildNode c ⇒ m c → m c
remove mChildNode = do
  childNode <- mChildNode
  _ <- empty $ pure (unsafeCoerce childNode :: P.ParentNode)
  detach <<< allOff $ pure childNode

-- | Calls `remove` on the all of the node's children. The emptied node is
-- | returned.
empty ∷ ∀ m p. MonadEffect m ⇒ IsParentNode p ⇒ m p → m p
empty mParentNode = do
  parentNode ← mParentNode
  let node = toNode parentNode
  mChild ← liftEffect $ firstChild node
  case mChild of
    Just child → (remove $ pure (unsafeCoerce child ∷ C.ChildNode)) *> empty (pure parentNode)
    _ → pure parentNode

-- | Sets the attributes of an element. Existing attributes of the same names
-- | are overwritten. New names create new attributes. The element is returned.
setAttrs ∷ ∀ e f m. Foldable f ⇒ IsElement e ⇒ MonadEffect m ⇒ f (Tuple String String) → m e → m e
setAttrs attributes mElement = do
  element ← mElement
  traverse_
    ( \ (Tuple name value) → liftEffect <<< setAttribute name value $ toElement element
    ) attributes *> pure element

-- | Gets the value of the named attibute.
attr ∷ ∀ m e. MonadEffect m ⇒ IsElement e ⇒ String → m e → m (Maybe String)
attr name element = liftEffect <<< getAttribute name =<< toElement <$> element

-- | Removes an attribute from an element. The element is returned.
rmAttr ∷ ∀ m e. MonadEffect m ⇒ IsElement e ⇒ String → m e → m e
rmAttr name mElement = do
  element ← mElement
  liftEffect <<< removeAttribute name $ toElement element
  pure element

-- | Creates an element, set attributes, and appends child nodes.
el ∷ ∀ m d f1 f2. MonadAsk d m ⇒ MonadEffect m ⇒ IsDocument d ⇒ Foldable f1 ⇒ Foldable f2 ⇒ String → f1 (Tuple String String) → f2 (N m) → m D.Element
el tagName attributes children = do
  elem ← asks $ liftEffect <<< createElement tagName <<< toDocument
  (setAttrs attributes elem) # appendNodes children

-- | Calls `el` and applies the result to `nd`: `nd <<< el tagName attributes`.
eln ∷ ∀ m d f1 f2. MonadAsk d m ⇒ MonadEffect m ⇒ IsDocument d ⇒ Foldable f1 ⇒ Foldable f2 ⇒ String → f1 (Tuple String String) → f2 (N m) → N m
eln tagName attributes = nd <<< el tagName attributes
