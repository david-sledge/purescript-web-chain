-- | DOM manipulation tools that can be chained together.

module Web.Chain.DOM where

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
import Web.DOM.Element (getAttribute, setAttribute)
import Web.DOM.Node (appendChild, firstChild)

newtype N m =
  N (forall r. (forall n. IsChildNode n => m n -> r) -> r)

nd :: forall n m. IsChildNode n => m n -> N m
nd mn = N \f -> f mn

runN :: forall r m.
  N m ->
  (forall n. IsChildNode n => m n -> r) ->
  r
runN (N f) = f

tx :: forall m d. MonadAsk d m => MonadEffect m => IsDocument d => String -> m D.Text
tx string = liftEffect <<< createTextNode string =<< asks toDocument

appendNodes :: forall m p f. IsParentNode p => Applicative m => Foldable f => MonadEffect m => f (N m) -> m p -> m p
appendNodes childrenM mParent = do
  parent <- mParent
  let parentNode = toNode parent
  traverse_
    ( \ mChild -> do
      child <- runN mChild (map toNode)
      liftEffect $ appendChild child parentNode
    ) childrenM *> pure parent

infix 9 appendNodes as >+

detach :: forall c m. MonadEffect m => IsChildNode c => m c -> m c
detach mChildNode = do
  c <- mChildNode
  let childNode = toChildNode c
  liftEffect $ C.remove childNode
  pure c

remove :: forall c m. MonadEffect m => IsChildNode c => m c -> m c
remove = detach <<< allOff

empty :: forall m p. MonadEffect m => IsParentNode p => m p -> m p
empty mParentNode = do
  parentNode <- mParentNode
  let node = toNode parentNode
  mChild <- liftEffect $ firstChild node
  case mChild of
    Just child -> (remove $ pure (unsafeCoerce child :: C.ChildNode)) *> empty (pure parentNode)
    _ -> pure parentNode

setAttrs :: forall e f m. Foldable f => IsElement e => MonadEffect m => f (Tuple String String) -> m e -> m e
setAttrs attributes mElement = do
  element <- mElement
  traverse_
    ( \ (Tuple name value) -> liftEffect <<< setAttribute name value $ toElement element
    ) attributes *> pure element

attr :: forall m e. MonadEffect m => IsElement e => String -> m e -> m (Maybe String)
attr name element = liftEffect <<< getAttribute name =<< toElement <$> element

el :: forall m d f1 f2. MonadAsk d m => MonadEffect m => IsDocument d => Foldable f1 => Foldable f2 => String -> f1 (Tuple String String) -> f2 (N m) -> m D.Element
el tagName attributes children = do
  elem <- asks $ liftEffect <<< createElement tagName <<< toDocument
  (setAttrs attributes elem) # appendNodes children
