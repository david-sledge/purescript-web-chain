-- | CSSOM manipulation tools that can be chained together.

module Web.Chain.CSSOM
  ( addClasses
  , addClassesM
  , classAttr
  , collapse
  , collapseM
  , conceal
  , concealM
  , hide
  , hideM
  , reveal
  , revealM
  , setCss
  , setCssM
  , setCssProp
  , setCssPropM
  , show
  , showM
  , styleAttr
  )
  where

import Prelude hiding (add, show)

import Control.Bind (bindFlipped)
import Data.Array (snoc)
import Data.Foldable (class Foldable, foldl, intercalate, traverse_)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Web.CSSOM.Lifted.CSSStyleDeclaration (getPropertyValue, setProperty)
import Web.CSSOM.Window (getDefaultComputedStyle)
import Web.DOM.Class.ElementOp (class ElementOp, classList)
import Web.DOM.DOMTokenList (add)
import Web.HTML (window)
import Web.HTML.Class.HTMLElementOp (class HTMLElementOp, style)

foreign import _storeDislayValue ∷ ∀ n. String → n → Effect Unit
foreign import _retrieveDislayValue ∷ ∀ n. n → Effect String

setCssProp ∷ ∀ m n. MonadEffect m ⇒ HTMLElementOp n ⇒ String → String → n → m n
setCssProp name val n = do
  style n >>= setProperty name val
  pure n

setCssPropM ∷ ∀ m n. MonadEffect m ⇒ HTMLElementOp n ⇒ String → String → m n → m n
setCssPropM = compose bindFlipped <<< setCssProp

setCss ∷ ∀ m n f. MonadEffect m ⇒ HTMLElementOp n ⇒ Foldable f ⇒ f (String /\ String) → n → m n
setCss cssProps n = traverse_ (\(name /\ val) -> setCssProp name val n) cssProps *> pure n

setCssM ∷ ∀ m n f. MonadEffect m ⇒ HTMLElementOp n ⇒ Foldable f ⇒ f (String /\ String) → m n → m n
setCssM = bindFlipped <<< setCss

conceal ∷ ∀ m n. MonadEffect m ⇒ HTMLElementOp n ⇒ n → m n
conceal n =
  -- set "visibility" to "hidden"
  setCssProp "visibility" "hidden" n

concealM ∷ ∀ m n. MonadEffect m ⇒ HTMLElementOp n ⇒ m n → m n
concealM = bindFlipped conceal

collapse ∷ ∀ m n. MonadEffect m ⇒ HTMLElementOp n ⇒ n → m n
collapse n =
  -- set "visibility" to "collapse"
  setCssProp "visibility" "collapse" n

collapseM ∷ ∀ m n. MonadEffect m ⇒ HTMLElementOp n ⇒ m n → m n
collapseM = bindFlipped collapse

reveal ∷ ∀ m n. MonadEffect m ⇒ HTMLElementOp n ⇒ n → m n
reveal n =
  -- set "visibility" to "visible"
  setCssProp "visibility" "visible" n

revealM ∷ ∀ m n. MonadEffect m ⇒ HTMLElementOp n ⇒ m n → m n
revealM = bindFlipped reveal

hide ∷ ∀ m n. MonadEffect m ⇒ HTMLElementOp n ⇒ n → m n
hide n = do
  stl ← style n
  -- get the current value of the CSS attribute "display"
  val ← getPropertyValue "display" stl
  when (val /= "none") do
    -- store the value for later
    liftEffect $ _storeDislayValue val n
    -- set "display" to "none"
    setProperty "display" "none" stl
  pure n

hideM ∷ ∀ m n. MonadEffect m ⇒ HTMLElementOp n ⇒ m n → m n
hideM = bindFlipped hide

show ∷ ∀ m n. MonadEffect m ⇒ HTMLElementOp n ⇒ n → m n
show n = do
  stl ← style n
  val ← getPropertyValue "display" stl
  when (val == "none") do
    -- get the stored value of the CSS attribute "display"
    val' ← liftEffect $ _retrieveDislayValue n
    val'' ←
      if (val' == "none" || val' == "") then do
        win ← liftEffect window
        compStl ← getDefaultComputedStyle n Nothing win
        val'' ← getPropertyValue "display" compStl
        pure $ if val'' == "" then "block" else val''
      else pure val'
    -- reapply the value to the attribute "display"
    setProperty "display" val'' stl
  pure n

showM ∷ ∀ m n. MonadEffect m ⇒ HTMLElementOp n ⇒ m n → m n
showM = bindFlipped show

addClasses :: forall m n f. MonadEffect m => ElementOp n => Foldable f => f String -> n -> m n
addClasses classes n = do
  classList n >>= compose liftEffect (flip traverse_ classes) <<< add
  pure n

addClassesM :: forall m n f. MonadEffect m => ElementOp n => Foldable f => f String -> m n -> m n
addClassesM = bindFlipped <<< addClasses

classAttr :: forall f. Foldable f => f String -> Array (String /\ String) -> Array (String /\ String)
classAttr classNames attrs = snoc attrs ("class" /\ intercalate " " classNames)

styleAttr :: forall f. Foldable f => f (String /\ String) -> Array (String /\ String) -> Array (String /\ String)
styleAttr properties attrs = snoc attrs ("style" /\ foldl (\ acc (name /\ val) -> acc <> name <> ":" <> val <> ";") "" properties)
