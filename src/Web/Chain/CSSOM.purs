-- | CSSOM manipulation tools that can be chained together.

module Web.Chain.CSSOM
  ( hide
  , hideM
  , show
  , showM
  )
  where

import Prelude hiding (show)

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Web.CSSOM.Lifted.CSSStyleDeclaration (getPropertyValue, setProperty)
import Web.CSSOM.Window (getDefaultComputedStyle)
import Web.HTML (window)
import Web.HTML.Class.HTMLElementOp (class HTMLElementOp, style)

foreign import _storeDislayValue ∷ ∀ n. String → n → Effect Unit
foreign import _retrieveDislayValue ∷ ∀ n. n → Effect String

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
hideM n = hide =<< n

show ∷ ∀ m n. MonadEffect m ⇒ HTMLElementOp n ⇒ n → m n
show n = do
  stl ← style n
  val ← getPropertyValue "display" stl
  when (val == "none") do
    -- get the stored value of the CSS attribute "display"
    val' ← liftEffect $ _retrieveDislayValue n
    val'' ← if (val' == "none" || val' == "")
      then do
        win ← liftEffect window
        compStl ← getDefaultComputedStyle n Nothing win
        val'' ← getPropertyValue "display" compStl
        pure $ if val'' == "" then "block" else val''
      else pure val'
    -- reapply the value to the attribute "display"
    setProperty "display" val'' stl
  pure n

showM ∷ ∀ m n. MonadEffect m ⇒ HTMLElementOp n ⇒ m n → m n
showM n = show =<< n
