module Data.Array.Mutable
  ( MArray
  , freeze
  , new
  , push
  ) where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)

foreign import data MArray ∷ Type → Type
foreign import _new ∷ ∀ a. Effect (MArray a)
foreign import _push ∷ ∀ a. a → MArray a → Effect Unit
foreign import _freeze ∷ ∀ a. MArray a → Effect (Array a)

new ∷ ∀ m a. MonadEffect m ⇒ m (MArray a)
new = liftEffect _new

push ∷ ∀ m a. MonadEffect m ⇒ a → MArray a → m Unit
push = (<<<) liftEffect <<< _push

freeze ∷ ∀ m a. MonadEffect m ⇒ MArray a → m (Array a)
freeze = liftEffect <<< _freeze
