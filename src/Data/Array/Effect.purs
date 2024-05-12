module Data.Array.Effect
  ( EffectArray
  , freeze
  , new
  , push
  ) where

import Prelude

import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)

foreign import data EffectArray ∷ Type → Type
foreign import _new ∷ ∀ a. Effect (EffectArray a)
foreign import _push ∷ ∀ a. a → EffectArray a → Effect Unit
foreign import _freeze ∷ ∀ a. EffectArray a → Effect (Array a)

new ∷ ∀ m a. MonadEffect m ⇒ m (EffectArray a)
new = liftEffect _new

push ∷ ∀ m a. MonadEffect m ⇒ a → EffectArray a → m Unit
push = compose liftEffect <<< _push

freeze ∷ ∀ m a. MonadEffect m ⇒ EffectArray a → m (Array a)
freeze = liftEffect <<< _freeze
