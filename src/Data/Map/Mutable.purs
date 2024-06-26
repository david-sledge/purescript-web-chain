module Data.Map.Mutable
  ( MMap
  , clear
  , delete
  , freeze
  , insert
  , lookup
  , new
  , size
  ) where

import Prelude

import Data.HashMap (HashMap, empty)
import Data.HashMap as M
import Data.Hashable (class Hashable)
import Data.Maybe (Maybe(Just, Nothing))
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)

foreign import data MMap ∷ Type → Type → Type
foreign import _new ∷ ∀ k v. Effect (MMap k v)
foreign import _insert ∷ ∀ k v. Maybe v → (v → Maybe v) → k → v → MMap k v → Effect (Maybe v)
foreign import _freeze ∷ ∀ k v. MMap k v → (k → v → HashMap k v → HashMap k v) → HashMap k v → Effect (HashMap k v)
foreign import _lookup ∷ ∀ k v. Maybe v → (v → Maybe v) → k → MMap k v → Effect (Maybe v)
foreign import _delete ∷ ∀ k v. Maybe v → (v → Maybe v) → k → MMap k v → Effect (Maybe v)
foreign import _size ∷ ∀ k v. MMap k v → Effect Int
foreign import _clear ∷ ∀ k v. MMap k v → Effect Unit

new ∷ ∀ m k v. MonadEffect m ⇒ m (MMap k v)
new = liftEffect _new

insert ∷ ∀ m k v. MonadEffect m ⇒ k → v → MMap k v → m (Maybe v)
insert = compose (compose liftEffect) <<< _insert Nothing Just

freeze ∷ ∀ m k v. MonadEffect m ⇒ Hashable k ⇒ MMap k v → m (HashMap k v)
freeze = liftEffect <<< flip (flip _freeze M.insert) empty

lookup ∷ ∀ m k v. MonadEffect m ⇒ k → MMap k v → m (Maybe v)
lookup = compose liftEffect <<< _lookup Nothing Just

delete ∷ ∀ m k v. MonadEffect m ⇒ k → MMap k v → m (Maybe v)
delete = compose liftEffect <<< _delete Nothing Just

size ∷ ∀ m k v. MonadEffect m ⇒ MMap k v → m Int
size = liftEffect <<< _size

clear ∷ ∀ m k v. MonadEffect m ⇒ MMap k v → m Unit
clear = liftEffect <<< _clear
