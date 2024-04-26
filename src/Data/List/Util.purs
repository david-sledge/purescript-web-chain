module Data.List.Util
  ( s
  ) where

import Data.List (singleton)
import Data.List.Types (List)

s ∷ ∀ a. a → List a
s = singleton
