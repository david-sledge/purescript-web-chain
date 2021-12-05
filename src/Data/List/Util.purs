module Data.List.Util
  ( (&:)
  , (++)
  , merge
  , pair
  , s
  )
  where

import Data.List (singleton)
import Data.List.Types (List(..), (:))

s ∷ ∀ a. a → List a
s = singleton

pair ∷ ∀ a. a → a → List a
pair x y = x : s y

infixr 6 pair as &:

merge ∷ ∀ a. List a → List a → List a
merge (Cons x xs) ys = x : (merge xs ys)
merge _ ys = ys

infixr 7 merge as ++
