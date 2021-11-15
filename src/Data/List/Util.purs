module Data.List.Util where

import Data.List (singleton)
import Data.List.Types (List(..), (:))

s :: forall a. a -> List a
s = singleton

pair :: forall a. a -> a -> List a
pair x y = x : s y

infixr 6 pair as &:

merge :: forall a. List a -> List a -> List a
merge (Cons x xs) ys = x : (merge xs ys)
merge _ ys = ys

infixr 7 merge as ++
