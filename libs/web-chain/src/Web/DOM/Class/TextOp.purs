module Web.DOM.Class.TextOp
  ( class TextOp
  , toText
  ) where

import Prelude

import Web.DOM (Text)
import Web.DOM.Class.CharacterDataOp (class CharacterDataOp)

class CharacterDataOp t <= TextOp t where
  toText ∷ t → Text

instance TextOp Text where
  toText = identity
