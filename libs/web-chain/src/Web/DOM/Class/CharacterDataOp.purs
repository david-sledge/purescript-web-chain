module Web.DOM.Class.CharacterDataOp where

import Prelude

import Web.DOM (CharacterData, Text)
import Web.DOM.Class.NodeOp (class NodeOp)
import Web.DOM.Text as T

class NodeOp c <= CharacterDataOp c where
  toCharacterData ∷ c → CharacterData

instance CharacterDataOp CharacterData where
  toCharacterData = identity

instance CharacterDataOp Text where
  toCharacterData = T.toCharacterData
