module Web.DOM.Class.CharacterDataOp
  ( appendData
  , class CharacterDataOp
  , data_
  , deleteData
  , insertData
  , length
  , replaceData
  , substringData
  , toCharacterData
  , toChildNode
  , toNonDocumentTypeChildNode
  )
  where

import Prelude

import Effect.Class (class MonadEffect, liftEffect)
import Web.DOM (CharacterData, ChildNode, NonDocumentTypeChildNode, Text)
import Web.DOM.CharacterData as C
import Web.DOM.Class.NodeOp (class NodeOp)
import Web.DOM.Comment as Co
import Web.DOM.ProcessingInstruction as P
import Web.DOM.Text as T

class NodeOp c ⇐ CharacterDataOp c where
  toCharacterData ∷ c → CharacterData

--------------------------------------------------------------------------------
instance CharacterDataOp CharacterData where
  toCharacterData = identity

--------------------------------------------------------------------------------
-- children
instance CharacterDataOp P.ProcessingInstruction where
  toCharacterData = P.toCharacterData

instance CharacterDataOp Co.Comment where
  toCharacterData = Co.toCharacterData

instance CharacterDataOp Text where
  toCharacterData = T.toCharacterData

toChildNode :: forall c. CharacterDataOp c => c -> ChildNode
toChildNode = C.toChildNode <<< toCharacterData

toNonDocumentTypeChildNode :: forall c. CharacterDataOp c => c -> NonDocumentTypeChildNode
toNonDocumentTypeChildNode = C.toNonDocumentTypeChildNode <<< toCharacterData

data_ :: forall m c. MonadEffect m => CharacterDataOp c => c -> m String
data_ = liftEffect <<< C.data_ <<< toCharacterData

length :: forall m c. MonadEffect m => CharacterDataOp c => c -> m Int
length = liftEffect <<< C.length <<< toCharacterData

substringData :: forall m c. MonadEffect m => CharacterDataOp c => Int -> Int -> c -> m String
substringData pos len = liftEffect <<< C.substringData pos len <<< toCharacterData

appendData :: forall m c. MonadEffect m => CharacterDataOp c => String -> c -> m Unit
appendData str = liftEffect <<< C.appendData str <<< toCharacterData

insertData :: forall m c. MonadEffect m => CharacterDataOp c => Int -> String -> c -> m Unit
insertData n str = liftEffect <<< C.insertData n str <<< toCharacterData

deleteData :: forall m c. MonadEffect m => CharacterDataOp c => Int -> Int -> c -> m Unit
deleteData pos len = liftEffect <<< C.deleteData pos len <<< toCharacterData

replaceData :: forall m c. MonadEffect m => CharacterDataOp c => Int -> Int -> String -> c -> m Unit
replaceData pos len str = liftEffect <<< C.replaceData pos len str <<< toCharacterData
