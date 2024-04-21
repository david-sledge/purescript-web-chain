module Web.HTML.HTMLDialogElement
  ( HTMLDialogElement
  , close
  , fromChildNode
  , fromElement
  , fromEventTarget
  , fromHTMLElement
  , fromNode
  , fromNonDocumentTypeChildNode
  , fromParentNode
  , open
  , returnValue
  , setOpen
  , setReturnValue
  , show
  , showModal
  , toHTMLElement
  , toNonDocumentTypeChildNode
  ) where

import Prelude

import Data.Maybe (Maybe(Just, Nothing))
import Effect (Effect)
import Unsafe.Coerce (unsafeCoerce)
import Web.DOM (ChildNode, Element, Node, NonDocumentTypeChildNode, ParentNode)
import Web.DOM.Class.ElementOp (class ElementOp)
import Web.DOM.Class.NodeOp (class NodeOp)
import Web.Event.Class.EventTargetOp (class EventTargetOp)
import Web.Event.EventTarget (EventTarget)
import Web.HTML.HTMLElement (HTMLElement)
import Web.Internal.FFI (unsafeReadProtoTagged)

foreign import data HTMLDialogElement ∷ Type

fromHTMLElement ∷ HTMLElement → Maybe HTMLDialogElement
fromHTMLElement = unsafeReadProtoTagged "HTMLDialogElement"

fromElement ∷ Element → Maybe HTMLDialogElement
fromElement = unsafeReadProtoTagged "HTMLDialogElement"

fromNode ∷ Node → Maybe HTMLDialogElement
fromNode = unsafeReadProtoTagged "HTMLDialogElement"

fromChildNode ∷ ChildNode → Maybe HTMLDialogElement
fromChildNode = unsafeReadProtoTagged "HTMLDialogElement"

fromNonDocumentTypeChildNode ∷ NonDocumentTypeChildNode → Maybe HTMLDialogElement
fromNonDocumentTypeChildNode = unsafeReadProtoTagged "HTMLDialogElement"

fromParentNode ∷ ParentNode → Maybe HTMLDialogElement
fromParentNode = unsafeReadProtoTagged "HTMLDialogElement"

fromEventTarget ∷ EventTarget → Maybe HTMLDialogElement
fromEventTarget = unsafeReadProtoTagged "HTMLDialogElement"

toHTMLElement ∷ HTMLDialogElement → HTMLElement
toHTMLElement = unsafeCoerce

toNonDocumentTypeChildNode ∷ HTMLDialogElement → NonDocumentTypeChildNode
toNonDocumentTypeChildNode = unsafeCoerce

foreign import open ∷ HTMLDialogElement → Effect Boolean

foreign import setOpen ∷ Boolean → HTMLDialogElement → Effect Unit

foreign import returnValue ∷ HTMLDialogElement → Effect String

foreign import setReturnValue ∷ String → HTMLDialogElement → Effect Unit

foreign import _closeNothing ∷ HTMLDialogElement → Effect Unit
foreign import _closeJust ∷ String → HTMLDialogElement → Effect Unit

close ∷ Maybe String → HTMLDialogElement → Effect Unit
close (Just str) = _closeJust str
close Nothing = _closeNothing

foreign import show ∷ HTMLDialogElement → Effect Unit

foreign import showModal ∷ HTMLDialogElement → Effect Unit

instance EventTargetOp HTMLDialogElement where
  toEventTarget = unsafeCoerce

instance NodeOp HTMLDialogElement where
  toNode = unsafeCoerce

instance ElementOp HTMLDialogElement where
  toElement = unsafeCoerce
