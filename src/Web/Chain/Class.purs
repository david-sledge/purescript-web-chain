-- | Type classes of convenience.

module Web.Chain.Class
( class IsChildNode
, class IsDocument
, class IsElement
, class IsEventTarget
, class IsNode
, class IsParentNode
, toChildNode
, toDocument
, toElement
, toEventTarget
, toNode
, toParentNode
) where

import Prelude

import Unsafe.Coerce (unsafeCoerce)
import Web.DOM as D
import Web.DOM.ChildNode as C
import Web.DOM.Element as E
import Web.DOM.Node as N
import Web.DOM.ParentNode as P
import Web.DOM.Text as T
import Web.Event.EventTarget as ET
import Web.HTML.HTMLButtonElement as HB
import Web.HTML.HTMLDocument as HD
import Web.HTML.HTMLElement as HE
import Web.HTML.HTMLInputElement as HI

class IsEventTarget et where
  toEventTarget ∷ et → ET.EventTarget

instance IsEventTarget N.Node where
  toEventTarget = N.toEventTarget

instance IsEventTarget HB.HTMLButtonElement where
  toEventTarget = HB.toEventTarget

instance IsEventTarget HD.HTMLDocument where
  toEventTarget = HD.toEventTarget

instance IsEventTarget E.Element where
  toEventTarget = E.toEventTarget

instance IsEventTarget HI.HTMLInputElement where
  toEventTarget = HI.toEventTarget

instance IsEventTarget D.Text where
  toEventTarget = T.toEventTarget

instance IsEventTarget HE.HTMLElement where
  toEventTarget = HE.toEventTarget

instance IsEventTarget C.ChildNode where
  toEventTarget = unsafeCoerce

instance IsEventTarget P.ParentNode where
  toEventTarget = unsafeCoerce



class IsEventTarget n <= IsNode n where
  toNode ∷ n → D.Node

instance IsNode N.Node where
  toNode = identity

instance IsNode D.Text where
  toNode = T.toNode

instance IsNode HB.HTMLButtonElement where
  toNode = HB.toNode

instance IsNode HE.HTMLElement where
  toNode = HE.toNode

instance IsNode HI.HTMLInputElement where
  toNode = HI.toNode

instance IsNode D.Element where
  toNode = E.toNode

instance IsNode HD.HTMLDocument where
  toNode = HD.toNode

instance IsNode C.ChildNode where
  toNode = unsafeCoerce

instance IsNode P.ParentNode where
  toNode = unsafeCoerce



class IsNode d <= IsDocument d where
  toDocument ∷ d → D.Document

instance IsDocument HD.HTMLDocument where
  toDocument = HD.toDocument



class IsNode e <= IsElement e where
  toElement ∷ e → D.Element

instance IsElement D.Element where
  toElement = identity



class IsNode c <= IsChildNode c where
  toChildNode ∷ c → D.ChildNode

instance IsChildNode HB.HTMLButtonElement where
  toChildNode = HB.toChildNode

instance IsChildNode HI.HTMLInputElement where
  toChildNode = HI.toChildNode

instance IsChildNode D.Element where
  toChildNode = E.toChildNode

instance IsChildNode D.Text where
  toChildNode = T.toChildNode

instance IsChildNode C.ChildNode where
  toChildNode = identity



class IsNode p <= IsParentNode p where
  toParentNode ∷ p → D.ParentNode

instance IsParentNode D.Element where
  toParentNode = E.toParentNode

instance IsParentNode HE.HTMLElement where
  toParentNode = HE.toParentNode

instance IsParentNode P.ParentNode where
  toParentNode = identity
