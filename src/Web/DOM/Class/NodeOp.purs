-- | Type classes of convenience.

module Web.DOM.Class.NodeOp
  ( appendChild
  , baseURI
  , childNodes
  , class NodeOp
  , clone
  , contains
  , deepClone
  , firstChild
  , hasChildNodes
  , insertBefore
  , isEqualNode
  , lastChild
  , nextSibling
  , nodeName
  , nodeType
  , nodeTypeIndex
  , nodeValue
  , normalize
  , ownerDocument
  , parentElement
  , parentNode
  , previousSibling
  , removeChild
  , replaceChild
  , setNodeValue
  , setTextContent
  , textContent
  , toNode
  )
  where

import Prelude

import Data.Maybe (Maybe)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Web.DOM (CharacterData, Document, Element, Node, NodeList, NodeType, Text)
import Web.DOM.CharacterData as C
import Web.DOM.Document as D
import Web.DOM.Element as E
import Web.DOM.Node as N
import Web.DOM.Text as T
import Web.Event.Class.EventTargetOp (class EventTargetOp)
import Web.HTML.HTMLAnchorElement as HAn
import Web.HTML.HTMLAreaElement as HAr
import Web.HTML.HTMLAudioElement as HAu
import Web.HTML.HTMLBRElement as HBR
import Web.HTML.HTMLBaseElement as HBa
import Web.HTML.HTMLBodyElement as HBo
import Web.HTML.HTMLButtonElement as HBu
import Web.HTML.HTMLCanvasElement as HCa
import Web.HTML.HTMLDListElement as HDL
import Web.HTML.HTMLDataElement as HDE
import Web.HTML.HTMLDataListElement as HDa
import Web.HTML.HTMLDivElement as HDv
import Web.HTML.HTMLDocument as HD
import Web.HTML.HTMLElement as HE
import Web.HTML.HTMLEmbedElement as HEm
import Web.HTML.HTMLFieldSetElement as HFi
import Web.HTML.HTMLFormElement as HFo
import Web.HTML.HTMLHRElement as HHR
import Web.HTML.HTMLHeadElement as HHe
import Web.HTML.HTMLHeadingElement as HHa
import Web.HTML.HTMLHtmlElement as HHt
import Web.HTML.HTMLIFrameElement as HIF
import Web.HTML.HTMLInputElement as HIn
import Web.HTML.HTMLKeygenElement as HKe
import Web.HTML.HTMLLIElement as HLI
import Web.HTML.HTMLLabelElement as HLa
import Web.HTML.HTMLLegendElement as HLe
import Web.HTML.HTMLLinkElement as HLi
import Web.HTML.HTMLMapElement as HMa
import Web.HTML.HTMLMediaElement as HMe
import Web.HTML.HTMLMetaElement as HMt
import Web.HTML.HTMLMeterElement as HMr
import Web.HTML.HTMLModElement as HMo
import Web.HTML.HTMLOListElement as HOL
import Web.HTML.HTMLObjectElement as HOb
import Web.HTML.HTMLOptGroupElement as HOG
import Web.HTML.HTMLOptionElement as HOp
import Web.HTML.HTMLOutputElement as HOu
import Web.HTML.HTMLParagraphElement as HPa
import Web.HTML.HTMLParamElement as HPm
import Web.HTML.HTMLPreElement as HPr
import Web.HTML.HTMLProgressElement as HPo
import Web.HTML.HTMLQuoteElement as HQu
import Web.HTML.HTMLScriptElement as HSc
import Web.HTML.HTMLSelectElement as HSe
import Web.HTML.HTMLSourceElement as HSo
import Web.HTML.HTMLSpanElement as HSp
import Web.HTML.HTMLStyleElement as HSt
import Web.HTML.HTMLTableCaptionElement as HTC
import Web.HTML.HTMLTableCellElement as HTe
import Web.HTML.HTMLTableColElement as HTo
import Web.HTML.HTMLTableDataCellElement as HTd
import Web.HTML.HTMLTableElement as HTE
import Web.HTML.HTMLTableHeaderCellElement as HTH
import Web.HTML.HTMLTableRowElement as HTr
import Web.HTML.HTMLTableSectionElement as HTS
import Web.HTML.HTMLTemplateElement as HTm
import Web.HTML.HTMLTextAreaElement as HTA
import Web.HTML.HTMLTimeElement as HTi
import Web.HTML.HTMLTitleElement as HTt
import Web.HTML.HTMLTrackElement as HTa
import Web.HTML.HTMLUListElement as HUL
import Web.HTML.HTMLVideoElement as HVi

class EventTargetOp n ⇐ NodeOp n where
  toNode ∷ n → Node

--------------------------------------------------------------------------------
instance NodeOp Node where
  toNode = identity

--------------------------------------------------------------------------------
-- children
instance NodeOp CharacterData where
  toNode = C.toNode

instance NodeOp Document where
  toNode = D.toNode

instance NodeOp Element where
  toNode = E.toNode

--------------------------------------------------------------------------------
-- grandchildren
instance NodeOp HE.HTMLElement where
  toNode = HE.toNode

instance NodeOp HD.HTMLDocument where
  toNode = HD.toNode

instance NodeOp Text where
  toNode = T.toNode

--------------------------------------------------------------------------------
-- great-grandchildren
instance NodeOp HAn.HTMLAnchorElement where
  toNode = HAn.toNode

instance NodeOp HAr.HTMLAreaElement where
  toNode = HAr.toNode

instance NodeOp HAu.HTMLAudioElement where
  toNode = HAu.toNode

instance NodeOp HBR.HTMLBRElement where
  toNode = HBR.toNode

instance NodeOp HBa.HTMLBaseElement where
  toNode = HBa.toNode

instance NodeOp HBo.HTMLBodyElement where
  toNode = HBo.toNode

instance NodeOp HBu.HTMLButtonElement where
  toNode = HBu.toNode

instance NodeOp HCa.HTMLCanvasElement where
  toNode = HCa.toNode

instance NodeOp HDL.HTMLDListElement where
  toNode = HDL.toNode

instance NodeOp HDE.HTMLDataElement where
  toNode = HDE.toNode

instance NodeOp HDa.HTMLDataListElement where
  toNode = HDa.toNode

instance NodeOp HDv.HTMLDivElement where
  toNode = HDv.toNode

instance NodeOp HEm.HTMLEmbedElement where
  toNode = HEm.toNode

instance NodeOp HFi.HTMLFieldSetElement where
  toNode = HFi.toNode

instance NodeOp HFo.HTMLFormElement where
  toNode = HFo.toNode

instance NodeOp HHR.HTMLHRElement where
  toNode = HHR.toNode

instance NodeOp HHe.HTMLHeadElement where
  toNode = HHe.toNode

instance NodeOp HHa.HTMLHeadingElement where
  toNode = HHa.toNode

instance NodeOp HHt.HTMLHtmlElement where
  toNode = HHt.toNode

instance NodeOp HIF.HTMLIFrameElement where
  toNode = HIF.toNode

instance NodeOp HIn.HTMLInputElement where
  toNode = HIn.toNode

instance NodeOp HKe.HTMLKeygenElement where
  toNode = HKe.toNode

instance NodeOp HLI.HTMLLIElement where
  toNode = HLI.toNode

instance NodeOp HLa.HTMLLabelElement where
  toNode = HLa.toNode

instance NodeOp HLe.HTMLLegendElement where
  toNode = HLe.toNode

instance NodeOp HLi.HTMLLinkElement where
  toNode = HLi.toNode

instance NodeOp HMa.HTMLMapElement where
  toNode = HMa.toNode

instance NodeOp HMe.HTMLMediaElement where
  toNode = HMe.toNode

instance NodeOp HMt.HTMLMetaElement where
  toNode = HMt.toNode

instance NodeOp HMr.HTMLMeterElement where
  toNode = HMr.toNode

instance NodeOp HMo.HTMLModElement where
  toNode = HMo.toNode

instance NodeOp HOL.HTMLOListElement where
  toNode = HOL.toNode

instance NodeOp HOb.HTMLObjectElement where
  toNode = HOb.toNode

instance NodeOp HOG.HTMLOptGroupElement where
  toNode = HOG.toNode

instance NodeOp HOp.HTMLOptionElement where
  toNode = HOp.toNode

instance NodeOp HOu.HTMLOutputElement where
  toNode = HOu.toNode

instance NodeOp HPa.HTMLParagraphElement where
  toNode = HPa.toNode

instance NodeOp HPm.HTMLParamElement where
  toNode = HPm.toNode

instance NodeOp HPr.HTMLPreElement where
  toNode = HPr.toNode

instance NodeOp HPo.HTMLProgressElement where
  toNode = HPo.toNode

instance NodeOp HQu.HTMLQuoteElement where
  toNode = HQu.toNode

instance NodeOp HSc.HTMLScriptElement where
  toNode = HSc.toNode

instance NodeOp HSe.HTMLSelectElement where
  toNode = HSe.toNode

instance NodeOp HSo.HTMLSourceElement where
  toNode = HSo.toNode

instance NodeOp HSp.HTMLSpanElement where
  toNode = HSp.toNode

instance NodeOp HSt.HTMLStyleElement where
  toNode = HSt.toNode

instance NodeOp HTC.HTMLTableCaptionElement where
  toNode = HTC.toNode

instance NodeOp HTe.HTMLTableCellElement where
  toNode = HTe.toNode

instance NodeOp HTo.HTMLTableColElement where
  toNode = HTo.toNode

instance NodeOp HTd.HTMLTableDataCellElement where
  toNode = HTd.toNode

instance NodeOp HTE.HTMLTableElement where
  toNode = HTE.toNode

instance NodeOp HTH.HTMLTableHeaderCellElement where
  toNode = HTH.toNode

instance NodeOp HTr.HTMLTableRowElement where
  toNode = HTr.toNode

instance NodeOp HTS.HTMLTableSectionElement where
  toNode = HTS.toNode

instance NodeOp HTm.HTMLTemplateElement where
  toNode = HTm.toNode

instance NodeOp HTA.HTMLTextAreaElement where
  toNode = HTA.toNode

instance NodeOp HTi.HTMLTimeElement where
  toNode = HTi.toNode

instance NodeOp HTt.HTMLTitleElement where
  toNode = HTt.toNode

instance NodeOp HTa.HTMLTrackElement where
  toNode = HTa.toNode

instance NodeOp HUL.HTMLUListElement where
  toNode = HUL.toNode

instance NodeOp HVi.HTMLVideoElement where
  toNode = HVi.toNode

meh ∷ ∀ n a. NodeOp n ⇒ (Node → a) → n → a
meh n = n <<< toNode

nodeType ∷ ∀ n. NodeOp n ⇒ Partial ⇒ n → NodeType
nodeType = meh N.nodeType

nodeTypeIndex ∷ ∀ n. NodeOp n ⇒ n → Int
nodeTypeIndex = meh N.nodeTypeIndex

nodeName ∷ ∀ n. NodeOp n ⇒ n → String
nodeName = meh N.nodeName

baseURI ∷ ∀ n. NodeOp n ⇒ n → Effect String
baseURI = meh N.baseURI

mehM ∷ ∀ (m ∷ Type → Type) (a ∷ Type) (n ∷ Type). MonadEffect m ⇒ NodeOp n ⇒ (Node → Effect a) → n → m a
mehM = compose liftEffect <<< meh

ownerDocument ∷ ∀ m n. MonadEffect m ⇒ NodeOp n ⇒ n → m (Maybe Document)
ownerDocument = mehM N.ownerDocument

parentNode ∷ ∀ m n. MonadEffect m ⇒ NodeOp n ⇒ n → m (Maybe Node)
parentNode = mehM N.parentNode

parentElement ∷ ∀ m n. MonadEffect m ⇒ NodeOp n ⇒ n → m (Maybe Element)
parentElement = mehM N.parentElement

hasChildNodes ∷ ∀ m n. MonadEffect m ⇒ NodeOp n ⇒ n → m Boolean
hasChildNodes = mehM N.hasChildNodes

childNodes ∷ ∀ m n. MonadEffect m ⇒ NodeOp n ⇒ n → m NodeList
childNodes = mehM N.childNodes

firstChild ∷ ∀ m n. MonadEffect m ⇒ NodeOp n ⇒ n → m (Maybe Node)
firstChild = mehM N.firstChild

lastChild ∷ ∀ m n. MonadEffect m ⇒ NodeOp n ⇒ n → m (Maybe Node)
lastChild = mehM N.lastChild

previousSibling ∷ ∀ m n. MonadEffect m ⇒ NodeOp n ⇒ n → m (Maybe Node)
previousSibling = mehM N.previousSibling

nextSibling ∷ ∀ m n. MonadEffect m ⇒ NodeOp n ⇒ n → m (Maybe Node)
nextSibling = mehM N.nextSibling

nodeValue ∷ ∀ m n. MonadEffect m ⇒ NodeOp n ⇒ n → m (Maybe String)
nodeValue = mehM N.nodeValue

mehM1 ∷ ∀ b m a n. MonadEffect m ⇒ NodeOp n ⇒ (b → Node → Effect a) → b → n → m a
mehM1 = compose mehM

setNodeValue ∷ ∀ m n. MonadEffect m ⇒ NodeOp n ⇒ String → n → m Unit
setNodeValue = mehM1 N.setNodeValue

textContent ∷ ∀ m n. MonadEffect m ⇒ NodeOp n ⇒ n → m String
textContent = mehM N.textContent

setTextContent ∷ ∀ m n. MonadEffect m ⇒ NodeOp n ⇒ String → n → m Unit
setTextContent = mehM1 N.setTextContent

normalize ∷ ∀ m n. MonadEffect m ⇒ NodeOp n ⇒ n → m Unit
normalize = mehM N.normalize

clone ∷ ∀ m n. MonadEffect m ⇒ NodeOp n ⇒ n → m Node
clone = mehM N.clone

deepClone ∷ ∀ m n. MonadEffect m ⇒ NodeOp n ⇒ n → m Node
deepClone = mehM N.deepClone

isEqualNode ∷ ∀ m n1 n2. MonadEffect m ⇒ NodeOp n1 ⇒ NodeOp n2 ⇒ n1 → n2 → m Boolean
isEqualNode = mehM <<< N.isEqualNode <<< toNode

appendChild ∷ ∀ m p c. MonadEffect m ⇒ NodeOp c ⇒ NodeOp p ⇒ c → p → m Unit
appendChild = mehM <<< N.appendChild <<< toNode

removeChild ∷ ∀ m p c. MonadEffect m ⇒ NodeOp c ⇒ NodeOp p ⇒ c → p → m Unit
removeChild = mehM <<< N.removeChild <<< toNode

contains ∷ ∀ m p c. MonadEffect m ⇒ NodeOp c ⇒ NodeOp p ⇒ c → p → m Boolean
contains = mehM <<< N.contains <<< toNode

insertBefore ∷ ∀ m n1 n2 n3. MonadEffect m ⇒ NodeOp n1 ⇒ NodeOp n2 ⇒ NodeOp n3 ⇒ n1 → n2 → n3 → m Unit
insertBefore n1 n2 = liftEffect <<< N.insertBefore (toNode n1) (toNode n2) <<< toNode

replaceChild ∷ ∀ m n1 n2 n3. MonadEffect m ⇒ NodeOp n1 ⇒ NodeOp n2 ⇒ NodeOp n3 ⇒ n1 → n2 → n3 → m Unit
replaceChild n1 n2 = liftEffect <<< N.replaceChild (toNode n1) (toNode n2) <<< toNode
