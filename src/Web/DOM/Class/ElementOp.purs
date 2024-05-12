-- | Type classes of convenience.

module Web.DOM.Class.ElementOp
  ( class ElementOp
  , classList
  , getAttribute
  , removeAttribute
  , setAttribute
  , toElement
  ) where

import Prelude

import Data.Maybe (Maybe)
import Effect.Class (class MonadEffect, liftEffect)
import Web.DOM (DOMTokenList)
import Web.DOM as D
import Web.DOM.Class.NodeOp (class NodeOp)
import Web.DOM.Element as E
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

class NodeOp n <= ElementOp n where
  toElement ∷ n → E.Element

--------------------------------------------------------------------------------
instance ElementOp D.Element where
  toElement = identity

--------------------------------------------------------------------------------
-- children
instance ElementOp HE.HTMLElement where
  toElement = HE.toElement

--------------------------------------------------------------------------------
-- grandchildren
instance ElementOp HAn.HTMLAnchorElement where
  toElement = HAn.toElement

instance ElementOp HAr.HTMLAreaElement where
  toElement = HAr.toElement

instance ElementOp HAu.HTMLAudioElement where
  toElement = HAu.toElement

instance ElementOp HBR.HTMLBRElement where
  toElement = HBR.toElement

instance ElementOp HBa.HTMLBaseElement where
  toElement = HBa.toElement

instance ElementOp HBo.HTMLBodyElement where
  toElement = HBo.toElement

instance ElementOp HBu.HTMLButtonElement where
  toElement = HBu.toElement

instance ElementOp HCa.HTMLCanvasElement where
  toElement = HCa.toElement

instance ElementOp HDL.HTMLDListElement where
  toElement = HDL.toElement

instance ElementOp HDE.HTMLDataElement where
  toElement = HDE.toElement

instance ElementOp HDa.HTMLDataListElement where
  toElement = HDa.toElement

instance ElementOp HDv.HTMLDivElement where
  toElement = HDv.toElement

instance ElementOp HEm.HTMLEmbedElement where
  toElement = HEm.toElement

instance ElementOp HFi.HTMLFieldSetElement where
  toElement = HFi.toElement

instance ElementOp HFo.HTMLFormElement where
  toElement = HFo.toElement

instance ElementOp HHR.HTMLHRElement where
  toElement = HHR.toElement

instance ElementOp HHe.HTMLHeadElement where
  toElement = HHe.toElement

instance ElementOp HHa.HTMLHeadingElement where
  toElement = HHa.toElement

instance ElementOp HHt.HTMLHtmlElement where
  toElement = HHt.toElement

instance ElementOp HIF.HTMLIFrameElement where
  toElement = HIF.toElement

instance ElementOp HIn.HTMLInputElement where
  toElement = HIn.toElement

instance ElementOp HKe.HTMLKeygenElement where
  toElement = HKe.toElement

instance ElementOp HLI.HTMLLIElement where
  toElement = HLI.toElement

instance ElementOp HLa.HTMLLabelElement where
  toElement = HLa.toElement

instance ElementOp HLe.HTMLLegendElement where
  toElement = HLe.toElement

instance ElementOp HLi.HTMLLinkElement where
  toElement = HLi.toElement

instance ElementOp HMa.HTMLMapElement where
  toElement = HMa.toElement

instance ElementOp HMe.HTMLMediaElement where
  toElement = HMe.toElement

instance ElementOp HMt.HTMLMetaElement where
  toElement = HMt.toElement

instance ElementOp HMr.HTMLMeterElement where
  toElement = HMr.toElement

instance ElementOp HMo.HTMLModElement where
  toElement = HMo.toElement

instance ElementOp HOL.HTMLOListElement where
  toElement = HOL.toElement

instance ElementOp HOb.HTMLObjectElement where
  toElement = HOb.toElement

instance ElementOp HOG.HTMLOptGroupElement where
  toElement = HOG.toElement

instance ElementOp HOp.HTMLOptionElement where
  toElement = HOp.toElement

instance ElementOp HOu.HTMLOutputElement where
  toElement = HOu.toElement

instance ElementOp HPa.HTMLParagraphElement where
  toElement = HPa.toElement

instance ElementOp HPm.HTMLParamElement where
  toElement = HPm.toElement

instance ElementOp HPr.HTMLPreElement where
  toElement = HPr.toElement

instance ElementOp HPo.HTMLProgressElement where
  toElement = HPo.toElement

instance ElementOp HQu.HTMLQuoteElement where
  toElement = HQu.toElement

instance ElementOp HSc.HTMLScriptElement where
  toElement = HSc.toElement

instance ElementOp HSe.HTMLSelectElement where
  toElement = HSe.toElement

instance ElementOp HSo.HTMLSourceElement where
  toElement = HSo.toElement

instance ElementOp HSp.HTMLSpanElement where
  toElement = HSp.toElement

instance ElementOp HSt.HTMLStyleElement where
  toElement = HSt.toElement

instance ElementOp HTC.HTMLTableCaptionElement where
  toElement = HTC.toElement

instance ElementOp HTe.HTMLTableCellElement where
  toElement = HTe.toElement

instance ElementOp HTo.HTMLTableColElement where
  toElement = HTo.toElement

instance ElementOp HTd.HTMLTableDataCellElement where
  toElement = HTd.toElement

instance ElementOp HTE.HTMLTableElement where
  toElement = HTE.toElement

instance ElementOp HTH.HTMLTableHeaderCellElement where
  toElement = HTH.toElement

instance ElementOp HTr.HTMLTableRowElement where
  toElement = HTr.toElement

instance ElementOp HTS.HTMLTableSectionElement where
  toElement = HTS.toElement

instance ElementOp HTm.HTMLTemplateElement where
  toElement = HTm.toElement

instance ElementOp HTA.HTMLTextAreaElement where
  toElement = HTA.toElement

instance ElementOp HTi.HTMLTimeElement where
  toElement = HTi.toElement

instance ElementOp HTt.HTMLTitleElement where
  toElement = HTt.toElement

instance ElementOp HTa.HTMLTrackElement where
  toElement = HTa.toElement

instance ElementOp HUL.HTMLUListElement where
  toElement = HUL.toElement

instance ElementOp HVi.HTMLVideoElement where
  toElement = HVi.toElement

setAttribute ∷ ∀ m e. MonadEffect m ⇒ ElementOp e ⇒ String → String → e → m Unit
setAttribute name value = liftEffect <<< E.setAttribute name value <<< toElement

getAttribute ∷ ∀ m e. MonadEffect m ⇒ ElementOp e ⇒ String → e → m (Maybe String)
getAttribute name = liftEffect <<< E.getAttribute name <<< toElement

removeAttribute ∷ ∀ m e. MonadEffect m ⇒ ElementOp e ⇒ String → e → m Unit
removeAttribute name = liftEffect <<< E.removeAttribute name <<< toElement

classList ∷ ∀ m e. MonadEffect m ⇒ ElementOp e ⇒ e → m DOMTokenList
classList = liftEffect <<< E.classList <<< toElement
