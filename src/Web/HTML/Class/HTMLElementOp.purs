-- | Type classes of convenience.

module Web.HTML.Class.HTMLElementOp
  ( class HTMLElementOp
  , style
  , toHTMLElement
  ) where

import Prelude

import Effect.Class (class MonadEffect, liftEffect)
import Web.CSSOM.CSSStyleDeclaration (CSSStyleDeclaration)
import Web.CSSOM.ElementCSSInlineStyle as C
import Web.DOM.Class.ElementOp (class ElementOp)
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

class ElementOp n ⇐ HTMLElementOp n where
  toHTMLElement ∷ n → HE.HTMLElement

--------------------------------------------------------------------------------
instance HTMLElementOp HE.HTMLElement where
  toHTMLElement = identity

--------------------------------------------------------------------------------
-- children
instance HTMLElementOp HAn.HTMLAnchorElement where
  toHTMLElement = HAn.toHTMLElement

instance HTMLElementOp HAr.HTMLAreaElement where
  toHTMLElement = HAr.toHTMLElement

instance HTMLElementOp HAu.HTMLAudioElement where
  toHTMLElement = HAu.toHTMLElement

instance HTMLElementOp HBR.HTMLBRElement where
  toHTMLElement = HBR.toHTMLElement

instance HTMLElementOp HBa.HTMLBaseElement where
  toHTMLElement = HBa.toHTMLElement

instance HTMLElementOp HBo.HTMLBodyElement where
  toHTMLElement = HBo.toHTMLElement

instance HTMLElementOp HBu.HTMLButtonElement where
  toHTMLElement = HBu.toHTMLElement

instance HTMLElementOp HCa.HTMLCanvasElement where
  toHTMLElement = HCa.toHTMLElement

instance HTMLElementOp HDL.HTMLDListElement where
  toHTMLElement = HDL.toHTMLElement

instance HTMLElementOp HDE.HTMLDataElement where
  toHTMLElement = HDE.toHTMLElement

instance HTMLElementOp HDa.HTMLDataListElement where
  toHTMLElement = HDa.toHTMLElement

instance HTMLElementOp HDv.HTMLDivElement where
  toHTMLElement = HDv.toHTMLElement

instance HTMLElementOp HEm.HTMLEmbedElement where
  toHTMLElement = HEm.toHTMLElement

instance HTMLElementOp HFi.HTMLFieldSetElement where
  toHTMLElement = HFi.toHTMLElement

instance HTMLElementOp HFo.HTMLFormElement where
  toHTMLElement = HFo.toHTMLElement

instance HTMLElementOp HHR.HTMLHRElement where
  toHTMLElement = HHR.toHTMLElement

instance HTMLElementOp HHe.HTMLHeadElement where
  toHTMLElement = HHe.toHTMLElement

instance HTMLElementOp HHa.HTMLHeadingElement where
  toHTMLElement = HHa.toHTMLElement

instance HTMLElementOp HHt.HTMLHtmlElement where
  toHTMLElement = HHt.toHTMLElement

instance HTMLElementOp HIF.HTMLIFrameElement where
  toHTMLElement = HIF.toHTMLElement

instance HTMLElementOp HIn.HTMLInputElement where
  toHTMLElement = HIn.toHTMLElement

instance HTMLElementOp HKe.HTMLKeygenElement where
  toHTMLElement = HKe.toHTMLElement

instance HTMLElementOp HLI.HTMLLIElement where
  toHTMLElement = HLI.toHTMLElement

instance HTMLElementOp HLa.HTMLLabelElement where
  toHTMLElement = HLa.toHTMLElement

instance HTMLElementOp HLe.HTMLLegendElement where
  toHTMLElement = HLe.toHTMLElement

instance HTMLElementOp HLi.HTMLLinkElement where
  toHTMLElement = HLi.toHTMLElement

instance HTMLElementOp HMa.HTMLMapElement where
  toHTMLElement = HMa.toHTMLElement

instance HTMLElementOp HMe.HTMLMediaElement where
  toHTMLElement = HMe.toHTMLElement

instance HTMLElementOp HMt.HTMLMetaElement where
  toHTMLElement = HMt.toHTMLElement

instance HTMLElementOp HMr.HTMLMeterElement where
  toHTMLElement = HMr.toHTMLElement

instance HTMLElementOp HMo.HTMLModElement where
  toHTMLElement = HMo.toHTMLElement

instance HTMLElementOp HOL.HTMLOListElement where
  toHTMLElement = HOL.toHTMLElement

instance HTMLElementOp HOb.HTMLObjectElement where
  toHTMLElement = HOb.toHTMLElement

instance HTMLElementOp HOG.HTMLOptGroupElement where
  toHTMLElement = HOG.toHTMLElement

instance HTMLElementOp HOp.HTMLOptionElement where
  toHTMLElement = HOp.toHTMLElement

instance HTMLElementOp HOu.HTMLOutputElement where
  toHTMLElement = HOu.toHTMLElement

instance HTMLElementOp HPa.HTMLParagraphElement where
  toHTMLElement = HPa.toHTMLElement

instance HTMLElementOp HPm.HTMLParamElement where
  toHTMLElement = HPm.toHTMLElement

instance HTMLElementOp HPr.HTMLPreElement where
  toHTMLElement = HPr.toHTMLElement

instance HTMLElementOp HPo.HTMLProgressElement where
  toHTMLElement = HPo.toHTMLElement

instance HTMLElementOp HQu.HTMLQuoteElement where
  toHTMLElement = HQu.toHTMLElement

instance HTMLElementOp HSc.HTMLScriptElement where
  toHTMLElement = HSc.toHTMLElement

instance HTMLElementOp HSe.HTMLSelectElement where
  toHTMLElement = HSe.toHTMLElement

instance HTMLElementOp HSo.HTMLSourceElement where
  toHTMLElement = HSo.toHTMLElement

instance HTMLElementOp HSp.HTMLSpanElement where
  toHTMLElement = HSp.toHTMLElement

instance HTMLElementOp HSt.HTMLStyleElement where
  toHTMLElement = HSt.toHTMLElement

instance HTMLElementOp HTC.HTMLTableCaptionElement where
  toHTMLElement = HTC.toHTMLElement

instance HTMLElementOp HTe.HTMLTableCellElement where
  toHTMLElement = HTe.toHTMLElement

instance HTMLElementOp HTo.HTMLTableColElement where
  toHTMLElement = HTo.toHTMLElement

instance HTMLElementOp HTd.HTMLTableDataCellElement where
  toHTMLElement = HTd.toHTMLElement

instance HTMLElementOp HTE.HTMLTableElement where
  toHTMLElement = HTE.toHTMLElement

instance HTMLElementOp HTH.HTMLTableHeaderCellElement where
  toHTMLElement = HTH.toHTMLElement

instance HTMLElementOp HTr.HTMLTableRowElement where
  toHTMLElement = HTr.toHTMLElement

instance HTMLElementOp HTS.HTMLTableSectionElement where
  toHTMLElement = HTS.toHTMLElement

instance HTMLElementOp HTm.HTMLTemplateElement where
  toHTMLElement = HTm.toHTMLElement

instance HTMLElementOp HTA.HTMLTextAreaElement where
  toHTMLElement = HTA.toHTMLElement

instance HTMLElementOp HTi.HTMLTimeElement where
  toHTMLElement = HTi.toHTMLElement

instance HTMLElementOp HTt.HTMLTitleElement where
  toHTMLElement = HTt.toHTMLElement

instance HTMLElementOp HTa.HTMLTrackElement where
  toHTMLElement = HTa.toHTMLElement

instance HTMLElementOp HUL.HTMLUListElement where
  toHTMLElement = HUL.toHTMLElement

instance HTMLElementOp HVi.HTMLVideoElement where
  toHTMLElement = HVi.toHTMLElement

style ∷ ∀ m n. MonadEffect m ⇒ HTMLElementOp n ⇒ n → m CSSStyleDeclaration
style n = liftEffect <<< C.style <<< C.fromHTMLElement $ toHTMLElement n
