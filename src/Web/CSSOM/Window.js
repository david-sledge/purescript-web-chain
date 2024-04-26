"use strict";

export const _getComputedStyleNothing = element => window => () => window.getComputedStyle(element);
export const _getComputedStyleJust = element => psuedoElt => window => () => window.getComputedStyle(element, psuedoElt);

const getDefaultComputedStyle_ = g => f => element => window => () => {
  if (window.getDefaultComputedStyle)
    return g(window.getDefaultComputedStyle);
  else {
    const tagName = element.tagName;
    const document = window.document;
    const newElem = document.createElement(tagName);
    document.body.appendChild(newElem);
    const style = f(window.getComputedStyle)(newElem);
    document.body.removeChild(newElem);
    return style;
  }
};

export const _getDefaultComputedStyleNothing = element =>
  getDefaultComputedStyle_(nativeGdcsF => nativeGdcsF(element))(gdcsF => gdcsF)(element);
export const _getDefaultComputedStyleJust = element => psuedoElt =>
  getDefaultComputedStyle_(nativeGdcsF => nativeGdcsF(element, psuedoElt))(gdcsF => elem => gdcsF(elem, psuedoElt))(element);
