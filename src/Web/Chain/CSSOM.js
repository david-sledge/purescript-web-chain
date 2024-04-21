"use strict";

export const _storeDislayValue = value => element => () => {
  if (element.storedValues)
    element.storedValues["display"] = value;
  else
    element.storedValues = {"display" : value};
}

export const _retrieveDislayValue = element => () =>
element && element.storedValues && element.storedValues["display"] && element.storedValues["display"] != "none" ?
    element.storedValues["display"] : "";
