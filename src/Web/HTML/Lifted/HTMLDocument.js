"use strict";

export const _setBody = body => doc => () => {
  doc.body = body;
};

export const _setHead = head => doc => () => {
doc.head = head;
};
