"use strict";

export const _new = () => [];

export const _push = a => arr => () => arr.push(a);

export const _freeze = arr => () => arr.slice();