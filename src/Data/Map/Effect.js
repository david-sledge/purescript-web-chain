"use strict";

export const _new = () => new Map();

export const _insert = nothing => just => k => v => map => () => {
  const oldVal = map.get(k);
  map.set(k, v);

  return typeof oldVal == 'undefined' ? nothing : just(oldVal);
}

export const _freeze = map => insertF => iMap => () => {
  const iter = map.entries();

  const f = map_ => {
    const result = iter.next();

    if (result.done)
      return map_;

    return f(insertF(result.value[0])(result.value[1])(map_));
  };

  return f(iMap);
};

export const _lookup = nothing => just => key => map => () => {
  const mVal = map.get(key);

  return typeof mVal == "undefined" ? nothing : just(mVal);
};

export const _delete = nothing => just => k => map => () => {
  const mVal = map.get(k);
  map.delete(k);

  return typeof mVal == 'undefined' ? nothing : just(mVal);
}

export const _size = map => () => map.size

export const _clear = map => () => map.clear()
