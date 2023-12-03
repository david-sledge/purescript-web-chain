"use strict";

export const _newEvent = type => () => new Event(type, {"bubbles": true});
export const _registerListener = listenerArgs => eventTarget => () =>
  eventTarget.registeredListeners ?
    eventTarget.registeredListeners.push(listenerArgs) :
    eventTarget.registeredListeners = [listenerArgs];
export const _getAllListeners = eventTarget => () =>
  eventTarget.registeredListeners ?
    eventTarget.registeredListeners :
    [];
export const _unregisterListener = listener => eventType => thirdArg =>
  listenerArgs => f => eventTarget => () => {
  if (eventTarget.registeredListeners) {
    for (var ndx = eventTarget.registeredListeners.length - 1; ndx >= 0; ndx--) {
      if (listenerArgs (listener0 => eventType0 => thirdArg0 => eventTarget0 =>
          listener === listener0 && eventType === eventType0 && thirdArg === thirdArg0)) {
        f(eventTarget.registeredListeners.splice(ndx, 1)[0]);
      }
    }
  }
}
export const _clearRegisteredListeners = eventTarget => () => (eventTarget.registeredListeners = []);
