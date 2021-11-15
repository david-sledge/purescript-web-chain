"use strict";

exports.newEvent = type => () => new Event(type, {"bubbles": true});
exports.registerListener = listenerArgs => eventTarget => () =>
  eventTarget.registeredListeners ?
    eventTarget.registeredListeners.push(listenerArgs) :
    eventTarget.registeredListeners = [listenerArgs];
exports.getAllListeners = eventTarget => () =>
  eventTarget.registeredListeners ?
    eventTarget.registeredListeners :
    [];
exports.unregisterListener = listener => eventType => thirdArg => listenerArgs => f
  => eventTarget => () => {
  if (eventTarget.registeredListeners) {
    for (var ndx = eventTarget.registeredListeners.length - 1; ndx >= 0; ndx--) {
      if (listenerArgs (listener0 => eventType0 => thirdArg0 => eventTarget0
          => listener === listener0 && eventType === eventType0 && thirdArg === thirdArg0)) {
        f(eventTarget.registeredListeners.splice(ndx, 1)[0]);
      }
    }
  }
}
exports.clearRegisteredListeners = eventTarget => () => (eventTarget.registeredListeners = []);
