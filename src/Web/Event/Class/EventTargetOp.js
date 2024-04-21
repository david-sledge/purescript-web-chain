"use strict";

export const _newEvent = type => options => new Event(type, options);

export const _registerListener = listenerDetails => eventTarget => () => {
  eventTarget.registeredListeners ?
    eventTarget.registeredListeners.push(listenerDetails) :
    eventTarget.registeredListeners = [listenerDetails];
}

export const _getAllListeners = eventTarget => () =>
  eventTarget.registeredListeners ?
    eventTarget.registeredListeners :
    [];

export const _rmListener = details => target =>
    target.removeEventListener(details.eventType, details.listener, details.options);
