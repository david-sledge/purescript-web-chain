export const open = dialog => () => dialog.open;
export const setOpen = b => dialog => () => {dialog.open = b};
export const returnValue = dialog => () => dialog.returnValue;
export const setReturnValue = str => dialog => () => {dialog.returnValue = str};
export const _closeNothing = dialog => () => dialog.close();
export const _closeJust = str => dialog => () => dialog.close(str);
export const show = dialog => () => dialog.show();
export const showModal = dialog => () => dialog.showModal();