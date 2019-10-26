//
// Common stuff
// ʕ•ᴥ•ʔ


export const debounce =
  (callback, time = 250, timeoutId) =>
  (...args) =>
  clearTimeout(timeoutId, timeoutId = setTimeout(callback, time, ...args))


export const throttle =
  (callback, time = 250, wasCalledBefore, lastestArgs) =>
  (...args) => {
    lastestArgs = args
    if (wasCalledBefore) { return } else { wasCalledBefore = true }
    setTimeout(() => { callback(...lastestArgs); wasCalledBefore = false }, time)
  }


export function identity(a) {
  return a
}
