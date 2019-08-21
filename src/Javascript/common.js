//
// Common stuff
// ʕ•ᴥ•ʔ


const debounce =
  (callback, time = 250, timeoutId) =>
  (...args) =>
  clearTimeout(timeoutId, timeoutId = setTimeout(callback, time, ...args))


const throttle =
  (callback, time = 250, wasCalledBefore, lastestArgs) =>
  (...args) => {
    lastestArgs = args
    if (wasCalledBefore) { return } else { wasCalledBefore = true }
    setTimeout(() => { callback(...lastestArgs); wasCalledBefore = false }, time)
  }


function identity(a) {
  return a
}
