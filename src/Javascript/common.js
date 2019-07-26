//
// Common stuff
// ʕ•ᴥ•ʔ


const debounce =
  (callback, time = 250, interval) =>
  (...args) =>
  clearTimeout(interval, interval = setTimeout(callback, time, ...args))
