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


export function mimeType(fileExtension) {
  switch (fileExtension) {
    case "mp3": return "audio/mpeg";
    case "mp4": return "audio/mp4";
    case "m4a": return "audio/mp4";
    case "flac": return "audio/flac";
    case "ogg": return "audio/ogg";
    case "wav": return "audio/wave";
    case "webm": return "audio/webm";
  }
}
