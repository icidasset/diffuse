//
// Common stuff
// ʕ•ᴥ•ʔ


export const WEBNATIVE_PERMISSIONS = {
  app: {
    name: "Diffuse",
    creator: "icidasset"
  }
}


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


export function fileExtension(mimeType) {
  const audioId = mimeType.toLowerCase().split("/")[1]

  switch (audioId) {
    case "mp3": return "mp3";
    case "mpeg": return "mp3";

    case "mp4a-latm": return "m4a";
    case "mp4": return "m4a";
    case "x-m4a": return "m4a";

    case "flac": return "flac";
    case "ogg": return "ogg";

    case "wav": return "wav";
    case "wave": return "wav";

    case "webm": return "webm";
  }
}


export function mimeType(fileExt) {
  switch (fileExt) {
    case "mp3": return "audio/mpeg";
    case "mp4": return "audio/mp4";
    case "m4a": return "audio/mp4";
    case "flac": return "audio/flac";
    case "ogg": return "audio/ogg";
    case "wav": return "audio/wave";
    case "webm": return "audio/webm";
  }
}
