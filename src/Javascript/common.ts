//
// Common stuff
// ʕ•ᴥ•ʔ


import * as localforage from "localforage"


// 🌳


export type CoverPrep = {
  cacheKey: string
  trackFilename: string
  trackPath: string
  trackSourceId: string
  variousArtists: string
}



// FUNCTIONS


export function db(storeName: string = "main"): LocalForage {
  return localforage.createInstance({
    name: "diffuse",
    storeName
  })
}


export function fileExtension(mimeType: string): string | undefined {
  const audioId = mimeType.toLowerCase().split("/")[ 1 ]

  switch (audioId) {
    case "mp3": return "mp3";
    case "mpeg": return "mp3";

    case "mp4a-latm": return "m4a";
    case "mp4": return "m4a";
    case "x-m4a": return "m4a";

    case "flac": return "flac";
    case "x-flac": return "flac";
    case "ogg": return "ogg";
    case "opus": return "opus";

    case "wav": return "wav";
    case "wave": return "wav";

    case "webm": return "webm";
  }
}


export function mimeType(fileExt: string): string | undefined {
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
