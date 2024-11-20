//
// Processing
// ♪(´ε｀ )
//
// Audio processing, getting metadata, etc.

import type { IAudioMetadata } from "music-metadata"
import type { GeneralTrack, MediaInfoResult } from "mediainfo.js"
import type { ITokenizer } from "strtok3"

import * as Uint8arrays from "uint8arrays"
import { type App } from "./elm/types"
import { transformUrl } from "../urls"


// 🏔️


const ENCODING_ISSUE_REPLACE_CHAR = '▩';

let app: App



// 🚀


export function init(a: App) {
  app = a

  app.ports.requestTags.subscribe(requestTags)
  app.ports.syncTags.subscribe(syncTags)
}



// Ports
// -----


function requestTags(context) {
  processContext(context, app).then(newContext => {
    app.ports.receiveTags.send(newContext)
  })
}


function syncTags(context) {
  processContext(context, app).then(newContext => {
    app.ports.replaceTags.send(newContext)
  })
}



// Contexts
// --------


export async function processContext(context, app) {
  const initialPromise = Promise.resolve([]);

  return context.urlsForTags
    .reduce((accumulator, urls, idx) => {
      return accumulator.then((col) => {
        const filename = context.receivedFilePaths[idx].split("/").reverse()[0];

        return Promise.all([transformUrl(urls.headUrl, app), transformUrl(urls.getUrl, app)])
          .then(([headUrl, getUrl]) => {
            return getTags(headUrl, getUrl, filename, { covers: false });
          })
          .then((r) => {
            return col.concat(r);
          })
          .catch((e) => {
            console.warn(e);
            return col.concat(null);
          });
      });
    }, initialPromise)
    .then((col) => {
      context.receivedTags = col;
      return context;
    });
}



// Tags - General
// --------------


type Tags = {
  disc: number;
  nr: number;
  album: string | null;
  artist: string | null;
  title: string;
  genre: string | null;
  year: number | null;
  picture: { data: Uint8Array; format: string } | null;
};

export async function getTags(
  headUrl: string,
  getUrl: string,
  filename: string,
  { covers }: { covers: boolean },
) {
  const musicMetadata = await import("music-metadata");
  const httpTokenizer = await import("@tokenizer/http");
  const rangeTokenizer = await import("@tokenizer/range");

  let tokenizer: ITokenizer;
  let mmResult;

  try {
    const httpClient = new httpTokenizer.HttpClient(headUrl, { resolveUrl: false });
    httpClient.resolvedUrl = getUrl

    tokenizer = await rangeTokenizer.tokenizer(httpClient);

    mmResult = await musicMetadata
      .parseFromTokenizer(tokenizer, { skipCovers: !covers })
      .catch((err) => {
        console.warn(err);
        return null;
      });
  } catch (err) {
    console.warn(err);
  }

  const mmTags = mmResult && pickTagsFromMusicMetadata(filename, mmResult);
  if (mmTags) return mmTags;

  const miResult = await (await mediaInfoClient(covers))
    .analyzeData(getSize(headUrl), readChunk(getUrl))
    .catch((err) => {
      console.warn(err);
      return null;
    });

  const miTags = miResult && pickTagsFromMediaInfo(filename, miResult);
  if (miTags) return miTags;

  return fallbackTags(filename);
}

function fallbackTags(filename: string): Tags {
  const filenameWithoutExt = filename.replace(/\.\w+$/, "");

  return {
    disc: 1,
    nr: 1,
    album: null,
    artist: null,
    title: filenameWithoutExt,
    genre: null,
    year: null,
    picture: null,
  };
}

// Tags - Media Info
// -----------------

const getSize = (headUrl: string) => async (): Promise<number> => {
  const response = await fetch(headUrl, { method: "HEAD" });

  if (!response.ok) {
    throw new Error(`HTTP error status=${response.status}: ${response.statusText}`);
  }

  const l = response.headers.get("Content-Length");

  if (l) {
    return parseInt(l, 10);
  } else {
    throw new Error("HTTP response doesn't have a Content-Length");
  }
};

const readChunk =
  (getUrl: string) =>
  async (chunkSize: number, offset: number): Promise<Uint8Array> => {
    if (chunkSize === 0) return new Uint8Array();

    const from = offset;
    const to = offset + chunkSize;

    const start = to < from ? to : from;
    const end = to < from ? from : to;

    const response = await fetch(getUrl, {
      method: "GET",
      headers: {
        Range: `bytes=${start}-${end}`,
      },
    });

    if (!response.ok) {
      throw new Error(`HTTP error status=${response.status}: ${response.statusText}`);
    }

    return new Uint8Array(await response.arrayBuffer());
  };

function pickTagsFromMediaInfo(filename: string, result: MediaInfoResult): Tags | null {
  const tagsRaw = result?.media?.track?.filter((t) => t["@type"] === "General")[0];
  const tags = tagsRaw === undefined ? undefined : tagsRaw as GeneralTrack;
  if (tags === undefined) return null;

  let artist = typeof tags.Performer == "string" ? tags.Performer : null;
  let album = typeof tags.Album == "string" ? tags.Album : null;

  let title =
    typeof tags.Track == "string" ? tags.Track : typeof tags.Title == "string" ? tags.Title : null;

  if (!artist && !title) return null;

  // TODO: Encoding issues with mediainfo.js
  // https://github.com/buzz/mediainfo.js/issues/150
  if (artist?.includes("�")) artist = artist.replace("�", ENCODING_ISSUE_REPLACE_CHAR)
  if (album?.includes("�")) album = album.replace("�", ENCODING_ISSUE_REPLACE_CHAR)
  if (title?.includes("�")) title = title.replace("�", ENCODING_ISSUE_REPLACE_CHAR)

  if (artist && artist.includes(" / ")) {
    artist = artist
      .split(" / ")
      .filter((a) => a.trim() !== "")
      .join(", ");
  }

  const year = tags.Recorded_Date ? new Date(Date.parse(tags.Recorded_Date)).getFullYear() : null;

  return {
    disc: tags.Part_Position || 1,
    nr: tags.Track_Position || 1,
    album: album,
    artist: artist,
    title: title || filename.replace(/\.\w+$/, ""),
    genre: tags.Genre || null,
    year: year !== null && isNaN(year) ? null : year,
    picture: tags.Cover_Data
      ? {
          data: Uint8arrays.fromString(tags.Cover_Data.split(" / ")[0], "base64pad"),
          format: tags.Cover_Mime || "image/jpeg",
        }
      : null,
  };
}


// Tags - Music Metadata
// ---------------------


function pickTagsFromMusicMetadata(filename: string, result: IAudioMetadata): Tags | null {
  const tags = result && result.common;
  if (!tags) return null;

  const artist = tags.artist && tags.artist.length ? tags.artist : null;
  const title = tags.title && tags.title.length ? tags.title : null;

  if (!artist && !title) return null;

  return {
    disc: tags.disk.no || 1,
    nr: tags.track.no || 1,
    album: tags.album && tags.album.length ? tags.album : null,
    artist: artist,
    title: title || filename.replace(/\.\w+$/, ""),
    genre: (tags.genre && tags.genre[0]) || null,
    year: tags.year || null,
    picture:
      tags.picture && tags.picture[0]
        ? { data: tags.picture[0].data, format: tags.picture[0].format }
        : null,
  };
}



// 🛠️


let client


async function mediaInfoClient(covers: boolean) {
  const MediaInfoFactory = await import("mediainfo.js").then((a) => a.default);

  if (client) return client

  client = await MediaInfoFactory({
    coverData: covers,
    locateFile: () => {
      return "../../wasm/media-info.wasm";
    },
  });

  return client
}
