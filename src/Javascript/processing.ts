//
// Processing
// ♪(´ε｀ )
//
// Audio processing, getting metadata, etc.

import type { IAudioMetadata } from "music-metadata";
import type { MediaInfoType } from "mediainfo.js";

import * as Uint8arrays from "uint8arrays";
import { transformUrl } from "./urls";

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
  const musicMetadata = await import("music-metadata-browser").then((a) => a.default);
  const httpTokenizer = await import("@tokenizer/http").then((a) => a.default);

  const tokenizer = await httpTokenizer.makeTokenizer(headUrl);
  tokenizer.fileInfo.url = getUrl;

  // @ts-ignore
  if (tokenizer.rangeRequestClient) {
    // @ts-ignore
    tokenizer.rangeRequestClient.url = getUrl;
    // @ts-ignore
    tokenizer.rangeRequestClient.resolvedUrl = undefined;
  }

  const mmResult = await musicMetadata.parseFromTokenizer(
    tokenizer,
    { skipCovers: !covers }
  ).catch(err => {
    console.warn(err)
    return null
  });

  const mmTags = mmResult && pickTagsFromMusicMetadata(filename, mmResult);
  if (mmTags) return mmTags;

  const miResult = await (await mediaInfoClient(covers))
    .analyzeData(getSize(headUrl), readChunk(getUrl))
    .catch(err => {
      console.warn(err)
      return null
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

function pickTagsFromMediaInfo(filename: string, result: MediaInfoType): Tags | null {
  const tags = result?.media?.track?.filter((t) => t["@type"] === "General")[0];
  if (!tags) return null;

  let artist = typeof tags.Performer == "string" ? tags.Performer : null;
  const album = typeof tags.Album == "string" ? tags.Album : null;

  const title = typeof tags.Track == "string"
    ? tags.Track
    : typeof tags.Title == "string"
    ? tags.Title
    : null;

  if (!artist && !title) return null;

  // TODO: Encoding issues with mediainfo.js
  if (artist?.includes("�") || album?.includes("�") || title?.includes("�")) return null

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
          data: Uint8arrays.fromString(tags.Cover_Data, "base64"),
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
// --

async function mediaInfoClient(covers: boolean) {
  const MediaInfoFactory = await import("mediainfo.js").then(a => a.default)

  return await MediaInfoFactory({
    coverData: covers,
    locateFile: () => {
      return "../../wasm/media-info.wasm";
    },
  });
}
