import { parseBuffer, parseFromTokenizer, parseWebStream } from "music-metadata";
import * as URI from "fast-uri";
import { HttpClient } from "@tokenizer/http";
import { tokenizer as rangeTokenizer } from "@tokenizer/range";

import { removeUndefinedValuesFromRecord } from "~/common/utils.js";

/**
 * @import { TrackStats, TrackTags } from "~/definitions/types.d.ts";
 * @import { Extraction, Urls } from "@specs/components/metadata/audio-file/types.d.ts";
 */

// 🛠️

/**
 * Maps the audio MIME types music-metadata's loaders register to the
 * extension `findLoaderForExtension` recognises. Used for blob URLs, which
 * carry no filename, so the parser can be picked by extension instead of
 * by content-type (whose matcher is broken in the browser bundle).
 *
 * @type {Record<string, string>}
 */
const MIME_TO_EXT = {
  "audio/mpeg": "mp3",
  "audio/mp3": "mp3",
  "audio/aac": "aac",
  "audio/aacp": "aacp",
  "audio/mp4": "m4a",
  "audio/m4a": "m4a",
  "audio/x-m4a": "m4a",
  "audio/ogg": "ogg",
  "audio/opus": "opus",
  "audio/speex": "spx",
  "audio/flac": "flac",
  "audio/aiff": "aiff",
  "audio/aif": "aif",
  "audio/aifc": "aifc",
  "audio/wav": "wav",
  "audio/wave": "wav",
  "audio/vnd.wave": "wav",
  "audio/x-wav": "wav",
  "audio/webm": "webm",
  "audio/ape": "ape",
  "audio/monkeys-audio": "ape",
  "audio/musepack": "mpc",
  "audio/wavpack": "wv",
  "audio/asf": "asf",
  "audio/ms-wma": "wma",
  "audio/dsf": "dsf",
};

/**
 * @param {string | undefined} mimeType
 * @returns {string | undefined}
 */
function mimeTypeToPath(mimeType) {
  const ext = mimeType && MIME_TO_EXT[mimeType];
  return ext ? `file.${ext}` : undefined;
}

/**
 * @param {{ includeArtwork?: boolean; filename?: string; mimeType?: string; stream?: ReadableStream; urls?: Urls; }} _
 * @returns {Promise<Extraction>}
 */
export async function musicMetadataTags({
  includeArtwork,
  filename,
  mimeType,
  stream,
  urls,
}) {
  const uri = urls ? URI.parse(urls.get) : undefined;
  const pathParts = uri?.path?.split("/");
  const urlFilename = pathParts?.[pathParts.length - 1];

  let meta;

  if (urls?.get.startsWith("blob:")) {
    const blob = await fetch(urls.get).then((r) => r.blob());
    // Blob URLs carry no filename, so the URL path is just the blob's UUID.
    // Without a path or a recognised MIME-type music-metadata falls back to
    // content-sniffing, whose content-type matcher is broken in the browser
    // bundle (the `content-type` CJS interop leaves `default` undefined, so
    // `findLoaderForContentType` always throws — see the range-tokenizer
    // branch below for the same issue). Derive a filename from the blob's
    // MIME type so the parser is picked by extension instead.
    const ext = MIME_TO_EXT[blob.type];
    const buffer = new Uint8Array(await blob.arrayBuffer());
    meta = await parseBuffer(
      buffer,
      ext ? { path: `file.${ext}` } : undefined,
      { skipCovers: !includeArtwork },
    );
  } else if (urls) {
    const httpClient = new HttpClient(urls.head, {
      resolveUrl: false,
    });
    httpClient.resolvedUrl = urls.get;
    const getHeadInfo = httpClient.getHeadInfo;

    // FUCKAROUND: Not sure of the downsides of this
    /** @type {any} */ (httpClient).getHeadInfo = async () => {
      try {
        const info = await getHeadInfo.call(httpClient);
        return { ...info, acceptPartialRequests: true };
      } catch {
        // Some servers (e.g. Dropbox temporary links) don't return Content-Length.
        // Fall back to downloading the full file without range requests.
        return { size: undefined, acceptPartialRequests: false };
      }
    };

    /** @type {any} */
    const tokenizer = await rangeTokenizer(httpClient);
    // The range tokenizer's fileInfo comes from the HEAD response, which only
    // has size/mimeType — no path. Without a path or a recognised MIME-type,
    // music-metadata falls back to content-sniffing, which rejects some files
    // (e.g. `audio/x-m4a`, or servers returning `application/octet-stream`).
    // Provide the filename so it can pick a parser by extension instead. Fall
    // back to the URL path, and then to a synthetic filename derived from the
    // HEAD response MIME type (useful for temporary links like Dropbox's that
    // don't preserve the original filename).
    const path = filename || urlFilename ||
      mimeTypeToPath(tokenizer.fileInfo?.mimeType);
    if (path) {
      tokenizer.fileInfo = { ...tokenizer.fileInfo, path };
    }
    meta = await parseFromTokenizer(tokenizer, { skipCovers: !includeArtwork });
  } else if (stream) {
    meta = await parseWebStream(stream, { mimeType }, {
      skipCovers: !includeArtwork,
    });
  } else {
    throw new Error("Missing args, need either some urls or a stream.");
  }

  /** @type {TrackStats} */
  const statsFull = {
    albumGain: maybeRound(meta.format.albumGain),
    bitrate: maybeRound(meta.format.bitrate),
    bitsPerSample: maybeRound(meta.format.bitsPerSample),
    codec: meta.format.codec,
    container: meta.format.container,
    duration: meta.format.duration != null
      ? Math.round(meta.format.duration * 1000)
      : undefined,
    lossless: meta.format.lossless,
    numberOfChannels: maybeRound(meta.format.numberOfChannels),
    sampleRate: maybeRound(meta.format.sampleRate),
    trackGain: maybeRound(meta.format.trackGain),
  };

  /** @type {TrackTags} */
  const tagsFull = {
    album: meta.common.album,
    albumartist: meta.common.albumartist,
    albumartists: Array.isArray(meta.common.albumartist)
      ? meta.common.albumartist
      : (meta.common.albumartist ? [meta.common.albumartist] : undefined),
    albumartistsort: meta.common.albumartistsort,
    albumsort: meta.common.albumsort,
    arranger: meta.common.arranger,
    artist: meta.common.artist,
    artists: meta.common.artists ??
      (meta.common.artist ? [meta.common.artist] : []),
    artistsort: meta.common.artistsort,
    asin: meta.common.asin,
    averageLevel: meta.common.averageLevel,
    barcode: meta.common.barcode,
    bpm: meta.common.bpm,
    catalognumbers: meta.common.catalognumber,
    compilation: meta.common.compilation,
    composers: meta.common.composer,
    composersort: meta.common.composersort,
    conductors: meta.common.conductor,
    date: meta.common.date,
    disc: {
      no: meta.common.disk.no || 1,
      ...(meta.common.disk.of && { of: meta.common.disk.of }),
    },
    djmixers: meta.common.djmixer,
    engineers: meta.common.engineer,
    gapless: meta.common.gapless,
    genres: Array.isArray(meta.common.genre)
      ? meta.common.genre
      : meta.common.genre
      ? [meta.common.genre]
      : undefined,
    isrc: meta.common.isrc,
    labels: meta.common.label,
    lyricists: meta.common.lyricist,
    media: meta.common.media,
    mixers: meta.common.mixer,
    moods: Array.isArray(meta.common.mood)
      ? meta.common.mood
      : meta.common.mood
      ? [meta.common.mood]
      : undefined,
    originaldate: meta.common.originaldate,
    originalyear: meta.common.originalyear,
    peakLevel: meta.common.peakLevel,
    producers: meta.common.producer,
    publishers: meta.common.publisher,
    releasecountry: meta.common.releasecountry,
    releasedate: meta.common.releasedate,
    releasestatus: meta.common.releasestatus,
    releasetypes: meta.common.releasetype,
    remixers: meta.common.remixer,
    technicians: meta.common.technician,
    title: meta.common.title || filename || urls?.head || "Unknown",
    titlesort: meta.common.titlesort,
    track: {
      no: meta.common.track.no || 1,
      ...(meta.common.track.of && { of: meta.common.track.of }),
    },
    work: meta.common.work,
    writers: meta.common.writer,
    year: meta.common.year,
  };

  const stats = removeUndefinedValuesFromRecord(statsFull);
  const tags = removeUndefinedValuesFromRecord(tagsFull);

  return {
    artwork: includeArtwork ? meta.common.picture : undefined,
    stats,
    tags,
  };
}

/**
 * @param {number | undefined} value
 * @returns {number | undefined}
 */
function maybeRound(value) {
  return typeof value === "number" ? Math.round(value) : value;
}
