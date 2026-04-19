import { parseBlob, parseFromTokenizer, parseWebStream } from "music-metadata";
import * as URI from "fast-uri";
import { HttpClient } from "@tokenizer/http";
import { tokenizer as rangeTokenizer } from "@tokenizer/range";

import { removeUndefinedValuesFromRecord } from "~/common/utils.js";

/**
 * @import { TrackStats, TrackTags } from "~/definitions/types.d.ts";
 * @import { Extraction, Urls } from "~/components/metadata/audio-file/types.d.ts";
 */

// 🛠️

/**
 * @param {{ includeArtwork?: boolean; mimeType?: string; stream?: ReadableStream; urls?: Urls; }} _
 * @returns {Promise<Extraction>}
 */
export async function musicMetadataTags({
  includeArtwork,
  mimeType,
  stream,
  urls,
}) {
  const uri = urls ? URI.parse(urls.get) : undefined;
  const pathParts = uri?.path?.split("/");
  const filename = pathParts?.[pathParts.length - 1];

  let meta;

  if (urls?.get.startsWith("blob:")) {
    const blob = await fetch(urls.get).then((r) => r.blob());
    meta = await parseBlob(blob, { skipCovers: !includeArtwork });
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
