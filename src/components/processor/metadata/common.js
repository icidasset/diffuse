import { parseBlob, parseFromTokenizer, parseWebStream } from "music-metadata";
import * as URI from "uri-js";
import { HttpClient } from "@tokenizer/http";
import { tokenizer as rangeTokenizer } from "@tokenizer/range";

/**
 * @import { TrackStats, TrackTags } from "@common/types.d.ts";
 * @import { Extraction, Urls } from "./types.d.ts";
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
    httpClient.getHeadInfo = async () => {
      const info = await getHeadInfo.call(httpClient);
      return { ...info, acceptPartialRequests: true };
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
  const stats = {
    albumGain: meta.format.albumGain,
    bitrate: meta.format.bitrate,
    bitsPerSample: meta.format.bitsPerSample,
    codec: meta.format.codec,
    container: meta.format.container,
    duration: meta.format.duration,
    lossless: meta.format.lossless,
    numberOfChannels: meta.format.numberOfChannels,
    sampleRate: meta.format.sampleRate,
    trackGain: meta.format.trackGain,
  };

  /** @type {TrackTags} */
  const tags = {
    album: meta.common.album,
    albumartist: meta.common.albumartist,
    albumartists: meta.common.albumartists ??
      (meta.common.albumartist ? [meta.common.albumartist] : []),
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
      of: meta.common.disk.of ?? undefined,
    },
    djmixers: meta.common.djmixer,
    engineers: meta.common.engineer,
    gapless: meta.common.gapless,
    genres: Array.isArray(meta.common.genre)
      ? meta.common.genre
      : [meta.common.genre],
    isrc: meta.common.isrc,
    labels: meta.common.label,
    lyricists: meta.common.lyricist,
    media: meta.common.media,
    mixers: meta.common.mixer,
    moods: meta.common.mood,
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
      of: meta.common.track.of ?? undefined,
    },
    work: meta.common.work,
    writers: meta.common.writer,
    year: meta.common.year,
  };

  return {
    artwork: includeArtwork ? meta.common.picture : undefined,
    stats,
    tags,
  };
}
