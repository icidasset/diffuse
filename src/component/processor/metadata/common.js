import { parseBlob, parseFromTokenizer, parseWebStream } from "music-metadata";
import * as URI from "uri-js";
import { HttpClient } from "@tokenizer/http";
import { tokenizer as rangeTokenizer } from "@tokenizer/range";

/**
 * @import { TrackStats, TrackTags } from "@component/core/types.d.ts";
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
    duration: meta.format.duration,
  };

  /** @type {TrackTags} */
  const tags = {
    album: meta.common.album,
    artist: meta.common.artist,
    disc: {
      no: meta.common.disk.no || 1,
      of: meta.common.disk.of ?? undefined,
    },
    genre: Array.isArray(meta.common.genre)
      ? meta.common.genre[0]
      : meta.common.genre,
    title: meta.common.title || filename || urls?.head || "Unknown",
    track: {
      no: meta.common.track.no || 1,
      of: meta.common.track.of ?? undefined,
    },
    year: meta.common.year,
  };

  return {
    artwork: includeArtwork ? meta.common.picture : undefined,
    stats,
    tags,
  };
}
