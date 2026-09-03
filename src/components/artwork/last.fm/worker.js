import { ostiary, rpc } from "~/common/worker.js";

/**
 * @import {Actions} from "@specs/components/artwork/types.d.ts"
 */

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

/**
 * Time budget for this provider's own HTTP requests. Once exceeded, in-flight
 * fetches are aborted so a slow/cancelled request doesn't keep occupying server
 * resources (and hold up the caller).
 */
const PROVIDER_TIMEOUT_MS = 60_000;

/**
 * @type {Actions['get']}
 */
export async function get(track) {
  if (!navigator.onLine) return null;

  const query = track.tags?.artist;
  if (!query) return null;

  const controller = new AbortController();
  const timer = setTimeout(() => controller.abort(), PROVIDER_TIMEOUT_MS);
  const signal = controller.signal;

  try {
    const search = await fetch(
      `https://ws.audioscrobbler.com/2.0/?method=album.search&album=${query}&api_key=4f0fe85b67baef8bb7d008a8754a95e5&format=json`,
      { signal },
    ).then((r) => r.json());
    return await findCover(search.results.albummatches.album, signal);
  } finally {
    clearTimeout(timer);
  }
}

////////////////////////////////////////////
// ⚡️
////////////////////////////////////////////

ostiary((context) => {
  rpc(context, { get });
});

////////////////////////////////////////////
// 🛠️
////////////////////////////////////////////

/**
 * @param {any[]} remainingMatches
 * @param {AbortSignal} signal
 * @returns {Promise<Uint8Array | null>}
 */
async function findCover(remainingMatches, signal) {
  const album = remainingMatches[0];
  const url = album ? album.image[album.image.length - 1]["#text"] : null;

  return url && url !== ""
    ? await fetch(url, { signal })
      .then((r) => r.blob())
      .then(async (b) => new Uint8Array(await b.arrayBuffer()))
      .catch(() => findCover(remainingMatches.slice(1), signal))
    : album
    ? findCover(remainingMatches.slice(1), signal)
    : null;
}
