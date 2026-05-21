import { ostiary, rpc } from "~/common/worker.js";

/**
 * @import {Actions} from "@specs/components/artwork/types.d.ts"
 */

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

/**
 * @type {Actions['get']}
 */
export async function get(track) {
  if (!navigator.onLine) return null;

  const query = track.tags?.artist;
  if (!query) return null;

  return await fetch(
    `https://ws.audioscrobbler.com/2.0/?method=album.search&album=${query}&api_key=4f0fe85b67baef8bb7d008a8754a95e5&format=json`,
  )
    .then((r) => r.json())
    .then((r) => findCover(r.results.albummatches.album))
    .catch((err) => {
      console.error(err);
      return null;
    });
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
 * @returns {Promise<Uint8Array | null>}
 */
async function findCover(remainingMatches) {
  const album = remainingMatches[0];
  const url = album ? album.image[album.image.length - 1]["#text"] : null;

  return url && url !== ""
    ? await fetch(url)
      .then((r) => r.blob())
      .then(async (b) => new Uint8Array(await b.arrayBuffer()))
      .catch(() => findCover(remainingMatches.slice(1)))
    : album
    ? findCover(remainingMatches.slice(1))
    : null;
}
