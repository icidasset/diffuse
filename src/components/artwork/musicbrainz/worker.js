import { ostiary, rpc } from "~/common/worker.js";

/**
 * @import {Track} from "~/definitions/types.d.ts"
 * @import {Actions} from "~/components/artwork/types.d.ts"
 */

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

/**
 * @type {Actions['get']}
 */
export async function get(track) {
  const artist = track.tags?.artist;
  const album = track.tags?.album;

  if (!navigator.onLine) return null;
  if (!album && !artist) return null;

  const variousArtists = artist?.toUpperCase() === "VA";

  return search(track, variousArtists);
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
 * @param {string} str
 */
function escapeLucene(str) {
  return [].map
    .call(str, (char) => {
      if (
        char === "+" ||
        char === "-" ||
        char === "&" ||
        char === "|" ||
        char === "!" ||
        char === "(" ||
        char === ")" ||
        char === "{" ||
        char === "}" ||
        char === "[" ||
        char === "]" ||
        char === "^" ||
        char === '"' ||
        char === "~" ||
        char === "*" ||
        char === "?" ||
        char === ":" ||
        char === "\\" ||
        char === "/"
      ) {
        return "\\" + char;
      } else return char;
    })
    .join("");
}

/**
 * @param {Track} track
 * @param {boolean} variousArtists
 * @returns {Promise<Uint8Array | null>}
 */
async function search(track, variousArtists) {
  const artist = track.tags?.artist;
  const album = track.tags?.album;

  const query = `release:"${escapeLucene(album || "")}"` +
    (variousArtists
      ? ``
      : ` AND artistname:"${escapeLucene(artist || "")}"`);
  const encodedQuery = encodeURIComponent(query);

  return await fetch(
    `https://musicbrainz.org/ws/2/release/?query=${encodedQuery}&fmt=json`,
  )
    .then((r) => r.json())
    .then((r) => {
      if (r.releases.length === 0 && !variousArtists) {
        return search(track, true);
      } else {
        return findCover(r.releases, track, variousArtists);
      }
    })
    .catch(() => null);
}

/**
 * @param {any[]} remainingReleases
 * @param {Track} track
 * @param {boolean} variousArtists
 * @returns {Promise<Uint8Array | null>}
 */
async function findCover(remainingReleases, track, variousArtists) {
  const release = remainingReleases[0];
  if (!release) return null;

  const credit = release?.["artist-credit"]?.[0]?.name;
  if (
    variousArtists && credit !== "Various Artists" &&
    credit !== track.tags?.artist
  ) return null;

  return await fetch(
    `https://coverartarchive.org/release/${release.id}/front-1200`,
  )
    .then((r) => r.blob())
    .then(async (b) => {
      if (b.type.startsWith("image/")) {
        return new Uint8Array(await b.arrayBuffer());
      } else {
        return findCover(remainingReleases.slice(1), track, variousArtists);
      }
    })
    .catch((err) => {
      console.error(err);
      return findCover(remainingReleases.slice(1), track, variousArtists);
    });
}
