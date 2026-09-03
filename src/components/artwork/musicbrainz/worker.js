import { ostiary, rpc } from "~/common/worker.js";

/**
 * @import {Track} from "~/definitions/types.d.ts"
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
  const artist = track.tags?.artist;
  const album = track.tags?.album;

  if (!navigator.onLine) return null;
  if (!album && !artist) return null;

  const variousArtists = artist?.toUpperCase() === "VA";

  const controller = new AbortController();
  const timer = setTimeout(() => controller.abort(), PROVIDER_TIMEOUT_MS);
  try {
    return await search(track, variousArtists, controller.signal);
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
 * @param {AbortSignal} signal
 * @returns {Promise<Uint8Array | null>}
 */
async function search(track, variousArtists, signal) {
  const artist = track.tags?.artist;
  const album = track.tags?.album;

  const query = `release:"${escapeLucene(album || "")}"` +
    (variousArtists ? `` : ` AND artistname:"${escapeLucene(artist || "")}"`);
  const encodedQuery = encodeURIComponent(query);

  const r = await fetch(
    `https://musicbrainz.org/ws/2/release/?query=${encodedQuery}&fmt=json`,
    { signal },
  ).then((r) => r.json()).catch(() => undefined);

  if (!r) return null;
  if (r.releases.length === 0 && !variousArtists) {
    return search(track, true, signal);
  } else {
    return findCover(r.releases, track, variousArtists, signal);
  }
}

/**
 * @param {any[]} remainingReleases
 * @param {Track} track
 * @param {boolean} variousArtists
 * @param {AbortSignal} signal
 * @returns {Promise<Uint8Array | null>}
 */
async function findCover(remainingReleases, track, variousArtists, signal) {
  const release = remainingReleases[0];
  if (!release) return null;

  const credit = release?.["artist-credit"]?.[0]?.name;
  if (
    variousArtists && credit !== "Various Artists" &&
    credit !== track.tags?.artist
  ) return null;

  return await fetch(
    `https://coverartarchive.org/release/${release.id}/front-1200`,
    { signal },
  )
    .then((r) => r.blob())
    .then(async (b) => {
      if (b.type.startsWith("image/")) {
        return new Uint8Array(await b.arrayBuffer());
      } else {
        return findCover(remainingReleases.slice(1), track, variousArtists, signal);
      }
    })
    .catch(() =>
      findCover(remainingReleases.slice(1), track, variousArtists, signal)
    );
}
