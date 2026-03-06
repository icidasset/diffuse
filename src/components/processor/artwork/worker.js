import * as IDB from "idb-keyval";

import { IDB_ARTWORK_PREFIX } from "./constants.js";
import { create as createCid } from "~/common/cid.js";
import { musicMetadataTags } from "../metadata/common.js";
import { ostiary, rpc } from "~/common/worker.js";

// multicodec raw bytes
const RAW = 0x55;

/**
 * @import {IPicture} from "music-metadata"
 * @import {Actions, Artwork, ArtworkRequest} from "./types.d.ts"
 * @import {Extraction} from "../metadata/types.d.ts"
 */

/**
 * @type {ArtworkRequest[]}
 */
let queue = [];

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

/**
 * @type {Actions['artwork']}
 */
export async function artwork(request) {
  const art = await processRequest(request);
  return art;
}

/**
 * @type {Actions['supply']}
 */
export function supply(items) {
  const exe = !queue[0];
  queue = [...queue, ...items];
  if (exe) shiftQueue();
}

////////////////////////////////////////////
// ⚡️
////////////////////////////////////////////

ostiary((context) => {
  rpc(context, {
    artwork,
    supply,
  });
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
 * @param {ArtworkRequest} req
 * @returns {Promise<Artwork[]>}
 */
async function lastFm(req) {
  if (!navigator.onLine) return [];

  const query = req.tags?.artist;

  return await fetch(
    `https://ws.audioscrobbler.com/2.0/?method=album.search&album=${query}&api_key=4f0fe85b67baef8bb7d008a8754a95e5&format=json`,
  )
    .then((r) => r.json())
    .then((r) => lastFmCover(r.results.albummatches.album))
    .catch((err) => {
      console.error(err);
      return [];
    });
}

/**
 * @param {any[]} remainingMatches
 * @returns {Promise<Artwork[]>}
 */
async function lastFmCover(remainingMatches) {
  const album = remainingMatches[0];
  const url = album ? album.image[album.image.length - 1]["#text"] : null;

  return url && url !== ""
    ? await fetch(url)
      .then((r) => r.blob())
      .then(async (b) => [
        {
          bytes: await b.arrayBuffer().then((buf) => new Uint8Array(buf)),
          mime: b.type,
        },
      ])
      .catch((err) => {
        // console.error(err);
        return lastFmCover(remainingMatches.slice(1));
      })
    : album
    ? lastFmCover(remainingMatches.slice(1))
    : [];
}

/**
 * @param {ArtworkRequest} req
 * @returns {Promise<Artwork[]>}
 */
async function musicBrainz(req) {
  const artist = req.tags?.artist;
  const album = req.tags?.album;

  if (!navigator.onLine) return [];
  if (!album && !artist) return [];

  const query = `release:"${escapeLucene(album || "")}"` +
    (req.variousArtists
      ? ``
      : ` AND artistname:"${escapeLucene(artist || "")}"`);
  const encodedQuery = encodeURIComponent(query);

  return await fetch(
    `https://musicbrainz.org/ws/2/release/?query=${encodedQuery}&fmt=json`,
  )
    .then((r) => r.json())
    .then((r) => {
      if (r.releases.length === 0 && !req.variousArtists) {
        return musicBrainz({ ...req, variousArtists: true });
      } else {
        return musicBrainzCover(r.releases, req);
      }
    })
    .catch((err) => {
      // console.error(err);
      return [];
    });
}

/**
 * @param {any[]} remainingReleases
 * @param {ArtworkRequest} req
 * @returns {Promise<Artwork[]>}
 */
async function musicBrainzCover(remainingReleases, req) {
  const release = remainingReleases[0];
  if (!release) return [];

  const credit = release?.["artist-credit"]?.[0]?.name;
  if (
    req.variousArtists && credit !== "Various Artists" &&
    credit !== req.tags?.artist
  ) return [];

  return await fetch(
    `https://coverartarchive.org/release/${release.id}/front-1200`,
  )
    .then((r) => r.blob())
    .then(async (b) => {
      if (b.type.startsWith("image/")) {
        return [{
          bytes: await b.arrayBuffer().then((buf) => new Uint8Array(buf)),
          mime: b.type,
        }];
      } else {
        return musicBrainzCover(remainingReleases.slice(1), req);
      }
    })
    .catch((err) => {
      console.error(err);
      return musicBrainzCover(remainingReleases.slice(1), req);
    });
}

/**
 * @param {ArtworkRequest} req
 * @returns {Promise<Artwork[]>}
 */
async function processRequest(req) {
  // Check if already processed

  /** @type {string[] | undefined} */
  const cachedCids = await IDB.get(
    `${IDB_ARTWORK_PREFIX}/track/${req.cacheId}`,
  );

  if (cachedCids?.length) {
    /** @type {Artwork[]} */
    const art = await Promise.all(
      cachedCids.map((cid) => IDB.get(`${IDB_ARTWORK_PREFIX}/image/${cid}`)),
    );

    const found = art.filter(Boolean);
    if (found.length) return found;
  }

  // Request override
  if (req.tags?.artist?.toUpperCase() === "VA") {
    req.variousArtists = true;
  }

  // 🚀

  /** @type {Artwork[]} */
  let art = [];

  // Get metadata + possible artwork from file metadata
  const meta = await musicMetadataTags({ ...req, includeArtwork: true }).catch(
    /** @param {Error} err */ (err) => {
      console.error("music-metadata error", err);
      /** @type {Extraction} */
      const extraction = {};
      return extraction;
    },
  );

  if (!req.tags && meta.tags) req.tags = meta.tags;

  // Add artwork from metadata
  const fromMeta = meta.artwork?.map(
    /**
     * @param {IPicture} a
     */
    (a) => {
      return { bytes: a.data, mime: a.format };
    },
  ) || [];

  art.push(...fromMeta);

  // Stop here if insufficient metadata is present
  if (!req.tags?.artist || !req.tags?.album) return art;

  // If no artwork, try finding it on other sources
  if (art.length === 0) {
    const fromMusicBrainz = await musicBrainz(req);
    art.push(...fromMusicBrainz);
  }

  if (art.length === 0) {
    const fromLastFm = await lastFm(req);
    art.push(...fromLastFm);
  }

  // Save artwork to IDB — store each image by its content CID,
  // then map the track to those CIDs
  const cids = await Promise.all(
    art.map(async (a) => {
      const cid = await createCid(RAW, a.bytes);
      const key = `${IDB_ARTWORK_PREFIX}/image/${cid}`;
      if (await IDB.get(key)) return cid;
      await IDB.set(key, a);
      return cid;
    }),
  );

  await IDB.set(`${IDB_ARTWORK_PREFIX}/track/${req.cacheId}`, cids);

  // Fin
  return art;
}

async function shiftQueue() {
  const next = queue.shift();
  if (!next) return;

  await processRequest(next);
  await shiftQueue();
}
