import type { IPicture } from "music-metadata";
import * as IDB from "idb-keyval";

import type { Artwork, ArtworkRequest } from "./types";
import type { Extraction } from "../metadata/types";
import { provide } from "@scripts/common";
import { IDB_ARTWORK_PREFIX } from "./constants";
import { musicMetadataTags } from "../metadata/common";

// State
let queue: ArtworkRequest[] = [];

////////////////////////////////////////////
// SETUP
////////////////////////////////////////////

const actions = {
  artwork,
  supply,
};

const { tasks } = provide({ actions, tasks: actions });

export type Actions = typeof actions;
export type Tasks = typeof tasks;

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

async function artwork(request: ArtworkRequest) {
  const art = await processRequest(request);
  return art;
}

function supply(items: ArtworkRequest[]) {
  const exe = !queue[0];
  queue = [...queue, ...items];
  if (exe) shiftQueue();
}

////////////////////////////////////////////
// 🛠️
////////////////////////////////////////////
function escapeLucene(str: string) {
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
      )
        return "\\" + char;
      else return char;
    })
    .join("");
}

async function lastFm(req: ArtworkRequest): Promise<Artwork[]> {
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

async function lastFmCover(remainingMatches: any[]): Promise<Artwork[]> {
  const album = remainingMatches[0];
  const url = album ? album.image[album.image.length - 1]["#text"] : null;

  return url && url !== ""
    ? await fetch(url)
        .then((r) => r.blob())
        .then(async (b) => [
          { bytes: await b.arrayBuffer().then((buf) => new Uint8Array(buf)), mime: b.type },
        ])
        .catch((err) => {
          console.error(err);
          return lastFmCover(remainingMatches.slice(1));
        })
    : album
      ? lastFmCover(remainingMatches.slice(1))
      : [];
}

async function musicBrainz(req: ArtworkRequest): Promise<Artwork[]> {
  const artist = req.tags?.artist;
  const album = req.tags?.album;

  if (!navigator.onLine) return [];
  if (!album && !artist) return [];

  const query =
    `release:"${escapeLucene(album || "")}"` +
    (req.variousArtists ? `` : ` AND artistname:"${escapeLucene(artist || "")}"`);
  const encodedQuery = encodeURIComponent(query);

  return await fetch(`https://musicbrainz.org/ws/2/release/?query=${encodedQuery}&fmt=json`)
    .then((r) => r.json())
    .then((r) => {
      if (r.releases.length === 0 && !req.variousArtists) {
        return musicBrainz({ ...req, variousArtists: true });
      } else {
        return musicBrainzCover(r.releases, req);
      }
    })
    .catch((err) => {
      console.error(err);
      return [];
    });
}

async function musicBrainzCover(remainingReleases: any[], req: ArtworkRequest): Promise<Artwork[]> {
  const release = remainingReleases[0];
  if (!release) return [];

  const credit = release?.["artist-credit"]?.[0]?.name;
  if (req.variousArtists && credit !== "Various Artists" && credit !== req.tags?.artist) return [];

  return await fetch(`https://coverartarchive.org/release/${release.id}/front-1200`)
    .then((r) => r.blob())
    .then(async (b) => {
      if (b.type.startsWith("image/")) {
        return [{ bytes: await b.arrayBuffer().then((buf) => new Uint8Array(buf)), mime: b.type }];
      } else {
        return musicBrainzCover(remainingReleases.slice(1), req);
      }
    })
    .catch((err) => {
      console.error(err);
      return musicBrainzCover(remainingReleases.slice(1), req);
    });
}

async function processRequest(req: ArtworkRequest): Promise<Artwork[]> {
  // Check if already processed
  // TODO: Retry if none was found?
  const cache = await IDB.get(`${IDB_ARTWORK_PREFIX}/${req.cacheId}`);
  if (cache && Array.isArray(cache) && cache.length) return cache;

  // Request override
  if (req.tags?.artist?.toUpperCase() === "VA") {
    req.variousArtists = true;
  }

  // 🚀
  let art: Artwork[] = [];

  // Get metadata + possible artwork from file metadata
  const meta = await musicMetadataTags({ ...req, includeArtwork: true }).catch((err) => {
    console.error("music-metadata error", err);
    const extraction: Extraction = {};
    return extraction;
  });

  if (!req.tags && meta.tags) req.tags = meta.tags;

  // Add artwork from metadata
  const fromMeta =
    meta.artwork?.map((a: IPicture) => {
      return { bytes: a.data, mime: a.format };
    }) || [];

  art.push(...fromMeta);

  // If no artwork, try finding it on other sources
  if (art.length === 0) {
    const fromMusicBrainz = await musicBrainz(req);
    art.push(...fromMusicBrainz);
  }

  if (art.length === 0) {
    const fromLastFm = await lastFm(req);
    art.push(...fromLastFm);
  }

  // Save artwork to IDB
  await IDB.set(`${IDB_ARTWORK_PREFIX}/${req.cacheId}`, art);

  // Fin
  return art;
}

async function shiftQueue() {
  const next = queue.shift();
  if (!next) return;

  await processRequest(next);
  await shiftQueue();
}
