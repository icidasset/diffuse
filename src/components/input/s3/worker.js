import { isAudioFile } from "@components/input/common.js";
import {
  bucketId,
  bucketsFromTracks,
  buildURI,
  consultBucket,
  createClient,
  groupTracksByBucket,
  loadBuckets,
  parseURI,
} from "./common.js";
import { SCHEME } from "./constants.js";

/**
 * @import { InputActions as Actions, Track } from "@common/types.d.ts";
 * @import { Bucket } from "./types.d.ts"
 */

////////////////////////////////////////////
// ACTIONS
////////////////////////////////////////////

/**
 * @type {Actions['consult']}
 */
export async function consult(fileUriOrScheme) {
  if (!fileUriOrScheme.includes(":")) {
    return { supported: true, consult: "undetermined" };
  }

  const parsed = parseURI(fileUriOrScheme);
  if (!parsed) return { supported: true, consult: "undetermined" };

  const consult = await consultBucket(parsed.bucket);
  return { supported: true, consult };
}

/**
 * @type {Actions['contextualize']}
 */
export async function contextualize(tracks) {
  bucketsFromTracks(tracks);
}

/**
 * @type {Actions['groupConsult']}
 */
export async function groupConsult(tracks) {
  const groups = groupTracksByBucket(tracks);

  const promises = Object.entries(groups).map(
    async ([bucketId, { bucket, tracks }]) => {
      const available = await consultBucket(bucket);
      const grouping = available
        ? { available, tracks }
        : { available, reason: "Bucket unavailable", tracks };

      return {
        key: `${SCHEME}:${bucketId}`,
        grouping,
      };
    },
  );

  const entries = (await Promise.all(promises)).map((
    entry,
  ) => [entry.key, entry.grouping]);
  return Object.fromEntries(entries);
}

/**
 * @type {Actions['list']}
 */
export async function list(cachedTracks = []) {
  /** @type {Record<string, Record<string, Track>>} */
  const cache = {};

  cachedTracks.forEach((t) => {
    const parsed = parseURI(t.uri);
    if (!parsed) return;

    const bid = bucketId(parsed?.bucket);

    if (cache[bid]) {
      cache[bid][parsed.path] = t;
    } else {
      cache[bid] = { [parsed.path]: t };
    }
  });

  const buckets = await loadBuckets();
  const promises = Object.values(buckets).map(async (bucket) => {
    const client = createClient(bucket);
    const bid = bucketId(bucket);

    const list = await Array.fromAsync(
      client.listObjects({
        prefix: bucket.path.replace(/^\//, ""),
      }),
    );

    return list
      .filter((l) => isAudioFile(l.key))
      .map((l) => {
        const cachedTrack = cache[bid]?.[l.key];

        const id = cachedTrack?.id || crypto.randomUUID();
        const stats = cachedTrack?.stats;
        const tags = cachedTrack?.tags;

        /** @type {Track} */
        const track = {
          $type: "sh.diffuse.output.tracks",
          id,
          stats,
          tags,
          uri: buildURI(bucket, l.key),
        };

        return track;
      });
  });

  const tracks = (await Promise.all(promises)).flat(1);
  return tracks;
}

/**
 * @type {Actions['resolve']}
 */
export async function resolve(
  { method, uri },
) {
  const parsed = parseURI(uri);
  if (!parsed) return undefined;

  const expiresInSeconds = 60 * 60 * 24 * 7; // 7 days
  const expiresAtSeconds = Math.round(Date.now() / 1000) + expiresInSeconds;

  const client = createClient(parsed.bucket);
  const url = await client.getPresignedUrl(
    /** @type {any} */ (method.toUpperCase()),
    parsed.path,
  );

  return { expiresAt: expiresAtSeconds, url };
}

// ADDITIONAL ACTIONS

export function demo() {
  // Credentials are read-only, no worries.

  /** @type {Bucket} */
  const bucket = {
    accessKey: atob("QUtJQTZPUTNFVk1BWFZDRFFINkI="),
    bucketName: "ongaku-ryoho-demo",
    host: "s3.amazonaws.com",
    path: "/",
    region: "us-east-1",
    secretKey: atob("Z0hPQkdHRzU1aXc0a0RDbjdjWlRJYTVTUDRZWnpERkRzQnFCYWI4Mg=="),
  };

  const uri = buildURI(bucket, "");

  /** @type {Track} */
  const track = {
    $type: "sh.diffuse.output.tracks",
    id: crypto.randomUUID(),
    kind: "placeholder",
    uri,
  };

  return {
    bucket,
    track,
  };
}
