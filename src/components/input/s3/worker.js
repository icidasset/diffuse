import { ostiary, rpc } from "@common/worker.js";
import {
  detach as detachUtil,
  groupKeyHash,
  isAudioFile,
} from "@components/input/common.js";
import {
  bucketId,
  buildURI,
  consultBucket,
  createClient,
  groupTracksByBucket,
  groupUrisByBucket,
  parseURI,
} from "./common.js";
import { SCHEME } from "./constants.js";

/**
 * @import { InputActions as Actions, ConsultGrouping } from "@components/input/types.d.ts";
 * @import { Track } from "@definitions/types.d.ts"
 * @import { Bucket, Demo } from "./types.d.ts"
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
 * @type {Actions['detach']}
 */
export async function detach(args) {
  return detachUtil({
    ...args,

    inputScheme: SCHEME,
    handleFileUri: ({ fileURI, tracks }) => {
      const result = parseURI(fileURI);
      if (!result) return tracks;

      const bid = bucketId(result.bucket);
      const groups = groupTracksByBucket(tracks);

      delete groups[bid];

      return Object.values(groups).map((a) => a.tracks).flat(1);
    },
  });
}

/**
 * @type {Actions['groupConsult']}
 */
export async function groupConsult(uris) {
  const groups = groupUrisByBucket(uris);

  const promises = Object.entries(groups).map(
    async ([bucketId, { bucket, uris }]) => {
      const available = await consultBucket(bucket);

      /** @type {ConsultGrouping} */
      const grouping = available
        ? { available, scheme: SCHEME, uris }
        : { available, reason: "Bucket unavailable", scheme: SCHEME, uris };

      return {
        key: await groupKeyHash(SCHEME, bucketId),
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

  /** @type {Record<string, Bucket>} */
  const buckets = {};

  cachedTracks.forEach((t) => {
    const parsed = parseURI(t.uri);
    if (!parsed) return;

    const bid = bucketId(parsed.bucket);
    buckets[bid] = parsed.bucket;

    if (cache[bid]) {
      cache[bid][parsed.path] = t;
    } else {
      cache[bid] = { [parsed.path]: t };
    }
  });

  const promises = Object.values(buckets).map(async (bucket) => {
    const client = createClient(bucket);
    const bid = bucketId(bucket);

    const list = await Array.fromAsync(
      client.listObjects({
        prefix: bucket.path.replace(/^\//, ""),
      }),
    );

    let tracks = list
      .filter((l) => isAudioFile(l.key))
      .map((l) => {
        const cachedTrack = cache[bid]?.[l.key];

        const id = cachedTrack?.id || crypto.randomUUID();
        const stats = cachedTrack?.stats;
        const tags = cachedTrack?.tags;

        /** @type {Track} */
        const track = {
          $type: "sh.diffuse.output.track",
          id,
          stats,
          tags,
          uri: buildURI(bucket, l.key),
        };

        return track;
      });

    // If a bucket didn't have any tracks,
    // keep a placeholder track so the bucket gets
    // picked up as a source.
    if (!tracks.length) {
      tracks = [{
        $type: "sh.diffuse.output.track",
        id: crypto.randomUUID(),
        kind: "placeholder",
        uri: buildURI(bucket),
      }];
    }

    return tracks;
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
    /** @type {any} */ (method?.toUpperCase() ?? "GET"),
    parsed.path,
  );

  return { expiresAt: expiresAtSeconds, url };
}

////////////////////////////////////////////
// ADDITIONAL ACTIONS
////////////////////////////////////////////

/**
 * @returns {Demo}
 */
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

  const uri = buildURI(bucket);

  /** @type {Track} */
  const track = {
    $type: "sh.diffuse.output.track",
    id: crypto.randomUUID(),
    kind: "placeholder",
    uri,
  };

  return {
    bucket,
    track,
  };
}

////////////////////////////////////////////
// ⚡️
////////////////////////////////////////////

ostiary((context) => {
  // Setup RPC

  rpc(context, {
    consult,
    detach,
    groupConsult,
    list,
    resolve,

    // Additional actions
    demo,
  });
});
