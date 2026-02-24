import { S3Client } from "@bradenmacdonald/s3-lite-client";
import * as IDB from "idb-keyval";
import * as URI from "fast-uri";
import QS from "query-string";

import { cachedConsult } from "@components/input/common.js";
import { ENCODINGS, IDB_BUCKETS, SCHEME } from "./constants.js";

/**
 * @import { Track } from "@definitions/types.d.ts";
 * @import { Bucket } from "./types.d.ts";
 */

////////////////////////////////////////////
// 🛠️
////////////////////////////////////////////

/**
 * @param {Track[]} tracks
 */
export function bucketsFromTracks(tracks) {
  /** @type {Record<string, Bucket>} */
  const acc = {};

  tracks.forEach((track) => {
    const parsed = parseURI(track.uri);
    if (!parsed) return;

    const id = bucketId(parsed.bucket);
    if (acc[id]) return;

    acc[id] = parsed.bucket;
  });

  return acc;
}

/**
 * @param {Bucket} bucket
 */
export function bucketId(bucket) {
  return `${bucket.accessKey}:${bucket.secretKey}@${bucket.host}`;
}

/**
 * @param {Bucket} bucket
 * @param {string} [path]
 */
export function buildURI(bucket, path) {
  return URI.serialize({
    scheme: SCHEME,
    userinfo: `${bucket.accessKey}:${bucket.secretKey}`,
    host: bucket.host.replace(/^\w+:\/\//, ""),
    path: path,
    query: QS.stringify({
      bucketName: bucket.bucketName,
      bucketPath: bucket.path,
      region: bucket.region,
    }),
  });
}

/**
 * @param {Bucket} bucket
 */
export async function consultBucket(bucket) {
  const client = createClient(bucket);
  return await client.bucketExists(bucket.bucketName);
}

export const consultBucketCached = cachedConsult(consultBucket, bucketId);

/**
 * @param {Bucket} bucket
 */
export function createClient(bucket) {
  return new S3Client({
    bucket: bucket.bucketName,
    endPoint: `http${
      bucket.host.startsWith("localhost") ? "" : "s"
    }://${bucket.host}`,
    region: bucket.region,
    pathStyle: false,
    accessKey: bucket.accessKey,
    secretKey: bucket.secretKey,
  });
}

/**
 * @param {string} a
 */
export function encodeAwsUriComponent(a) {
  return encodeURIComponent(a).replace(
    /(\+|!|"|#|\$|&|'|\(|\)|\*|\+|,|:|;|=|\?|@)/gim,
    (match) => /** @type {any} */ (ENCODINGS)[match] ?? match,
  );
}

/**
 * @param {Track[]} tracks
 */
export function groupTracksByBucket(tracks) {
  /** @type {Record<string, { bucket: Bucket; tracks: Track[] }>} */
  const acc = {};

  tracks.forEach((track) => {
    const parsed = parseURI(track.uri);
    if (!parsed) return acc;

    const id = bucketId(parsed.bucket);

    if (acc[id]) {
      acc[id].tracks.push(track);
    } else {
      acc[id] = { bucket: parsed.bucket, tracks: [track] };
    }
  });

  return acc;
}

/**
 * @param {string[]} uris
 */
export function groupUrisByBucket(uris) {
  /** @type {Record<string, { bucket: Bucket; uris: string[] }>} */
  const acc = {};

  uris.forEach((uri) => {
    const parsed = parseURI(uri);
    if (!parsed) return acc;

    const id = bucketId(parsed.bucket);

    if (acc[id]) {
      acc[id].uris.push(uri);
    } else {
      acc[id] = { bucket: parsed.bucket, uris: [uri] };
    }
  });

  return acc;
}

/**
 * @returns {Promise<Record<string, Bucket>>}
 */
export async function loadBuckets() {
  const i = await IDB.get(IDB_BUCKETS);
  return i ? i : {};
}

/**
 * @param {string} uriString
 * @returns {{ bucket: Bucket; path: string } | undefined}
 */
export function parseURI(uriString) {
  const uri = URI.parse(uriString);
  if (uri.scheme !== SCHEME) return undefined;
  if (!uri.host) return undefined;

  const [accessKey, secretKey] = uri.userinfo?.split(":") ?? [];
  if (!accessKey || !secretKey) return undefined;

  const qs = QS.parse(uri.query || "");

  const bucket = {
    accessKey,
    bucketName: typeof qs.bucketName === "string" ? qs.bucketName : "",
    host: uri.host,
    path: qs.bucketPath === "string" ? qs.bucketPath : "/",
    region: typeof qs.region === "string" ? qs.region : "",
    secretKey,
  };

  const path =
    (bucket.path.replace(/\/$/, "") + decodeURIComponent(uri.path || ""))
      .replace(
        /^\//,
        "",
      );

  return { bucket, path };
}

/**
 * @param {Record<string, Bucket>} items
 */
export async function saveBuckets(items) {
  await IDB.set(IDB_BUCKETS, items);
}
