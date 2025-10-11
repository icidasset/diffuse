import { S3Client } from "@bradenmacdonald/s3-lite-client";
import * as IDB from "idb-keyval";
import * as URI from "uri-js";
import QS from "query-string";

import type { Track } from "@applets/core/types.d.ts";
import { ENCODINGS, IDB_BUCKETS, SCHEME } from "./constants";
import type { Bucket } from "./types";

////////////////////////////////////////////
// 🛠️
////////////////////////////////////////////
export function bucketsFromTracks(tracks: Track[]) {
  const acc: Record<string, Bucket> = {};

  tracks.forEach((track: Track) => {
    const parsed = parseURI(track.uri);
    if (!parsed) return;

    const id = bucketId(parsed.bucket);
    if (acc[id]) return;

    acc[id] = parsed.bucket;
  });

  return acc;
}

export function bucketId(bucket: Bucket) {
  return `${bucket.accessKey}:${bucket.secretKey}@${bucket.host}`;
}

export function buildURI(bucket: Bucket, path: string) {
  return URI.serialize({
    scheme: SCHEME,
    userinfo: `${bucket.accessKey}:${bucket.secretKey}`,
    host: bucket.host.replace(/^https?:\/\//, ""),
    path: path,
    query: QS.stringify({
      bucketName: bucket.bucketName,
      bucketPath: bucket.path,
      region: bucket.region,
    }),
  });
}

export async function consultBucket(bucket: Bucket) {
  const client = createClient(bucket);
  return await client.bucketExists(bucket.bucketName);
}

export function createClient(bucket: Bucket) {
  return new S3Client({
    bucket: bucket.bucketName,
    endPoint: `http${bucket.host.startsWith("localhost") ? "" : "s"}://${bucket.host}`,
    region: bucket.region,
    pathStyle: false,
    accessKey: bucket.accessKey,
    secretKey: bucket.secretKey,
  });
}

export function encodeAwsUriComponent(a: string) {
  return encodeURIComponent(a).replace(
    /(\+|!|"|#|\$|&|'|\(|\)|\*|\+|,|:|;|=|\?|@)/gim,
    (match) => (ENCODINGS as any)[match] ?? match,
  );
}

export function groupTracksByBucket(tracks: Track[]) {
  const acc: Record<string, { bucket: Bucket; tracks: Track[] }> = {};

  tracks.forEach((track: Track) => {
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

export async function loadBuckets(): Promise<Record<string, Bucket>> {
  const i = await IDB.get(IDB_BUCKETS);
  return i ? i : {};
}

export function parseURI(uriString: string): { bucket: Bucket; path: string } | undefined {
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

  const path = (bucket.path.replace(/\/$/, "") + URI.unescapeComponent(uri.path || "")).replace(
    /^\//,
    "",
  );

  return { bucket, path };
}

export async function saveBuckets(items: Record<string, Bucket>) {
  await IDB.set(IDB_BUCKETS, items);
}
