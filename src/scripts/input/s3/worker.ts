import type { Consult, ConsultGrouping, GroupConsult, Track } from "@applets/core/types.d.ts";
import { isAudioFile } from "@scripts/input/common";
import {
  bucketId,
  bucketsFromTracks,
  buildURI,
  consultBucket,
  createClient,
  groupTracksByBucket,
  loadBuckets,
  parseURI,
} from "./common";
import { provide, transfer } from "@scripts/common";
import { SCHEME } from "./constants";

////////////////////////////////////////////
// TASKS
////////////////////////////////////////////
const actions = {
  consult,
  contextualize,
  groupConsult,
  list,
  resolve,
};

const { tasks } = provide({ actions, tasks: actions });

export type Actions = typeof actions;
export type Tasks = typeof tasks;

// Tasks

async function consult(fileUriOrScheme: string): Promise<Consult> {
  if (!fileUriOrScheme.includes(":")) return { supported: true, consult: "undetermined" };

  const parsed = parseURI(fileUriOrScheme);
  if (!parsed) return { supported: true, consult: "undetermined" };

  const consult = await consultBucket(parsed.bucket);
  return { supported: true, consult };
}

async function contextualize(tracks: Track[]) {
  return bucketsFromTracks(tracks);
}

async function groupConsult(tracks: Track[]): Promise<GroupConsult> {
  const groups = groupTracksByBucket(tracks);

  const promises = Object.entries(groups).map(async ([bucketId, { bucket, tracks }]) => {
    const available = await consultBucket(bucket);
    const grouping: ConsultGrouping = available
      ? { available, tracks }
      : { available, reason: "Bucket unavailable", tracks };

    return {
      key: `${SCHEME}:${bucketId}`,
      grouping,
    };
  });

  const entries = (await Promise.all(promises)).map((entry) => [entry.key, entry.grouping]);
  const obj = Object.fromEntries(entries);

  return transfer(obj);
}

async function list(cachedTracks: Track[] = []) {
  const cache: Record<string, Record<string, Track>> = {};

  cachedTracks.forEach((t: Track) => {
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

        const track: Track = {
          id,
          stats,
          tags,
          uri: buildURI(bucket, l.key),
        };

        return track;
      });
  });

  const tracks = (await Promise.all(promises)).flat(1);
  return transfer(tracks);
}

async function resolve({ method, uri }: { method: string; uri: string }) {
  const parsed = parseURI(uri);
  if (!parsed) return undefined;

  const expiresInSeconds = 60 * 60 * 24 * 7; // 7 days
  const expiresAtSeconds = Math.round(Date.now() / 1000) + expiresInSeconds;

  const client = createClient(parsed.bucket);
  const url = await client.getPresignedUrl(method.toUpperCase() as any, parsed.path);

  return { expiresAt: expiresAtSeconds, url };
}
