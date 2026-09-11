import { afterAll, beforeAll, describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import type { Bucket } from "@specs/components/input/s3/types.d.ts";
import type { Track } from "~/definitions/types.d.ts";
import * as Worker from "~/components/input/s3/worker.js";
import { buildURI, parseURI } from "~/components/input/s3/common.js";

/**
 * Integration tests for the s3 input that verify the worker's S3 API usage
 * (list, consult, resolve, artwork, detach) against an in-memory mock S3
 * endpoint.
 *
 * The worker talks to S3 through `@bradenmacdonald/s3-lite-client`, which
 * performs its HTTP requests using the global `fetch`. We intercept `fetch`
 * and respond with S3 API XML (`ListBucketResult` / `Error`), so no real
 * network, endpoints or credentials are involved.
 */

type StoredObject = { key: string; bytes: Uint8Array<ArrayBuffer> };

type MockBucket = {
  bucketName: string;
  host: string;
  objects: StoredObject[];
  /** When set, the mock endpoint fails all requests (transient network blip). */
  failRequests?: boolean;
};

const mockBuckets: MockBucket[] = [];

function listResultXml(bucketName: string, keys: string[]): string {
  const contents = keys.map((key) =>
    `  <Contents>
    <Key>${key}</Key>
    <LastModified>2026-01-01T00:00:00.000Z</LastModified>
    <ETag>"a1b2c3d4"</ETag>
    <Size>1024</Size>
    <StorageClass>STANDARD</StorageClass>
  </Contents>`
  ).join("\n");

  return `<?xml version="1.0" encoding="UTF-8"?>
<ListBucketResult xmlns="http://s3.amazonaws.com/doc/2006-03-01/">
  <Name>${bucketName}</Name>
  <Prefix></Prefix>
  <KeyCount>${keys.length}</KeyCount>
  <MaxKeys>1000</MaxKeys>
  <IsTruncated>false</IsTruncated>
${contents}
</ListBucketResult>`;
}

function errorXml(code: string, message: string): string {
  return `<?xml version="1.0" encoding="UTF-8"?>
<Error>
  <Code>${code}</Code>
  <Message>${message}</Message>
</Error>`;
}

function registerMockBucket(bucket: Bucket, objects: StoredObject[]): MockBucket {
  const mock: MockBucket = { bucketName: bucket.bucketName, host: bucket.host, objects };
  mockBuckets.push(mock);
  return mock;
}

const ACCESS_KEY = "AKIAINTEGRATIONTEST";
const SECRET_KEY = "integration-secret";

function makeBucket(host: string, bucketName = "music"): Bucket {
  return {
    accessKey: ACCESS_KEY,
    secretKey: SECRET_KEY,
    bucketName,
    host,
    path: "/",
    region: "us-east-1",
  };
}

let originalFetch: typeof globalThis.fetch;

beforeAll(() => {
  originalFetch = globalThis.fetch;

  globalThis.fetch = (async (input: string | URL | Request, _init?: RequestInit) => {
    const url = new URL(
      typeof input === "string" ? input : input instanceof URL ? input.href : input.url,
    );

    // The client uses virtual-host style requests: https://bucket.host/...
    const mock = mockBuckets.find((b) => url.hostname === `${b.bucketName}.${b.host}`);

    if (!mock) {
      return new Response(
        errorXml("NoSuchBucket", "The specified bucket does not exist."),
        { status: 404, headers: { "content-type": "application/xml" } },
      );
    }

    if (mock.failRequests) {
      throw new TypeError("fetch failed");
    }

    // ListObjectsV2 — used by list (no prefix) and artwork (directory prefix).
    if (url.pathname === "/") {
      const prefix = url.searchParams.get("prefix") ?? "";
      const keys = mock.objects.filter((o) => o.key.startsWith(prefix))
        .map((o) => o.key);
      return new Response(listResultXml(mock.bucketName, keys), {
        status: 200,
        headers: { "content-type": "application/xml" },
      });
    }

    // Object fetch through a presigned URL (artwork).
    const key = url.pathname.replace(/^\//, "");
    const object = mock.objects.find((o) => o.key === key);
    if (object) {
      return new Response(object.bytes, { status: 200 });
    }

    return new Response(
      errorXml("NoSuchKey", "The specified key does not exist."),
      { status: 404, headers: { "content-type": "application/xml" } },
    );
  }) as typeof globalThis.fetch;
});

afterAll(() => {
  globalThis.fetch = originalFetch;
});

describe("components/input/s3 (integration)", () => {
  it("list returns tracks for the audio objects in a bucket, preserving cached metadata", async () => {
    const bucket = makeBucket("list.example.com");
    registerMockBucket(bucket, [
      { key: "albums-a/track1.mp3", bytes: new Uint8Array(1) },
      { key: "albums-a/track2.flac", bytes: new Uint8Array(1) },
      { key: "albums-a/cover.jpg", bytes: new Uint8Array(1) },
      { key: "albums-a/notes.txt", bytes: new Uint8Array(1) },
    ]);

    const cached: Track[] = [{
      $type: "sh.diffuse.output.track",
      id: "c1",
      uri: buildURI(bucket, "/albums-a/track1.mp3"),
      stats: { duration: 183000 },
      tags: { title: "Cached Song" },
    }];

    const tracks = await Worker.list(cached);

    // Only audio objects become tracks; non-audio objects are filtered out.
    expect(tracks.length).toBe(2);
    const paths = tracks.map((t) => parseURI(t.uri)?.path);
    expect(paths).toContain("albums-a/track1.mp3");
    expect(paths).toContain("albums-a/track2.flac");
    expect(paths.some((p) => p?.includes("cover.jpg"))).toBe(false);
    expect(paths.some((p) => p?.includes("notes.txt"))).toBe(false);

    // Cached stats/tags come back on the refreshed track.
    const cachedTrack = tracks.find((t) => t.id === "c1");
    expect(cachedTrack).toBeDefined();
    if (cachedTrack) {
      expect(cachedTrack.stats).toEqual({ duration: 183000 });
      expect(cachedTrack.tags).toEqual({ title: "Cached Song" });
    }
  });

  it("list returns a placeholder track when the bucket has no audio objects", async () => {
    const bucket = makeBucket("empty.example.com");
    registerMockBucket(bucket, [{ key: "readme.txt", bytes: new Uint8Array(1) }]);

    const tracks = await Worker.list([{
      $type: "sh.diffuse.output.track",
      id: "p1",
      kind: "placeholder",
      uri: buildURI(bucket),
    }]);

    expect(tracks.length).toBe(1);
    expect(tracks[0].kind).toBe("placeholder");
    expect(parseURI(tracks[0].uri)?.path).toBe("");
  });

  it("resolve returns a presigned GET URL with a one-week expiry", async () => {
    const bucket = makeBucket("resolve.example.com");
    const resolved = await Worker.resolve({
      uri: buildURI(bucket, "/albums-a/track1.mp3"),
    });

    expect(resolved).not.toBe(undefined);
    if (resolved && "url" in resolved) {
      expect(resolved.url).toContain(`https://${bucket.bucketName}.${bucket.host}`);
      expect(resolved.url).toContain("/albums-a/track1.mp3");
      expect(resolved.url).toContain("X-Amz-Expires=604800");
      expect(resolved.url).toContain("X-Amz-Credential=");
      expect(resolved.url).toContain("X-Amz-Signature=");

      const nowSeconds = Math.round(Date.now() / 1000);
      const weekSeconds = 7 * 24 * 60 * 60;
      expect(resolved.expiresAt).toBeGreaterThan(nowSeconds + weekSeconds - 10);
      expect(resolved.expiresAt).toBeLessThanOrEqual(nowSeconds + weekSeconds);
    }
  });

  it("resolve returns undefined for an unparseable URI", async () => {
    const resolved = await Worker.resolve({
      uri: "s3://no-credentials.example.com/track.mp3?bucketName=music&region=us-east-1",
    });
    expect(resolved).toBe(undefined);
  });

  it("consult returns yes for an existing bucket", async () => {
    const bucket = makeBucket("consult-yes.example.com");
    registerMockBucket(bucket, []);

    const result = await Worker.consult(buildURI(bucket));
    expect(result.supported).toBe(true);
    if (result.supported) {
      expect(result.consult).toBe("yes");
    }
  });

  it("consult returns no for a bucket that does not exist", async () => {
    const bucket = makeBucket("consult-no.example.com");
    // No bucket registered — the mock endpoint answers 404 NoSuchBucket.

    const result = await Worker.consult(buildURI(bucket));
    expect(result.supported).toBe(true);
    if (result.supported) {
      expect(result.consult).toBe("no");
    }
  });

  it("consult retries after an inconclusive network failure (unsure is not cached)", async () => {
    const bucket = makeBucket("consult-retry.example.com");
    const mock = registerMockBucket(bucket, []);

    // First consult: the endpoint errors out (transient network blip).
    mock.failRequests = true;
    const first = await Worker.consult(buildURI(bucket));
    expect(first.supported).toBe(true);
    if (first.supported) {
      // An inconclusive consult is surfaced as "no" to callers...
      expect(first.consult).toBe("no");
    }

    // Second consult: endpoint healthy again. The inconclusive result was
    // *not* cached, so this consults the endpoint afresh instead of waiting
    // out the consult TTL.
    mock.failRequests = false;
    const second = await Worker.consult(buildURI(bucket));
    expect(second.supported).toBe(true);
    if (second.supported) {
      expect(second.consult).toBe("yes");
    }
  });

  it("groupConsult groups URIs by bucket with availability", async () => {
    const bucket = makeBucket("group.example.com");
    registerMockBucket(bucket, []);

    const uris = [
      buildURI(bucket, "/a/track1.mp3"),
      buildURI(bucket, "/a/track2.mp3"),
    ];
    const result = await Worker.groupConsult(uris);

    const key = `s3://${bucket.accessKey}:${bucket.secretKey}@${bucket.host}`;
    expect(Object.keys(result)).toEqual([key]);
    expect(result[key].available).toBe("yes");
    expect(result[key].scheme).toBe("s3");
    expect(result[key].uris).toEqual(uris);
  });

  it("groupConsult reports unavailable for an unknown bucket", async () => {
    const bucket = makeBucket("group-missing.example.com");
    const uri = buildURI(bucket, "/x.mp3");
    const result = await Worker.groupConsult([uri]);

    const key = `s3://${bucket.accessKey}:${bucket.secretKey}@${bucket.host}`;
    expect(Object.keys(result)).toEqual([key]);
    expect(result[key].available).toBe("no");
    expect(result[key].reason).toBe("Bucket unavailable");
  });

  it("artwork returns the cover image from the track's directory", async () => {
    const bucket = makeBucket("artwork.example.com");
    registerMockBucket(bucket, [
      { key: "albums-a/track1.mp3", bytes: new Uint8Array(1) },
      { key: "albums-a/cover.jpg", bytes: new TextEncoder().encode("fake-jpeg-bytes") },
    ]);

    const art = await Worker.artwork(buildURI(bucket, "/albums-a/track1.mp3"));
    expect(art).not.toBe(null);
    if (art) {
      expect(new TextDecoder().decode(art)).toBe("fake-jpeg-bytes");
    }
  });

  it("artwork returns null when there are no images in the directory", async () => {
    const noImages = makeBucket("artwork-none.example.com");
    registerMockBucket(noImages, [
      { key: "albums-a/track1.mp3", bytes: new Uint8Array(1) },
    ]);
    expect(await Worker.artwork(buildURI(noImages, "/albums-a/track1.mp3"))).toBe(null);

    // Images nested in subdirectories don't count — only the track's own directory.
    const nested = makeBucket("artwork-nested.example.com");
    registerMockBucket(nested, [
      { key: "albums-a/track1.mp3", bytes: new Uint8Array(1) },
      { key: "albums-a/sub/cover.jpg", bytes: new Uint8Array(1) },
    ]);
    expect(await Worker.artwork(buildURI(nested, "/albums-a/track1.mp3"))).toBe(null);
  });

  it("detach with a specific bucket URI removes only that bucket's tracks", async () => {
    const bucketA = makeBucket("detach-a.example.com");
    const bucketB = makeBucket("detach-b.example.com");

    const tracks: Track[] = [
      { $type: "sh.diffuse.output.track", id: "1", uri: buildURI(bucketA, "/x/a.mp3") },
      { $type: "sh.diffuse.output.track", id: "2", uri: buildURI(bucketB, "/x/b.mp3") },
    ];
    const remaining = await Worker.detach({
      fileUriOrScheme: buildURI(bucketA, "/x/a.mp3"),
      tracks,
    });

    expect(remaining.length).toBe(1);
    expect(remaining[0].id).toBe("2");
  });

  it("detach with the s3 scheme removes all s3 tracks", async () => {
    const bucket = makeBucket("detach-scheme.example.com");
    const tracks: Track[] = [
      { $type: "sh.diffuse.output.track", id: "1", uri: buildURI(bucket, "/a.mp3") },
      { $type: "sh.diffuse.output.track", id: "2", uri: buildURI(bucket, "/b.mp3") },
    ];
    const remaining = await Worker.detach({ fileUriOrScheme: "s3", tracks });
    expect(remaining.length).toBe(0);
  });
});