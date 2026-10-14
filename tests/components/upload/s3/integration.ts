import { afterAll, beforeAll, describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import type { Bucket } from "@specs/components/input/s3/types.d.ts";
import * as Worker from "~/components/upload/s3/worker.js";
import { bucketAndKeyFromURI } from "~/components/upload/s3/common.js";
import { buildURI, parseURI } from "~/components/input/s3/common.js";

/**
 * Integration tests for the s3 upload worker that verify its S3 API usage
 * (upload, delete, consult) against an in-memory mock S3 endpoint.
 *
 * The worker talks to S3 through `@bradenmacdonald/s3-lite-client`, which
 * performs its HTTP requests using the global `fetch`. We intercept `fetch`
 * and respond with S3 API responses (`ListBucketResult` / `Error`), so no
 * real network, endpoints or credentials are involved.
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

function registerMockBucket(
  bucket: Bucket,
  objects: StoredObject[],
): MockBucket {
  const mock: MockBucket = {
    bucketName: bucket.bucketName,
    host: bucket.host,
    objects,
  };
  mockBuckets.push(mock);
  return mock;
}

const ACCESS_KEY = "AKIAINTEGRATIONTEST";
const SECRET_KEY = "integration-secret";

function makeBucket(bucketName: string, path = "/"): Bucket {
  return {
    accessKey: ACCESS_KEY,
    secretKey: SECRET_KEY,
    bucketName,
    // Unique host per bucket: the consult cache is keyed by
    // `accessKey:secretKey@host`, so shared hosts would leak cached
    // availability between tests.
    host: `${bucketName}.s3.example.com`,
    path,
    region: "us-east-1",
  };
}

function makeFile(name: string, contents: string, type = "audio/mpeg"): File {
  return new File([new TextEncoder().encode(contents)], name, { type });
}

let originalFetch: typeof globalThis.fetch;

beforeAll(() => {
  originalFetch = globalThis.fetch;

  globalThis.fetch =
    (async (input: string | URL | Request, init?: RequestInit) => {
      const url = new URL(
        typeof input === "string"
          ? input
          : input instanceof URL
          ? input.href
          : input.url,
      );

      // The client uses virtual-host style requests: https://bucket.host/...
      const mock = mockBuckets.find((b) =>
        url.hostname === `${b.bucketName}.${b.host}`
      );

      if (!mock) {
        return new Response(
          errorXml("NoSuchBucket", "The specified bucket does not exist."),
          { status: 404, headers: { "content-type": "application/xml" } },
        );
      }

      if (mock.failRequests) {
        throw new TypeError("fetch failed");
      }

      // ListObjectsV2 — used by bucketExists (consult).
      if (url.pathname === "/") {
        const prefix = url.searchParams.get("prefix") ?? "";
        const keys = mock.objects.filter((o) => o.key.startsWith(prefix))
          .map((o) => o.key);
        return new Response(listResultXml(mock.bucketName, keys), {
          status: 200,
          headers: { "content-type": "application/xml" },
        });
      }

      const key = url.pathname.replace(/^\//, "");

      // PutObject — store the body under the key.
      if (init?.method === "PUT") {
        const bytes = new Uint8Array(
          await new Response(init.body).arrayBuffer(),
        );
        const existing = mock.objects.findIndex((o) => o.key === key);
        const stored = { key, bytes };
        if (existing >= 0) mock.objects[existing] = stored;
        else mock.objects.push(stored);
        return new Response("", {
          status: 200,
          headers: { etag: '"d41d8cd98f00b204e9800998ecf8427e"' },
        });
      }

      // DeleteObject — remove the object under the key.
      if (init?.method === "DELETE") {
        const existing = mock.objects.findIndex((o) => o.key === key);
        if (existing >= 0) mock.objects.splice(existing, 1);
        // 204 is a null-body status; the body must be null.
        return new Response(null, { status: 204 });
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

describe("components/upload/s3 (integration)", () => {
  it("upload stores the file under the bucket's configured path and returns a parseable URI", async () => {
    // A bucket with a path prefix — uploads must land inside it so the
    // matching input component (which lists that prefix) picks them up.
    const bucket = makeBucket("upload-prefixed", "/Music");
    registerMockBucket(bucket, []);

    const file = makeFile("bafkqaaa.mp3", "fake-audio-bytes");
    const fileUri = await Worker.upload({
      file,
      uri: buildURI(bucket),
    });

    expect(bucketAndKeyFromURI(fileUri).key).toBe("Music/bafkqaaa.mp3");

    // The returned URI parses as an s3 file URI for the account.
    const parsed = parseURI(fileUri);
    expect(parsed).not.toBe(undefined);
    expect(parsed?.bucket.bucketName).toBe("upload-prefixed");
    expect(parsed?.bucket.path).toBe("/Music");
  });

  it("upload places files at the bucket root when no path is configured", async () => {
    const bucket = makeBucket("upload-root");
    const mock = registerMockBucket(bucket, []);

    const file = makeFile("bafkqaaa.flac", "fake-audio-bytes", "audio/flac");
    const fileUri = await Worker.upload({ file, uri: buildURI(bucket) });

    expect(bucketAndKeyFromURI(fileUri).key).toBe("bafkqaaa.flac");

    // The stored object's content matches the file, and its content type
    // was passed along (so presigned GET responses serve a useful type).
    expect(mock.objects.length).toBe(1);
    expect(new TextDecoder().decode(mock.objects[0].bytes)).toBe(
      "fake-audio-bytes",
    );
  });

  it("upload overwrites an existing object with the same key (idempotent re-upload)", async () => {
    const bucket = makeBucket("upload-overwrite");
    const mock = registerMockBucket(bucket, []);

    await Worker.upload({
      file: makeFile("bafkqaaa.mp3", "first-version"),
      uri: buildURI(bucket),
    });
    await Worker.upload({
      file: makeFile("bafkqaaa.mp3", "second-version"),
      uri: buildURI(bucket),
    });

    expect(mock.objects.length).toBe(1);
    expect(new TextDecoder().decode(mock.objects[0].bytes)).toBe(
      "second-version",
    );
  });

  it("upload throws a descriptive error when the bucket is unavailable", async () => {
    const bucket = makeBucket("upload-failing");
    const mock = registerMockBucket(bucket, []);
    mock.failRequests = true;

    await expect(Worker.upload({
      file: makeFile("bafkqaaa.mp3", "fake-audio-bytes"),
      uri: buildURI(bucket),
    })).rejects.toThrow(
      /Failed to upload "bafkqaaa.mp3" to S3 bucket "upload-failing"/,
    );
  });

  it("delete removes the object, including for buckets with a path prefix", async () => {
    // This URI shape is what the matching input component's `list()` stores on
    // tracks: the object key sits in the URI path. The delete must read it
    // from there — not from `parseURI`'s path, which prepends the bucket's
    // configured path and would double the prefix.
    const bucket = makeBucket("delete-prefixed", "/Music");
    const mock = registerMockBucket(bucket, [
      { key: "Music/bafkqaaa.mp3", bytes: new Uint8Array(1) },
    ]);

    const fileUri = buildURI(bucket, "Music/bafkqaaa.mp3");
    await Worker.deleteFn(fileUri);

    expect(mock.objects.length).toBe(0);
  });

  it("delete throws for an account URI without an object key", async () => {
    const bucket = makeBucket("delete-account");
    registerMockBucket(bucket, []);

    await expect(Worker.deleteFn(buildURI(bucket))).rejects.toThrow(
      /Invalid S3 file URI/,
    );
  });

  it("delete throws for an unparseable URI", async () => {
    await expect(Worker.deleteFn("s3://notvalid")).rejects.toThrow(
      /Invalid S3 URI/,
    );
  });

  it("consult returns yes for an existing bucket", async () => {
    const bucket = makeBucket("consult-yes");
    registerMockBucket(bucket, []);

    const result = await Worker.consult(buildURI(bucket));
    expect(result.supported).toBe(true);
    if (result.supported) {
      expect(result.consult).toBe("yes");
    }
  });

  it("consult returns no for a bucket that does not exist", async () => {
    const bucket = makeBucket("consult-no");
    // No bucket registered — the mock endpoint answers 404 NoSuchBucket.

    const result = await Worker.consult(buildURI(bucket));
    expect(result.supported).toBe(true);
    if (result.supported) {
      expect(result.consult).toBe("no");
    }
  });

  it("consult returns undetermined for scheme only and invalid URIs", async () => {
    const schemeResult = await Worker.consult("s3");
    expect(schemeResult.supported).toBe(true);
    if (schemeResult.supported) {
      expect(schemeResult.consult).toBe("undetermined");
    }

    const invalidResult = await Worker.consult("s3://notvalid");
    expect(invalidResult.supported).toBe(true);
    if (invalidResult.supported) {
      expect(invalidResult.consult).toBe("undetermined");
    }
  });

  it("createSource is rejected — S3 buckets are connected via the Connect page", async () => {
    await expect(Worker.createSource({
      scheme: "s3",
      refreshToken: "irrelevant",
      directoryPath: "/Diffuse",
    })).rejects.toThrow(/Connect page/);
  });
});
