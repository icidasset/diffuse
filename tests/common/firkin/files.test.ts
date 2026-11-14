// @ts-nocheck -- the fake client intentionally implements only the subset it uses

import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import {
  buildURI,
  decodeFileName,
  deleteFile,
  encodeFileName,
  FILES_NAMESPACE,
  fileURL,
  getFile,
  listFiles,
  mimeType,
  parseURI,
  readFile,
  safeFileName,
  SCHEME,
  SOURCE_URI,
  uploadFile,
} from "~/common/firkin/files.js";

/**
 * @param {Record<string, string>} [initial]
 */
function fakeClient(initial = {}) {
  const names = new Map(Object.entries(initial));
  const blobs = new Map();
  const deleted = [];

  return {
    names,
    blobs,
    deleted,

    /** @param {string} namespace */
    async registryNames(namespace) {
      if (namespace !== FILES_NAMESPACE || names.size === 0) return null;
      return Object.fromEntries(names);
    },

    /** @param {string} namespace @param {string} name */
    async registryGet(namespace, name) {
      if (namespace !== FILES_NAMESPACE) return null;
      return names.get(name) ?? null;
    },

    /** @param {string} namespace @param {string} name @param {string} cid */
    async registrySet(namespace, name, cid) {
      if (namespace !== FILES_NAMESPACE) throw new Error("wrong namespace");
      names.set(name, cid);
    },

    /** @param {string} namespace @param {string} name */
    async registryDelete(namespace, name) {
      if (namespace !== FILES_NAMESPACE) throw new Error("wrong namespace");
      names.delete(name);
    },

    /** @param {Uint8Array} bytes */
    async upload(bytes) {
      const cid = `bafk-${[...bytes].join("")}`;
      blobs.set(cid, bytes);
      return { cid, deduplicated: false };
    },

    /** @param {string} cid */
    async delete(cid) {
      blobs.delete(cid);
      deleted.push(cid);
    },

    /** @param {string} cid */
    async get(cid) {
      return blobs.get(cid);
    },

    /** @param {string} cid @param {{ expireIn?: number }} [_opts] */
    async readUrl(cid, _opts) {
      return `https://firkin.test/v1/blob/${cid}?token=x`;
    },
  };
}

describe("common/firkin/files", () => {
  it("builds and parses file URIs", () => {
    expect(SCHEME).toBe("firkin");
    expect(SOURCE_URI).toBe("firkin://me");

    const uri = buildURI("Artist - Title.mp3");
    expect(uri).toBe("firkin://me/files/Artist%20-%20Title.mp3");
    expect(parseURI(uri)).toEqual({ name: "Artist - Title.mp3" });

    expect(parseURI("firkin://me/other/x.mp3")).toBeNull();
    expect(parseURI("firkin://me/files/a/b.mp3")).toBeNull();
    expect(parseURI("firkin://me/files/%E0%A4%A")).toBeNull();
  });

  it("encodes arbitrary file names as registry names", () => {
    for (
      const name of [
        "Artist - Title.mp3",
        "Björk — Jóga.flac",
        "a/b (live) [remix].ogg",
        "quote's & hash#.wav",
      ]
    ) {
      const encoded = encodeFileName(name);
      expect(encoded).toMatch(/^[A-Za-z0-9._~@+:-]+$/);
      expect(decodeFileName(encoded)).toBe(name);
    }
  });

  it("guesses audio mime types from the file name", () => {
    expect(mimeType("a.mp3")).toBe("audio/mpeg");
    expect(mimeType("a.flac")).toBe("audio/flac");
    expect(mimeType("a.unknown")).toBe("application/octet-stream");
  });

  it("sanitizes browser-provided file names", () => {
    expect(safeFileName("Artist - Title.mp3")).toBe("Artist - Title.mp3");
    expect(safeFileName("../../etc/passwd")).toBe("passwd");
    expect(safeFileName("\u0000")).toBe("audio");
  });

  it("lists registered files by their decoded names", async () => {
    const rb = fakeClient({
      [encodeFileName("a.mp3")]: "bafk-a",
      [encodeFileName("b.flac")]: "bafk-b",
    });

    const files = await listFiles(/** @type {any} */ (rb));

    expect(files).toEqual([
      { name: "a.mp3", cid: "bafk-a" },
      { name: "b.flac", cid: "bafk-b" },
    ]);
  });

  it("resolves, reads and links one file", async () => {
    const rb = fakeClient({ [encodeFileName("a.mp3")]: "bafk-a" });
    rb.blobs.set("bafk-a", new Uint8Array([1, 2, 3]));

    const file = await getFile(/** @type {any} */ (rb), "a.mp3");
    const bytes = await readFile(/** @type {any} */ (rb), "a.mp3");
    const url = await fileURL(/** @type {any} */ (rb), "a.mp3", 60);

    expect(file).toEqual({ name: "a.mp3", cid: "bafk-a" });
    expect([...bytes]).toEqual([1, 2, 3]);
    expect(url).toBe("https://firkin.test/v1/blob/bafk-a?token=x");
  });

  it("uploads a file and replaces the pointer", async () => {
    const rb = fakeClient({ [encodeFileName("a.mp3")]: "bafk-old" });
    rb.blobs.set("bafk-old", new Uint8Array([9]));

    const result = await uploadFile(
      /** @type {any} */ (rb),
      "a.mp3",
      new Uint8Array([4, 5]),
    );

    expect(result.cid).toBe("bafk-45");
    expect(rb.names.get(encodeFileName("a.mp3"))).toBe("bafk-45");
    expect(rb.deleted).toEqual(["bafk-old"]);
  });

  it("deletes a file and detaches its blob when unreferenced", async () => {
    const rb = fakeClient({ [encodeFileName("a.mp3")]: "bafk-a" });
    rb.blobs.set("bafk-a", new Uint8Array([1]));

    await deleteFile(/** @type {any} */ (rb), "a.mp3");

    expect(rb.names.has(encodeFileName("a.mp3"))).toBe(false);
    expect(rb.deleted).toEqual(["bafk-a"]);
  });

  it("keeps a shared blob when another file name references it", async () => {
    const rb = fakeClient({
      [encodeFileName("a.mp3")]: "bafk-shared",
      [encodeFileName("copy.mp3")]: "bafk-shared",
    });
    rb.blobs.set("bafk-shared", new Uint8Array([1]));

    await deleteFile(/** @type {any} */ (rb), "a.mp3");

    expect(rb.deleted).toEqual([]);
    expect(rb.names.get(encodeFileName("copy.mp3"))).toBe("bafk-shared");
  });
});
