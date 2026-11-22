// @ts-nocheck -- the fake client intentionally implements only the subset it uses

import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import * as DagCBOR from "@ipld/dag-cbor";
import { CID } from "multiformats/cid";
import { ApiError } from "@tokono.ma/firkin";
import { encodeFileName } from "~/common/firkin/files.js";
import {
  cidForBytes,
  parseCarBlocks,
} from "~/vendor/firkin/index.js";

import {
  INLINE_ENTRY_MAX_BYTES,
  MANIFEST_VERSION,
  logName,
  registryNamespace,
} from "~/components/output/raw/firkin/encrypted/incremental-loro/constants.js";
import {
  appendEntry,
  extendLogSignature,
  latestEntry,
  logSignature,
  readEntries,
} from "~/components/output/raw/firkin/encrypted/incremental-loro/log.js";

/**
 * The subset of the Firkin client the log helpers use, backed by maps.
 */
function fakeClient() {
  /** @type {Map<string, Uint8Array>} */
  const blobs = new Map();
  /** @type {Map<string, Map<string, string>>} */
  const registry = new Map();

  const namespace = (/** @type {string} */ ns) => {
    const stored = registry.get(ns);
    if (stored) return stored;

    const created = new Map();
    registry.set(ns, created);
    return created;
  };

  const client = {
    blobs,
    registry,

    /**
     * @param {Uint8Array} bytes
     * @param {{ codec?: number }} [opts]
     */
    async upload(bytes, opts = {}) {
      const cid = cidForBytes(bytes, { codec: opts.codec ?? 0x55 });
      const deduplicated = blobs.has(cid);
      blobs.set(cid, bytes);
      return { cid, deduplicated };
    },

    /** @param {string} cid */
    async get(cid) {
      const stored = blobs.get(cid);
      if (!stored) throw new ApiError("Not found", 404);

      return stored;
    },

    /**
     * @param {string} ns
     * @param {string} name
     * @param {string} cid
     */
    async registrySet(ns, name, cid) {
      namespace(ns).set(name, cid);
    },

    /**
     * Compare-and-swap, like the real `If-Match` write.
     *
     * @param {string} ns
     * @param {string} name
     * @param {string} cid
     * @param {string | null} when
     */
    async registrySetIf(ns, name, cid, when) {
      const stored = namespace(ns);
      const current = stored.get(name) ?? null;

      if (when === null ? current !== null : current !== when) {
        return { ok: false, current };
      }

      stored.set(name, cid);
      return { ok: true, cid };
    },

    /** @param {string} ns; @param {string} name */
    async registryGet(ns, name) {
      return namespace(ns).get(name) ?? null;
    },

    /** @param {string} ns */
    async registryNames(ns) {
      // Current servers answer 200 with an empty object for an empty namespace.
      return Object.fromEntries(namespace(ns));
    },

    /**
     * @param {string} ns
     * @param {string[]} names
     */
    async registryLookup(ns, names) {
      const stored = namespace(ns);

      return Object.fromEntries(
        names.flatMap((name) => {
          const cid = stored.get(name);
          return cid === undefined ? [] : [[name, cid]];
        }),
      );
    },

    /** @param {Uint8Array} car */
    async ingestCar(car) {
      const blocks = parseCarBlocks(car);
      for (const [cid, value] of blocks) blobs.set(cid, value);

      return {
        blocks: blocks.size,
        stored: blocks.size,
        already: 0,
        failed_total: 0,
        failed: [],
      };
    },

    /** @param {string} root */
    async dagPull(root) {
      const found = new Map();
      const queue = [root];
      const seen = new Set();

      while (queue.length > 0) {
        const cid = queue.shift();
        if (cid === undefined || seen.has(cid)) continue;
        seen.add(cid);

        const value = blobs.get(cid);
        if (value === undefined) throw new ApiError("Not found", 404);

        found.set(cid, value);

        let decoded;
        try {
          decoded = DagCBOR.decode(value);
        } catch {
          continue;
        }

        const visit = (node) => {
          const link = CID.asCID(node);
          if (link) {
            queue.push(link.toString());
            return;
          }
          if (Array.isArray(node)) {
            node.forEach(visit);
            return;
          }
          if (node && typeof node === "object") {
            Object.values(node).forEach(visit);
          }
        };
        visit(decoded);
      }

      return found;
    },
  };

  return /** @type {any} */ (client);
}

/**
 * A stand-in for the session signer: a cheap digest over the key and the whole
 * message, so both a forged key and a tampered payload fail verification.
 *
 * @param {string} device
 * @param {Uint8Array} bytes
 */
function digest(device, bytes) {
  let hash = 2166136261;

  for (const byte of new TextEncoder().encode(device)) {
    hash ^= byte;
    hash = Math.imul(hash, 16777619);
  }
  for (const byte of bytes) {
    hash ^= byte;
    hash = Math.imul(hash, 16777619);
  }

  const out = new Uint8Array(4);
  new DataView(out.buffer).setUint32(0, hash >>> 0);

  return out;
}

/** @param {string} device */
function fakeAuth(device) {
  return {
    device,
    sign: async (/** @type {Uint8Array} */ bytes) => digest(device, bytes),
    verify: async (
      /** @type {string} */ signedBy,
      /** @type {Uint8Array} */ signature,
      /** @type {Uint8Array} */ bytes,
    ) => {
      const expected = digest(signedBy, bytes);

      return signature.length === expected.length &&
        signature.every((byte, index) => byte === expected[index]);
    },
  };
}

const deviceA = "a".repeat(64);
const deviceB = "b".repeat(64);

/** @param {string} text */
const bytes = (text) => new TextEncoder().encode(text);

/** @param {Uint8Array} value */
const text = (value) => new TextDecoder().decode(value);

/**
 * An entry's bytes, whether the writer inlined them in the manifest or stored
 * them as a blob.
 *
 * @param {any} rb
 * @param {any} entry
 */
const entryBytes = (rb, entry) => entry.bytes ?? rb.blobs.get(entry.cid);

/**
 * Read a log the way the element does.
 *
 * @param {any} rb
 * @param {string} name
 * @param {ReturnType<typeof fakeAuth>} auth
 * @param {{ floors?: Map<string, number>; onAccept?: (device: string, seq: number) => void }} [options]
 */
const read = (rb, name, auth, options = {}) =>
  readEntries(rb, "ns", name, { auth, ...options });

describe("components/output/raw/firkin/encrypted/incremental-loro/log", () => {
  it("advances a stored signature for one writer", () => {
    expect(extendLogSignature("a=1;b=2", "b", "3")).toBe("a=1;b=3");
    expect(extendLogSignature("a=1", "c", "4")).toBe("a=1;c=4");
    expect(extendLogSignature(undefined, "a", "1")).toBe("a=1");
  });

  it("fingerprints only the refs that make up one log", () => {
    const tracks = encodeFileName("tracks");
    const facets = encodeFileName("facets");
    const settings = encodeFileName("settings");

    const names = {
      [`${tracks}.a`]: "cid-a",
      [`${tracks}.b`]: "cid-b",
      [`${facets}.a`]: "cid-c",
      [`${settings}.a`]: "cid-d",
    };

    expect(logSignature(names, "tracks")).toBe(
      `${tracks}.a=cid-a;${tracks}.b=cid-b`,
    );
    expect(logSignature(names, "facets")).toBe(`${facets}.a=cid-c`);
    expect(logSignature({}, "tracks")).toBe("");
    expect(logSignature(null, "tracks")).toBe("");
  });

  it("unions the entries of every writer", async () => {
    const rb = fakeClient();
    const auth = fakeAuth(deviceA);

    await appendEntry(rb, "ns", "tracks", bytes("a"), { auth });
    await appendEntry(rb, "ns", "tracks", bytes("b"), {
      auth: fakeAuth(deviceB),
    });

    const entries = await read(rb, "tracks", auth);

    expect(entries.length).toBe(2);
    expect(entries.map((entry) => text(entryBytes(rb, entry))).sort())
      .toEqual(["a", "b"]);
  });

  it("returns the entry that was appended last", async () => {
    const rb = fakeClient();
    const auth = fakeAuth(deviceA);

    await appendEntry(rb, "ns", "tracks", bytes("old"), { auth });
    const newest = await appendEntry(rb, "ns", "tracks", bytes("new"), {
      auth,
    });

    const entry = await latestEntry(rb, "ns", "tracks", { auth });

    expect(entry?.cid).toBe(newest.cid);
    expect(text(entryBytes(rb, entry))).toBe("new");
  });

  it("returns nothing for an empty log", async () => {
    const rb = fakeClient();
    const auth = fakeAuth(deviceA);

    expect(await latestEntry(rb, "ns", "tracks", { auth })).toBe(undefined);
    expect(await read(rb, "tracks", auth)).toEqual([]);
  });

  it("appends without reading a ref that doesn't exist yet", async () => {
    const rb = fakeClient();
    const auth = fakeAuth(deviceA);

    // `registryLookup` is the only registry read on the write path.
    let gets = 0;
    const originalGet = rb.registryGet;
    rb.registryGet = (...args) => {
      gets += 1;
      return originalGet(...args);
    };

    const first = await appendEntry(rb, "ns", "tracks", bytes("a"), { auth });

    expect(first.appended).toBe(true);
    expect(gets).toBe(0);
  });

  it("does not append identical bytes twice", async () => {
    const rb = fakeClient();
    const auth = fakeAuth(deviceA);

    const first = await appendEntry(rb, "ns", "tracks", bytes("same"), { auth });
    const second = await appendEntry(rb, "ns", "tracks", bytes("same"), {
      auth,
    });

    expect(first.appended).toBe(true);
    expect(second.appended).toBe(false);
    expect(second.cid).toBe(first.cid);
    expect((await read(rb, "tracks", auth)).map((entry) => entry.cid))
      .toEqual([first.cid]);
  });

  it("retries an append that lost the manifest race", async () => {
    const rb = fakeClient();
    const auth = fakeAuth(deviceA);

    await appendEntry(rb, "ns", "tracks", bytes("one"), { auth });

    const registrySetIf = rb.registrySetIf;
    let raced = false;

    // The first CAS write observes another tab of the same device landing an
    // append between the read and the write.
    rb.registrySetIf = async (namespace, name, cid, when) => {
      if (!raced) {
        raced = true;
        await appendEntry(rb, "ns", "tracks", bytes("other"), { auth });

        return { ok: false, current: rb.registry.get(namespace).get(name) ?? null };
      }

      return registrySetIf(namespace, name, cid, when);
    };

    const result = await appendEntry(rb, "ns", "tracks", bytes("mine"), {
      auth,
    });
    const entries = await read(rb, "tracks", auth);

    expect(result.appended).toBe(true);
    expect(entries.map((entry) => text(entryBytes(rb, entry))).sort())
      .toEqual(["mine", "one", "other"]);
  });

  it("bumps the sequence on every write", async () => {
    const rb = fakeClient();
    const auth = fakeAuth(deviceA);

    /** @type {number[]} */
    const seqs = [];

    await appendEntry(rb, "ns", "tracks", bytes("1"), { auth });
    await read(rb, "tracks", auth, {
      onAccept: (_device, seq) => seqs.push(seq),
    });
    await appendEntry(rb, "ns", "tracks", bytes("2"), { auth });
    await read(rb, "tracks", auth, {
      onAccept: (_device, seq) => seqs.push(seq),
    });
    await appendEntry(rb, "ns", "tracks", bytes("3"), { auth });
    await read(rb, "tracks", auth, {
      onAccept: (_device, seq) => seqs.push(seq),
    });

    expect(seqs).toEqual([1, 2, 3]);
  });

  it("ignores a manifest signed by someone else", async () => {
    const rb = fakeClient();
    const auth = fakeAuth(deviceA);

    await appendEntry(rb, "ns", "tracks", bytes("mine"), { auth });

    // A different key writes under device-a's ref. Anyone with the account's
    // `/firkin` capability can do this, so the reader must not trust it — the
    // write is a denial of service on that ref, not a forgery it accepts.
    await appendEntry(rb, "ns", "tracks", bytes("forged"), {
      auth: { ...fakeAuth(deviceB), device: deviceA },
    });

    expect(await read(rb, "tracks", auth)).toEqual([]);

    // The real device repairs its own ref on the next write.
    await appendEntry(rb, "ns", "tracks", bytes("mine again"), { auth });

    const entries = await read(rb, "tracks", auth);

    expect(entries.map((entry) => text(entryBytes(rb, entry)))).toEqual([
      "mine again",
    ]);
  });

  it("ignores a manifest filed under the wrong collection", async () => {
    const rb = fakeClient();
    const auth = fakeAuth(deviceA);

    await appendEntry(rb, "ns", "tracks", bytes("tracks"), { auth });

    // Copy the tracks manifest to the facets ref: same signature, wrong log.
    const signed = rb.registry
      .get(registryNamespace("ns"))
      .get(logName("tracks", deviceA));
    await rb.registrySet(
      registryNamespace("ns"),
      logName("facets", deviceA),
      signed,
    );

    expect(await read(rb, "facets", auth)).toEqual([]);
    expect((await read(rb, "tracks", auth)).length).toBe(1);
  });

  it("ignores a tampered manifest", async () => {
    const rb = fakeClient();
    const auth = fakeAuth(deviceA);

    await appendEntry(rb, "ns", "tracks", bytes("original"), { auth });

    const cid = rb.registry
      .get(registryNamespace("ns"))
      .get(logName("tracks", deviceA));
    /** @type {{ p: Uint8Array; s: Uint8Array }} */
    const envelope = DagCBOR.decode(rb.blobs.get(cid));

    /** @type {any} */
    const payload = DagCBOR.decode(rb.blobs.get(envelope.p.toString()));
    payload.packs = [
      { cid: CID.parse(cidForBytes(new Uint8Array([1, 2, 3]))), at: 1 },
    ];

    const tamperedPayload = DagCBOR.encode(payload);
    const tamperedPayloadCid = cidForBytes(tamperedPayload, { codec: 0x71 });
    rb.blobs.set(tamperedPayloadCid, tamperedPayload);

    const tampered = DagCBOR.encode({
      v: MANIFEST_VERSION,
      p: CID.parse(tamperedPayloadCid),
      s: envelope.s,
    });
    const tamperedCid = cidForBytes(tampered, { codec: 0x71 });
    rb.blobs.set(tamperedCid, tampered);
    await rb.registrySet(
      registryNamespace("ns"),
      logName("tracks", deviceA),
      tamperedCid,
    );

    expect(await read(rb, "tracks", auth)).toEqual([]);
  });

  it("ignores a manifest that rolls a device's sequence back", async () => {
    const rb = fakeClient();
    const auth = fakeAuth(deviceA);

    await appendEntry(rb, "ns", "tracks", bytes("one"), { auth });

    /** @type {Map<string, number>} */
    const floors = new Map();
    const first = await read(rb, "tracks", auth, {
      floors,
      onAccept: (device, seq) => floors.set(device, seq),
    });

    await appendEntry(rb, "ns", "tracks", bytes("two"), { auth });
    const second = await read(rb, "tracks", auth, {
      floors,
      onAccept: (device, seq) => floors.set(device, seq),
    });

    // Replay the seq=1 manifest; with a floor of 2 its entries are ignored.
    const replayed = await read(rb, "tracks", auth, { floors });

    expect(first.length).toBe(1);
    expect(second.length).toBe(2);
    expect(replayed.map((entry) => text(entryBytes(rb, entry)))).toEqual([
      "one",
      "two",
    ]);
  });

  it("keeps logs separate per collection", async () => {
    const rb = fakeClient();
    const auth = fakeAuth(deviceA);

    await appendEntry(rb, "ns", "tracks", bytes("t"), { auth });
    await appendEntry(rb, "ns", "facets", bytes("f"), { auth });

    expect((await read(rb, "tracks", auth)).length).toBe(1);
    expect((await read(rb, "facets", auth)).length).toBe(1);
    expect(await read(rb, "settings", auth)).toEqual([]);
  });

  it("trims a log to its newest entries when a limit is set", async () => {
    const rb = fakeClient();
    const auth = fakeAuth(deviceA);

    await appendEntry(rb, "ns", "tracks", bytes("1"), { auth, keep: 2 });
    await appendEntry(rb, "ns", "tracks", bytes("2"), { auth, keep: 2 });
    await appendEntry(rb, "ns", "tracks", bytes("3"), { auth, keep: 2 });

    const entries = await read(rb, "tracks", auth);

    expect(entries.map((entry) => text(entryBytes(rb, entry)))).toEqual([
      "2",
      "3",
    ]);
    expect((await read(rb, "tracks", auth)).length).toBe(2);
  });

  it("trims each writer's log independently", async () => {
    const rb = fakeClient();
    const auth = fakeAuth(deviceA);

    await appendEntry(rb, "ns", "tracks", bytes("a1"), { auth, keep: 1 });
    await appendEntry(rb, "ns", "tracks", bytes("a2"), { auth, keep: 1 });
    await appendEntry(rb, "ns", "tracks", bytes("b1"), {
      auth: fakeAuth(deviceB),
      keep: 1,
    });

    const entries = await read(rb, "tracks", auth);

    expect(entries.map((entry) => text(entryBytes(rb, entry))).sort())
      .toEqual(["a2", "b1"]);
  });

  it("keeps everything when the limit is not set", async () => {
    const rb = fakeClient();
    const auth = fakeAuth(deviceA);

    await appendEntry(rb, "ns", "tracks", bytes("1"), { auth });
    await appendEntry(rb, "ns", "tracks", bytes("2"), { auth });

    expect((await read(rb, "tracks", auth)).length).toBe(2);
  });

  it("treats a manifest with a missing blob as an empty log", async () => {
    const rb = fakeClient();
    const auth = fakeAuth(deviceA);

    await appendEntry(rb, "ns", "tracks", bytes("a"), { auth });

    const manifestCid = rb.registry
      .get(registryNamespace("ns"))
      .get(logName("tracks", deviceA));
    rb.blobs.delete(manifestCid);

    expect(await read(rb, "tracks", auth)).toEqual([]);
  });

  it("carries a small entry inside the manifest", async () => {
    const rb = fakeClient();
    const auth = fakeAuth(deviceA);

    const appended = await appendEntry(rb, "ns", "tracks", bytes("small"), {
      auth,
    });

    // No blob of its own: the signed manifest carries the bytes.
    expect(rb.blobs.has(appended.cid)).toBe(false);

    const [entry] = await read(rb, "tracks", auth);

    expect(text(entryBytes(rb, entry))).toBe("small");
    expect(entry.bytes instanceof Uint8Array).toBe(true);
  });

  it("appends a large entry in one CAR as well", async () => {
    const rb = fakeClient();
    const auth = fakeAuth(deviceA);

    const first = await appendEntry(rb, "ns", "tracks", bytes("one"), { auth });

    let ingests = 0;
    let uploads = 0;
    let cas = 0;
    const originalIngest = rb.ingestCar;
    const originalUpload = rb.upload;
    const originalSetIf = rb.registrySetIf;
    rb.ingestCar = (...args) => {
      ingests += 1;
      return originalIngest(...args);
    };
    rb.upload = (...args) => {
      uploads += 1;
      return originalUpload(...args);
    };
    rb.registrySetIf = (...args) => {
      cas += 1;
      return originalSetIf(...args);
    };

    await appendEntry(
      rb,
      "ns",
      "tracks",
      bytes("x".repeat(INLINE_ENTRY_MAX_BYTES + 1)),
      { auth, current: first.manifest },
    );

    // Manifest and entry ride in one CAR; no separate blob upload.
    expect(ingests).toBe(1);
    expect(uploads).toBe(0);
    expect(cas).toBe(1);
  });

  it("reads a whole log through one DAG pull", async () => {
    const rb = fakeClient();
    const auth = fakeAuth(deviceA);

    for (let index = 0; index < 3; index++) {
      await appendEntry(
        rb,
        "ns",
        "tracks",
        bytes(`entry-${index}-${"x".repeat(INLINE_ENTRY_MAX_BYTES)}`),
        { auth },
      );
    }

    let pulls = 0;
    let gets = 0;
    const originalPull = rb.dagPull;
    const originalGet = rb.get;
    rb.dagPull = (...args) => {
      pulls += 1;
      return originalPull(...args);
    };
    rb.get = (...args) => {
      gets += 1;
      return originalGet(...args);
    };

    const entries = await read(rb, "tracks", auth);

    expect(entries.length).toBe(3);
    expect(entries.every((entry) => entry.bytes instanceof Uint8Array))
      .toBe(true);

    // The manifest and its entry blocks arrive in one DAG pull, so no block is
    // fetched individually.
    expect(pulls).toBe(1);
    expect(gets).toBe(0);
  });

  it("appends a small entry in one upload and one registry write", async () => {
    const rb = fakeClient();
    const auth = fakeAuth(deviceA);

    const first = await appendEntry(rb, "ns", "tracks", bytes("one"), { auth });

    let ingests = 0;
    let cas = 0;
    const originalIngest = rb.ingestCar;
    const originalSetIf = rb.registrySetIf;
    rb.ingestCar = (...args) => {
      ingests += 1;
      return originalIngest(...args);
    };
    rb.registrySetIf = (...args) => {
      cas += 1;
      return originalSetIf(...args);
    };

    await appendEntry(rb, "ns", "tracks", bytes("two"), {
      auth,
      current: first.manifest,
    });

    // The entry is inline: the manifest's two blocks go up in one CAR, and
    // the registry write is the compare-and-swap.
    expect(ingests).toBe(1);
    expect(cas).toBe(1);
  });

  it("keeps an entry larger than the inline limit as a blob", async () => {
    const rb = fakeClient();
    const auth = fakeAuth(deviceA);

    const large = bytes("x".repeat(INLINE_ENTRY_MAX_BYTES + 1));
    const appended = await appendEntry(rb, "ns", "tracks", large, { auth });

    expect(rb.blobs.has(appended.cid)).toBe(true);

    const [entry] = await read(rb, "tracks", auth);

    // The reader still gets the bytes — through the manifest's DAG, not
    // through the payload's inline field.
    expect(text(entryBytes(rb, entry))).toBe(text(large));
  });

  it("appends from a cached manifest without reading the ref", async () => {
    const rb = fakeClient();
    const auth = fakeAuth(deviceA);

    const first = await appendEntry(rb, "ns", "tracks", bytes("one"), { auth });

    let lookups = 0;
    let gets = 0;
    const originalLookup = rb.registryLookup;
    const originalGet = rb.get;
    rb.registryLookup = (...args) => {
      lookups += 1;
      return originalLookup(...args);
    };
    rb.get = (...args) => {
      gets += 1;
      return originalGet(...args);
    };

    const second = await appendEntry(rb, "ns", "tracks", bytes("two"), {
      auth,
      current: first.manifest,
    });

    expect(second.appended).toBe(true);
    expect(lookups).toBe(0);
    expect(gets).toBe(0);

    const entries = await read(rb, "tracks", auth);
    expect(entries.map((entry) => text(entryBytes(rb, entry)))).toEqual([
      "one",
      "two",
    ]);
  });

  it("writes lowercase registry names", async () => {
    // The namespace and name end up in the UCAN command
    // (`/firkin/registry/read/<ns>/<name>`), and the server rejects commands
    // that aren't lowercase.
    const namespace = registryNamespace("firkin-loro");
    const name = logName("playlistItems", deviceA);

    expect(namespace).toMatch(/^[a-z0-9._-]+$/);
    expect(name).toMatch(/^[a-z0-9._-]+$/);
    expect(logName("tracks", deviceA)).toMatch(/^[a-z0-9._-]+$/);
  });

  it("writes the version and device into the payload", async () => {
    const rb = fakeClient();
    const auth = fakeAuth(deviceA);

    await appendEntry(rb, "ns", "tracks", bytes("a"), { auth });

    const cid = rb.registry
      .get(registryNamespace("ns"))
      .get(logName("tracks", deviceA));
    /** @type {{ p: Uint8Array }} */
    const envelope = DagCBOR.decode(rb.blobs.get(cid));
    /** @type {any} */
    const payload = DagCBOR.decode(rb.blobs.get(envelope.p.toString()));

    expect(payload.v).toBe(MANIFEST_VERSION);
    expect(payload.device).toBe(deviceA);
    expect(payload.seq).toBe(1);
    expect(payload.packs.length).toBe(1);
  });
});
