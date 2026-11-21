// @ts-nocheck -- the fake client intentionally implements only the subset it uses

import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import * as DagCBOR from "@ipld/dag-cbor";
import { ApiError } from "@tokono.ma/firkin";
import { cidForBytes } from "~/vendor/firkin/index.js";

import {
  MANIFEST_VERSION,
  logName,
  registryNamespace,
} from "~/components/output/raw/firkin/encrypted/incremental-loro/constants.js";
import {
  appendEntry,
  latestEntry,
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
  it("unions the entries of every writer", async () => {
    const rb = fakeClient();
    const auth = fakeAuth(deviceA);

    await appendEntry(rb, "ns", "tracks", bytes("a"), { auth });
    await appendEntry(rb, "ns", "tracks", bytes("b"), {
      auth: fakeAuth(deviceB),
    });

    const entries = await read(rb, "tracks", auth);

    expect(entries.length).toBe(2);
    expect(entries.map((entry) => text(rb.blobs.get(entry.cid))).sort())
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
    expect(text(rb.blobs.get(entry.cid))).toBe("new");
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
    expect(entries.map((entry) => text(rb.blobs.get(entry.cid))).sort())
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

    expect(entries.map((entry) => text(rb.blobs.get(entry.cid)))).toEqual([
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
    const payload = DagCBOR.decode(envelope.p);
    payload.packs = [{ cid: "bafkreiforged", at: 1 }];

    const tampered = DagCBOR.encode({
      p: DagCBOR.encode(payload),
      s: envelope.s,
    });
    const { cid: tamperedCid } = await rb.upload(tampered);
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
    expect(replayed.map((entry) => text(rb.blobs.get(entry.cid)))).toEqual([
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

    expect(entries.map((entry) => text(rb.blobs.get(entry.cid)))).toEqual([
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

    expect(entries.map((entry) => text(rb.blobs.get(entry.cid))).sort())
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
    const payload = DagCBOR.decode(envelope.p);

    expect(payload.v).toBe(MANIFEST_VERSION);
    expect(payload.device).toBe(deviceA);
    expect(payload.seq).toBe(1);
    expect(payload.packs.length).toBe(1);
  });
});
