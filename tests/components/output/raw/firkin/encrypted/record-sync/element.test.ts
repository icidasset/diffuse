// @ts-nocheck -- the fake server, crypto shim and page wiring are untyped

import { launch } from "@astral/astral";
import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { startFakeFirkin } from "../incremental-loro/fake-firkin-server.ts";

/**
 * Two independent Record Sync clients on one Firkin server: separate browser
 * profiles, so separate IndexedDB and device keys, but the same passphrase
 * salt, so the same entry and field keys.
 *
 * Covers the whole path the unit tests cannot: save → seal → patch → log →
 * hint → pull → decrypt → LWW merge, and that settling leaves no writes.
 */

const DEVICE_URL = "http://localhost:3000/testing/index.html";
const PASSPHRASE = "record-sync-passphrase";
const SALT = new Uint8Array(16).fill(11);
const HEX = (bytes) =>
  Array.from(bytes).map((value) => value.toString(16).padStart(2, "0")).join(
    "",
  );

/** Runs inside a page: sign in, unlock both keys, and append the output. */
async function setupDevice(args) {
  const { Signer, delegate, WebCryptoSigner } = await import(
    "~/vendor/firkin/index.js"
  );

  // Test browser predates native WebCrypto Ed25519.
  const signers = new Map();
  const pubBytes = new Map();
  const toHex = (bytes) =>
    Array.from(bytes).map((b) => b.toString(16).padStart(2, "0")).join("");

  const ogImportKey = crypto.subtle.importKey.bind(crypto.subtle);
  const ogVerify = crypto.subtle.verify.bind(crypto.subtle);

  crypto.subtle.importKey = async (format, data, alg, extractable, usages) => {
    if (alg?.name === "Ed25519") {
      const bytes = new Uint8Array(data);
      const decoy = await ogImportKey(
        "raw",
        new Uint8Array([0]),
        { name: "HMAC", hash: "SHA-256" },
        false,
        ["sign", "verify"],
      );
      pubBytes.set(decoy, bytes);
      return decoy;
    }

    return ogImportKey(format, data, alg, extractable, usages);
  };

  crypto.subtle.verify = async (alg, key, signature, data) => {
    const name = typeof alg === "string" ? alg : alg?.name;
    const bytes = pubBytes.get(key);

    if (name === "Ed25519" && bytes) {
      const verifier = new Signer(new Uint8Array(32), bytes);
      return await verifier.verify(
        new Uint8Array(signature),
        new Uint8Array(data),
      );
    }

    return ogVerify(alg, key, signature, data);
  };

  const ogSign = WebCryptoSigner.prototype.sign;
  WebCryptoSigner.prototype.sign = async function (message) {
    const signer = this.pubkeyBytes
      ? signers.get(toHex(this.pubkeyBytes))
      : undefined;

    if (signer) return await signer.sign(message);
    return await ogSign.call(this, message);
  };

  const IDB = await import("idb-keyval");

  const sessionSigner = await Signer.fromSeed(
    new Uint8Array(args.sessionSeed),
  );
  const accountSigner = await Signer.fromSeed(
    new Uint8Array(args.accountSeed),
  );
  signers.set(toHex(sessionSigner.pubkeyBytes), sessionSigner);

  const { proof, cid: proofCid } = await delegate(accountSigner, {
    subject: accountSigner.did(),
    audience: sessionSigner.did(),
    command: "/firkin",
    expireIn: 365 * 24 * 3600,
  });

  const decoy = await ogImportKey(
    "raw",
    new Uint8Array([0]),
    { name: "HMAC", hash: "SHA-256" },
    false,
    ["sign", "verify"],
  );

  await IDB.set("diffuse/firkin/session", {
    apiBase: args.apiBase,
    did: accountSigner.did(),
    privateKey: decoy,
    publicKey: sessionSigner.pubkeyBytes,
    proof,
    proofCid,
    expiresAt: Date.now() + 365 * 24 * 3600 * 1000,
    inlineProofs: true,
  });

  // The same salt on both devices, as a synced passkey would give them.
  const salt = new Uint8Array(
    args.saltHex.match(/../g).map((pair) => parseInt(pair, 16)),
  );
  await IDB.set("diffuse/firkin/output-key-meta", {
    source: "passphrase",
    salt,
  });

  const { unlockOutputKeyWithPassphrase } = await import(
    "~/common/firkin/output-key.js"
  );
  await unlockOutputKeyWithPassphrase("record-sync-passphrase");

  // --- Request counters ------------------------------------------------------
  const { Firkin } = await import("~/vendor/firkin/index.js");

  globalThis.__writes = [];
  globalThis.__reads = [];

  for (const name of ["ingestCar", "upload", "registrySet", "registrySetIf"]) {
    const original = Firkin.prototype[name];
    Firkin.prototype[name] = async function (...rest) {
      globalThis.__writes.push(name);
      return await original.apply(this, rest);
    };
  }

  for (
    const name of [
      "dagPull",
      "get",
      "registryNames",
      "registryList",
      "registryLookup",
    ]
  ) {
    const original = Firkin.prototype[name];
    Firkin.prototype[name] = async function (...rest) {
      globalThis.__reads.push(name);
      return await original.apply(this, rest);
    };
  }

  globalThis.__warnings = [];
  const ogWarn = console.warn.bind(console);
  const ogError = console.error.bind(console);
  console.warn = (...args) => {
    globalThis.__warnings.push(args.map(String).join(" "));
    ogWarn(...args);
  };
  console.error = (...args) => {
    globalThis.__warnings.push(args.map(String).join(" "));
    ogError(...args);
  };

  // --- The output ------------------------------------------------------------
  await import(
    "~/components/output/raw/firkin/encrypted/record-sync/element.js"
  );
  await customElements.whenDefined("dob-record-sync");

  const element = document.createElement("dob-record-sync");
  element.id = args.id;
  element.setAttribute("namespace", args.namespace);
  element.setAttribute("keep", "64");
  document.body.append(element);

  await new Promise((resolve) => {
    const deadline = Date.now() + 20_000;
    const check = () => {
      if (element.ready() || Date.now() > deadline) return resolve();
      setTimeout(check, 50);
    };
    check();
  });

  globalThis.__el = element;

  return {
    device: toHex(sessionSigner.pubkeyBytes),
    ready: element.ready(),
    warnings: globalThis.__warnings,
  };
}

/** Runs inside a page: save the given track ids as the whole collection. */
async function saveTracks(ids) {
  await globalThis.__el.tracks.save(
    ids.map((id) => ({
      $type: "sh.diffuse.output.track",
      id,
      uri: `https://example.com/${id}.mp3`,
    })),
  );
}

/** Runs inside a page: the visible ids, or undefined while loading. */
function trackIds() {
  const collection = globalThis.__el.tracks.collection();
  return collection.state === "loaded"
    ? collection.data.map((track) => track.id).sort()
    : undefined;
}

/** Runs inside a page: writes made since the last reset. */
function operationCounts() {
  return {
    writes: globalThis.__writes,
    reads: globalThis.__reads,
    warnings: globalThis.__warnings,
    state: globalThis.__el?.tracks.collection().state,
  };
}

/** Runs inside a page: forget the request counters. */
function resetOperations() {
  globalThis.__writes = [];
  globalThis.__reads = [];
}

const settle = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

const until = async (fn, ms = 15_000) => {
  const deadline = Date.now() + ms;

  while (Date.now() < deadline) {
    const value = await fn();
    if (value) return value;
    await settle(50);
  }

  return await fn();
};

describe("components/output/raw/firkin/encrypted/record-sync", () => {
  it("syncs records between two devices and settles", async () => {
    const { http, port, registry } = startFakeFirkin();
    const apiBase = `http://127.0.0.1:${port}`;
    const accountSeed = Array.from(new Uint8Array(32).fill(21));
    const namespace = "record-sync-test";

    await using a = await launch();
    await using b = await launch();

    const pageA = await a.newPage(DEVICE_URL);
    const pageB = await b.newPage(DEVICE_URL);

    try {
      const deviceA = await pageA.evaluate(setupDevice, {
        args: [{
          apiBase,
          accountSeed,
          sessionSeed: Array.from(new Uint8Array(32).fill(41)),
          saltHex: HEX(SALT),
          id: "record-sync-a",
          namespace,
        }],
      });
      expect(deviceA.ready).toBe(true);

      await pageA.evaluate(resetOperations);
      await pageA.evaluate(saveTracks, { args: [["t1"]] });

      const encodedTracks = [...new TextEncoder().encode("tracks")]
        .map((byte) => byte.toString(16).padStart(2, "0"))
        .join("");

      const published = await until(() =>
        [...registry.keys()].some((key) =>
          key.startsWith("diffuse.output.record-sync.") &&
          key.includes(`/${encodedTracks}.`) &&
          key.endsWith(deviceA.device)
        )
      );
      expect(published).toBe(true);

      // Wait for the coalesced push and its bookkeeping to settle.
      await until(async () => {
        const before = (await pageA.evaluate(operationCounts)).writes.length;
        await settle(1_000);
        const after = (await pageA.evaluate(operationCounts)).writes.length;
        return before === after;
      });

      // --- A second device reads the log --------------------------------
      const deviceB = await pageB.evaluate(setupDevice, {
        args: [{
          apiBase,
          accountSeed,
          sessionSeed: Array.from(new Uint8Array(32).fill(42)),
          saltHex: HEX(SALT),
          id: "record-sync-b",
          namespace,
        }],
      });
      expect(deviceB.ready).toBe(true);

      const readOnB = await until(async () => {
        const ids = await pageB.evaluate(trackIds);
        return ids?.length === 1 ? ids : undefined;
      });
      if (readOnB === undefined) {
        console.log(
          "RECORD_SYNC_B_EMPTY",
          await pageB.evaluate(operationCounts),
        );
        console.log("RECORD_SYNC_REGISTRY", [...registry.keys()]);
      }
      expect(readOnB).toEqual(["t1"]);

      await settle(2_000);
      await pageB.evaluate(resetOperations);
      await settle(2_000);
      expect((await pageB.evaluate(operationCounts)).writes).toEqual([]);

      // --- An update from A reaches B ----------------------------------
      await pageA.evaluate(resetOperations);
      await pageB.evaluate(resetOperations);
      await pageA.evaluate(saveTracks, { args: [["t1", "t2"]] });

      const updateOnB = await until(async () => {
        const ids = await pageB.evaluate(trackIds);
        return ids?.length === 2 ? ids : undefined;
      }, 20_000);
      expect(updateOnB).toEqual(["t1", "t2"]);

      // --- An update from B reaches A ----------------------------------
      await pageB.evaluate(saveTracks, { args: [["t1", "t2", "t3"]] });

      const updateOnA = await until(async () => {
        const ids = await pageA.evaluate(trackIds);
        return ids?.length === 3 ? ids : undefined;
      }, 20_000);
      expect(updateOnA).toEqual(["t1", "t2", "t3"]);

      // --- Both settle -------------------------------------------------
      await settle(2_000);
      await pageA.evaluate(resetOperations);
      await pageB.evaluate(resetOperations);
      await settle(2_000);

      expect((await pageA.evaluate(operationCounts)).writes).toEqual([]);
      expect((await pageB.evaluate(operationCounts)).writes).toEqual([]);
    } finally {
      await pageA.close();
      await pageB.close();
      await http.shutdown();
    }
  });
});
