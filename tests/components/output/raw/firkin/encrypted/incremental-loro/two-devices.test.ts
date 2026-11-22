// @ts-nocheck -- the fake server, crypto shim and page wiring are untyped

import { launch } from "@astral/astral";
import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { startFakeFirkin } from "./fake-firkin-server.ts";

/**
 * Two genuinely separate devices: each gets its own browser profile (so its own
 * IndexedDB, Loro actor and localStorage) and they talk to one Firkin server.
 *
 * The other integration tests put several elements in one page, which shares
 * the device key, the IndexedDB state and the Loro actor — a setup that can
 * never show what two independent writers do to each other. This one can:
 *
 * - a new device must read the log without writing anything of its own;
 * - after both devices settle, neither may keep writing; an update on one
 *   device must reach the other and then stop.
 */

const DEVICE_URL = "http://localhost:3000/testing/index.html";
const SALT = new Uint8Array(16).fill(7);

const hex = (bytes: Uint8Array) =>
  Array.from(bytes).map((value) => value.toString(16).padStart(2, "0")).join("");


/**
 * Runs inside a page: sign in as this device, unlock the shared output key,
 * install request counters, and append the output element.
 *
 * Self-contained on purpose — `page.evaluate` serialises this function, so it
 * cannot close over anything from the test file.
 */
async function setupDevice(args: {
  apiBase: string;
  accountSeed: number[];
  sessionSeed: number[];
  saltHex: string;
  id: string;
  namespace: string;
}) {
  const { Signer, delegate, WebCryptoSigner } = await import(
    "~/vendor/firkin/index.js"
  );

  // The stored session's private key is a decoy; sign and verify through the
  // real Ed25519 signers, keyed by public key (same shim as the other tests).
  const signers = new Map();
  const pubBytes = new Map();
  const toHex = (bytes: Uint8Array) =>
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
  WebCryptoSigner.prototype.sign = async function (message: Uint8Array) {
    const signer = this.pubkeyBytes
      ? signers.get(toHex(this.pubkeyBytes))
      : undefined;

    if (signer) return await signer.sign(message);
    return await ogSign.call(this, message);
  };

  const IDB = await import("idb-keyval");

  globalThis.__warnings = [];
  const ogWarn = console.warn.bind(console);
  const ogError = console.error.bind(console);
  console.warn = (...args: any[]) => {
    globalThis.__warnings.push(args.map(String).join(" "));
    ogWarn(...args);
  };
  console.error = (...args: any[]) => {
    globalThis.__warnings.push(args.map(String).join(" "));
    ogError(...args);
  };

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

  // Same salt on both devices, as a synced passkey would give them.
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
  await unlockOutputKeyWithPassphrase("two-device-passphrase");

  // --- Request counters ------------------------------------------------------
  const { Firkin } = await import("~/vendor/firkin/index.js");

  globalThis.__writes = [];
  globalThis.__reads = [];

  const writes = ["ingestCar", "upload", "registrySet", "registrySetIf"];
  const reads = ["dagPull", "get", "registryNames", "registryLookup"];

  for (const name of writes) {
    const original = Firkin.prototype[name];
    Firkin.prototype[name] = async function (...rest: any[]) {
      globalThis.__writes.push(name);
      return await original.apply(this, rest);
    };
  }

  for (const name of reads) {
    const original = Firkin.prototype[name];
    Firkin.prototype[name] = async function (...rest: any[]) {
      globalThis.__reads.push(name);
      return await original.apply(this, rest);
    };
  }

  // --- The output ------------------------------------------------------------
  await import(
    "~/components/output/raw/firkin/encrypted/incremental-loro/element.js"
  );
  await customElements.whenDefined("dob-firkin-loro");

  const element = document.createElement("dob-firkin-loro");
  element.id = args.id;
  element.setAttribute("namespace", args.namespace);
  element.setAttribute("keep", "64");
  element.setAttribute("bundled", "");
  document.body.append(element);

  await new Promise<void>((resolve) => {
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
    state: element.tracks.collection().state,
    warnings: globalThis.__warnings,
  };
}

/** Runs inside a page: save the given track ids as the whole collection. */
async function saveTracks(ids: string[]) {
  const element = globalThis.__el;
  await element.tracks.save(
    ids.map((id) => ({
      $type: "sh.diffuse.output.track",
      id,
      uri: `https://example.com/${id}.mp3`,
    })),
  );
}

/** Runs inside a page: re-read the tracks collection, as a page switch does. */
async function catchUp() {
  document.dispatchEvent(new Event("visibilitychange"));
}

/** Runs inside a page: the visible track ids, or `undefined` while loading. */
function trackIds() {
  const collection = globalThis.__el.tracks.collection();
  return collection.state === "loaded"
    ? collection.data.map((track: any) => track.id).sort()
    : undefined;
}

/** Runs inside a page: writes made since the last reset. */
function operationCounts() {
  return {
    writes: globalThis.__writes,
    reads: globalThis.__reads,
    warnings: globalThis.__warnings,
    ready: globalThis.__el?.ready(),
    state: globalThis.__el?.tracks.collection().state,
  };
}

/** Runs inside a page: forget the request counters. */
function resetOperations() {
  globalThis.__writes = [];
  globalThis.__reads = [];
}

const settle = (ms: number) => new Promise((resolve) => setTimeout(resolve, ms));

const until = async <T>(fn: () => T | Promise<T>, ms = 15_000) => {
  const deadline = Date.now() + ms;

  while (Date.now() < deadline) {
    const value = await fn();
    if (value) return value;
    await settle(50);
  }

  return await fn();
};

describe("components/output/raw/firkin/encrypted/incremental-loro two devices", () => {
  it("a new device reads without writing, and two devices settle", async () => {
    const { http, port, registry } = startFakeFirkin();
    const apiBase = `http://127.0.0.1:${port}`;

    const accountSeed = Array.from(new Uint8Array(32).fill(3));
    const namespace = "firkin-two-devices";

    await using a = await launch();
    await using b = await launch();

    const pageA = await a.newPage(DEVICE_URL);
    const pageB = await b.newPage(DEVICE_URL);

    try {
      const deviceA = await pageA.evaluate(setupDevice, {
        args: [{
          apiBase,
          accountSeed,
          sessionSeed: Array.from(new Uint8Array(32).fill(11)),
          saltHex: hex(SALT),
          id: "device-a",
          namespace,
        }],
      });
      expect(typeof deviceA.device).toBe("string");

      const logsFor = (prefix: string) => {
        const encoded = [...new TextEncoder().encode(prefix)]
          .map((byte) => byte.toString(16).padStart(2, "0"))
          .join("");

        return [...registry.keys()].filter((key) =>
          key.includes(`/${encoded}.`)
        );
      };

      // --- 1. The first device publishes t1 --------------------------------
      await pageA.evaluate(resetOperations);
      await pageA.evaluate(saveTracks, { args: [["t1"]] });

      const manifestPublished = await until(
        () => logsFor("tracks#manifest").length > 0,
        10_000,
      );
      if (!manifestPublished) {
        console.log("PUBLISH_FAILED", await pageA.evaluate(operationCounts));
        console.log("REGISTRY", [...registry.keys()]);
      }
      expect(manifestPublished).toBe(true);

      // Let the bundle pushes finish before the second device reads.
      const publishedSettled = await until(async () => {
        const before = (await pageA.evaluate(operationCounts)).writes.length;
        await settle(1_000);
        const after = (await pageA.evaluate(operationCounts)).writes.length;
        return before === after && after > 0;
      });
      expect(publishedSettled).toBe(true);

      // --- 2. A fresh device reads everything, and writes nothing ----------
      await pageB.evaluate(setupDevice, {
        args: [{
          apiBase,
          accountSeed,
          sessionSeed: Array.from(new Uint8Array(32).fill(22)),
          saltHex: hex(SALT),
          id: "device-b",
          namespace,
        }],
      });

      const readOnB = await until(async () => {
        const ids = await pageB.evaluate(trackIds);
        return ids?.length === 1 ? ids : undefined;
      });
      expect(readOnB).toEqual(["t1"]);

      // Give a late push the chance to show up, then check nothing was written.
      await settle(2_000);
      const bAfterRead = await pageB.evaluate(operationCounts);
      expect(bAfterRead.writes).toEqual([]);

      // The first device must not react to the second device's read either.
      await pageA.evaluate(resetOperations);
      await settle(2_000);
      expect((await pageA.evaluate(operationCounts)).writes).toEqual([]);

      // --- 3. An update propagates through the hint channel ----------------
      await pageB.evaluate(resetOperations);
      await pageA.evaluate(resetOperations);
      await pageA.evaluate(saveTracks, { args: [["t1", "t2"]] });

      const readOnBAfterUpdate = await until(async () => {
        const ids = await pageB.evaluate(trackIds);
        return ids?.length === 2 ? ids : undefined;
      }, 20_000);
      expect(readOnBAfterUpdate).toEqual(["t1", "t2"]);

      // --- 4. ...and then everything settles --------------------------------
      await settle(2_000);
      await pageA.evaluate(resetOperations);
      await pageB.evaluate(resetOperations);
      await settle(3_000);

      expect((await pageA.evaluate(operationCounts)).writes).toEqual([]);
      expect((await pageB.evaluate(operationCounts)).writes).toEqual([]);

      // --- 5. Concurrent edits converge without echoing --------------------
      await pageA.evaluate(resetOperations);
      await pageB.evaluate(resetOperations);

      await Promise.all([
        pageA.evaluate(saveTracks, { args: [["t1", "t2", "t3"]] }),
        pageB.evaluate(saveTracks, { args: [["t1", "t2", "t4"]] }),
      ]);

      const converged = await until(async () => {
        const [fromA, fromB] = await Promise.all([
          pageA.evaluate(trackIds),
          pageB.evaluate(trackIds),
        ]);

        return fromA?.length === 4 && fromB?.length === 4
          ? { fromA, fromB }
          : undefined;
      }, 30_000);
      expect(converged.fromA).toEqual(converged.fromB);

      await settle(3_000);
      await pageA.evaluate(resetOperations);
      await pageB.evaluate(resetOperations);
      await settle(3_000);

      expect((await pageA.evaluate(operationCounts)).writes).toEqual([]);
      expect((await pageB.evaluate(operationCounts)).writes).toEqual([]);

      // --- 6. Repeated catch-ups on an unchanged log write nothing ---------
      await pageA.evaluate(resetOperations);
      await pageB.evaluate(resetOperations);

      for (let index = 0; index < 3; index++) {
        await Promise.all([
          pageA.evaluate(catchUp),
          pageB.evaluate(catchUp),
        ]);
        await settle(500);
      }

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
