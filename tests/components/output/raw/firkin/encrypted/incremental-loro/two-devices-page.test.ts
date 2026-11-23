// @ts-nocheck -- the fake server, crypto shim and page driving are untyped

import { launch } from "@astral/astral";
import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { startFakeFirkin } from "./fake-firkin-server.ts";
import { startStaticServer } from "./static-server.ts";

/**
 * Two real dashboard pages, each in its own browser profile, sharing one Firkin
 * server — the closest the test suite gets to "two devices with the app open".
 *
 * The element-level test (`two-devices.test.ts`) drives the output directly.
 * This one drives the whole app, which is where the feedback loops live: the
 * output configurator mirrors the selected output into the local store and
 * seeds a selected output from it, and every collection change flows through
 * the UI's reactive pipeline.
 */

const SALT = new Uint8Array(16).fill(7);

const hex = (bytes: Uint8Array) =>
  Array.from(bytes).map((value) => value.toString(16).padStart(2, "0")).join("");

/**
 * Runs inside a page: sign in, turn the Firkin output option on, select it, and
 * expose the orchestrator + request counters.
 */
async function setupDashboard(args: {
  apiBase: string;
  accountSeed: number[];
  sessionSeed: number[];
  saltHex: string;
}) {
  // The injected script reads this on the next load and installs the shim
  // before the app's modules run, so a reload behaves like a real one.
  localStorage.setItem(
    "__test_session_seed",
    JSON.stringify(args.sessionSeed),
  );

  const { Signer, delegate, WebCryptoSigner } = await import(
    "~/vendor/firkin/index.js"
  );

  // Test browser predates native WebCrypto Ed25519.
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

  globalThis.__warnings = [];
  const ogWarn = console.warn.bind(console);
  const ogError = console.error.bind(console);
  console.warn = (...rest: any[]) => {
    globalThis.__warnings.push(rest.map(String).join(" "));
    ogWarn(...rest);
  };
  console.error = (...rest: any[]) => {
    globalThis.__warnings.push(rest.map(String).join(" "));
    ogError(...rest);
  };

  const IDB = await import("idb-keyval");

  const sessionSigner = await Signer.fromSeed(new Uint8Array(args.sessionSeed));
  const accountSigner = await Signer.fromSeed(new Uint8Array(args.accountSeed));
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

  const salt = new Uint8Array(
    args.saltHex.match(/../g).map((pair) => parseInt(pair, 16)),
  );
  await IDB.set("diffuse/firkin/output-key-meta", { source: "passphrase", salt });

  const { unlockOutputKeyWithPassphrase } = await import(
    "~/common/firkin/output-key.js"
  );
  await unlockOutputKeyWithPassphrase("two-device-passphrase");

  const sleep = (ms: number) => new Promise((resolve) => setTimeout(resolve, ms));
  const until = async (fn: () => any, ms = 20_000) => {
    const deadline = Date.now() + ms;
    while (Date.now() < deadline) {
      const value = await fn();
      if (value) return value;
      await sleep(50);
    }
    return await fn();
  };

  const output = await until(() => document.querySelector("do-output"));
  if (!output) return { fatal: "no do-output element" };

  // On a reload the option is already part of the stored facet set; adding the
  // facet again would duplicate its prelude.
  const alreadyEnabled = Boolean(
    await until(
      () => document.getElementById("do-output__dc-output__firkin-loro"),
      2_000,
    ),
  );

  let facet;

  if (!alreadyEnabled) {
    const { buildFacets } = await import("~/common/facets/utils.js");
    const { insertPreludes } = await import("~/common/facets/prelude.js");

    facet = buildFacets(["facets/data/firkin-loro/index.tile"])[0];
    const facets = output.facets.collection();
    const existing = facets.state === "loaded" ? facets.data : [];

    await output.facets.save([
      ...existing.filter((saved: any) => saved.id !== facet.id),
      facet,
    ]);

    await until(() => {
      const collection = output.facets.collection();
      return collection.state === "loaded" &&
        collection.data.some((saved: any) => saved.uri === facet.uri);
    }, 10_000);

    await insertPreludes([facet]);
  }
  const option = await until(() => document.getElementById("do-output__dc-output__firkin-loro"));
  if (!option) return { fatal: "no firkin option" };

  await output.select("do-output__dc-output__firkin-loro");

  const selected = await until(() =>
    output.selected()?.id === "do-output__dc-output__firkin-loro"
  );
  if (!selected) return { fatal: "selection did not stick" };

  // Wait for the output to actually answer.
  await until(() => output.ready(), 30_000);
  await until(() => output.tracks.collection().state === "loaded", 30_000);

  globalThis.__output = output;

  return {
    device: toHex(sessionSigner.pubkeyBytes),
    ready: output.ready(),
    tracks: output.tracks.collection().state,
    warnings: globalThis.__warnings,
  };
}

/** Runs inside a page: save the given ids through the app's output chain. */
async function saveThroughOutput(ids: string[]) {
  await globalThis.__output.tracks.save(
    ids.map((id) => ({
      $type: "sh.diffuse.output.track",
      id,
      uri: `https://example.com/${id}.mp3`,
    })),
  );
}

/**
 * Runs inside a page: fill every collection, with tracks and playlist items
 * split across bundles (the shape a real library has).
 */
async function seedEverything() {
  const output = globalThis.__output;
  const at = "2020-01-01T00:00:00.000Z";
  const track = (id: string, uri: string) => ({
    $type: "sh.diffuse.output.track",
    id,
    uri,
    createdAt: at,
    updatedAt: at,
  });

  await output.tracks.save([
    track("t1", "https://a.example/1.mp3"),
    track("t2", "https://b.example/2.mp3"),
    track("t3", "https://a.example/3.mp3"),
  ]);

  await output.playlistItems.save([
    {
      $type: "sh.diffuse.output.playlistItem",
      id: "p1",
      playlist: "https://a.example/list",
      uri: "https://a.example/1.mp3",
    },
    {
      $type: "sh.diffuse.output.playlistItem",
      id: "p2",
      playlist: "https://b.example/list",
      uri: "https://b.example/2.mp3",
    },
  ]);

  await output.settings.save([
    { $type: "sh.diffuse.output.setting", id: "s1", key: "volume", value: "0.5" },
  ]);

  await output.collaborations.save([
    { $type: "sh.diffuse.output.collaboration", id: "c1", did: "did:example:1" },
  ]);
}

/** Runs inside a page: the ids the app shows for tracks. */
function visibleTracks() {
  const output = globalThis.__output ?? document.querySelector("do-output");
  if (!output) return undefined;

  const collection = output.tracks.collection();
  return collection.state === "loaded"
    ? collection.data.map((track: any) => track.id).sort()
    : undefined;
}

/** Runs inside a page: request counters + a little state for debugging. */
function pageState() {
  return {
    writes: globalThis.__writes,
    reads: globalThis.__reads,
    warnings: globalThis.__warnings,
    tracks: (globalThis.__output ?? document.querySelector("do-output"))
      ?.tracks.collection().state,
  };
}

/** Runs inside a page: forget the request counters. */
function resetPageOperations() {
  globalThis.__writes = [];
  globalThis.__reads = [];
}

const settle = (ms: number) => new Promise((resolve) => setTimeout(resolve, ms));

const until = async <T>(fn: () => T | Promise<T>, ms = 20_000) => {
  const deadline = Date.now() + ms;

  while (Date.now() < deadline) {
    const value = await fn();
    if (value) return value;
    await settle(100);
  }

  return await fn();
};

describe("firkin output on two real dashboards", () => {
  it("a new device reads, and both dashboards settle", async () => {
    const { http, port, registry, dropChannels, setDelay } = startFakeFirkin();
    const staticServer = startStaticServer("dist");
    const apiBase = `http://127.0.0.1:${port}`;
    const dashboardUrl = `${staticServer.url}/dashboard`;
    const accountSeed = Array.from(new Uint8Array(32).fill(5));

    const encoded = (value: string) =>
      [...new TextEncoder().encode(value)]
        .map((byte) => byte.toString(16).padStart(2, "0"))
        .join("");

    await using a = await launch();
    await using b = await launch();

    const pageA = await a.newPage(dashboardUrl);
    const pageB = await b.newPage(dashboardUrl);

    try {
      // --- 1. The first dashboard publishes t1 ------------------------------
      const deviceA = await pageA.evaluate(setupDashboard, {
        args: [{
          apiBase,
          accountSeed,
          sessionSeed: Array.from(new Uint8Array(32).fill(31)),
          saltHex: hex(SALT),
        }],
      });
      expect(deviceA.fatal).toBeUndefined();

      await pageA.evaluate(resetPageOperations);
      await pageA.evaluate(saveThroughOutput, { args: [["t1"]] });

      const published = await until(
        () => [...registry.keys()].some((key) => key.includes("/" + encoded("tracks#manifest") + ".")),
        20_000,
      );
      if (!published) console.log("DASH_A_NOT_PUBLISHED", await pageA.evaluate(pageState));
      expect(published).toBe(true);

      // Let the save's pushes and any follow-up reads settle.
      await until(async () => {
        const before = (await pageA.evaluate(pageState)).writes.length;
        await settle(1_000);
        const after = (await pageA.evaluate(pageState)).writes.length;
        return before === after;
      });

      // --- 2. A second, fresh dashboard reads it ----------------------------
      // Slow the data plane so the second dashboard's output settles *before*
      // its pull lands. The carry-over must wait for the read instead of
      // seeding from the empty cache (which used to erase the log).
      setDelay(2_000);
      const deviceB = await pageB.evaluate(setupDashboard, {
        args: [{
          apiBase,
          accountSeed,
          sessionSeed: Array.from(new Uint8Array(32).fill(32)),
          saltHex: hex(SALT),
        }],
      });
      expect(deviceB.fatal).toBeUndefined();

      const readOnB = await until(async () => {
        const ids = await pageB.evaluate(visibleTracks);
        return ids?.length === 1 ? ids : undefined;
      }, 30_000);
      if (readOnB === undefined) {
        console.log("DASH_B_NO_TRACKS", await pageB.evaluate(pageState));
        console.log("DASH_REGISTRY", [...registry.keys()]);
      }
      expect(readOnB).toEqual(["t1"]);

      await settle(1_000);
      const bBoot = await pageB.evaluate(pageState);
      expect(bBoot.writes).toEqual([]);

      setDelay(0);

      // Let anything the second dashboard did on its own finish, then check it
      // has stopped writing.
      await settle(4_000);
      await pageB.evaluate(resetPageOperations);
      await settle(4_000);
      const bIdle = await pageB.evaluate(pageState);
      if (bIdle.writes.length > 0) console.log("DASH_B_STILL_WRITING", bIdle);
      expect(bIdle.writes).toEqual([]);

      // --- 3. An update from the first dashboard reaches the second ---------
      await pageA.evaluate(resetPageOperations);
      await pageB.evaluate(resetPageOperations);
      await pageA.evaluate(saveThroughOutput, { args: [["t1", "t2"]] });

      const readOnBAfterUpdate = await until(async () => {
        const ids = await pageB.evaluate(visibleTracks);
        return ids?.length === 2 ? ids : undefined;
      }, 30_000);
      expect(readOnBAfterUpdate).toEqual(["t1", "t2"]);

      // --- 4. Neither dashboard keeps writing -------------------------------
      await settle(4_000);
      await pageA.evaluate(resetPageOperations);
      await pageB.evaluate(resetPageOperations);
      await settle(5_000);

      const aIdle = await pageA.evaluate(pageState);
      const bSettled = await pageB.evaluate(pageState);
      if (aIdle.writes.length > 0) console.log("DASH_A_LOOP", aIdle);
      if (bSettled.writes.length > 0) console.log("DASH_B_LOOP", bSettled);

      expect(aIdle.writes).toEqual([]);
      expect(bSettled.writes).toEqual([]);

      // --- 4b. Seed a full library, then let both settle --------------------
      await pageA.evaluate(resetPageOperations);
      await pageB.evaluate(resetPageOperations);
      await pageA.evaluate(seedEverything);

      const seededOnB = await until(async () => {
        const ids = await pageB.evaluate(visibleTracks);
        return ids?.length === 3 ? ids : undefined;
      }, 30_000);
      expect(seededOnB).toEqual(["t1", "t2", "t3"]);

      // Let the seed's pushes drain before measuring idleness.
      await until(async () => {
        const before = (await pageA.evaluate(pageState)).writes.length;
        await settle(1_500);
        const after = (await pageA.evaluate(pageState)).writes.length;
        return before === after && after > 0;
      }, 30_000);

      await pageA.evaluate(resetPageOperations);
      await pageB.evaluate(resetPageOperations);
      await settle(4_000);
      const seededA = await pageA.evaluate(pageState);
      const seededB = await pageB.evaluate(pageState);
      if (seededA.writes.length > 0) console.log("SEED_A_STILL_WRITING", seededA);
      if (seededB.writes.length > 0) {
        console.log("SEED_B_STILL_WRITING", seededB);
        console.log(
          "SEED_B_REFS",
          [...registry.keys()].filter((key) => key.endsWith(deviceB.device)),
        );
      }
      expect(seededA.writes).toEqual([]);
      expect(seededB.writes).toEqual([]);

      // --- 5. Reloading the settled dashboard writes nothing ---------------
      const registryBeforeReload = [...registry.keys()].sort();

      // A real reload: the stored session is present, so the output reads
      // before any of the app's effects settle.
      await pageB.goto(dashboardUrl);

      const readAfterReload = await until(async () => {
        const ids = await pageB.evaluate(visibleTracks);
        return ids?.length === 3 ? ids : undefined;
      }, 30_000);
      expect(readAfterReload).toEqual(["t1", "t2", "t3"]);

      await settle(2_000);
      const reloadState = await pageB.evaluate(pageState);
      if (reloadState.writes.length > 0) {
        console.log("DASH_B_RELOAD_WROTE", reloadState);
        console.log("DASH_REGISTRY_AFTER_RELOAD", {
          before: registryBeforeReload,
          after: [...registry.keys()].sort(),
        });
      }
      expect(reloadState.writes).toEqual([]);

      // --- 5. A hint-channel reconnect on a settled log is a read only -----
      await pageA.evaluate(resetPageOperations);
      await pageB.evaluate(resetPageOperations);

      dropChannels();
      await settle(4_000);

      const aReconnected = await pageA.evaluate(pageState);
      const bReconnected = await pageB.evaluate(pageState);
      if (aReconnected.writes.length > 0) {
        console.log("DASH_A_RECONNECT_WROTE", aReconnected);
      }
      if (bReconnected.writes.length > 0) {
        console.log("DASH_B_RECONNECT_WROTE", bReconnected);
      }
      expect(aReconnected.writes).toEqual([]);
      expect(bReconnected.writes).toEqual([]);
    } finally {
      await pageA.close();
      await pageB.close();
      await staticServer.http.shutdown();
      await http.shutdown();
    }
  });
});
