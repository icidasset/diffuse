// @ts-nocheck -- the fake server, crypto shim and dynamic wiring are untyped

import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

/**
 * A bulk save (the export-import facet's path) applies the change in
 * a worker and streams per-batch deltas back. The main thread only applies the
 * deltas, with yields.
 */
describe("components/output/raw/firkin/encrypted/incremental-loro bulk save", () => {
  it("imports a large collection through the worker", async () => {
    const result = await testWeb(async () => {
      const { Signer, delegate, WebCryptoSigner } = await import(
        "~/vendor/firkin/index.js"
      );

      // --- Ed25519 shim ------------------------------------------------------
      const signerByPub = new Map();
      const pubBytes = new Map();

      const hex = (bytes) =>
        Array.from(bytes).map((b) => b.toString(16).padStart(2, "0")).join("");

      const ogImportKey = crypto.subtle.importKey.bind(crypto.subtle);
      const ogVerify = crypto.subtle.verify.bind(crypto.subtle);

      const decoyKey = () =>
        ogImportKey(
          "raw",
          new Uint8Array([0]),
          { name: "HMAC", hash: "SHA-256" },
          false,
          ["sign", "verify"],
        );

      crypto.subtle.importKey = async (
        format,
        data,
        alg,
        extractable,
        usages,
      ) => {
        if (alg?.name === "Ed25519") {
          const bytes = new Uint8Array(data);
          const id = hex(bytes);
          let decoy = pubBytes.get(id);
          if (!decoy) {
            decoy = await decoyKey();
            pubBytes.set(decoy, bytes);
          }
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

      const ogWebSign = WebCryptoSigner.prototype.sign;
      WebCryptoSigner.prototype.sign = async function (message) {
        const signer = this.pubkeyBytes
          ? signerByPub.get(hex(this.pubkeyBytes))
          : undefined;
        if (signer) return signer.sign(message);
        return ogWebSign.call(this, message);
      };

      // --- Fake Firkin server ------------------------------------------------
      const blobs = new Map();
      const registry = new Map();
      const warnings = [];

      const ogWarn = console.warn.bind(console);
      console.warn = (...args) => {
        warnings.push(args.map(String).join(" "));
        ogWarn(...args);
      };

      const json = (body, status = 200) =>
        new Response(JSON.stringify(body), {
          status,
          headers: { "Content-Type": "application/json" },
        });

      const fakeFetch = async (input, init) => {
        const url = typeof input === "string" ? input : input.url;
        const path = decodeURIComponent(new URL(url).pathname);
        const method = init?.method ?? "GET";

        if (path === "/v1/registry") return json({ namespaces: [] });

        if (path === "/v1/registry/lookup") {
          const body = JSON.parse(init.body);
          const found = {};
          for (const name of body.names ?? []) {
            const cid = registry.get(`${body.namespace}/${name}`);
            if (cid) found[name] = cid;
          }
          return json({ found });
        }

        if (path.startsWith("/v1/registry/") && method === "PUT") {
          const [ns, name] = path.slice("/v1/registry/".length).split("/");
          registry.set(`${ns}/${name}`, JSON.parse(init.body).cid);
          return new Response(null, { status: 204 });
        }

        if (path.startsWith("/v1/registry/")) {
          const ns = path.slice("/v1/registry/".length);
          const names = {};
          for (const [key, cid] of registry) {
            const [kns, kname] = key.split("/");
            if (kns === ns) names[kname] = cid;
          }
          return json({ names });
        }

        if (path.startsWith("/v1/blob/") && method === "PUT") {
          const cid = path.slice("/v1/blob/".length);
          blobs.set(
            cid,
            new Uint8Array(await new Response(init.body).arrayBuffer()),
          );
          return json({ status: "stored" });
        }

        if (path.startsWith("/v1/blob/")) {
          const bytes = blobs.get(path.slice("/v1/blob/".length));
          if (!bytes) return new Response("missing", { status: 404 });
          return new Response(bytes, { status: 200 });
        }

        if (path.startsWith("/v1/channel/")) {
          return new Response("missing", { status: 404 });
        }

        if (path === "/v1/car" && method === "POST") {
          const { parseCarBlocks } = await import("~/vendor/firkin/index.js");
          const blocks = parseCarBlocks(
            new Uint8Array(await new Response(init.body).arrayBuffer()),
          );
          for (const [cid, bytes] of blocks) blobs.set(cid, bytes);

          return json({
            blocks: blocks.size,
            stored: blocks.size,
            already: 0,
            failed_total: 0,
            failed: [],
          });
        }

        if (path.startsWith("/v1/dag/pull/") && method === "POST") {
          const { carFile } = await import("~/vendor/firkin/index.js");
          const { CID } = await import("~/vendor/multiformats/cid.js");
          const root = path.slice("/v1/dag/pull/".length);
          if (!blobs.has(root)) return new Response("missing", { status: 404 });

          const car = carFile(
            CID.parse(root),
            [...blobs].map(([cid, data]) => [CID.parse(cid), data]),
          );

          return new Response(car, {
            status: 200,
            headers: { "Content-Type": "application/vnd.ipld.car" },
          });
        }

        throw new Error(`unexpected call: ${method} ${path}`);
      };

      const realFetch = globalThis.fetch;
      globalThis.fetch = fakeFetch;

      // Count worker starts, so the test proves the bulk path used one.
      let workers = 0;
      const RealWorker = globalThis.Worker;
      globalThis.Worker = class extends RealWorker {
        constructor(...args) {
          workers++;
          super(...args);
        }
      };

      try {
        const IDB = await import("idb-keyval");

        const sessionSigner = await Signer.fromSeed(
          crypto.getRandomValues(new Uint8Array(32)),
        );
        const sessionKey = await decoyKey();
        signerByPub.set(hex(sessionSigner.pubkeyBytes), sessionSigner);

        const accountSigner = await Signer.fromSeed(
          crypto.getRandomValues(new Uint8Array(32)),
        );
        const { proof, cid: proofCid } = await delegate(accountSigner, {
          subject: accountSigner.did(),
          audience: sessionSigner.did(),
          command: "/firkin",
          expireIn: 365 * 24 * 3600,
        });

        await IDB.set("diffuse/firkin/session", {
          apiBase: "https://firkin.test",
          did: accountSigner.did(),
          privateKey: sessionKey,
          publicKey: sessionSigner.pubkeyBytes,
          proof,
          proofCid,
          expiresAt: Date.now() + 365 * 24 * 3600 * 1000,
          inlineProofs: true,
        });

        const { unlockOutputKeyWithPassphrase } = await import(
          "~/common/firkin/output-key.js"
        );
        await unlockOutputKeyWithPassphrase("test-passphrase");

        await import(
          "~/components/output/raw/firkin/encrypted/incremental-loro/element.js"
        );

        const { IDB_DOCS, logName, registryNamespace } = await import(
          "~/components/output/raw/firkin/encrypted/incremental-loro/constants.js"
        );
        const { sessionScope } = await import("~/common/firkin/session.js");
        const scope = sessionScope({
          apiBase: "https://firkin.test",
          did: accountSigner.did(),
        });

        const option = document.createElement("dob-firkin-loro");
        option.id = "bulk-output";
        option.setAttribute("namespace", "bulk-output");
        option.setAttribute("keep", "64");
        document.body.append(option);

        const until = async (fn, ms = 20_000) => {
          const deadline = Date.now() + ms;
          while (Date.now() < deadline) {
            if (await fn()) return true;
            await new Promise((resolve) => setTimeout(resolve, 50));
          }
          return await fn();
        };

        await until(() => option.ready());

        const items = Array.from({ length: 1_000 }, (_, index) => ({
          $type: "sh.diffuse.output.track",
          id: `t${index}`,
          uri: `https://example.com/${index}.mp3`,
        }));

        const tracksRef = `${registryNamespace("bulk-output")}/${
          logName("tracks", hex(sessionSigner.pubkeyBytes))
        }`;
        const docKey = `${IDB_DOCS}/${scope}/bulk-output/tracks`;
        const appliedKey = `${docKey}/applied`;
        const { headsOf, loadDocument } = await import(
          "~/components/output/raw/firkin/encrypted/incremental-loro/doc.js"
        );

        // Let the startup bootstrap settle first, so "the manifest changed"
        // below can only mean this save's push.
        await until(async () => {
          const record = await IDB.get(appliedKey);
          return typeof record?.signature === "string";
        }, 5_000);

        const manifestBeforeSave = registry.get(tracksRef);

        const startedAt = performance.now();
        await option.tracks.save(items);
        const saveMs = performance.now() - startedAt;

        const collection = option.tracks.collection();
        const ids = collection.state === "loaded"
          ? collection.data.map((item) => item.id)
          : null;

        // Let the coalesced push of this save land, as a user would before
        // reloading the page.
        const pushed = await until(
          () => registry.get(tracksRef) !== manifestBeforeSave,
          15_000,
        );
        const manifestAfterSave = registry.get(tracksRef);

        // The push records itself as applied just after the append lands, and
        // a reload happens well after that. Wait for the exact state a reload
        // checks: manifest signature, applied heads, document heads and cursor
        // all agreeing.
        await until(async () => {
          const [record, stored, cursor] = await Promise.all([
            IDB.get(appliedKey),
            IDB.get(docKey),
            IDB.get(`${docKey}/cursor`),
          ]);

          if (
            typeof record?.signature !== "string" ||
            !record.signature.includes(manifestAfterSave)
          ) {
            return false;
          }

          const doc = stored instanceof Uint8Array
            ? loadDocument(stored)
            : undefined;
          if (!doc) return false;

          const heads = headsOf(doc).join(";");

          return record.heads === heads &&
            (cursor?.heads ?? []).join(";") === heads;
        }, 5_000);

        // A reload must not pull its own write back: the push records what it
        // appended, so the manifest is recognised as applied.
        const { Firkin } = await import("~/vendor/firkin/index.js");
        const pulls = [];
        const ogDagPull = Firkin.prototype.dagPull;
        Firkin.prototype.dagPull = async function (root) {
          pulls.push(root);
          return await ogDagPull.call(this, root);
        };

        // A reloaded element loads the base written by the compact persist.
        const reloaded = document.createElement("dob-firkin-loro");
        reloaded.id = "bulk-output-reloaded";
        reloaded.setAttribute("namespace", "bulk-output");
        reloaded.setAttribute("keep", "64");
        document.body.append(reloaded);

        const reloadedCount = await until(() => {
          const col = reloaded.tracks.collection();
          return col.state === "loaded" && col.data.length === items.length;
        });

        const reloadPulledTracks = pulls.includes(manifestAfterSave);

        // A settled reload — nothing touched since the last push — should make
        // no log requests at all beyond the namespace listing: no DAG pull, no
        // upload, no registry write, no manifest lookup.
        await new Promise((resolve) => setTimeout(resolve, 1_200));

        let logRequests = 0;
        const tracked = [
          "dagPull",
          "ingestCar",
          "upload",
          "registrySetIf",
          "registryLookup",
        ];
        const originals = new Map();
        for (const name of tracked) {
          originals.set(name, Firkin.prototype[name]);
          Firkin.prototype[name] = async function (...args) {
            logRequests += 1;
            return await originals.get(name).apply(this, args);
          };
        }

        const settled = document.createElement("dob-firkin-loro");
        settled.id = "bulk-output-settled";
        settled.setAttribute("namespace", "bulk-output");
        settled.setAttribute("keep", "64");
        document.body.append(settled);

        const settledCount = await until(() => {
          const col = settled.tracks.collection();
          return col.state === "loaded" && col.data.length === items.length;
        });
        await new Promise((resolve) => setTimeout(resolve, 800));

        const settledLogRequests = logRequests;

        for (const name of tracked) {
          Firkin.prototype[name] = originals.get(name);
        }

        settled.remove();

        option.remove();
        reloaded.remove();

        await IDB.del(docKey);
        await IDB.del(`${docKey}/cursor`);
        await IDB.del(`${docKey}/applied`);
        await IDB.del(`${docKey}/changes`);

        // A fresh device reads the log; it has nothing of its own to publish.
        let remoteWrites = 0;
        const writeNames = [
          "ingestCar",
          "upload",
          "registrySetIf",
          "registryLookup",
        ];
        const writeOriginals = new Map();
        for (const name of writeNames) {
          writeOriginals.set(name, Firkin.prototype[name]);
          Firkin.prototype[name] = async function (...args) {
            remoteWrites += 1;
            return await writeOriginals.get(name).apply(this, args);
          };
        }

        const remoteReader = document.createElement("dob-firkin-loro");
        remoteReader.id = "bulk-output-remote";
        remoteReader.setAttribute("namespace", "bulk-output");
        remoteReader.setAttribute("keep", "64");
        document.body.append(remoteReader);

        const remoteCount = await until(() => {
          const col = remoteReader.tracks.collection();
          return col.state === "loaded" && col.data.length === items.length;
        }, 20_000);

        for (const name of writeNames) {
          Firkin.prototype[name] = writeOriginals.get(name);
        }

        return {
          ready: option.ready(),
          workers,
          saveMs,
          ids,
          reloadedCount,
          reloadPulledTracks,
          settledCount,
          settledLogRequests,
          pushed,
          remoteCount,
          remoteWrites,
          warnings: warnings.filter((warning) =>
            !warning.includes("initSync") &&
            !warning.includes("Could not push the") &&
            // Two elements share this browser's device key, so full documents
            // can repeat a change; applying the duplicate is a no-op.
            !warning.includes("Dropping a log entry")
          ),
        };
      } finally {
        globalThis.fetch = realFetch;
      }
    });

    console.log(
      "BULK_RESULT",
      JSON.stringify({
        ...result,
        ids: Array.isArray(result.ids) ? result.ids.length : result.ids,
      }),
    );

    expect(result.ready).toBe(true);
    expect(result.workers).toBeGreaterThan(0);
    expect(result.ids).toHaveLength(1_000);
    expect(result.reloadedCount).toBe(true);
    expect(result.reloadPulledTracks).toBe(false);
    expect(result.settledCount).toBe(true);
    expect(result.settledLogRequests).toBe(0);
    expect(result.pushed).toBe(true);
    expect(result.remoteCount).toBe(true);
    expect(result.remoteWrites).toBe(0);
    expect(result.warnings).toEqual([]);
  });
});
