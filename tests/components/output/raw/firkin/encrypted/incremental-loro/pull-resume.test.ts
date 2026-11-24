// @ts-nocheck -- the fake server, crypto shim and dynamic wiring are untyped

import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

/**
 * Interrupted pulls must continue where they left off.
 *
 * Entries are cached as they arrive, so a pull that stopped partway is
 * finished by the next one (only the missing packs are fetched). Reloads pull
 * again, and a page switch or a hint-channel reconnect triggers a catch-up read
 * because hints are best effort.
 */
describe("components/output/raw/firkin/encrypted/incremental-loro pull resume", () => {
  it("continues an interrupted pull across reloads and page switches", async () => {
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
      const errors = [];

      const pushLog = (list, args) => {
        try {
          list.push(
            Array.from(args).map((item) =>
              item instanceof Error
                ? `${item.name}: ${item.message}`
                : String(item)
            ).join(" "),
          );
        } catch {
          list.push("(unserializable)");
        }
      };

      const ogWarn = console.warn.bind(console);
      const ogError = console.error.bind(console);
      console.warn = (...args) => {
        pushLog(warnings, args);
        ogWarn(...args);
      };
      console.error = (...args) => {
        pushLog(errors, args);
        ogError(...args);
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

      try {
        // --- A session + output key, as sign-in would have stored them ------
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

        // --- Read control -----------------------------------------------------
        const { Firkin } = await import("~/vendor/firkin/index.js");

        let failGets = false;
        let countGets = false;
        /** @type {string[]} */
        const getCids = [];
        let listingGets = 0;

        const ogRegistryNames = Firkin.prototype.registryNames;
        Firkin.prototype.registryNames = async function (ns) {
          listingGets += 1;
          return await ogRegistryNames.call(this, ns);
        };

        const ogGet = Firkin.prototype.get;
        Firkin.prototype.get = async function (cid) {
          if (countGets) getCids.push(cid);
          if (failGets) throw new Error("simulated read interruption");
          return await ogGet.call(this, cid);
        };

        const ogDagPull = Firkin.prototype.dagPull;
        Firkin.prototype.dagPull = async function (root) {
          if (countGets) getCids.push(root);
          if (failGets) throw new Error("simulated read interruption");
          return await ogDagPull.call(this, root);
        };

        // --- The output elements ----------------------------------------------
        await import(
          "~/components/output/raw/firkin/encrypted/incremental-loro/element.js"
        );

        const { COLLECTIONS, IDB_DOCS, IDB_PACKS, logName, registryNamespace } =
          await import(
            "~/components/output/raw/firkin/encrypted/incremental-loro/constants.js"
          );
        const { logSignature } = await import(
          "~/components/output/raw/firkin/encrypted/incremental-loro/log.js"
        );
        const { sessionScope } = await import("~/common/firkin/session.js");

        const scope = sessionScope({
          apiBase: "https://firkin.test",
          did: accountSigner.did(),
        });

        const root = document.createElement("div");
        document.body.append(root);

        const makeOption = (id, namespace) => {
          const option = document.createElement("dob-firkin-loro");
          option.id = id;
          option.setAttribute("namespace", namespace);
          option.setAttribute("keep", "64");
          root.append(option);
          return option;
        };

        const until = async (fn, ms = 10_000) => {
          const deadline = Date.now() + ms;
          while (Date.now() < deadline) {
            const value = await fn();
            if (value) return value;
            await new Promise((resolve) => setTimeout(resolve, 50));
          }
          return await fn();
        };

        const track = (id) => ({
          $type: "sh.diffuse.output.track",
          id,
          uri: `https://example.com/${id}.mp3`,
        });

        const tracksOf = (element) => {
          const collection = element.tracks.collection();
          return collection.state === "loaded" && collection.data.length > 0
            ? collection.data.map((item) => item.id).sort()
            : undefined;
        };

        const docKey = (namespace, name) =>
          `${IDB_DOCS}/${scope}/${namespace}/${name}`;

        const clearLocal = async (namespace, name) => {
          await IDB.del(docKey(namespace, name));
          await IDB.del(`${docKey(namespace, name)}/cursor`);
          await IDB.del(`${docKey(namespace, name)}/applied`);
          await IDB.del(`${docKey(namespace, name)}/changes`);
        };

        const cursorHeads = async (namespace, name) =>
          (await IDB.get(`${docKey(namespace, name)}/cursor`))?.heads;

        const tracksManifest = (namespace) =>
          registry.get(
            `${registryNamespace(namespace)}/${
              logName("tracks", hex(sessionSigner.pubkeyBytes))
            }`,
          );

        /** The listing the output reads, as `readEntries` would see it. */
        const namesOf = (namespace) => {
          const prefix = `${registryNamespace(namespace)}/`;

          return Object.fromEntries(
            [...registry]
              .filter(([key]) => key.startsWith(prefix))
              .map(([key, cid]) => [key.slice(prefix.length), cid]),
          );
        };

        /** Drop the cached packs so a pull that reads them has to fetch them. */
        const clearPackCache = async () => {
          for (const key of await IDB.keys()) {
            if (
              typeof key === "string" && key.startsWith(`${IDB_PACKS}/`)
            ) {
              await IDB.del(key);
            }
          }
        };

        // ===== 1. An interrupted pull does not poison a reload ==============
        const namespaceA = "firkin-pull-a";

        const writer = makeOption("pull-writer", namespaceA);
        await until(() => writer.ready());
        await writer.tracks.save([track("t1")]);

        const pushed = await until(async () => {
          const heads = await cursorHeads(namespaceA, "tracks");
          return Array.isArray(heads) && heads.length > 0;
        });

        // The writer is gone and the local cache is empty: what the phone has
        // after a reload (or on a first visit).
        writer.remove();
        await clearLocal(namespaceA, "tracks");

        warnings.length = 0;
        failGets = true;

        const broken = makeOption("pull-broken", namespaceA);
        const failed = await until(() =>
          warnings.some((warning) => warning.includes("Could not pull the"))
        );

        broken.remove();
        failGets = false;

        const reloaded = makeOption("pull-reloaded", namespaceA);
        const reloadedTracks = await until(() => tracksOf(reloaded));

        // ===== 2. A page switch continues an interrupted pull ===============
        const namespaceC = "firkin-pull-c";

        const writerC = makeOption("pull-writer-c", namespaceC);
        await until(() => writerC.ready());
        await writerC.tracks.save([track("t1")]);
        await until(async () => {
          const heads = await cursorHeads(namespaceC, "tracks");
          return Array.isArray(heads) && heads.length > 0;
        });

        // Let the writer's own pull finish before tearing it down: removing it
        // mid-apply would let a late IDB write race the fresh state below.
        await until(async () =>
          (await IDB.get(`${docKey(namespaceC, "tracks")}/applied`)) !==
            undefined
        );

        writerC.remove();
        await clearLocal(namespaceC, "tracks");

        warnings.length = 0;
        failGets = true;

        const paused = makeOption("pull-paused", namespaceC);
        // Only the collection whose local state was cleared has a log to read;
        // a settled one is recognised from the registry listing and never
        // requests its manifest, so it cannot fail.
        const failedBeforeSwitch = await until(() =>
          warnings.some((warning) => warning.includes("Could not pull the"))
        );

        // Let the failed pull clear its in-flight marker before the catch-up.
        await new Promise((resolve) => setTimeout(resolve, 100));

        // The network is back; the page is shown again (the pull was
        // interrupted, so whatever pack it did fetch is cached and the rest is
        // fetched now).
        failGets = false;

        Object.defineProperty(document, "visibilityState", {
          configurable: true,
          get: () => "visible",
        });
        document.dispatchEvent(new Event("visibilitychange"));
        // If the failed pass was still finishing, the first event is a no-op
        // (a pull is already in flight); a second one after it settles catches
        // up either way.
        await new Promise((resolve) => setTimeout(resolve, 250));
        document.dispatchEvent(new Event("visibilitychange"));

        const continued = await until(() => tracksOf(paused));

        // ===== 3. A page switch catches up once the listing is stale ========
        const namespaceB = "firkin-pull-b";

        const liveWriter = makeOption("pull-live-writer", namespaceB);
        await until(() => liveWriter.ready());
        await liveWriter.tracks.save([track("t1")]);
        await until(() => tracksManifest(namespaceB));

        const follower = makeOption("pull-follower", namespaceB);
        await until(() => follower.ready());

        // Let the follower's initial pull finish before the next write, so the
        // assertion below is about a missed hint rather than a pull in flight.
        await new Promise((resolve) => setTimeout(resolve, 300));

        const initial = await until(() => tracksOf(follower));

        const manifestBefore = tracksManifest(namespaceB);

        await liveWriter.tracks.save([track("t1"), track("t2")]);
        await until(() => tracksManifest(namespaceB) !== manifestBefore);

        // No hint is delivered in this harness, so the follower is still on t1.
        await new Promise((resolve) => setTimeout(resolve, 500));
        const whileHidden = tracksOf(follower);

        // A page switch reuses a fresh listing; only a stale one re-reads the
        // registry. Wait past the listing TTL to model a missed hint.
        await new Promise((resolve) => setTimeout(resolve, 5_500));

        Object.defineProperty(document, "visibilityState", {
          configurable: true,
          get: () => "visible",
        });
        document.dispatchEvent(new Event("visibilitychange"));

        const afterVisible = await until(() => {
          const ids = tracksOf(follower);
          return ids?.length === 2 ? ids : undefined;
        });

        // Isolate the read count below: only the element under test should be
        // pulling from here on.
        reloaded.remove();
        paused.remove();
        follower.remove();
        liveWriter.remove();

        // ===== 4. An unchanged log is recognised from its pack CIDs ========
        const namespaceD = "firkin-pull-d";

        const writerD = makeOption("pull-writer-d", namespaceD);
        await until(() => writerD.ready());

        // A collection with no data has no log and no bootstrap cursor; the
        // save below creates the first entry.
        const headsBeforeSave =
          ((await cursorHeads(namespaceD, "tracks")) ?? []).join(";");

        await writerD.tracks.save([track("t1")]);

        await until(async () => {
          const heads = await cursorHeads(namespaceD, "tracks");
          return Array.isArray(heads) && heads.join(";") !== headsBeforeSave;
        });

        const firstReader = makeOption("pull-first-reader", namespaceD);
        await until(() => tracksOf(firstReader));

        const appliedKey = (name) => `${docKey(namespaceD, name)}/applied`;
        const applied = await until(async () => {
          const record = await IDB.get(appliedKey("tracks"));

          // Wait for the signature too: that is what the next pull compares.
          return record && typeof record.packs === "string" &&
              typeof record.signature === "string"
            ? record
            : undefined;
        }, 5_000);
        const tracksPacks = new Set(applied?.packs.split(";") ?? []);

        // The writer and the reader share IndexedDB. Make the reader converge
        // on the current listing (a hint can be missed) so what it applied is
        // what a new element will read.
        const converged = Date.now() + 10_000;
        while (Date.now() < converged) {
          const record = await IDB.get(appliedKey("tracks"));
          if (
            record?.signature === logSignature(namesOf(namespaceD), "tracks")
          ) {
            break;
          }

          await firstReader.tracks.reload();
          await new Promise((resolve) => setTimeout(resolve, 100));
        }

        firstReader.remove();

        // No pack cache: only the registry signature can keep the next reader
        // from reading the log.
        await clearPackCache();

        getCids.length = 0;
        listingGets = 0;
        countGets = true;

        const secondReader = makeOption("pull-second-reader", namespaceD);
        const manifest = tracksManifest(namespaceD);
        // The registry listing is the log's identity now: an unchanged log is
        // recognised from it, before any manifest is read.
        await until(() => listingGets > 0);
        await new Promise((resolve) => setTimeout(resolve, 400));

        const tracksPacksRefetched = getCids.some((cid) =>
          tracksPacks.has(cid)
        );
        const tracksManifestRead = getCids.includes(manifest);
        const secondRead = await until(() => tracksOf(secondReader));
        const secondGets = getCids.length;
        countGets = false;

        writerD.remove();

        // ===== 5. Only an appended entry is fetched on the next pull ========
        const namespaceE = "firkin-pull-e";

        const writerE = makeOption("pull-writer-e", namespaceE);
        await until(() => writerE.ready());
        const headsBeforeE = ((await cursorHeads(namespaceE, "tracks")) ?? [])
          .join(";");

        await writerE.tracks.save([track("t1")]);
        await until(async () => {
          const heads = await cursorHeads(namespaceE, "tracks");
          return Array.isArray(heads) && heads.join(";") !== headsBeforeE;
        });

        const beforeReader = makeOption("pull-before-reader", namespaceE);
        const appliedE = await until(async () => {
          const record = await IDB.get(
            `${docKey(namespaceE, "tracks")}/applied`,
          );

          return record && typeof record.packs === "string"
            ? record
            : undefined;
        }, 5_000);
        const oldPacks = new Set(appliedE?.packs.split(";") ?? []);

        // Append a second entry. The next reader starts from the stored applied
        // log, which is a prefix of the new one.
        const manifestBeforeE = tracksManifest(namespaceE);
        await writerE.tracks.save([track("t1"), track("t2")]);
        await until(() => tracksManifest(namespaceE) !== manifestBeforeE);

        beforeReader.remove();
        writerE.remove();
        await clearPackCache();

        getCids.length = 0;
        countGets = true;

        const afterReader = makeOption("pull-after-reader", namespaceE);
        const manifestE = tracksManifest(namespaceE);
        await until(() => getCids.includes(manifestE));
        await new Promise((resolve) => setTimeout(resolve, 400));

        const afterRead = await until(() => tracksOf(afterReader));
        countGets = false;

        const currentApplied = await IDB.get(
          `${docKey(namespaceE, "tracks")}/applied`,
        );
        const newPacks = (currentApplied?.packs ?? "")
          .split(";")
          .filter((cid) => cid.length > 0 && !oldPacks.has(cid));

        const oldPacksFetched = [...oldPacks].filter((cid) =>
          getCids.includes(cid)
        );
        const newPacksFetched = newPacks.every((cid) => getCids.includes(cid));

        return {
          pushed,
          failed,
          reloadedTracks,
          tracksManifestRead,
          failedBeforeSwitch,
          continued,
          initial,
          whileHidden,
          afterVisible,
          appliedPacks: typeof applied?.packs === "string",
          tracksPacksRefetched,
          tracksPacksRefetched,
          secondRead,
          secondGets,
          afterRead,
          oldPacksFetched: oldPacksFetched.length,
          newPacksFetched,
          errors,
          warnings: warnings.filter((warning) =>
            !warning.includes("initSync") &&
            !warning.includes("Could not pull the") &&
            // The simulated read failure also breaks a push, because appending
            // re-reads the writer's current manifest.
            !warning.includes("Could not push the") &&
            // Both elements share this browser's device key, so full documents
            // written by each can repeat a change; applying the duplicate is a
            // no-op (the log tolerates it).
            !warning.includes("Dropping a log entry")
          ),
        };
      } finally {
        globalThis.fetch = realFetch;
      }
    });

    console.log("PULL_RESUME_RESULT", JSON.stringify(result, null, 2));

    expect(result.pushed).toBe(true);
    expect(result.failed).toBe(true);
    expect(result.reloadedTracks).toEqual(["t1"]);

    expect(result.failedBeforeSwitch).toBe(true);
    expect(result.continued).toEqual(["t1"]);

    expect(result.initial).toEqual(["t1"]);
    expect(result.whileHidden).toEqual(["t1"]);
    expect(result.afterVisible).toEqual(["t1", "t2"]);

    // The stored signature proved the tracks log was unchanged: neither its
    // manifest nor its packs were read, and the reader still had the data.
    expect(result.appliedPacks).toBe(true);
    expect(result.tracksPacksRefetched).toBe(false);
    expect(result.tracksManifestRead).toBe(false);
    expect(result.secondRead).toEqual(["t1"]);

    // An appended small entry rides inside the manifest, so it needs no pack
    // fetch at all; the stored log's packs are not re-read either.
    expect(result.afterRead).toEqual(["t1", "t2"]);
    expect(result.oldPacksFetched).toBe(0);
    expect(result.newPacksFetched).toBe(false);

    expect(result.warnings).toEqual([]);
    expect(result.errors).toEqual([]);
  });
});
