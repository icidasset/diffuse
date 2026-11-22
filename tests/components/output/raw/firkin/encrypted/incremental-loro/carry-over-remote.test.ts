// @ts-nocheck -- the fake server, crypto shim and dynamic wiring are untyped

import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

/**
 * Whether the data the configurator **carries over** from the local store into
 * a newly selected Firkin output reaches the remote, or only the local cache.
 *
 * The carry-over (`OutputConfigurator.#carryOver`) saves through the selected
 * output's standard interface, so every carried-over collection should be
 * pushed to the Firkin log like any other save. Collections with no local data
 * must be left alone: saving an empty collection is not a no-op on a syncing
 * output, because it may already have read its own log into the document.
 */
describe("components/output/raw/firkin/encrypted/incremental-loro carry-over", () => {
  it("pushes carried-over collections to the remote log", async () => {
    const result = await testWeb(async () => {
      const { Signer, delegate, WebCryptoSigner } = await import(
        "~/vendor/firkin/index.js"
      );

      // --- Ed25519 shim (test browser predates native WebCrypto Ed25519) ----
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

      crypto.subtle.importKey = async (format, data, alg, extractable, usages) => {
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
      const calls = [];
      const errors = [];

      const json = (body, status = 200) =>
        new Response(JSON.stringify(body), {
          status,
          headers: { "Content-Type": "application/json" },
        });

      const fakeFetch = async (input, init) => {
        const url = typeof input === "string" ? input : input.url;
        const path = decodeURIComponent(new URL(url).pathname);
        const method = init?.method ?? "GET";

        calls.push(`${method} ${path}`);

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

        // The element reads the shared output key itself, so store it the way
        // sign-in does (this also updates the reactive signal).
        const { unlockOutputKeyWithPassphrase } = await import(
          "~/common/firkin/output-key.js"
        );
        await unlockOutputKeyWithPassphrase("test-passphrase");

        // --- The output element ---------------------------------------
        await import(
          "~/components/output/raw/firkin/encrypted/incremental-loro/element.js"
        );
        const idb = await import(
          "~/components/output/polymorphic/indexed-db/element.js"
        );
        const { CLASS: Configurator } = await import(
          "~/components/configurator/output/element.js"
        );

        const root = document.createElement("div");
        document.body.append(root);

        const option = document.createElement("dob-firkin-loro");
        option.id = "option-carry-test";
        option.setAttribute("namespace", "firkin-loro");
        option.setAttribute("keep", "64");

        const def = new idb.CLASS();
        def.id = "def-carry-test";
        def.setAttribute("namespace", "def-carry-test");

        const configurator = new Configurator();
        configurator.setAttribute("default", def.id);
        configurator.append(def, option);

        root.append(configurator);

        // Let the configurator's async connectedCallback settle.
        await new Promise((resolve) => setTimeout(resolve, 100));

        // Data in the local store, before the Firkin output is selected —
        // this is what the carry-over copies into the selected output.
        await def.tracks.save([
          {
            $type: "sh.diffuse.output.track",
            id: "t1",
            uri: "https://example.com/t1.mp3",
          },
        ]);
        await def.facets.save([
          { $type: "sh.diffuse.output.facet", id: "f1", name: "Carried" },
        ]);

        const { registryNamespace, logName } = await import(
          "~/components/output/raw/firkin/encrypted/incremental-loro/constants.js"
        );
        const names = ["facets", "tracks"];
        const allNames = [
          "collaborations",
          "facets",
          "playlistItems",
          "settings",
          "tracks",
        ];
        const ref = (name) =>
          `${registryNamespace("firkin-loro")}/${
            logName(name, hex(sessionSigner.pubkeyBytes))
          }`;
        const manifestsBefore = new Map(
          allNames.map((name) => [name, registry.get(ref(name)) ?? null]),
        );

        await configurator.select(option.id);

        // Wait until the collections that had local data have a manifest on the
        // remote — that is the carry-over having been pushed, not just cached.
        // Each such manifest has to *change*: the startup bootstrap writes one
        // too, and starting a second element while the carry-over push is still
        // coalescing would hand leadership over before it lands.
        const deadline = Date.now() + 15_000;
        while (Date.now() < deadline) {
          const pushed = names.every((name) => {
            const cid = registry.get(ref(name));
            return cid !== undefined && cid !== manifestsBefore.get(name);
          });

          if (pushed) break;

          await new Promise((resolve) => setTimeout(resolve, 50));
        }

        const carried = new Set(
          [...registry.keys()].map((key) => {
            // "<namespace>/<encoded-collection>.<device-hex>"
            const [encoded] = key.split("/")[1].split(".");
            return [...Array(encoded.length / 2)]
              .map((_, i) =>
                parseInt(encoded.slice(i * 2, i * 2 + 2), 16)
              )
              .map((byte) => String.fromCharCode(byte))
              .join("");
          }),
        );

        // Wait for the carry-over pushes to finish and their cursors to be
        // recorded before starting a second element. Both elements share this
        // browser's device manifest, so starting one while the writer still has
        // pushes queued would race it for that manifest.
        const { IDB_DOCS } = await import(
          "~/components/output/raw/firkin/encrypted/incremental-loro/constants.js"
        );

        const cursorHeads = async (name) =>
          (await IDB.get(`${IDB_DOCS}/firkin-loro/${name}/cursor`))?.heads;

        const settled = Date.now() + 5_000;
        while (Date.now() < settled) {
          const heads = await Promise.all([
            cursorHeads("tracks"),
            cursorHeads("facets"),
          ]);

          if (
            heads.every((value) => Array.isArray(value) && value.length > 0)
          ) {
            break;
          }

          await new Promise((resolve) => setTimeout(resolve, 50));
        }

        // Read the carried-over data back through a second, fresh instance.
        const reader = document.createElement("dob-firkin-loro");
        reader.id = "option-carry-read";
        reader.setAttribute("namespace", "firkin-loro");
        reader.setAttribute("keep", "64");

        root.append(reader);

        const read = async (name) => {
          const deadlineRead = Date.now() + 8_000;
          let collection = { state: "loading" };
          while (Date.now() < deadlineRead) {
            try {
              await reader[name].reload();
              collection = reader[name].collection();
            } catch (err) {
              collection = { state: "error", error: String(err) };
            }
            if (collection.state === "loaded" && collection.data?.length > 0) {
              break;
            }
            await new Promise((resolve) => setTimeout(resolve, 50));
          }
          return collection;
        };

        const tracks = await read("tracks");
        const facets = await read("facets");

        return {
          carried: [...carried].sort(),
          uploads: calls.filter((call) =>
            call.startsWith("PUT /v1/blob/") || call.startsWith("POST /v1/car")
          ).length,
          tracks: tracks.state === "loaded"
            ? tracks.data.map((item) => item.id)
            : tracks.state,
          facets: facets.state === "loaded"
            ? facets.data.map((item) => item.id)
            : facets.state,
          errors,
        };
      } finally {
        globalThis.fetch = realFetch;
      }
    });

    console.log("CARRY_OVER_RESULT", JSON.stringify(result, null, 2));

    expect(result.errors).toEqual([]);
    // Only the collections with local data are carried over. Writing empty
    // collections would create logs (and, on a device that already pulled,
    // delete records) for data that was never local.
    expect(result.carried).toEqual(["facets", "tracks"]);
    expect(result.uploads).toBeGreaterThan(0);
    expect(result.tracks).toEqual(["t1"]);
    expect(result.facets).toEqual(["f1"]);
  });
});
