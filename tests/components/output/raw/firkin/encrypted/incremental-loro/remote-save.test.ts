// @ts-nocheck -- the fake server, crypto shim and dynamic wiring are untyped

import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

/**
 * The full Firkin save path in a browser: the single
 * `dob-firkin-loro` output element, with a fake in-page Firkin server
 * standing in for the network.
 *
 * Regression target: selecting the Firkin option must append entries to the
 * remote log (blob upload + signed manifest registry write), not only to the
 * local cache.
 */
describe("components/output/raw/firkin/encrypted/incremental-loro remote save", () => {
  it("pushes saved collections to the remote log", async () => {
    const result = await testWeb(async () => {
      const { Signer, delegate, WebCryptoSigner } = await import(
        "~/vendor/firkin/index.js"
      );

      // --- Ed25519 shim ------------------------------------------------------
      // The test browser's WebCrypto predates native Ed25519, but the app's
      // signing paths ({ name: "Ed25519" }) must still work. Back them with
      // the SDK's pure-JS signer.
      //
      // Signing is patched on `WebCryptoSigner.prototype.sign`, keyed by the
      // public key bytes: a session key stored in IndexedDB comes back as a
      // NEW CryptoKey object (structured clone), so keying on object identity
      // would not survive the round-trip. Verification goes through
      // `crypto.subtle.importKey`/`verify`, keyed by the raw public key.
      const signerByPub = new Map(); // hex(pubkey) -> Signer
      const pubBytes = new Map(); // decoy public CryptoKey -> pubkey bytes

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
      const blobs = new Map(); // cid -> bytes
      const registry = new Map(); // "namespace/name" -> cid
      const calls = [];
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

        throw new Error(`fake server got an unexpected call: ${method} ${path}`);
      };

      const realFetch = globalThis.fetch;
      globalThis.fetch = fakeFetch;

      // Record every blob the client uploads/fetches, so a bad write can be
      // told apart from a bad read.
      const transfers = [];
      const saveCalls = [];
      const pageErrors = [];

      window.addEventListener("error", (event) => {
        pageErrors.push(String(event.error ?? event.message));
      });
      window.addEventListener("unhandledrejection", (event) => {
        pageErrors.push(`unhandledrejection: ${event.reason}`);
      });

      try {
        const { Firkin } = await import("~/vendor/firkin/index.js");

        const ogUpload = Firkin.prototype.upload;
        Firkin.prototype.upload = async function (data, opts) {
          const bytes = data instanceof Blob
            ? new Uint8Array(await data.arrayBuffer())
            : data;
          const out = await ogUpload.call(this, data, opts);
          transfers.push({
            dir: "put",
            cid: out.cid,
            len: bytes.length,
            head: hex(bytes.slice(0, 24)),
          });
          return out;
        };

        const ogGet = Firkin.prototype.get;
        Firkin.prototype.get = async function (cid) {
          const bytes = await ogGet.call(this, cid);
          transfers.push({
            dir: "get",
            cid,
            len: bytes.length,
            head: hex(bytes.slice(0, 24)),
          });
          return bytes;
        };
      } catch (err) {
        errors.push(`instrumentation failed: ${err}`);
      }

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

        // Directly exercise the session → client → server path.
        const diag = {};
        try {
          const { getSessionClient } = await import(
            "~/common/firkin/session.js"
          );
          const client = await getSessionClient();
          diag.hasClient = client !== undefined;
          if (client) {
            try {
              diag.registryList = await client.rb.registryList();
            } catch (err) {
              diag.registryListError = `${err?.name}: ${err?.message}`;
            }
          }
        } catch (err) {
          diag.sessionError = `${err?.name}: ${err?.message}`;
        }

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
        option.id = "option-remote-test";
        option.setAttribute("namespace", "firkin-loro");
        option.setAttribute("keep", "64");

        // Observe saves reaching the output.
        const ogSave = option.tracks.save.bind(option);
        option.tracks.save = async (items) => {
          saveCalls.push(`save:${items?.length ?? "null"}`);
          return await ogSave(items);
        };

        const def = new idb.CLASS();
        def.id = "def-remote-test";
        def.setAttribute("namespace", "def-remote-test");

        const configurator = new Configurator();
        configurator.setAttribute("default", def.id);
        configurator.append(def, option);

        root.append(configurator);

        // Let the configurator's async connectedCallback finish before
        // selecting, the same way a real page has.
        await new Promise((resolve) => setTimeout(resolve, 100));

        await configurator.select(option.id);

        await configurator.tracks.save([
          {
            $type: "sh.diffuse.output.track",
            id: "t1",
            uri: "https://example.com/t1.mp3",
          },
        ]);

        // Give the queued push a moment to land remotely.
        const deadline = Date.now() + 8_000;
        while (
          Date.now() < deadline &&
          ![...registry.keys()].some((name) => name.includes("tracks"))
        ) {
          await new Promise((resolve) => setTimeout(resolve, 50));
        }

        // A second, fresh instance reading the same remote log must see the
        // save.
        const reader = document.createElement("dob-firkin-loro");
        reader.id = "option-remote-read";
        reader.setAttribute("namespace", "firkin-loro");
        reader.setAttribute("keep", "64");

        root.append(reader);

        let read = { state: "loading" };
        const readDeadline = Date.now() + 8_000;
        while (Date.now() < readDeadline) {
          try {
            read = await reader.tracks.reload().then(() =>
              reader.tracks.collection()
            );
          } catch (err) {
            read = { state: "error", error: String(err) };
          }
          if (read.state === "loaded" && read.data?.length > 0) break;
          await new Promise((resolve) => setTimeout(resolve, 50));
        }

        return {
          diag,
          selectedId: configurator.selected()?.id ?? null,
          optionReady: option.ready(),
          saveCalls,
          pageErrors,
          find: {
            id: option.id,
            localName: option.localName,
            isConnected: option.isConnected,
            byId: document.getElementById("option-remote-test") === option,
            inRoot: configurator.root().querySelector("#option-remote-test") !==
              null,
            hasIdentifier: "identifier" in option,
            hasTracks: "tracks" in option,
          },
          calls: calls.slice(0, 40),
          uploads: calls.filter((call) =>
            call.startsWith("PUT /v1/blob/") || call.startsWith("POST /v1/car")
          ).length,
          manifests: [...registry.keys()],
          transfers,
          tracks: read.state === "loaded"
            ? read.data.map((item) => item.id)
            : read.state,
          warnings,
          errors,
        };
      } finally {
        globalThis.fetch = realFetch;
      }
    });

    console.log("REMOTE_SAVE_RESULT", JSON.stringify(result, null, 2));

    // The engine's wasm glue warns about its deprecated initSync
    // under the full-fat entrypoint; everything else must be silent.
    expect(
      result.warnings.filter((warning) => !warning.includes("initSync")),
    ).toEqual([]);
    expect(result.errors).toEqual([]);
    expect(result.uploads).toBeGreaterThan(0);
    expect(result.manifests.length).toBeGreaterThan(0);
    expect(result.tracks).toEqual(["t1"]);
  });
});
