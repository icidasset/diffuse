// @ts-nocheck -- the fake server, crypto shim and dynamic wiring are untyped

import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

/**
 * Interrupted pushes must resume.
 *
 * Saves are written to IndexedDB first and pushed from an in-memory queue, so
 * navigating away (or closing the tab) before a queued push runs used to leave
 * the change local-only: nothing pushed it on the next load, and another
 * device never saw it. A fresh element reading the same local cache stands in
 * for that reload here.
 */
describe("components/output/raw/firkin/encrypted/incremental-automerge resume", () => {
  it("pushes changes whose upload was interrupted by a reload", async () => {
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

        // --- Upload control ---------------------------------------------------
        const { Firkin } = await import("~/vendor/firkin/index.js");

        let uploads = 0;
        let broken = false;

        const ogUpload = Firkin.prototype.upload;
        Firkin.prototype.upload = async function (data, opts) {
          if (broken) throw new Error("simulated interruption");
          const out = await ogUpload.call(this, data, opts);
          uploads++;
          return out;
        };

        // --- The output elements ----------------------------------------------
        await import(
          "~/components/output/raw/firkin/encrypted/incremental-automerge/element.js"
        );

        const { logName, registryNamespace } = await import(
          "~/components/output/raw/firkin/encrypted/incremental-automerge/constants.js"
        );

        const namespace = "firkin-resume";

        const root = document.createElement("div");
        document.body.append(root);

        const makeOption = (id) => {
          const option = document.createElement("dob-firkin-automerge");
          option.id = id;
          option.setAttribute("namespace", namespace);
          option.setAttribute("keep", "64");
          root.append(option);
          return option;
        };

        const until = async (fn, ms = 8_000) => {
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

        /** Registry ref of this device's tracks manifest. */
        const tracksRef = `${registryNamespace(namespace)}/${
          logName("tracks", hex(sessionSigner.pubkeyBytes))
        }`;

        /** CID of the tracks manifest, which changes whenever it is rewritten. */
        const trackManifestCid = () => registry.get(tracksRef);

        // 1. First save: pushed and acknowledged.
        const writer = makeOption("resume-writer");
        await until(() => writer.ready());

        await writer.tracks.save([track("t1")]);

        // Wait for the first push and the startup bootstrap pushes to drain, so
        // the manifest captured below is the one the interrupted save sees.
        let lastUploads = -1;
        while (lastUploads !== uploads) {
          lastUploads = uploads;
          await new Promise((resolve) => setTimeout(resolve, 300));
        }

        const manifestBefore = trackManifestCid();
        const uploadsBeforeInterruption = uploads;

        // 2. The upload breaks and the page "navigates away" before the queue
        //    drains: the save lands locally, the push fails.
        broken = true;
        await writer.tracks.save([track("t1"), track("t2")]);

        const failed = await until(() =>
          warnings.some((warning) => warning.includes("Could not push the"))
        );

        writer.remove();
        broken = false;

        const manifestAfterFailure = trackManifestCid();
        const uploadsAfterFailure = uploads;

        // 3. A fresh element with the same namespace is what a reload does: it
        //    loads the local document and must notice the unpushed heads.
        const reloaded = makeOption("resume-reloaded");
        await until(() => reloaded.ready());

        // A rebuilt manifest means a new entry was appended to the tracks log.
        const manifestAfterResume = await until(() => {
          const cid = trackManifestCid();
          return cid !== undefined && cid !== manifestAfterFailure
            ? cid
            : undefined;
        });

        const reloadedTracks = await until(() => {
          const collection = reloaded.tracks.collection();
          return collection.state === "loaded" && collection.data.length > 0
            ? collection.data.map((item) => item.id).sort()
            : undefined;
        });

        return {
          failed,
          manifestBefore,
          manifestAfterFailure,
          resumed: manifestAfterResume !== undefined &&
            manifestAfterResume !== manifestAfterFailure,
          uploadsBeforeInterruption,
          uploadsAfterFailure,
          uploads,
          reloadedTracks,
          errors,
          warnings: warnings.filter((warning) =>
            !warning.includes("initSync") &&
            !warning.includes("Could not push the")
          ),
        };
      } finally {
        globalThis.fetch = realFetch;
      }
    });

    console.log("RESUME_RESULT", JSON.stringify(result, null, 2));

    expect(result.failed).toBe(true);
    expect(result.manifestAfterFailure).toBe(result.manifestBefore);
    expect(result.resumed).toBe(true);
    expect(result.warnings).toEqual([]);
    expect(result.errors).toEqual([]);
    expect(result.reloadedTracks).toEqual(["t1", "t2"]);
  });
});
