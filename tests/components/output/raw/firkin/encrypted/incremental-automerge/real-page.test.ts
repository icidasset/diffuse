// @ts-nocheck -- the fake server, crypto shim and page driving are untyped

import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { launch } from "@astral/astral";

/**
 * Drives the REAL dashboard page to prove the opt-in feature facet works end to
 * end: enabling the facet adds the Firkin output option to the orchestrator's
 * configurator, selecting it saves through the single merged output element,
 * and the save lands on the remote log.
 *
 * The reload path (persisted selection after a reload) is covered by the
 * chain-level tests instead: an opt-in output facet bootstraps from the local
 * facet store, which is a separate concern from this element's wiring.
 */
describe("firkin output facet on the real page", () => {
  it("adds the output option and saves through it", async () => {
    await using browser = await launch();
    const page = await browser.newPage("http://localhost:3000/dashboard");

    const result = await page.evaluate(async () => {
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

      // --- Fake Firkin server ----------------------------------------------
      const blobs = new Map();
      const registry = new Map();
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
        if (!url.startsWith("https://firkin.test")) {
          return realFetch(url, init);
        }

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

        // --- Enable the feature facet the way "add software" does -----------
        const optionId = "do-output__dc-output__firkin-automerge";

        const deadlineBoot = Date.now() + 20_000;
        let output = null;
        while (Date.now() < deadlineBoot) {
          output = document.querySelector("do-output");
          if (output) break;
          await new Promise((resolve) => setTimeout(resolve, 100));
        }
        if (!output) return { fatal: "no do-output element" };

        const { buildFacets } = await import("~/common/facets/utils.js");
        const { insertPreludes } = await import("~/common/facets/prelude.js");

        const facet = buildFacets([
          "facets/data/firkin-automerge/index.tile",
        ])[0];

        const facetsCollection = output.facets.collection();
        const existingFacets = facetsCollection.state === "loaded"
          ? facetsCollection.data
          : [];

        await output.facets.save([
          ...existingFacets.filter((existing) => existing.id !== facet.id),
          facet,
        ]);

        // Wait until the record is readable through the output (it is the
        // prelude source the page's boot uses).
        const deadlineSaved = Date.now() + 10_000;
        while (Date.now() < deadlineSaved) {
          const collection = output.facets.collection();
          if (
            collection.state === "loaded" &&
            collection.data.some((saved) => saved.uri === facet.uri)
          ) {
            break;
          }
          await new Promise((resolve) => setTimeout(resolve, 50));
        }

        await insertPreludes([facet]);

        // --- The facet's prelude adds the option ----------------------------
        const deadlineOption = Date.now() + 10_000;
        while (
          Date.now() < deadlineOption &&
          !document.getElementById(optionId)
        ) {
          await new Promise((resolve) => setTimeout(resolve, 50));
        }

        const option = document.getElementById(optionId);
        if (!option) {
          return {
            fatal: "no firkin option",
            options: (await output.options()).map((o) => o.id),
            warnings,
            errors,
          };
        }

        // --- Select it and save through it ----------------------------------
        await output.select(optionId);

        await output.tracks.save([
          {
            $type: "sh.diffuse.output.track",
            id: "t1",
            uri: "https://example.com/t1.mp3",
          },
        ]);

        const deadline = Date.now() + 10_000;
        while (
          Date.now() < deadline &&
          ![...registry.keys()].some((name) => name.includes("tracks"))
        ) {
          await new Promise((resolve) => setTimeout(resolve, 50));
        }

        return {
          selectedId: output.selected()?.id ?? null,
          uploads: calls.filter((call) => call.startsWith("PUT /v1/blob/")).length,
          manifests: [...registry.keys()],
          tracks: output.tracks.collection(),
          warnings,
          errors,
        };
      } finally {
        globalThis.fetch = realFetch;
      }
    });

    console.log("REAL_PAGE_RESULT", JSON.stringify({
      ...result,
      tracks: result.tracks?.state === "loaded"
        ? result.tracks.data.map((t) => t.id)
        : result.tracks,
    }, null, 2));

    expect(result.fatal).toBeUndefined();
    expect(result.errors).toEqual([]);
    expect(result.selectedId).toBe("do-output__dc-output__firkin-automerge");
    expect(result.uploads).toBeGreaterThan(0);
    expect(result.manifests.length).toBeGreaterThan(0);
    expect(
      result.tracks?.state === "loaded"
        ? result.tracks.data.map((t) => t.id)
        : result.tracks,
    ).toEqual(["t1"]);
  });
});
