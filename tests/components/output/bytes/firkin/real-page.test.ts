// @ts-nocheck -- the fake server, crypto shim and page driving are untyped

import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { launch } from "@astral/astral";

/**
 * Drives the REAL main page: the output orchestrator, the output bundle facet
 * and the Firkin chain it wires (`dtob-automerge` → `dtob-encrypt` →
 * `dob-firkin`), against a fake in-page Firkin server.
 *
 * Regression target: selecting the Firkin option on the real page must push
 * saves to the remote log.
 */
describe("firkin output on the real page", () => {
  it("pushes saved collections to the remote log", async () => {
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

      // --- Fake Firkin server ------------------------------------------------
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
          return fetch(url, init);
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

      globalThis.fetch = fakeFetch;

      // --- A session + output key, as sign-in would have stored them --------
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

      // --- Select the Firkin option the way the connect page does -----------
      const optionId = "do-output__dc-output__firkin-automerge";

      // The orchestrator is created lazily by the page's boot code, and the
      // Firkin option is appended by the output bundle facet.
      const deadlineBoot = Date.now() + 20_000;
      let output = null;
      while (Date.now() < deadlineBoot) {
        output = document.querySelector("do-output");
        if (output && document.getElementById(optionId)) break;
        await new Promise((resolve) => setTimeout(resolve, 100));
      }
      if (!output) return { fatal: "no do-output element" };

      const option = document.getElementById(optionId);
      if (!option) {
        return {
          fatal: "no firkin option",
          options: (await output.options()).map((o) => o.id),
        };
      }

      await output.select(optionId);

      // The key arrives only now, the way a sign-in does: store it through the
      // module's own path so the reactive delivery picks it up while the
      // output is already alive.
      const { unlockOutputKeyWithPassphrase } = await import(
        "~/common/firkin/output-key.js"
      );
      await unlockOutputKeyWithPassphrase("test-passphrase");

      // Save through the orchestrator, like any UI action would.
      await output.tracks.save([
        {
          $type: "sh.diffuse.output.track",
          id: "t1",
          uri: "https://example.com/t1.mp3",
        },
      ]);

      // Give the push a moment to land remotely.
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
        warnings: warnings.slice(0, 10),
        errors: errors.slice(0, 10),
        // Signal the outer test to reload and re-check the boot path.
        reload: true,
      };
    });

    console.log("REAL_PAGE_RESULT", JSON.stringify({
      ...result,
      tracks: result.tracks.state === "loaded"
        ? result.tracks.data.map((t) => t.id)
        : result.tracks,
    }, null, 2));

    expect(result.fatal).toBeUndefined();
    expect(result.errors).toEqual([]);
    expect(result.uploads).toBeGreaterThan(0);
    expect(result.manifests.length).toBeGreaterThan(0);

    // --- Reload with the selection persisted: the boot-time path -----------
    await page.reload();

    const result2 = await page.evaluate(async () => {
      const { Signer, WebCryptoSigner } = await import(
        "~/vendor/firkin/index.js"
      );

      const hex = (bytes) =>
        Array.from(bytes).map((b) => b.toString(16).padStart(2, "0")).join("");

      // Same shims as before the reload (fresh page context).
      const signerByPub = new Map();
      const pubBytes = new Map();

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

      // Re-register the session signer (new key objects after the reload).
      const IDB = await import("idb-keyval");
      const session = await IDB.get("diffuse/firkin/session");
      signerByPub.set(hex(session.publicKey), null); // placeholder, set below

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
          return fetch(url, init);
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

      globalThis.fetch = fakeFetch;

      // The session's signer must sign again on this page: rebuild the
      // pure-JS signer from the stored public key is impossible (the private
      // seed is gone), so re-derive a fresh delegation? No — instead re-run
      // sign-in semantics: store a NEW session (the same account).
      const { delegate } = await import("~/vendor/firkin/index.js");
      const sessionSigner = await Signer.fromSeed(
        crypto.getRandomValues(new Uint8Array(32)),
      );
      const sessionKey = await decoyKey();
      signerByPub.set(hex(sessionSigner.pubkeyBytes), sessionSigner);
      signerByPub.delete(hex(session.publicKey));

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

      // Wait for the boot-time selection of the persisted output.
      const deadlineBoot = Date.now() + 20_000;
      let output = null;
      while (Date.now() < deadlineBoot) {
        output = document.querySelector("do-output");
        if (output?.selected()?.id) break;
        await new Promise((resolve) => setTimeout(resolve, 100));
      }
      if (!output) return { fatal: "no do-output element" };

      // Save a second track after the boot has settled, appending to whatever
      // the output already holds (the UI always saves the full collection).
      await new Promise((resolve) => setTimeout(resolve, 500));

      const col = output.tracks.collection();
      const existing = col.state === "loaded" && Array.isArray(col.data)
        ? col.data
        : [];

      await output.tracks.save([
        ...existing,
        {
          $type: "sh.diffuse.output.track",
          id: "t2",
          uri: "https://example.com/t2.mp3",
        },
      ]);

      const deadline = Date.now() + 10_000;
      while (
        Date.now() < deadline &&
        calls.filter((c) => c.startsWith("PUT /v1/blob/")).length === 0
      ) {
        await new Promise((resolve) => setTimeout(resolve, 50));
      }

      return {
        selectedId: output.selected()?.id ?? null,
        uploads: calls.filter((call) => call.startsWith("PUT /v1/blob/")).length,
        registryCalls: calls.filter((c) =>
          c.startsWith("PUT /v1/registry/") && c.includes("747261636b73")
        ).length,
        tracks: output.tracks.collection(),
        warnings: warnings.slice(0, 10),
        errors: errors.slice(0, 10),
      };
    });

    console.log(
      "REAL_PAGE_RELOAD_RESULT",
      JSON.stringify({
        ...result2,
        tracks: result2.tracks.state === "loaded"
          ? result2.tracks.data.map((t) => t.id)
          : result2.tracks,
      }, null, 2),
    );

    expect(result2.fatal).toBeUndefined();
    expect(result2.errors).toEqual([]);
    expect(result2.uploads).toBeGreaterThan(0);
    expect(result2.registryCalls).toBeGreaterThan(0);
    expect(
      result2.tracks.state === "loaded"
        ? result2.tracks.data.map((t) => t.id)
        : result2.tracks,
    ).toEqual(["t1", "t2"]);

    await page.close();
  });
});
