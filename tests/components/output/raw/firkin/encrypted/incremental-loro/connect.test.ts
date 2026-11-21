// @ts-nocheck -- the fake server, crypto shim and dynamic wiring are untyped

import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

/**
 * Setting up the output through the connect facet signs in *after* the option
 * element exists. The element checks the session when it connects, so without a
 * re-check on sign-in it can stay not-ready until an unrelated online event —
 * and the configurator falls back to the default output after its grace period.
 */
describe("components/output/raw/firkin/encrypted/incremental-loro connect", () => {
  it("becomes ready when the session and key arrive after it connected", async () => {
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
      console.warn = (...args) => {
        pushLog(warnings, args);
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

        // The hint channel is optional; refusing it must not break the output.
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

      try {
        const IDB = await import("idb-keyval");

        // --- The output exists *before* sign-in, as on the connect page ----
        await import(
          "~/components/output/raw/firkin/encrypted/incremental-loro/element.js"
        );
        const { CLASS: Configurator } = await import(
          "~/components/configurator/output/element.js"
        );

        const option = document.createElement("dob-firkin-loro");
        option.id = "connect-output";
        option.setAttribute("namespace", "connect-output");
        option.setAttribute("keep", "64");

        const configurator = new Configurator();
        configurator.append(option);
        document.body.append(configurator);

        const until = async (fn, ms = 5_000) => {
          const deadline = Date.now() + ms;
          while (Date.now() < deadline) {
            if (await fn()) return true;
            await new Promise((resolve) => setTimeout(resolve, 50));
          }
          return await fn();
        };

        // Let it connect with no session at all.
        await new Promise((resolve) => setTimeout(resolve, 400));
        const connectedWithoutSession = option.ready();

        // --- Sign in: session first, then the passkey-derived output key ---
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

        const readyAfterSignIn = await until(() => option.ready(), 3_000);

        // Select it the way the connect facet does, then outlive the
        // configurator's grace period.
        await configurator.select(option.id);
        await new Promise((resolve) => setTimeout(resolve, 6_000));

        return {
          connectedWithoutSession,
          readyAfterSignIn,
          readyAfterGrace: option.ready(),
          selectedAfterGrace: configurator.selected()?.id ?? null,
          hasSelectedAfterGrace: configurator.hasSelected(),
          warnings: warnings.filter((warning) =>
            !warning.includes("initSync")
          ),
        };
      } finally {
        globalThis.fetch = realFetch;
      }
    });

    console.log("CONNECT_RESULT", JSON.stringify(result, null, 2));

    expect(result.connectedWithoutSession).toBe(false);
    expect(result.readyAfterSignIn).toBe(true);
    expect(result.readyAfterGrace).toBe(true);
    expect(result.selectedAfterGrace).toBe("connect-output");
    expect(result.hasSelectedAfterGrace).toBe(true);
    expect(result.warnings).toEqual([]);
  });
});
