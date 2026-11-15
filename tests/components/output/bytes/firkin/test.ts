// @ts-nocheck -- the evaluated browser function intentionally uses untyped JS

import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("components/output/bytes/firkin", () => {
  it("is not ready and has no data without a session", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/output/bytes/firkin/element.js");

      const output = new mod.CLASS();
      document.body.append(output);

      await new Promise((resolve) => setTimeout(resolve, 0));
      await output.tracks.reload();

      const collection = output.tracks.collection();

      let error = "";
      try {
        await output.tracks.save(new Uint8Array([1]));
      } catch (err) {
        error = err instanceof Error ? err.message : String(err);
      }

      return {
        ready: output.ready(),
        state: collection.state,
        data: collection.state === "loaded" ? collection.data ?? null : null,
        error,
      };
    });

    expect(result.ready).toBe(false);
    expect(result.state).toBe("loaded");
    expect(result.data).toBe(null);
    expect(result.error).toContain("Firkin is not connected");
  });

  it("becomes ready when a shared session exists", async () => {
    const result = await testWeb(async () => {
      const { saveSession } = await import("~/common/firkin/session.js");

      // Only session presence is under test; no signing happens.
      const keyPair = await crypto.subtle.generateKey(
        { name: "ECDSA", namedCurve: "P-256" },
        false,
        ["sign"],
      );

      await saveSession({
        apiBase: "http://127.0.0.1:1",
        did: "did:key:test",
        privateKey: keyPair.privateKey,
        publicKey: new Uint8Array(32),
        proof: new Uint8Array([1]),
        proofCid: "bafkreitest",
        expiresAt: Date.now() + 60_000,
      });

      const mod = await import("~/components/output/bytes/firkin/element.js");
      const output = new mod.CLASS();
      document.body.append(output);

      for (let i = 0; i < 100 && !output.ready(); i++) {
        await new Promise((resolve) => setTimeout(resolve, 10));
      }

      return output.ready();
    });

    expect(result).toBe(true);
  });
});
