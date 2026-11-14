// @ts-nocheck -- the evaluated browser function intentionally uses untyped JS

import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("common/firkin/session", () => {
  it("reads the shared session and builds a client", async () => {
    const result = await testWeb(async () => {
      const sdk = await import("~/vendor/firkin/index.js");
      const {
        deleteSession,
        getSessionClient,
        saveSession,
      } = await import("~/common/firkin/session.js");

      const account = await sdk.Signer.generate();
      // Only persistence and client construction are under test; no signing
      // happens, so a widely-supported P-256 handle is enough here.
      const keyPair = await crypto.subtle.generateKey(
        { name: "ECDSA", namedCurve: "P-256" },
        false,
        ["sign"],
      );

      await saveSession({
        apiBase: "http://example.invalid",
        did: account.did(),
        privateKey: keyPair.privateKey,
        publicKey: account.pubkeyBytes,
        proof: new Uint8Array([1]),
        proofCid: "bafkreitest",
        expiresAt: Date.now() + 60_000,
      });

      const client = await getSessionClient();
      const connected = Boolean(client);
      const did = client?.session.did;

      await deleteSession();
      const afterDelete = await getSessionClient();

      return {
        connected,
        did,
        expectedDid: account.did(),
        afterDelete: Boolean(afterDelete),
      };
    });

    expect(result).toEqual({
      connected: true,
      did: result.expectedDid,
      expectedDid: result.expectedDid,
      afterDelete: false,
    });
  });
});
