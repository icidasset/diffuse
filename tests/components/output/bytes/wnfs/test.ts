import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("components/output/bytes/wnfs", () => {
  it("ready is false before a session exists", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/output/bytes/wnfs/element.js");
      const output = new mod.CLASS();
      document.body.append(output);

      await output.whenRestored();

      return {
        ready: output.ready(),
        did: output.did() ?? null,
        root: output.dataRoot() ?? null,
      };
    });

    expect(result).toEqual({ ready: false, did: null, root: null });
  });

  it("signOut is safe when nothing was configured", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/output/bytes/wnfs/element.js");
      const output = new mod.CLASS();
      document.body.append(output);

      await output.whenRestored();
      await output.signOut();

      return {
        ready: output.ready(),
        did: output.did() ?? null,
        root: output.dataRoot() ?? null,
      };
    });

    expect(result).toEqual({ ready: false, did: null, root: null });
  });

  it("Firkin SDK can mint a delegation in the browser", async () => {
    const result = await testWeb(async () => {
      const sdk = await import("~/vendor/firkin/index.js");

      const account = await sdk.Signer.fromSeedHex("11".repeat(32));
      const session = await sdk.Signer.generate();

      const delegation = await sdk.delegate(account, {
        subject: account.did(),
        audience: session.did(),
        command: "/firkin",
        expireIn: 60,
      });

      const token = await sdk.inlineInvocation(session, {
        command: "/firkin/blob/read",
        proofs: [delegation],
        expireIn: 60,
      });

      return {
        hasProcess: typeof globalThis.process !== "undefined",
        hasBuffer: typeof globalThis.Buffer !== "undefined",
        proof: delegation.proof.length,
        token: token.length,
      };
    });

    expect(result.proof).toBeGreaterThan(0);
    expect(result.token).toBeGreaterThan(0);
  });

  it("bloom filters work in the browser bundle", async () => {
    const result = await testWeb(async () => {
      const { BloomFilter } = await import(
        "~/vendor/firkin/index.js"
      );

      const filter = await BloomFilter.build(1);
      const item = new Uint8Array([1, 2, 3]);

      await filter.insert(item);
      return await filter.contains(item);
    });

    expect(result).toBe(true);
  });

  it("collections load as empty without a session", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/output/bytes/wnfs/element.js");
      const common = await import("~/common/output.js");

      const output = new mod.CLASS();
      document.body.append(output);

      await output.whenRestored();

      const [tracks, facets] = await Promise.all([
        common.data(output.tracks),
        common.data(output.facets),
      ]);

      return { tracks: tracks.length, facets: facets.length };
    });

    expect(result).toEqual({ tracks: 0, facets: 0 });
  });
});
