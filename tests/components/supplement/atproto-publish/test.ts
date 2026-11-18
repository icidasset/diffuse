import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

// NOTE: The `testWeb` callback is serialized and evaluated inside the browser,
// so it can't reference module-scope values. Keys are inlined on purpose.

describe("components/supplement/atproto-publish", () => {
  // Initial state

  it("did returns null when not authenticated", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/supplement/atproto-publish/element.js"
      );
      const el = new mod.CLASS();
      document.body.append(el);
      return el.did();
    });

    expect(result).toBe(null);
  });

  it("handle returns null when not authenticated", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/supplement/atproto-publish/element.js"
      );
      const el = new mod.CLASS();
      document.body.append(el);
      return el.handle();
    });

    expect(result).toBe(null);
  });

  it("ready returns false when not authenticated", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/supplement/atproto-publish/element.js"
      );
      const el = new mod.CLASS();
      document.body.append(el);
      return el.ready();
    });

    expect(result).toBe(false);
  });

  it("syncing returns false initially", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/supplement/atproto-publish/element.js"
      );
      const el = new mod.CLASS();
      document.body.append(el);
      return el.syncing();
    });

    expect(result).toBe(false);
  });

  // Unauthenticated actions

  it("publish resolves without throwing when not authenticated", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/supplement/atproto-publish/element.js"
      );
      const el = new mod.CLASS();
      document.body.append(el);

      await el.publish();

      return el.syncing();
    });

    expect(result).toBe(false);
  });

  it("clears the stored DID when the OAuth session is gone", async () => {
    const result = await testWeb(async () => {
      localStorage.setItem(
        "diffuse/output/raw/atproto-passkey/did",
        "did:plc:example",
      );

      const mod = await import(
        "~/components/supplement/atproto-publish/element.js"
      );
      const el = new mod.CLASS();
      document.body.append(el);

      await el.whenRestored();

      return {
        did: el.did(),
        ready: el.ready(),
        stored: localStorage.getItem(
          "diffuse/output/raw/atproto-passkey/did",
        ),
      };
    });

    expect(result.did).toBe(null);
    expect(result.ready).toBe(false);
    expect(result.stored).toBe(null);
  });
});
