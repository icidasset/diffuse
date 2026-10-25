import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

// NOTE: The `testWeb` callback is serialized and evaluated inside the browser,
// so it can't reference module-scope values. Keys are inlined on purpose.

describe("components/supplement/rocksky", () => {
  // Initial state

  it("isAuthenticated returns false initially", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/supplement/rocksky/element.js");
      const el = new mod.CLASS();
      document.body.append(el);
      return el.isAuthenticated();
    });

    expect(result).toBe(false);
  });

  it("isAuthenticating returns false initially", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/supplement/rocksky/element.js");
      const el = new mod.CLASS();
      document.body.append(el);
      return el.isAuthenticating();
    });

    expect(result).toBe(false);
  });

  it("handle returns null initially", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/supplement/rocksky/element.js");
      const el = new mod.CLASS();
      document.body.append(el);
      return el.handle();
    });

    expect(result).toBe(null);
  });

  // Connection state must reflect the OAuth session, not just the saved marker.

  it("does not report connected when the saved session marker has no OAuth session", async () => {
    const result = await testWeb(async () => {
      localStorage.setItem(
        "diffuse/supplement/rocksky/session",
        JSON.stringify({ did: "did:plc:example" }),
      );

      const mod = await import("~/components/supplement/rocksky/element.js");
      const el = new mod.CLASS();
      document.body.append(el);

      // #tryRestore is async; wait for it to settle.
      await new Promise((r) => setTimeout(r, 50));

      return {
        authenticated: el.isAuthenticated(),
        handle: el.handle(),
        marker: localStorage.getItem("diffuse/supplement/rocksky/session"),
      };
    });

    expect(result.authenticated).toBe(false);
    expect(result.handle).toBe(null);
    // The stale marker must be removed so the next load doesn't show
    // "connected" either.
    expect(result.marker).toBe(null);
  });

  it("clears the saved session marker when the OAuth session is gone", async () => {
    const result = await testWeb(async () => {
      localStorage.setItem(
        "diffuse/supplement/rocksky/session",
        JSON.stringify({ did: "did:plc:example" }),
      );
      // A DID with no stored atcute session means the session is gone.
      localStorage.setItem(
        "diffuse/supplement/rocksky/atproto/did",
        "did:plc:example",
      );

      const mod = await import("~/components/supplement/rocksky/element.js");
      const el = new mod.CLASS();
      document.body.append(el);

      await new Promise((r) => setTimeout(r, 100));

      return {
        authenticated: el.isAuthenticated(),
        handle: el.handle(),
        did: localStorage.getItem("diffuse/supplement/rocksky/atproto/did"),
        marker: localStorage.getItem("diffuse/supplement/rocksky/session"),
      };
    });

    expect(result.authenticated).toBe(false);
    expect(result.handle).toBe(null);
    expect(result.did).toBe(null);
    expect(result.marker).toBe(null);
  });

  // Sign out

  it("signOut clears the connection state and saved session", async () => {
    const result = await testWeb(async () => {
      localStorage.setItem(
        "diffuse/supplement/rocksky/session",
        JSON.stringify({ did: "did:plc:example" }),
      );

      const mod = await import("~/components/supplement/rocksky/element.js");
      const el = new mod.CLASS();
      document.body.append(el);

      await new Promise((r) => setTimeout(r, 50));
      await el.signOut();

      return {
        authenticated: el.isAuthenticated(),
        handle: el.handle(),
        marker: localStorage.getItem("diffuse/supplement/rocksky/session"),
      };
    });

    expect(result.authenticated).toBe(false);
    expect(result.handle).toBe(null);
    expect(result.marker).toBe(null);
  });

  // Unauthenticated API calls

  it("scrobble returns quietly when not authenticated", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/supplement/rocksky/element.js");
      const el = new mod.CLASS();
      document.body.append(el);

      await el.scrobble(
        {
          $type: "sh.diffuse.output.track",
          id: "t1",
          uri: "https://example.com/track.mp3",
        },
        Date.now(),
        { duration: 180_000 },
      );

      return el.isAuthenticated();
    });

    expect(result).toBe(false);
  });
});
