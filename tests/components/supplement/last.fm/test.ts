import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("components/supplement/last.fm", () => {
  // Initial state

  it("isAuthenticated returns false initially", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/supplement/last.fm/element.js");
      const el = new mod.CLASS();
      document.body.append(el);
      return el.isAuthenticated();
    });

    expect(result).toBe(false);
  });

  it("isAuthenticating returns false initially", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/supplement/last.fm/element.js");
      const el = new mod.CLASS();
      document.body.append(el);
      return el.isAuthenticating();
    });

    expect(result).toBe(false);
  });

  it("handle returns null initially", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/supplement/last.fm/element.js");
      const el = new mod.CLASS();
      document.body.append(el);
      return el.handle();
    });

    expect(result).toBe(null);
  });

  // Session restore from localStorage

  it("restores session from localStorage on connectedCallback", async () => {
    const result = await testWeb(async () => {
      localStorage.setItem(
        "diffuse/supplement/last.fm/session",
        JSON.stringify({ key: "test-session-key", name: "testuser" }),
      );

      const mod = await import("~/components/supplement/last.fm/element.js");
      const el = new mod.CLASS();
      document.body.append(el);

      // #tryRestore is async; wait for the microtasks to settle
      await new Promise((r) => setTimeout(r, 10));

      return { authenticated: el.isAuthenticated(), handle: el.handle() };
    });

    expect(result.authenticated).toBe(true);
    expect(result.handle).toBe("testuser");
  });

  // Sign out

  it("signOut clears isAuthenticated and handle", async () => {
    const result = await testWeb(async () => {
      localStorage.setItem(
        "diffuse/supplement/last.fm/session",
        JSON.stringify({ key: "test-session-key", name: "testuser" }),
      );

      const mod = await import("~/components/supplement/last.fm/element.js");
      const el = new mod.CLASS();
      document.body.append(el);

      await new Promise((r) => setTimeout(r, 10));

      el.signOut();

      return { authenticated: el.isAuthenticated(), handle: el.handle() };
    });

    expect(result.authenticated).toBe(false);
    expect(result.handle).toBe(null);
  });

  it("signOut removes session from localStorage", async () => {
    const result = await testWeb(async () => {
      localStorage.setItem(
        "diffuse/supplement/last.fm/session",
        JSON.stringify({ key: "test-session-key", name: "testuser" }),
      );

      const mod = await import("~/components/supplement/last.fm/element.js");
      const el = new mod.CLASS();
      document.body.append(el);

      el.signOut();

      return localStorage.getItem("diffuse/supplement/last.fm/session");
    });

    expect(result).toBe(null);
  });

  it("signOut on unauthenticated element does not throw", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/supplement/last.fm/element.js");
      const el = new mod.CLASS();
      document.body.append(el);
      el.signOut();
      return el.isAuthenticated();
    });

    expect(result).toBe(false);
  });

  // Unauthenticated API calls

  it("nowPlaying throws when not authenticated", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/supplement/last.fm/element.js");
      const el = new mod.CLASS();
      document.body.append(el);

      try {
        await el.nowPlaying({
          $type: "sh.diffuse.output.track",
          id: "t1",
          uri: "https://example.com/track.mp3",
        });
        return false;
      } catch (err) {
        return err instanceof Error &&
          err.message.includes("Not authenticated");
      }
    });

    expect(result).toBe(true);
  });

  it("scrobble throws when not authenticated", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/supplement/last.fm/element.js");
      const el = new mod.CLASS();
      document.body.append(el);

      try {
        await el.scrobble(
          {
            $type: "sh.diffuse.output.track",
            id: "t1",
            uri: "https://example.com/track.mp3",
          },
          Date.now(),
        );
        return false;
      } catch (err) {
        return err instanceof Error &&
          err.message.includes("Not authenticated");
      }
    });

    expect(result).toBe(true);
  });
});
