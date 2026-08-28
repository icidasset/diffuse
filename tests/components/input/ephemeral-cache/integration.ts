import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

/**
 * Integration tests for the ephemeral-cache input that verify the blob URL
 * lifecycle (create → fetch → revoke → fetch fails) and cache entry cleanup.
 *
 * These run in the browser via {@link testWeb} because `indexedDB` and
 * `URL.createObjectURL` are only available in a DOM context.
 */
describe("components/input/ephemeral-cache (integration)", () => {
  it("resolve creates a playable blob URL that is revoked after detach by scheme", async () => {
    const result = await testWeb(async () => {
      const IDB = await import("idb-keyval");
      const { CACHE_KEY_PREFIX } = await import(
        "~/components/input/ephemeral-cache/constants.js"
      );
      const W = await import("~/components/input/ephemeral-cache/worker.js");

      const uri = "ephemeral+cache://bafk-lifecycle-scheme";
      await IDB.set(
        CACHE_KEY_PREFIX + uri,
        new Blob(["audio-bytes"], { type: "audio/mpeg" }),
      );

      // Resolve → creates a blob URL
      const resolved = await W.resolve({ uri });
      if (!resolved || !("url" in resolved)) return { error: "resolve failed" };

      // Blob URL should be fetchable
      const resp = await fetch(resolved.url);
      const text = await resp.text();

      // Detach by scheme → revokes blob URL and removes cache entry
      await W.detach({
        fileUriOrScheme: "ephemeral+cache",
        tracks: [
          { $type: "sh.diffuse.output.track", id: "t1", uri },
        ],
      });

      // Blob URL should now be revoked
      let revoked = false;
      try {
        await fetch(resolved.url);
      } catch {
        revoked = true;
      }

      // Cache entry should be removed
      const cached = await IDB.get(CACHE_KEY_PREFIX + uri);
      await IDB.del(CACHE_KEY_PREFIX + uri);

      return {
        blobUrl: resolved.url,
        fetchedText: text,
        revoked,
        cacheRemoved: cached === undefined,
      };
    });

    expect(result.blobUrl).toMatch(/^blob:/);
    expect(result.fetchedText).toBe("audio-bytes");
    expect(result.revoked).toBe(true);
    expect(result.cacheRemoved).toBe(true);
  });

  it("detach by specific URI revokes only that blob URL, leaving others playable", async () => {
    const result = await testWeb(async () => {
      const IDB = await import("idb-keyval");
      const { CACHE_KEY_PREFIX } = await import(
        "~/components/input/ephemeral-cache/constants.js"
      );
      const W = await import("~/components/input/ephemeral-cache/worker.js");

      const keepUri = "ephemeral+cache://bafk-lifecycle-keep";
      const removeUri = "ephemeral+cache://bafk-lifecycle-remove";
      await IDB.set(
        CACHE_KEY_PREFIX + keepUri,
        new Blob(["keep-audio"], { type: "audio/mpeg" }),
      );
      await IDB.set(
        CACHE_KEY_PREFIX + removeUri,
        new Blob(["remove-audio"], { type: "audio/mpeg" }),
      );

      const rKeep = await W.resolve({ uri: keepUri });
      const rRemove = await W.resolve({ uri: removeUri });
      if (!rKeep || !("url" in rKeep) || !rRemove || !("url" in rRemove)) {
        return { error: "resolve failed" };
      }

      // Detach by specific URI
      const remaining = await W.detach({
        fileUriOrScheme: removeUri,
        tracks: [
          { $type: "sh.diffuse.output.track", id: "t1", uri: keepUri },
          { $type: "sh.diffuse.output.track", id: "t2", uri: removeUri },
        ],
      });

      // Removed blob URL should be revoked
      let removeRevoked = false;
      try {
        await fetch(rRemove.url);
      } catch {
        removeRevoked = true;
      }

      // Kept blob URL should still be fetchable
      const keepResp = await fetch(rKeep.url);
      const keepText = await keepResp.text();

      // Cleanup
      await IDB.del(CACHE_KEY_PREFIX + keepUri);

      return {
        remainingCount: remaining.length,
        remainingId: remaining[0]?.id,
        removeRevoked,
        keepText,
      };
    });

    expect(result.remainingCount).toBe(1);
    expect(result.remainingId).toBe("t1");
    expect(result.removeRevoked).toBe(true);
    expect(result.keepText).toBe("keep-audio");
  });

  it("resolve returns the same blob URL on repeated calls (cached)", async () => {
    const result = await testWeb(async () => {
      const IDB = await import("idb-keyval");
      const { CACHE_KEY_PREFIX } = await import(
        "~/components/input/ephemeral-cache/constants.js"
      );
      const W = await import("~/components/input/ephemeral-cache/worker.js");

      const uri = "ephemeral+cache://bafk-lifecycle-stable";
      await IDB.set(
        CACHE_KEY_PREFIX + uri,
        new Blob(["audio"], { type: "audio/mpeg" }),
      );

      const r1 = await W.resolve({ uri });
      const r2 = await W.resolve({ uri });

      await IDB.del(CACHE_KEY_PREFIX + uri);

      return {
        url1: r1 && "url" in r1 ? r1.url : null,
        url2: r2 && "url" in r2 ? r2.url : null,
      };
    });

    expect(result.url1).toMatch(/^blob:/);
    expect(result.url1).toBe(result.url2);
  });
});
