import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("components/transformer/output/raw/atproto-sync", () => {
  it("ready returns true", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/transformer/output/raw/atproto-sync/element.js"
      );
      const t = new mod.CLASS();
      return t.ready();
    });

    expect(result).toBe(true);
  });

  it("tracks.collection is loading when no local output is connected", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/transformer/output/raw/atproto-sync/element.js"
      );
      const t = new mod.CLASS() as any;
      // Not connected to DOM — #localOutput stays undefined
      return t.tracks.collection().state;
    });

    expect(result).toBe("loading");
  });

  it("facets.collection is loading when no local output is connected", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/transformer/output/raw/atproto-sync/element.js"
      );
      const t = new mod.CLASS() as any;
      return t.facets.collection().state;
    });

    expect(result).toBe("loading");
  });

  it("playlistItems.collection is loading when no local output is connected", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/transformer/output/raw/atproto-sync/element.js"
      );
      const t = new mod.CLASS() as any;
      return t.playlistItems.collection().state;
    });

    expect(result).toBe("loading");
  });

  it("settings.collection is loading when no local output is connected", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/transformer/output/raw/atproto-sync/element.js"
      );
      const t = new mod.CLASS() as any;
      return t.settings.collection().state;
    });

    expect(result).toBe("loading");
  });
});
