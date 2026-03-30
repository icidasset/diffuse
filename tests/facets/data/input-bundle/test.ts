import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("facets/data/input-bundle", () => {
  it("ephemeralCache appends an ephemeral-cache element", async () => {
    const result = await testWeb(async () => {
      const { ephemeralCache } = await import(
        "~/facets/data/input-bundle/index.inline.js"
      );
      const container = document.createElement("div");
      ephemeralCache(container as never);
      return container.children[0]?.localName ?? null;
    });
    expect(result).toBe("di-ephemeral-cache");
  });

  it("https appends an https element", async () => {
    const result = await testWeb(async () => {
      const { https } = await import(
        "~/facets/data/input-bundle/index.inline.js"
      );
      const container = document.createElement("div");
      https(container as never);
      return container.children[0]?.localName ?? null;
    });
    expect(result).toBe("di-https");
  });

  it("icecast appends an icecast element", async () => {
    const result = await testWeb(async () => {
      const { icecast } = await import(
        "~/facets/data/input-bundle/index.inline.js"
      );
      const container = document.createElement("div");
      icecast(container as never);
      return container.children[0]?.localName ?? null;
    });
    expect(result).toBe("di-icecast");
  });

  it("local appends a local element", async () => {
    const result = await testWeb(async () => {
      const { local } = await import(
        "~/facets/data/input-bundle/index.inline.js"
      );
      const container = document.createElement("div");
      local(container as never);
      return container.children[0]?.localName ?? null;
    });
    expect(result).toBe("di-local");
  });

  it("opensubsonic appends an opensubsonic element", async () => {
    const result = await testWeb(async () => {
      const { opensubsonic } = await import(
        "~/facets/data/input-bundle/index.inline.js"
      );
      const container = document.createElement("div");
      opensubsonic(container as never);
      return container.children[0]?.localName ?? null;
    });
    expect(result).toBe("di-opensubsonic");
  });

  it("s3 appends an s3 element", async () => {
    const result = await testWeb(async () => {
      const { s3 } = await import(
        "~/facets/data/input-bundle/index.inline.js"
      );
      const container = document.createElement("div");
      s3(container as never);
      return container.children[0]?.localName ?? null;
    });
    expect(result).toBe("di-s3");
  });

  it("each function appends exactly one element", async () => {
    const result = await testWeb(async () => {
      const { ephemeralCache, https, icecast, local, opensubsonic, s3 } =
        await import("~/facets/data/input-bundle/index.inline.js");

      const counts: Record<string, number> = {};
      for (const [name, fn] of Object.entries({ ephemeralCache, https, icecast, local, opensubsonic, s3 })) {
        const container = document.createElement("div");
        fn(container as never);
        counts[name] = container.children.length;
      }
      return counts;
    });

    for (const count of Object.values(result)) {
      expect(count).toBe(1);
    }
  });
});
