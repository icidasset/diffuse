import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("components/output/raw/atproto", () => {
  it("did returns null when not authenticated", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/output/raw/atproto/element.js");
      const output = new mod.CLASS();
      document.body.append(output);
      return output.did() ?? null;
    });

    expect(result).toBe(null);
  });

  it("rev returns null when not authenticated", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/output/raw/atproto/element.js");
      const output = new mod.CLASS();
      document.body.append(output);
      return output.rev() ?? null;
    });

    expect(result).toBe(null);
  });

  it("ready returns false when not authenticated", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/output/raw/atproto/element.js");
      const output = new mod.CLASS();
      document.body.append(output);
      return output.ready();
    });

    expect(result).toBe(false);
  });

  it("listRecords returns empty array when not authenticated", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/output/raw/atproto/element.js");
      const output = new mod.CLASS();
      document.body.append(output);
      return output.listRecords("sh.diffuse.output.facet");
    });

    expect(result).toEqual([]);
  });

  it("getLatestCommit returns null when not authenticated", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/output/raw/atproto/element.js");
      const output = new mod.CLASS();
      document.body.append(output);
      return output.getLatestCommit();
    });

    expect(result).toBe(null);
  });

  it("putRecords resolves without throwing when not authenticated", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/output/raw/atproto/element.js");
      const output = new mod.CLASS();
      document.body.append(output);

      await output.putRecords("sh.diffuse.output.facet", []);
      return true;
    });

    expect(result).toBe(true);
  });

  it("tracks.collection is loading initially", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/output/raw/atproto/element.js");
      const output = new mod.CLASS();
      document.body.append(output);
      return output.tracks.collection().state;
    });

    expect(result).toBe("loading");
  });
});
