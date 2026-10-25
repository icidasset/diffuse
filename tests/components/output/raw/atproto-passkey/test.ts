import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("components/output/raw/atproto-passkey", () => {
  it("did returns null when not authenticated", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/output/raw/atproto-passkey/element.js");
      const output = new mod.CLASS();
      document.body.append(output);
      return output.did() ?? null;
    });

    expect(result).toBe(null);
  });

  it("rev returns null when not authenticated", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/output/raw/atproto-passkey/element.js");
      const output = new mod.CLASS();
      document.body.append(output);
      return output.rev() ?? null;
    });

    expect(result).toBe(null);
  });

  it("ready returns false when not authenticated", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/output/raw/atproto-passkey/element.js");
      const output = new mod.CLASS();
      document.body.append(output);
      return output.ready();
    });

    expect(result).toBe(false);
  });

  it("listRecords returns empty array when not authenticated", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/output/raw/atproto-passkey/element.js");
      const output = new mod.CLASS();
      document.body.append(output);
      return output.listRecords("sh.diffuse.output.facet");
    });

    expect(result).toEqual([]);
  });

  it("getLatestCommit returns null when not authenticated", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/output/raw/atproto-passkey/element.js");
      const output = new mod.CLASS();
      document.body.append(output);
      return output.getLatestCommit();
    });

    expect(result).toBe(null);
  });

  it("putRecords resolves without throwing when not authenticated", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/output/raw/atproto-passkey/element.js");
      const output = new mod.CLASS();
      document.body.append(output);

      await output.facets.save([]);
      return true;
    });

    expect(result).toBe(true);
  });

  // Session restore

  it("clears a stored DID when the OAuth session is gone", async () => {
    const result = await testWeb(async () => {
      localStorage.setItem(
        "diffuse/output/raw/atproto-passkey/did",
        "did:plc:example",
      );

      const mod = await import("~/components/output/raw/atproto-passkey/element.js");
      const output = new mod.CLASS();
      document.body.append(output);

      await output.whenRestored();

      return {
        did: output.did(),
        ready: output.ready(),
        stored: localStorage.getItem(
          "diffuse/output/raw/atproto-passkey/did",
        ),
      };
    });

    expect(result.did).toBe(null);
    expect(result.ready).toBe(false);
    expect(result.stored).toBe(null);
  });

  it("settings.collection is loading initially", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/output/raw/atproto-passkey/element.js");
      const output = new mod.CLASS();
      document.body.append(output);
      return output.settings.collection().state;
    });

    expect(result).toBe("loading");
  });

  it("tracks.collection is loading initially", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/output/raw/atproto-passkey/element.js");
      const output = new mod.CLASS();
      document.body.append(output);
      return output.tracks.collection().state;
    });

    expect(result).toBe("loading");
  });
});
