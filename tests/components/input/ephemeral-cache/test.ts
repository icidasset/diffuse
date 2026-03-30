import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";
import type { Track } from "~/definitions/types.d.ts";

describe("components/input/ephemeral-cache", () => {
  it("has correct SCHEME property", async () => {
    const scheme = await testWeb(async () => {
      const mod = await import(
        "~/components/input/ephemeral-cache/element.js"
      );
      const input = new mod.CLASS();
      document.body.append(input);
      return input.SCHEME;
    });

    expect(scheme).toBe("ephemeral+cache");
  });

  it("artwork returns null", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/input/ephemeral-cache/element.js"
      );
      const input = new mod.CLASS();
      document.body.append(input);
      return await input.artwork("ephemeral+cache://bafktest");
    });

    expect(result).toBe(null);
  });

  it("consult returns undetermined for scheme only", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/input/ephemeral-cache/element.js"
      );
      const input = new mod.CLASS();
      document.body.append(input);
      return await input.consult("ephemeral+cache");
    });

    expect(result.supported).toBe(true);
    if (result.supported) {
      expect(result.consult).toBe("undetermined");
    }
  });

  it("consult returns false for an uncached URI", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/input/ephemeral-cache/element.js"
      );
      const input = new mod.CLASS();
      document.body.append(input);
      return await input.consult("ephemeral+cache://bafknotcached");
    });

    expect(result.supported).toBe(true);
    if (result.supported) {
      expect(result.consult).toBe(false);
    }
  });

  it("consult returns true for a cached URI", async () => {
    const result = await testWeb(async () => {
      const IDB = await import("idb-keyval");
      const { CACHE_KEY_PREFIX } = await import(
        "~/components/input/ephemeral-cache/constants.js"
      );
      const mod = await import(
        "~/components/input/ephemeral-cache/element.js"
      );
      const input = new mod.CLASS();
      document.body.append(input);

      const uri = "ephemeral+cache://bafkcacheduri";
      await IDB.set(
        CACHE_KEY_PREFIX + uri,
        new Blob(["audio"], { type: "audio/mpeg" }),
      );

      const result = await input.consult(uri);
      await IDB.del(CACHE_KEY_PREFIX + uri);
      return result;
    });

    expect(result.supported).toBe(true);
    if (result.supported) {
      expect(result.consult).toBe(true);
    }
  });

  it("groupConsult returns only cached URIs as available", async () => {
    const result = await testWeb(async () => {
      const IDB = await import("idb-keyval");
      const { CACHE_KEY_PREFIX } = await import(
        "~/components/input/ephemeral-cache/constants.js"
      );
      const mod = await import(
        "~/components/input/ephemeral-cache/element.js"
      );
      const input = new mod.CLASS();
      document.body.append(input);

      const cachedUri = "ephemeral+cache://bafkgroupcached";
      const uncachedUri = "ephemeral+cache://bafkgroupuncached";
      await IDB.set(
        CACHE_KEY_PREFIX + cachedUri,
        new Blob(["audio"], { type: "audio/mpeg" }),
      );

      const result = await input.groupConsult([cachedUri, uncachedUri]);
      await IDB.del(CACHE_KEY_PREFIX + cachedUri);
      return result;
    });

    expect(result["ephemeral+cache"]?.available).toBe(true);
    expect(result["ephemeral+cache"]?.uris).toEqual([
      "ephemeral+cache://bafkgroupcached",
    ]);
  });

  it("groupConsult with no cached URIs returns empty uris list", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/input/ephemeral-cache/element.js"
      );
      const input = new mod.CLASS();
      document.body.append(input);

      return await input.groupConsult([
        "ephemeral+cache://bafkuncached1",
        "ephemeral+cache://bafkuncached2",
      ]);
    });

    expect(result["ephemeral+cache"]?.available).toBe(true);
    expect(result["ephemeral+cache"]?.uris).toEqual([]);
  });

  it("resolve returns undefined for an uncached URI", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/input/ephemeral-cache/element.js"
      );
      const input = new mod.CLASS();
      document.body.append(input);
      const r = await input.resolve({ uri: "ephemeral+cache://bafknotcached" });
      return r ?? null;
    });

    expect(result).toBe(null);
  });

  it("resolve returns a blob URL with Infinity expiry for a cached URI", async () => {
    const result = await testWeb(async () => {
      const IDB = await import("idb-keyval");
      const { CACHE_KEY_PREFIX } = await import(
        "~/components/input/ephemeral-cache/constants.js"
      );
      const mod = await import(
        "~/components/input/ephemeral-cache/element.js"
      );
      const input = new mod.CLASS();
      document.body.append(input);

      const uri = "ephemeral+cache://bafkresolvetest";
      await IDB.set(
        CACHE_KEY_PREFIX + uri,
        new Blob(["audio"], { type: "audio/mpeg" }),
      );

      const resolved = await input.resolve({ uri });
      await IDB.del(CACHE_KEY_PREFIX + uri);
      if (!resolved || !("url" in resolved)) return null;
      return { url: resolved.url, neverExpires: resolved.expiresAt === Infinity };
    });

    expect(result).not.toBe(null);
    if (result) {
      expect(result.url).toMatch(/^blob:/);
      expect(result.neverExpires).toBe(true);
    }
  });

  it("resolve returns the same blob URL on repeated calls", async () => {
    const [url1, url2] = await testWeb(async () => {
      const IDB = await import("idb-keyval");
      const { CACHE_KEY_PREFIX } = await import(
        "~/components/input/ephemeral-cache/constants.js"
      );
      const mod = await import(
        "~/components/input/ephemeral-cache/element.js"
      );
      const input = new mod.CLASS();
      document.body.append(input);

      const uri = "ephemeral+cache://bafkresolvetwice";
      await IDB.set(
        CACHE_KEY_PREFIX + uri,
        new Blob(["audio"], { type: "audio/mpeg" }),
      );

      const r1 = await input.resolve({ uri });
      const r2 = await input.resolve({ uri });
      await IDB.del(CACHE_KEY_PREFIX + uri);
      return [
        r1 && "url" in r1 ? r1.url : null,
        r2 && "url" in r2 ? r2.url : null,
      ];
    });

    expect(url1).not.toBe(null);
    expect(url1).toBe(url2);
  });

  it("list returns tracks unchanged", async () => {
    const result = await testWeb(async () => {
      const mod = await import(
        "~/components/input/ephemeral-cache/element.js"
      );
      const input = new mod.CLASS();
      document.body.append(input);

      const tracks: Track[] = [
        {
          $type: "sh.diffuse.output.track",
          id: "t1",
          uri: "ephemeral+cache://bafktrack1",
        },
        {
          $type: "sh.diffuse.output.track",
          id: "t2",
          uri: "ephemeral+cache://bafktrack2",
        },
      ];

      return await input.list(tracks);
    });

    expect(result.map((t) => t.id)).toEqual(["t1", "t2"]);
  });

  it("detaches all tracks when given the scheme", async () => {
    const remaining = await testWeb(async () => {
      const IDB = await import("idb-keyval");
      const { CACHE_KEY_PREFIX } = await import(
        "~/components/input/ephemeral-cache/constants.js"
      );
      const mod = await import(
        "~/components/input/ephemeral-cache/element.js"
      );
      const input = new mod.CLASS();
      document.body.append(input);

      const tracks: Track[] = [
        {
          $type: "sh.diffuse.output.track",
          id: "t1",
          uri: "ephemeral+cache://bafkdetach1",
        },
        {
          $type: "sh.diffuse.output.track",
          id: "t2",
          uri: "ephemeral+cache://bafkdetach2",
        },
      ];

      for (const t of tracks) {
        await IDB.set(CACHE_KEY_PREFIX + t.uri, new Blob(["audio"]));
      }

      return await input.detach({ fileUriOrScheme: "ephemeral+cache", tracks });
    });

    expect(remaining.length).toBe(0);
  });

  it("detaches a specific track when given a URI", async () => {
    const remaining = await testWeb(async () => {
      const IDB = await import("idb-keyval");
      const { CACHE_KEY_PREFIX } = await import(
        "~/components/input/ephemeral-cache/constants.js"
      );
      const mod = await import(
        "~/components/input/ephemeral-cache/element.js"
      );
      const input = new mod.CLASS();
      document.body.append(input);

      const tracks: Track[] = [
        {
          $type: "sh.diffuse.output.track",
          id: "t1",
          uri: "ephemeral+cache://bafkremove",
        },
        {
          $type: "sh.diffuse.output.track",
          id: "t2",
          uri: "ephemeral+cache://bafkkeep",
        },
      ];

      for (const t of tracks) {
        await IDB.set(CACHE_KEY_PREFIX + t.uri, new Blob(["audio"]));
      }

      return await input.detach({
        fileUriOrScheme: "ephemeral+cache://bafkremove",
        tracks,
      });
    });

    expect(remaining.length).toBe(1);
    expect(remaining[0].id).toBe("t2");
  });

  it("detach with non-matching scheme returns tracks unchanged", async () => {
    const remaining = await testWeb(async () => {
      const mod = await import(
        "~/components/input/ephemeral-cache/element.js"
      );
      const input = new mod.CLASS();
      document.body.append(input);

      const tracks: Track[] = [
        {
          $type: "sh.diffuse.output.track",
          id: "t1",
          uri: "ephemeral+cache://bafk1",
        },
        {
          $type: "sh.diffuse.output.track",
          id: "t2",
          uri: "ephemeral+cache://bafk2",
        },
      ];

      return await input.detach({ fileUriOrScheme: "https", tracks });
    });

    expect(remaining.map((t) => t.id)).toEqual(["t1", "t2"]);
  });

  it("sources returns tracks mapped by URI as label", async () => {
    const sources = await testWeb(async () => {
      const mod = await import(
        "~/components/input/ephemeral-cache/element.js"
      );
      const input = new mod.CLASS();
      document.body.append(input);

      const tracks: Track[] = [
        {
          $type: "sh.diffuse.output.track",
          id: "t1",
          uri: "ephemeral+cache://bafksrc1",
        },
        {
          $type: "sh.diffuse.output.track",
          id: "t2",
          uri: "ephemeral+cache://bafksrc2",
          tags: { title: "My Song" },
        },
      ];

      return input.sources(tracks);
    });

    expect(sources.length).toBe(2);
    expect(sources[0].uri).toBe("ephemeral+cache://bafksrc1");
    expect(sources[0].label).toBe("ephemeral+cache://bafksrc1");
    // label is always the URI regardless of tags, to keep sources stable across processing
    expect(sources[1].uri).toBe("ephemeral+cache://bafksrc2");
    expect(sources[1].label).toBe("ephemeral+cache://bafksrc2");
  });
});
