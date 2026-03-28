import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";
import type { Track } from "~/definitions/types.d.ts";

describe("components/input/local", () => {
  it("has correct SCHEME property", async () => {
    const scheme = await testWeb(async () => {
      const mod = await import("~/components/input/local/element.js");
      const input = new mod.CLASS();
      document.body.append(input);
      return input.SCHEME;
    });

    expect(scheme).toBe("local");
  });

  it("consult returns unsupported for an unknown TID URI", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/input/local/element.js");
      const input = new mod.CLASS();
      document.body.append(input);
      return await input.consult("local://unknown-tid-abc123/");
    });

    // Either no browser support or unknown handle — both return supported: false
    expect(result.supported).toBe(false);
  });

  it("listCached returns empty array initially", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/input/local/element.js");
      const input = new mod.CLASS();
      document.body.append(input);
      return await input.list([]);
    });

    expect(result).toEqual([]);
  });

  it("detach with local scheme removes all local tracks", async () => {
    const remaining = await testWeb(async () => {
      const mod = await import("~/components/input/local/element.js");
      const input = new mod.CLASS();
      document.body.append(input);

      const tracks: Track[] = [
        {
          $type: "sh.diffuse.output.track",
          id: "1",
          uri: "local://tid-aaa/track1.mp3",
        },
        {
          $type: "sh.diffuse.output.track",
          id: "2",
          uri: "local://tid-bbb/track2.mp3",
        },
      ];

      return await input.detach({ fileUriOrScheme: "local", tracks });
    });

    expect(remaining.length).toBe(0);
  });

  it("detach with non-matching scheme returns all tracks", async () => {
    const remaining = await testWeb(async () => {
      const mod = await import("~/components/input/local/element.js");
      const input = new mod.CLASS();
      document.body.append(input);

      const tracks: Track[] = [
        {
          $type: "sh.diffuse.output.track",
          id: "1",
          uri: "local://tid-aaa/track1.mp3",
        },
      ];

      return await input.detach({ fileUriOrScheme: "https", tracks });
    });

    expect(remaining.length).toBe(1);
  });

  it("detach with a non-local URI returns all tracks unchanged", async () => {
    const remaining = await testWeb(async () => {
      const mod = await import("~/components/input/local/element.js");
      const input = new mod.CLASS();
      document.body.append(input);

      const tracks: Track[] = [
        {
          $type: "sh.diffuse.output.track",
          id: "1",
          uri: "local://tid-aaa/track1.mp3",
        },
        {
          $type: "sh.diffuse.output.track",
          id: "2",
          uri: "local://tid-bbb/track2.mp3",
        },
      ];

      return await input.detach({
        fileUriOrScheme: "icecast://some-host/stream",
        tracks,
      });
    });

    // fileUriOrScheme has "://" but scheme doesn't match "local" → tracks unchanged
    expect(remaining.length).toBe(2);
  });

  it("resolve returns undefined for an unknown TID", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/input/local/element.js");
      const input = new mod.CLASS();
      document.body.append(input);
      const r = await input.resolve({ uri: "local://unknown-tid/track.mp3" });
      return r ?? null;
    });

    expect(result).toBe(null);
  });

  it("groupConsult returns empty object for unknown TIDs", async () => {
    const result = await testWeb(async () => {
      const mod = await import("~/components/input/local/element.js");
      const input = new mod.CLASS();
      document.body.append(input);
      return await input.groupConsult([
        "local://tid-unknown-1/track.mp3",
        "local://tid-unknown-2/song.flac",
      ]);
    });

    expect(Object.keys(result).length).toBe(0);
  });
});
