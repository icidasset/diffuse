import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";
import { trackA, trackB, tracks } from "~/testing/sample/tracks.js";

describe("components/orchestrator/scoped-tracks", () => {
  it("finds tracks by album", async () => {
    const results = await testWeb(async () => {
      const { supply, search } = await import(
        "~/components/orchestrator/scoped-tracks/worker.js"
      );
      const { tracks } = await import("~/testing/sample/tracks.js");

      await supply({ tracks });
      return search({ term: tracks[1]?.tags?.album });
    });

    expect(results[0]?.id).toBe(trackB.id);
  });

  it("finds tracks by artist", async () => {
    const results = await testWeb(async () => {
      const { supply, search } = await import(
        "~/components/orchestrator/scoped-tracks/worker.js"
      );
      const { tracks } = await import("~/testing/sample/tracks.js");

      await supply({ tracks });
      return search({ term: tracks[0]?.tags?.artist });
    });

    expect(results[0]?.id).toBe(trackA.id);
  });

  it("finds tracks by title", async () => {
    const results = await testWeb(async () => {
      const { supply, search } = await import(
        "~/components/orchestrator/scoped-tracks/worker.js"
      );
      const { tracks } = await import("~/testing/sample/tracks.js");

      await supply({ tracks });
      return search({ term: tracks[1]?.tags?.title });
    });

    expect(results[0]?.id).toBe(trackB.id);
  });

  it("returns empty array when no tracks match the search term", async () => {
    const results = await testWeb(async () => {
      const { supply, search } = await import(
        "~/components/orchestrator/scoped-tracks/worker.js"
      );
      const { tracks } = await import("~/testing/sample/tracks.js");

      await supply({ tracks });
      return search({ term: "zzz-no-match-zzz" });
    });

    expect(results).toEqual([]);
  });

  it("supplyFingerprint is undefined before first supply", async () => {
    const fp = await testWeb(async () => {
      const { $supplyFingerprint } = await import(
        "~/components/orchestrator/scoped-tracks/worker.js"
      );

      return $supplyFingerprint.value ?? null;
    });

    expect(fp).toBe(null);
  });

  it("supplyFingerprint is set after supply", async () => {
    const fp = await testWeb(async () => {
      const { supply, $supplyFingerprint } = await import(
        "~/components/orchestrator/scoped-tracks/worker.js"
      );
      const { tracks } = await import("~/testing/sample/tracks.js");

      await supply({ tracks });
      return $supplyFingerprint.value ?? null;
    });

    expect(fp).not.toBe(null);
    expect(typeof fp).toBe("string");
  });

  it("supply with same tracks does not change the fingerprint", async () => {
    const [fp1, fp2] = await testWeb(async () => {
      const { supply, $supplyFingerprint } = await import(
        "~/components/orchestrator/scoped-tracks/worker.js"
      );
      const { tracks } = await import("~/testing/sample/tracks.js");

      await supply({ tracks });
      const fp1 = $supplyFingerprint.value;

      await supply({ tracks });
      const fp2 = $supplyFingerprint.value;

      return [fp1, fp2];
    });

    expect(fp1).toBe(fp2);
  });

  it("supply removes tracks no longer in the list", async () => {
    const results = await testWeb(async () => {
      const { supply, search } = await import(
        "~/components/orchestrator/scoped-tracks/worker.js"
      );
      const { trackA, trackB } = await import("~/testing/sample/tracks.js");

      await supply({ tracks: [trackA, trackB] });
      await supply({ tracks: [trackB] });

      return search({ term: "Artist" });
    });

    expect(results).toEqual([]);
  });

  it("supply with empty list clears all tracks from the index", async () => {
    const results = await testWeb(async () => {
      const { supply, search } = await import(
        "~/components/orchestrator/scoped-tracks/worker.js"
      );
      const { tracks } = await import("~/testing/sample/tracks.js");

      await supply({ tracks });
      await supply({ tracks: [] });

      return search({ term: "Sample" });
    });

    expect(results).toEqual([]);
  });

  it("sorts results by artist alphabetically", async () => {
    const ids = await testWeb(async () => {
      const { supply, search } = await import(
        "~/components/orchestrator/scoped-tracks/worker.js"
      );

      const testTracks = [
        {
          $type: "sh.diffuse.output.track" as const,
          id: "sort-zebra",
          uri: "diffuse://sort-zebra",
          tags: { artist: "Zebra", title: "Sort Test" },
        },
        {
          $type: "sh.diffuse.output.track" as const,
          id: "sort-apple",
          uri: "diffuse://sort-apple",
          tags: { artist: "Apple", title: "Sort Test" },
        },
        {
          $type: "sh.diffuse.output.track" as const,
          id: "sort-mango",
          uri: "diffuse://sort-mango",
          tags: { artist: "Mango", title: "Sort Test" },
        },
      ];

      await supply({ tracks: testTracks });
      const results = await search({ term: "Sort Test" });
      return results.map((t) => t.id);
    });

    expect(ids).toEqual(["sort-apple", "sort-mango", "sort-zebra"]);
  });

  it("sorts results by track number within the same album", async () => {
    const ids = await testWeb(async () => {
      const { supply, search } = await import(
        "~/components/orchestrator/scoped-tracks/worker.js"
      );

      const testTracks = [
        {
          $type: "sh.diffuse.output.track" as const,
          id: "track-03",
          uri: "diffuse://track-03",
          tags: { artist: "Band", album: "Album", track: { no: 3 }, title: "C" },
        },
        {
          $type: "sh.diffuse.output.track" as const,
          id: "track-01",
          uri: "diffuse://track-01",
          tags: { artist: "Band", album: "Album", track: { no: 1 }, title: "A" },
        },
        {
          $type: "sh.diffuse.output.track" as const,
          id: "track-02",
          uri: "diffuse://track-02",
          tags: { artist: "Band", album: "Album", track: { no: 2 }, title: "B" },
        },
      ];

      await supply({ tracks: testTracks });
      const results = await search({ term: "Band" });
      return results.map((t) => t.id);
    });

    expect(ids).toEqual(["track-01", "track-02", "track-03"]);
  });
});

describe("components/orchestrator/scoped-tracks – disabled sources", () => {
  it("includes all tracks when no disabled sources setting is present", async () => {
    const ids = await testWeb(async () => {
      const Output = await import(
        "~/components/output/polymorphic/indexed-db/element.js"
      );
      const ScopedTracks = await import(
        "~/components/orchestrator/scoped-tracks/element.js"
      );

      class TestInput extends HTMLElement {
        SCHEME = "test";
        sources() { return []; }
        async groupConsult(uris: string[]) {
          return { all: { available: true, scheme: "test", uris } };
        }
      }
      customElements.define("di-test-input-1", TestInput);

      const output = new Output.CLASS();
      output.id = "test-output-ds-1";
      document.body.append(output);

      await output.tracks.save([
        { $type: "sh.diffuse.output.track", id: "ds-alpha-1", uri: "https://alpha.example/song-1.mp3", tags: {} },
        { $type: "sh.diffuse.output.track", id: "ds-alpha-2", uri: "https://alpha.example/song-2.mp3", tags: {} },
        { $type: "sh.diffuse.output.track", id: "ds-beta-1",  uri: "https://beta.example/song-3.mp3",  tags: {} },
      ]);
      // No settings saved — disabled sources key is absent

      const input = new TestInput();
      input.id = "test-input-ds-1";
      document.body.append(input);

      const el = new ScopedTracks.CLASS();
      el.setAttribute("output-selector", "#test-output-ds-1");
      el.setAttribute("input-selector",  "#test-input-ds-1");
      document.body.append(el);

      const deadline = Date.now() + 2000;
      while (Date.now() < deadline) {
        if (el.tracks().length === 3) break;
        await new Promise((r) => setTimeout(r, 20));
      }

      return el.tracks().map((t: { id: string }) => t.id).sort();
    });

    expect(ids).toEqual(["ds-alpha-1", "ds-alpha-2", "ds-beta-1"]);
  });

  it("excludes tracks whose URI starts with a disabled source", async () => {
    const ids = await testWeb(async () => {
      const Output = await import(
        "~/components/output/polymorphic/indexed-db/element.js"
      );
      const ScopedTracks = await import(
        "~/components/orchestrator/scoped-tracks/element.js"
      );

      class TestInput extends HTMLElement {
        SCHEME = "test";
        sources() { return []; }
        async groupConsult(uris: string[]) {
          return { all: { available: true, scheme: "test", uris } };
        }
      }
      customElements.define("di-test-input-2", TestInput);

      const output = new Output.CLASS();
      output.id = "test-output-ds-2";
      document.body.append(output);

      await output.tracks.save([
        { $type: "sh.diffuse.output.track", id: "ds-alpha-1", uri: "https://alpha.example/song-1.mp3", tags: {} },
        { $type: "sh.diffuse.output.track", id: "ds-alpha-2", uri: "https://alpha.example/song-2.mp3", tags: {} },
        { $type: "sh.diffuse.output.track", id: "ds-beta-1",  uri: "https://beta.example/song-3.mp3",  tags: {} },
      ]);
      await output.settings.save([
        { $type: "sh.diffuse.output.setting", id: "ds-setting", key: "sh.diffuse.input.disabled.uris", value: JSON.stringify(["https://alpha.example"]) },
      ]);

      const input = new TestInput();
      input.id = "test-input-ds-2";
      document.body.append(input);

      const el = new ScopedTracks.CLASS();
      el.setAttribute("output-selector", "#test-output-ds-2");
      el.setAttribute("input-selector",  "#test-input-ds-2");
      document.body.append(el);

      const deadline = Date.now() + 2000;
      while (Date.now() < deadline) {
        if (el.tracks().length === 1) break;
        await new Promise((r) => setTimeout(r, 20));
      }

      return el.tracks().map((t: { id: string }) => t.id);
    });

    expect(ids).toEqual(["ds-beta-1"]);
  });

  it("handles multiple disabled sources", async () => {
    const ids = await testWeb(async () => {
      const Output = await import(
        "~/components/output/polymorphic/indexed-db/element.js"
      );
      const ScopedTracks = await import(
        "~/components/orchestrator/scoped-tracks/element.js"
      );

      class TestInput extends HTMLElement {
        SCHEME = "test";
        sources() { return []; }
        async groupConsult(uris: string[]) {
          return { all: { available: true, scheme: "test", uris } };
        }
      }
      customElements.define("di-test-input-3", TestInput);

      const output = new Output.CLASS();
      output.id = "test-output-ds-3";
      document.body.append(output);

      await output.tracks.save([
        { $type: "sh.diffuse.output.track", id: "ds-alpha-1", uri: "https://alpha.example/song-1.mp3", tags: {} },
        { $type: "sh.diffuse.output.track", id: "ds-beta-1",  uri: "https://beta.example/song-2.mp3",  tags: {} },
        { $type: "sh.diffuse.output.track", id: "ds-gamma-1", uri: "https://gamma.example/song-3.mp3", tags: {} },
      ]);
      await output.settings.save([
        { $type: "sh.diffuse.output.setting", id: "ds-setting", key: "sh.diffuse.input.disabled.uris", value: JSON.stringify(["https://alpha.example", "https://beta.example"]) },
      ]);

      const input = new TestInput();
      input.id = "test-input-ds-3";
      document.body.append(input);

      const el = new ScopedTracks.CLASS();
      el.setAttribute("output-selector", "#test-output-ds-3");
      el.setAttribute("input-selector",  "#test-input-ds-3");
      document.body.append(el);

      const deadline = Date.now() + 2000;
      while (Date.now() < deadline) {
        if (el.tracks().length === 1) break;
        await new Promise((r) => setTimeout(r, 20));
      }

      return el.tracks().map((t: { id: string }) => t.id);
    });

    expect(ids).toEqual(["ds-gamma-1"]);
  });

  it("includes all tracks when disabled sources value is an empty array", async () => {
    const ids = await testWeb(async () => {
      const Output = await import(
        "~/components/output/polymorphic/indexed-db/element.js"
      );
      const ScopedTracks = await import(
        "~/components/orchestrator/scoped-tracks/element.js"
      );

      class TestInput extends HTMLElement {
        SCHEME = "test";
        sources() { return []; }
        async groupConsult(uris: string[]) {
          return { all: { available: true, scheme: "test", uris } };
        }
      }
      customElements.define("di-test-input-4", TestInput);

      const output = new Output.CLASS();
      output.id = "test-output-ds-4";
      document.body.append(output);

      await output.tracks.save([
        { $type: "sh.diffuse.output.track", id: "ds-alpha-1", uri: "https://alpha.example/song-1.mp3", tags: {} },
        { $type: "sh.diffuse.output.track", id: "ds-alpha-2", uri: "https://alpha.example/song-2.mp3", tags: {} },
        { $type: "sh.diffuse.output.track", id: "ds-beta-1",  uri: "https://beta.example/song-3.mp3",  tags: {} },
      ]);
      await output.settings.save([
        { $type: "sh.diffuse.output.setting", id: "ds-setting", key: "sh.diffuse.input.disabled.uris", value: "[]" },
      ]);

      const input = new TestInput();
      input.id = "test-input-ds-4";
      document.body.append(input);

      const el = new ScopedTracks.CLASS();
      el.setAttribute("output-selector", "#test-output-ds-4");
      el.setAttribute("input-selector",  "#test-input-ds-4");
      document.body.append(el);

      const deadline = Date.now() + 2000;
      while (Date.now() < deadline) {
        if (el.tracks().length === 3) break;
        await new Promise((r) => setTimeout(r, 20));
      }

      return el.tracks().map((t: { id: string }) => t.id).sort();
    });

    expect(ids).toEqual(["ds-alpha-1", "ds-alpha-2", "ds-beta-1"]);
  });

  it("includes all tracks when disabled sources value is invalid JSON", async () => {
    const ids = await testWeb(async () => {
      const Output = await import(
        "~/components/output/polymorphic/indexed-db/element.js"
      );
      const ScopedTracks = await import(
        "~/components/orchestrator/scoped-tracks/element.js"
      );

      class TestInput extends HTMLElement {
        SCHEME = "test";
        sources() { return []; }
        async groupConsult(uris: string[]) {
          return { all: { available: true, scheme: "test", uris } };
        }
      }
      customElements.define("di-test-input-5", TestInput);

      const output = new Output.CLASS();
      output.id = "test-output-ds-5";
      document.body.append(output);

      await output.tracks.save([
        { $type: "sh.diffuse.output.track", id: "ds-alpha-1", uri: "https://alpha.example/song-1.mp3", tags: {} },
        { $type: "sh.diffuse.output.track", id: "ds-alpha-2", uri: "https://alpha.example/song-2.mp3", tags: {} },
        { $type: "sh.diffuse.output.track", id: "ds-beta-1",  uri: "https://beta.example/song-3.mp3",  tags: {} },
      ]);
      await output.settings.save([
        { $type: "sh.diffuse.output.setting", id: "ds-setting", key: "sh.diffuse.input.disabled.uris", value: "not-valid-json" },
      ]);

      const input = new TestInput();
      input.id = "test-input-ds-5";
      document.body.append(input);

      const el = new ScopedTracks.CLASS();
      el.setAttribute("output-selector", "#test-output-ds-5");
      el.setAttribute("input-selector",  "#test-input-ds-5");
      document.body.append(el);

      const deadline = Date.now() + 2000;
      while (Date.now() < deadline) {
        if (el.tracks().length === 3) break;
        await new Promise((r) => setTimeout(r, 20));
      }

      return el.tracks().map((t: { id: string }) => t.id).sort();
    });

    expect(ids).toEqual(["ds-alpha-1", "ds-alpha-2", "ds-beta-1"]);
  });
});
