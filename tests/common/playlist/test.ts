import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

// Minimal test fixtures
const trackA = {
  $type: "sh.diffuse.output.track",
  id: "track-a",
  uri: "http://example.com/a.mp3",
  tags: { artist: "Artist A", title: "Song A" },
};

const trackB = {
  $type: "sh.diffuse.output.track",
  id: "track-b",
  uri: "http://example.com/b.mp3",
  tags: { artist: "Artist B", title: "Song B" },
};

describe("common/playlist", () => {
  describe("match", () => {
    it("matches a track that satisfies all criteria", async () => {
      const result = await testWeb(async () => {
        const { match } = await import("~/common/playlist.js");
        const track = {
          $type: "sh.diffuse.output.track",
          id: "t",
          uri: "http://x.com/t.mp3",
          tags: { artist: "Artist A", title: "Song A" },
        };
        const item = {
          $type: "sh.diffuse.output.playlistItem",
          id: "i",
          playlist: "Favourites",
          criteria: [
            { field: "tags.artist", value: "Artist A" },
            { field: "tags.title", value: "Song A" },
          ],
        };
        return match(track as never, item as never);
      });
      expect(result).toBe(true);
    });

    it("does not match when one criterion fails", async () => {
      const result = await testWeb(async () => {
        const { match } = await import("~/common/playlist.js");
        const track = {
          $type: "sh.diffuse.output.track",
          id: "t",
          uri: "http://x.com/t.mp3",
          tags: { artist: "Artist A", title: "Wrong Title" },
        };
        const item = {
          $type: "sh.diffuse.output.playlistItem",
          id: "i",
          playlist: "Favourites",
          criteria: [
            { field: "tags.artist", value: "Artist A" },
            { field: "tags.title", value: "Song A" },
          ],
        };
        return match(track as never, item as never);
      });
      expect(result).toBe(false);
    });

    it("applies transformations before comparing", async () => {
      const result = await testWeb(async () => {
        const { match } = await import("~/common/playlist.js");
        const track = {
          $type: "sh.diffuse.output.track",
          id: "t",
          uri: "http://x.com/t.mp3",
          tags: { artist: "Artist A" },
        };
        const item = {
          $type: "sh.diffuse.output.playlistItem",
          id: "i",
          playlist: "Favourites",
          criteria: [
            { field: "tags.artist", value: "ARTIST A", transformations: ["toLowerCase"] },
          ],
        };
        return match(track as never, item as never);
      });
      expect(result).toBe(true);
    });

    it("does not match when transformations result in inequality", async () => {
      const result = await testWeb(async () => {
        const { match } = await import("~/common/playlist.js");
        const track = {
          $type: "sh.diffuse.output.track",
          id: "t",
          uri: "http://x.com/t.mp3",
          tags: { artist: "Artist A" },
        };
        const item = {
          $type: "sh.diffuse.output.playlistItem",
          id: "i",
          playlist: "Favourites",
          criteria: [
            { field: "tags.artist", value: "Artist B", transformations: ["toLowerCase"] },
          ],
        };
        return match(track as never, item as never);
      });
      expect(result).toBe(false);
    });
  });

  describe("gather", () => {
    it("groups items by playlist name", async () => {
      const result = await testWeb(async () => {
        const { gather } = await import("~/common/playlist.js");
        const items = [
          { $type: "sh.diffuse.output.playlistItem", id: "1", playlist: "Rock", criteria: [], positionedAfter: "prev" },
          { $type: "sh.diffuse.output.playlistItem", id: "2", playlist: "Pop", criteria: [], positionedAfter: "prev" },
          { $type: "sh.diffuse.output.playlistItem", id: "3", playlist: "Rock", criteria: [], positionedAfter: "prev" },
        ] as never[];
        const map = gather(items);
        return { rockCount: map.get("Rock")!.items.length, popCount: map.get("Pop")!.items.length };
      });
      expect(result.rockCount).toBe(2);
      expect(result.popCount).toBe(1);
    });

    it("marks playlist as unordered when all items lack positionedAfter", async () => {
      const result = await testWeb(async () => {
        const { gather } = await import("~/common/playlist.js");
        const items = [
          { $type: "sh.diffuse.output.playlistItem", id: "1", playlist: "Mix", criteria: [] },
          { $type: "sh.diffuse.output.playlistItem", id: "2", playlist: "Mix", criteria: [] },
        ] as never[];
        const map = gather(items);
        return map.get("Mix")!.unordered;
      });
      expect(result).toBe(true);
    });

    it("marks playlist as ordered when any item has positionedAfter", async () => {
      const result = await testWeb(async () => {
        const { gather } = await import("~/common/playlist.js");
        const items = [
          { $type: "sh.diffuse.output.playlistItem", id: "1", playlist: "Mix", criteria: [], positionedAfter: null },
          { $type: "sh.diffuse.output.playlistItem", id: "2", playlist: "Mix", criteria: [], positionedAfter: "1" },
        ] as never[];
        const map = gather(items);
        return map.get("Mix")!.unordered;
      });
      expect(result).toBe(false);
    });

    it("preserves playlist name", async () => {
      const result = await testWeb(async () => {
        const { gather } = await import("~/common/playlist.js");
        const items = [
          { $type: "sh.diffuse.output.playlistItem", id: "1", playlist: "My Playlist", criteria: [] },
        ] as never[];
        const map = gather(items);
        return map.get("My Playlist")!.name;
      });
      expect(result).toBe("My Playlist");
    });
  });

  describe("sort", () => {
    it("returns single-item array unchanged", async () => {
      const result = await testWeb(async () => {
        const { sort } = await import("~/common/playlist.js");
        const items = [
          { $type: "sh.diffuse.output.playlistItem", id: "a", playlist: "p", criteria: [], positionedAfter: null },
        ] as never[];
        return sort(items).map((i: { id: string }) => i.id);
      });
      expect(result).toEqual(["a"]);
    });

    it("sorts a simple linked list in order", async () => {
      const result = await testWeb(async () => {
        const { sort } = await import("~/common/playlist.js");
        const items = [
          { $type: "sh.diffuse.output.playlistItem", id: "c", playlist: "p", criteria: [], positionedAfter: "b" },
          { $type: "sh.diffuse.output.playlistItem", id: "a", playlist: "p", criteria: [], positionedAfter: null },
          { $type: "sh.diffuse.output.playlistItem", id: "b", playlist: "p", criteria: [], positionedAfter: "a" },
        ] as never[];
        return sort(items).map((i: { id: string }) => i.id);
      });
      expect(result).toEqual(["a", "b", "c"]);
    });

    it("places head items (positionedAfter null) first", async () => {
      const result = await testWeb(async () => {
        const { sort } = await import("~/common/playlist.js");
        const items = [
          { $type: "sh.diffuse.output.playlistItem", id: "b", playlist: "p", criteria: [], positionedAfter: "a" },
          { $type: "sh.diffuse.output.playlistItem", id: "a", playlist: "p", criteria: [], positionedAfter: null },
        ] as never[];
        return sort(items).map((i: { id: string }) => i.id);
      });
      expect(result[0]).toBe("a");
    });

    it("appends unreachable items at the end", async () => {
      const result = await testWeb(async () => {
        const { sort } = await import("~/common/playlist.js");
        const items = [
          { $type: "sh.diffuse.output.playlistItem", id: "a", playlist: "p", criteria: [], positionedAfter: null },
          { $type: "sh.diffuse.output.playlistItem", id: "b", playlist: "p", criteria: [], positionedAfter: "a" },
          { $type: "sh.diffuse.output.playlistItem", id: "orphan", playlist: "p", criteria: [], positionedAfter: "missing" },
        ] as never[];
        const sorted = sort(items).map((i: { id: string }) => i.id);
        return sorted[sorted.length - 1];
      });
      expect(result).toBe("orphan");
    });

    it("sorts multiple heads by updatedAt ascending", async () => {
      const result = await testWeb(async () => {
        const { sort } = await import("~/common/playlist.js");
        const items = [
          { $type: "sh.diffuse.output.playlistItem", id: "b", playlist: "p", criteria: [], positionedAfter: null, updatedAt: "2024-06-01T00:00:00.000Z" },
          { $type: "sh.diffuse.output.playlistItem", id: "a", playlist: "p", criteria: [], positionedAfter: null, updatedAt: "2024-01-01T00:00:00.000Z" },
        ] as never[];
        return sort(items).map((i: { id: string }) => i.id);
      });
      expect(result[0]).toBe("a");
      expect(result[1]).toBe("b");
    });
  });

  describe("filterByPlaylist", () => {
    it("returns tracks matching any playlist item criteria", async () => {
      const result = await testWeb(async () => {
        const { filterByPlaylist } = await import("~/common/playlist.js");
        const tracks = [
          { $type: "sh.diffuse.output.track", id: "a", uri: "http://x.com/a.mp3", tags: { artist: "A", title: "T1" } },
          { $type: "sh.diffuse.output.track", id: "b", uri: "http://x.com/b.mp3", tags: { artist: "B", title: "T2" } },
        ] as never[];
        const items = [
          { $type: "sh.diffuse.output.playlistItem", id: "i1", playlist: "p", criteria: [{ field: "tags.artist", value: "A" }] },
        ] as never[];
        return filterByPlaylist(tracks, items).map((t: { id: string }) => t.id);
      });
      expect(result).toEqual(["a"]);
    });

    it("returns empty array when no tracks match", async () => {
      const result = await testWeb(async () => {
        const { filterByPlaylist } = await import("~/common/playlist.js");
        const tracks = [
          { $type: "sh.diffuse.output.track", id: "a", uri: "http://x.com/a.mp3", tags: { artist: "A", title: "T1" } },
        ] as never[];
        const items = [
          { $type: "sh.diffuse.output.playlistItem", id: "i1", playlist: "p", criteria: [{ field: "tags.artist", value: "Z" }] },
        ] as never[];
        return filterByPlaylist(tracks, items).length;
      });
      expect(result).toBe(0);
    });

    it("applies transformations when matching", async () => {
      const result = await testWeb(async () => {
        const { filterByPlaylist } = await import("~/common/playlist.js");
        const tracks = [
          { $type: "sh.diffuse.output.track", id: "a", uri: "http://x.com/a.mp3", tags: { artist: "Artist" } },
        ] as never[];
        const items = [
          {
            $type: "sh.diffuse.output.playlistItem",
            id: "i1",
            playlist: "p",
            criteria: [{ field: "tags.artist", value: "ARTIST", transformations: ["toLowerCase"] }],
          },
        ] as never[];
        return filterByPlaylist(tracks, items).length;
      });
      expect(result).toBe(1);
    });

    it("returns all tracks when multiple criteria match different tracks", async () => {
      const result = await testWeb(async () => {
        const { filterByPlaylist } = await import("~/common/playlist.js");
        const tracks = [
          { $type: "sh.diffuse.output.track", id: "a", uri: "http://x.com/a.mp3", tags: { artist: "A" } },
          { $type: "sh.diffuse.output.track", id: "b", uri: "http://x.com/b.mp3", tags: { artist: "B" } },
        ] as never[];
        const items = [
          { $type: "sh.diffuse.output.playlistItem", id: "i1", playlist: "p", criteria: [{ field: "tags.artist", value: "A" }] },
          { $type: "sh.diffuse.output.playlistItem", id: "i2", playlist: "p", criteria: [{ field: "tags.artist", value: "B" }] },
        ] as never[];
        return filterByPlaylist(tracks, items).length;
      });
      expect(result).toBe(2);
    });

    it("returns empty array for empty playlist items", async () => {
      const result = await testWeb(async () => {
        const { filterByPlaylist } = await import("~/common/playlist.js");
        const tracks = [
          { $type: "sh.diffuse.output.track", id: "a", uri: "http://x.com/a.mp3", tags: { artist: "A" } },
        ] as never[];
        return filterByPlaylist(tracks, []).length;
      });
      expect(result).toBe(0);
    });
  });
});
