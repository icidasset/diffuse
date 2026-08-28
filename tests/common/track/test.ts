import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("common/track", () => {
  describe("trackURIBase", () => {
    it("strips path from URI", async () => {
      const result = await testWeb(async () => {
        const { trackURIBase } = await import("~/common/track.js");
        return trackURIBase("https://example.com/music/track.mp3");
      });
      expect(result).toBe("https://example.com/");
    });

    it("strips query string from URI", async () => {
      const result = await testWeb(async () => {
        const { trackURIBase } = await import("~/common/track.js");
        return trackURIBase("https://example.com/track.mp3?token=abc");
      });
      expect(result).toBe("https://example.com/");
    });

    it("handles URIs with no path", async () => {
      const result = await testWeb(async () => {
        const { trackURIBase } = await import("~/common/track.js");
        return trackURIBase("https://example.com");
      });
      expect(result).toBe("https://example.com/");
    });

    it("preserves scheme and host", async () => {
      const result = await testWeb(async () => {
        const { trackURIBase } = await import("~/common/track.js");
        return trackURIBase("s3://my-bucket/path/to/track.flac");
      });
      expect(result).toBe("s3://my-bucket");
    });
  });

  describe("uniqueTrackURIs", () => {
    it("returns a set of base URIs", async () => {
      const result = await testWeb(async () => {
        const { uniqueTrackURIs } = await import("~/common/track.js");
        const tracks = [
          { $type: "sh.diffuse.output.track", id: "1", uri: "https://example.com/a.mp3" },
          { $type: "sh.diffuse.output.track", id: "2", uri: "https://example.com/b.mp3" },
        ] as never[];
        const set = uniqueTrackURIs(tracks);
        return [...set];
      });
      expect(result).toEqual(["https://example.com/"]);
    });

    it("deduplicates tracks from the same source", async () => {
      const result = await testWeb(async () => {
        const { uniqueTrackURIs } = await import("~/common/track.js");
        const tracks = [
          { $type: "sh.diffuse.output.track", id: "1", uri: "https://a.com/1.mp3" },
          { $type: "sh.diffuse.output.track", id: "2", uri: "https://a.com/2.mp3" },
          { $type: "sh.diffuse.output.track", id: "3", uri: "https://b.com/3.mp3" },
        ] as never[];
        return uniqueTrackURIs(tracks).size;
      });
      expect(result).toBe(2);
    });

    it("returns empty set for empty input", async () => {
      const result = await testWeb(async () => {
        const { uniqueTrackURIs } = await import("~/common/track.js");
        return uniqueTrackURIs([]).size;
      });
      expect(result).toBe(0);
    });
  });
});
