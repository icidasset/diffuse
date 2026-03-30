import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("facets/data/artwork-bundle", () => {
  it("inputArtwork prepends an input artwork element", async () => {
    const result = await testWeb(async () => {
      const { inputArtwork } = await import(
        "~/facets/data/artwork-bundle/index.inline.js"
      );
      const artwork = document.createElement("div");
      const input = { selector: "#test-input" };
      inputArtwork(artwork as never, input as never);
      return artwork.children[0]?.localName ?? null;
    });
    expect(result).toBe("da-input");
  });

  it("inputArtwork sets input-selector attribute", async () => {
    const result = await testWeb(async () => {
      const { inputArtwork } = await import(
        "~/facets/data/artwork-bundle/index.inline.js"
      );
      const artwork = document.createElement("div");
      const input = { selector: "#my-input" };
      inputArtwork(artwork as never, input as never);
      return artwork.children[0]?.getAttribute("input-selector") ?? null;
    });
    expect(result).toBe("#my-input");
  });

  it("audioMetadata appends an audio-metadata element", async () => {
    const result = await testWeb(async () => {
      const { audioMetadata } = await import(
        "~/facets/data/artwork-bundle/index.inline.js"
      );
      const artwork = document.createElement("div");
      const input = { selector: "#test-input" };
      audioMetadata(artwork as never, input as never);
      return artwork.children[0]?.localName ?? null;
    });
    expect(result).toBe("da-audio-metadata");
  });

  it("audioMetadata sets input-selector attribute", async () => {
    const result = await testWeb(async () => {
      const { audioMetadata } = await import(
        "~/facets/data/artwork-bundle/index.inline.js"
      );
      const artwork = document.createElement("div");
      const input = { selector: "#my-input" };
      audioMetadata(artwork as never, input as never);
      return artwork.children[0]?.getAttribute("input-selector") ?? null;
    });
    expect(result).toBe("#my-input");
  });

  it("lastFm appends a Last.fm artwork element", async () => {
    const result = await testWeb(async () => {
      const { lastFm } = await import(
        "~/facets/data/artwork-bundle/index.inline.js"
      );
      const artwork = document.createElement("div");
      lastFm(artwork as never);
      return artwork.children[0]?.localName ?? null;
    });
    expect(result).toBe("da-lastfm");
  });

  it("musicBrainz appends a MusicBrainz artwork element", async () => {
    const result = await testWeb(async () => {
      const { musicBrainz } = await import(
        "~/facets/data/artwork-bundle/index.inline.js"
      );
      const artwork = document.createElement("div");
      musicBrainz(artwork as never);
      return artwork.children[0]?.localName ?? null;
    });
    expect(result).toBe("da-musicbrainz");
  });

  it("inputArtwork is prepended before audioMetadata", async () => {
    const result = await testWeb(async () => {
      const { inputArtwork, audioMetadata } = await import(
        "~/facets/data/artwork-bundle/index.inline.js"
      );
      const artwork = document.createElement("div");
      const input = { selector: "#test-input" };

      // audioMetadata appended first, then inputArtwork prepended
      audioMetadata(artwork as never, input as never);
      inputArtwork(artwork as never, input as never);

      return Array.from(artwork.children).map((el) => el.localName);
    });
    expect(result[0]).toBe("da-input");
    expect(result[1]).toBe("da-audio-metadata");
  });
});
