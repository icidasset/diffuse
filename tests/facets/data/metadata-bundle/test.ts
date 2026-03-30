import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("facets/data/metadata-bundle", () => {
  it("audioFile appends an audio-file element to metadata", async () => {
    const result = await testWeb(async () => {
      const { audioFile } = await import(
        "~/facets/data/metadata-bundle/index.inline.js"
      );
      const metadata = document.createElement("div");
      const input = { selector: "#test-input" };
      audioFile(metadata as never, input as never);
      return metadata.children[0]?.localName ?? null;
    });
    expect(result).toBe("dm-audio-file");
  });

  it("audioFile sets input-selector attribute from input.selector", async () => {
    const result = await testWeb(async () => {
      const { audioFile } = await import(
        "~/facets/data/metadata-bundle/index.inline.js"
      );
      const metadata = document.createElement("div");
      const input = { selector: "#my-input-configurator" };
      audioFile(metadata as never, input as never);
      return metadata.children[0]?.getAttribute("input-selector") ?? null;
    });
    expect(result).toBe("#my-input-configurator");
  });

  it("audioFile appends exactly one element", async () => {
    const result = await testWeb(async () => {
      const { audioFile } = await import(
        "~/facets/data/metadata-bundle/index.inline.js"
      );
      const metadata = document.createElement("div");
      const input = { selector: "#test-input" };
      audioFile(metadata as never, input as never);
      return metadata.children.length;
    });
    expect(result).toBe(1);
  });
});
