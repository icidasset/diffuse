import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("components/artwork/input", () => {
  it("delegates to input.artwork and returns null when input has no artwork", async () => {
    const result = await testWeb(async () => {
      const HttpsInput = await import("~/components/input/https/element.js");
      const InputArtwork = await import(
        "~/components/artwork/input/element.js"
      );

      const input = new HttpsInput.CLASS();
      input.id = "test-https-input-artwork";
      document.body.append(input);

      const artwork = new InputArtwork.CLASS();
      artwork.setAttribute("input-selector", "#test-https-input-artwork");
      document.body.append(artwork);

      await customElements.whenDefined(input.localName);
      await customElements.whenDefined(artwork.localName);

      const blob = await fetch("http://localhost:3000/testing/sample/audio.mp3")
        .then((r) => r.blob());
      const blobUri = URL.createObjectURL(blob);

      const result = await artwork.get({
        $type: "sh.diffuse.output.track" as const,
        id: "input-artwork-test",
        uri: blobUri,
      });

      URL.revokeObjectURL(blobUri);

      return result ?? null;
    });

    expect(result).toBe(null);
  });
});
