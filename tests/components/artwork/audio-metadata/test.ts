import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("components/artwork/audio-metadata", () => {
  it("extracts embedded artwork from the sample audio file", async () => {
    const byteLength = await testWeb(async () => {
      const HttpsInput = await import("~/components/input/https/element.js");
      const AudioMetadata = await import(
        "~/components/artwork/audio-metadata/element.js"
      );

      const input = new HttpsInput.CLASS();
      input.id = "test-https-input";
      document.body.append(input);

      const audioMetadata = new AudioMetadata.CLASS();
      audioMetadata.setAttribute("input-selector", "#test-https-input");
      document.body.append(audioMetadata);

      await customElements.whenDefined(input.localName);
      await customElements.whenDefined(audioMetadata.localName);

      const blob = await fetch("http://localhost:3000/testing/sample/audio.mp3")
        .then((r) => r.blob());
      const blobUri = URL.createObjectURL(blob);

      const result = await audioMetadata.get({
        $type: "sh.diffuse.output.track" as const,
        id: "audio-metadata-artwork-test",
        uri: blobUri,
      });

      URL.revokeObjectURL(blobUri);
      return result?.length ?? 0;
    });

    expect(byteLength).toBeGreaterThan(0);
  });
});
