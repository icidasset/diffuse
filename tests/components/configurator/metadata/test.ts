import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("components/configurator/metadata", () => {
  it("returns track unchanged when there are no children", async () => {
    const result = await testWeb(async () => {
      const { CLASS } = await import(
        "~/components/configurator/metadata/element.js"
      );

      const configurator = new CLASS();
      document.body.append(configurator);
      await customElements.whenDefined(configurator.localName);

      const track = {
        $type: "sh.diffuse.output.track" as const,
        id: "metadata-configurator-test-no-children",
        uri: "local://test",
      };

      const result = await configurator.patch(track);
      return { sameId: result.id === track.id, hasTags: !!result.tags };
    });

    expect(result.sameId).toBe(true);
    expect(result.hasTags).toBe(false);
  });

  it("chains patches through children in sequence", async () => {
    const result = await testWeb(async () => {
      const HttpsInput = await import("~/components/input/https/element.js");
      const AudioFile = await import(
        "~/components/metadata/audio-file/element.js"
      );
      const { CLASS } = await import(
        "~/components/configurator/metadata/element.js"
      );

      const input = new HttpsInput.CLASS();
      input.id = "test-metadata-configurator-input";
      document.body.append(input);

      const audioFile = new AudioFile.CLASS();
      audioFile.setAttribute(
        "input-selector",
        "#test-metadata-configurator-input",
      );

      const configurator = new CLASS();
      configurator.append(audioFile);
      document.body.append(configurator);

      await customElements.whenDefined(input.localName);
      await customElements.whenDefined(configurator.localName);

      const blob = await fetch("/testing/sample/audio.mp3").then((r) =>
        r.blob()
      );
      const blobUri = URL.createObjectURL(blob);

      const track = {
        $type: "sh.diffuse.output.track" as const,
        id: "metadata-configurator-test-chain",
        uri: blobUri,
      };

      const patched = await configurator.patch(track);
      URL.revokeObjectURL(blobUri);

      return { title: patched.tags?.title ?? null };
    });

    expect(result.title).toBe("Mr. Sandman");
  });
});
