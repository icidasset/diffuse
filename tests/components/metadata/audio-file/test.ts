import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("components/metadata/audio-file", () => {
  it("returns track unchanged when URI is unresolvable", async () => {
    const result = await testWeb(async () => {
      const HttpsInput = await import("~/components/input/https/element.js");
      const AudioFile = await import(
        "~/components/metadata/audio-file/element.js"
      );

      const input = new HttpsInput.CLASS();
      input.id = "test-metadata-https-input-1";
      document.body.append(input);

      const audioFile = new AudioFile.CLASS();
      audioFile.setAttribute("input-selector", "#test-metadata-https-input-1");
      document.body.append(audioFile);

      await customElements.whenDefined(input.localName);
      await customElements.whenDefined(audioFile.localName);

      const track = {
        $type: "sh.diffuse.output.track" as const,
        id: "metadata-audio-file-test-unresolvable",
        uri: "local://no-such-file",
      };

      const result = await audioFile.patch(track);
      return { hasTags: !!result.tags, hasStats: !!result.stats };
    });

    expect(result.hasTags).toBe(false);
    expect(result.hasStats).toBe(false);
  });

  it("extracts tags and stats from sample audio file", async () => {
    const result = await testWeb(async () => {
      const HttpsInput = await import("~/components/input/https/element.js");
      const AudioFile = await import(
        "~/components/metadata/audio-file/element.js"
      );

      const input = new HttpsInput.CLASS();
      input.id = "test-metadata-https-input-2";
      document.body.append(input);

      const audioFile = new AudioFile.CLASS();
      audioFile.setAttribute("input-selector", "#test-metadata-https-input-2");
      document.body.append(audioFile);

      await customElements.whenDefined(input.localName);
      await customElements.whenDefined(audioFile.localName);

      const blob = await fetch("/testing/sample/audio.mp3").then((r) =>
        r.blob()
      );
      const blobUri = URL.createObjectURL(blob);

      const track = {
        $type: "sh.diffuse.output.track" as const,
        id: "metadata-audio-file-test-sample",
        uri: blobUri,
      };

      const patched = await audioFile.patch(track);
      URL.revokeObjectURL(blobUri);

      return { tags: patched.tags ?? null, stats: patched.stats ?? null };
    });

    expect(result.tags).not.toBe(null);
    expect(result.tags?.title).toBe("Mr. Sandman");
    expect(result.tags?.album).toBe("Mr. Sandman");
    expect(result.tags?.year).toBe(1954);
    expect(result.tags?.track?.no).toBe(1);
    expect(result.tags?.artist).toContain("The Chordettes");

    expect(result.stats).not.toBe(null);
    expect(result.stats?.bitrate).toBe(143320);
    expect(result.stats?.duration).toBeGreaterThan(150000);
    expect(result.stats?.duration).toBeLessThan(152000);
  });
});
