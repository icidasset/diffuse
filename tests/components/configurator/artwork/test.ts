import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("components/configurator/artwork", () => {
  it("returns null when there are no children", async () => {
    const result = await testWeb(async () => {
      const { CLASS } = await import(
        "~/components/configurator/artwork/element.js"
      );

      const configurator = new CLASS();
      document.body.append(configurator);
      await customElements.whenDefined(configurator.localName);

      return configurator.get({
        $type: "sh.diffuse.output.track" as const,
        id: "artwork-configurator-test",
        uri: "local://test",
      });
    });

    expect(result).toBe(null);
  });

  it("returns the result of the first child that returns non-null", async () => {
    const result = await testWeb(async () => {
      const { CLASS } = await import(
        "~/components/configurator/artwork/element.js"
      );
      const MusicBrainz = await import(
        "~/components/artwork/musicbrainz/element.js"
      );
      const LastFm = await import("~/components/artwork/last.fm/element.js");

      const configurator = new CLASS();
      configurator.append(new MusicBrainz.CLASS(), new LastFm.CLASS());
      document.body.append(configurator);
      await customElements.whenDefined(configurator.localName);

      // Track has no artist or album — both components return null
      return configurator.get({
        $type: "sh.diffuse.output.track" as const,
        id: "artwork-configurator-test",
        uri: "local://test",
      });
    });

    expect(result).toBe(null);
  });
});
