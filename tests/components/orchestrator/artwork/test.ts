import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";

describe("components/orchestrator/artwork", () => {
  it("returns null when track has no artist or album", async () => {
    const result = await testWeb(async () => {
      const LastFm = await import("~/components/artwork/last.fm/element.js");
      const MusicBrainz = await import(
        "~/components/artwork/musicbrainz/element.js"
      );
      const Configurator = await import(
        "~/components/configurator/artwork/element.js"
      );
      const Orchestrator = await import(
        "~/components/orchestrator/artwork/element.js"
      );

      const configurator = new Configurator.CLASS();
      configurator.id = "test-artwork-configurator-1";
      configurator.append(new LastFm.CLASS(), new MusicBrainz.CLASS());
      document.body.append(configurator);

      const orchestrator = new Orchestrator.CLASS();
      orchestrator.setAttribute(
        "artwork-selector",
        "#test-artwork-configurator-1",
      );
      document.body.append(orchestrator);

      await customElements.whenDefined(configurator.localName);
      await customElements.whenDefined(orchestrator.localName);

      return orchestrator.get({
        $type: "sh.diffuse.output.track" as const,
        id: "artwork-orch-test-no-tags",
        uri: "local://no-tags",
      });
    });

    expect(result).toBe(null);
  });

  it("returns cached result on subsequent calls for the same track", async () => {
    const [first, second] = await testWeb(async () => {
      const LastFm = await import("~/components/artwork/last.fm/element.js");
      const MusicBrainz = await import(
        "~/components/artwork/musicbrainz/element.js"
      );
      const Configurator = await import(
        "~/components/configurator/artwork/element.js"
      );
      const Orchestrator = await import(
        "~/components/orchestrator/artwork/element.js"
      );

      const configurator = new Configurator.CLASS();
      configurator.id = "test-artwork-configurator-2";
      configurator.append(new LastFm.CLASS(), new MusicBrainz.CLASS());
      document.body.append(configurator);

      const orchestrator = new Orchestrator.CLASS();
      orchestrator.setAttribute(
        "artwork-selector",
        "#test-artwork-configurator-2",
      );
      document.body.append(orchestrator);

      await customElements.whenDefined(configurator.localName);
      await customElements.whenDefined(orchestrator.localName);

      const track = {
        $type: "sh.diffuse.output.track" as const,
        id: "artwork-orch-test-cached",
        uri: "local://no-tags",
      };

      const first = await orchestrator.get(track);
      const second = await orchestrator.get(track);
      return [first?.length ?? null, second?.length ?? null];
    });

    expect(first).toEqual(second);
  });

});
