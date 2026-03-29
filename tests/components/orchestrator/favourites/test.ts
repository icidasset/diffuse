import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";
import { tracks } from "~/testing/sample/tracks.js";

describe("components/orchestrator/favourites", () => {
  it("includes tracks", async () => {
    const favourites = await testWeb(async () => {
      const Output = await import(
        "~/components/configurator/output/element.js"
      );
      const Favourites = await import(
        "~/components/orchestrator/favourites/element.js"
      );
      const { tracks } = await import("~/testing/sample/tracks.js");

      const output = new Output.CLASS();
      output.id = "test-output";
      document.body.append(output);

      const fav = new Favourites.CLASS();
      fav.setAttribute("output-selector", "#test-output");
      document.body.append(fav);

      await customElements.whenDefined(output.localName);
      await customElements.whenDefined(fav.localName);

      await fav.include(tracks[0]);

      return fav.playlistItems();
    });

    expect(favourites.length).toBe(1);
    expect(favourites[0].criteria[0].value).toBe(tracks[0].tags?.artist);
    expect(favourites[0].criteria[1].value).toBe(tracks[0].tags?.title);
  });

  it("includes multiple tracks", async () => {
    const favourites = await testWeb(async () => {
      const Output = await import(
        "~/components/configurator/output/element.js"
      );
      const Favourites = await import(
        "~/components/orchestrator/favourites/element.js"
      );
      const { tracks } = await import("~/testing/sample/tracks.js");

      const output = new Output.CLASS();
      output.id = "test-output";
      document.body.append(output);

      const fav = new Favourites.CLASS();
      fav.setAttribute("output-selector", "#test-output");
      document.body.append(fav);

      await customElements.whenDefined(output.localName);
      await customElements.whenDefined(fav.localName);

      await fav.include(tracks);

      return fav.playlistItems();
    });

    expect(favourites.length).toBe(2);
    expect(favourites[0].criteria[0].value).toBe(tracks[0].tags?.artist);
    expect(favourites[0].criteria[1].value).toBe(tracks[0].tags?.title);
    expect(favourites[1].criteria[0].value).toBe(tracks[1].tags?.artist);
    expect(favourites[1].criteria[1].value).toBe(tracks[1].tags?.title);
  });

  it("does not include duplicate tracks", async () => {
    const favourites = await testWeb(async () => {
      const Output = await import(
        "~/components/configurator/output/element.js"
      );
      const Favourites = await import(
        "~/components/orchestrator/favourites/element.js"
      );
      const { tracks } = await import("~/testing/sample/tracks.js");

      const output = new Output.CLASS();
      output.id = "test-output";
      document.body.append(output);

      const fav = new Favourites.CLASS();
      fav.setAttribute("output-selector", "#test-output");
      document.body.append(fav);

      await customElements.whenDefined(output.localName);
      await customElements.whenDefined(fav.localName);

      await fav.include(tracks[0]);
      await fav.include(tracks[0]);

      return fav.playlistItems();
    });

    expect(favourites.length).toBe(1);
  });

  it("does not include duplicate tracks with different casing", async () => {
    const favourites = await testWeb(async () => {
      const Output = await import(
        "~/components/configurator/output/element.js"
      );
      const Favourites = await import(
        "~/components/orchestrator/favourites/element.js"
      );
      const { tracks } = await import("~/testing/sample/tracks.js");

      const output = new Output.CLASS();
      output.id = "test-output";
      document.body.append(output);

      const fav = new Favourites.CLASS();
      fav.setAttribute("output-selector", "#test-output");
      document.body.append(fav);

      await customElements.whenDefined(output.localName);
      await customElements.whenDefined(fav.localName);

      await fav.include(tracks[0]);
      await fav.include({
        ...tracks[0],
        tags: {
          ...tracks[0].tags,
          artist: tracks[0].tags?.artist?.toUpperCase(),
          title: tracks[0].tags?.title?.toUpperCase(),
        },
      });

      return fav.playlistItems();
    });

    expect(favourites.length).toBe(1);
  });

  it("expels tracks", async () => {
    const favourites = await testWeb(async () => {
      const Output = await import(
        "~/components/configurator/output/element.js"
      );
      const Favourites = await import(
        "~/components/orchestrator/favourites/element.js"
      );
      const { tracks } = await import("~/testing/sample/tracks.js");

      const output = new Output.CLASS();
      output.id = "test-output";
      document.body.append(output);

      const fav = new Favourites.CLASS();
      fav.setAttribute("output-selector", "#test-output");
      document.body.append(fav);

      await customElements.whenDefined(output.localName);
      await customElements.whenDefined(fav.localName);

      await fav.include(tracks);
      await fav.expel(tracks[0]);

      return fav.playlistItems();
    });

    expect(favourites.length).toBe(1);
    expect(favourites[0].criteria[0].value).toBe(tracks[1].tags?.artist);
    expect(favourites[0].criteria[1].value).toBe(tracks[1].tags?.title);
  });

  it("expels tracks with different casing", async () => {
    const favourites = await testWeb(async () => {
      const Output = await import(
        "~/components/configurator/output/element.js"
      );
      const Favourites = await import(
        "~/components/orchestrator/favourites/element.js"
      );
      const { tracks } = await import("~/testing/sample/tracks.js");

      const output = new Output.CLASS();
      output.id = "test-output";
      document.body.append(output);

      const fav = new Favourites.CLASS();
      fav.setAttribute("output-selector", "#test-output");
      document.body.append(fav);

      await customElements.whenDefined(output.localName);
      await customElements.whenDefined(fav.localName);

      await fav.include(tracks[0]);
      await fav.expel({
        ...tracks[0],
        tags: {
          ...tracks[0].tags,
          artist: tracks[0].tags?.artist?.toUpperCase(),
          title: tracks[0].tags?.title?.toUpperCase(),
        },
      });

      return fav.playlistItems();
    });

    expect(favourites.length).toBe(0);
  });

  it("toggles tracks", async () => {
    const result = await testWeb(async () => {
      const Output = await import(
        "~/components/configurator/output/element.js"
      );
      const Favourites = await import(
        "~/components/orchestrator/favourites/element.js"
      );
      const { tracks } = await import("~/testing/sample/tracks.js");

      const output = new Output.CLASS();
      output.id = "test-output";
      document.body.append(output);

      const fav = new Favourites.CLASS();
      fav.setAttribute("output-selector", "#test-output");
      document.body.append(fav);

      await customElements.whenDefined(output.localName);
      await customElements.whenDefined(fav.localName);

      // Toggle on
      await fav.toggle(tracks[0]);
      const afterAdd = fav.playlistItems();

      // Toggle off
      await fav.toggle(tracks[0]);
      const afterRemove = fav.playlistItems();

      return {
        afterAddCount: afterAdd.length,
        afterRemoveCount: afterRemove.length,
      };
    });

    expect(result.afterAddCount).toBe(1);
    expect(result.afterRemoveCount).toBe(0);
  });

  it("isFavourite returns false before include", async () => {
    const result = await testWeb(async () => {
      const Output = await import(
        "~/components/configurator/output/element.js"
      );
      const Favourites = await import(
        "~/components/orchestrator/favourites/element.js"
      );
      const { tracks } = await import("~/testing/sample/tracks.js");

      const output = new Output.CLASS();
      output.id = "test-output";
      document.body.append(output);

      const fav = new Favourites.CLASS();
      fav.setAttribute("output-selector", "#test-output");
      document.body.append(fav);

      await customElements.whenDefined(output.localName);
      await customElements.whenDefined(fav.localName);

      return fav.isFavourite(tracks[0]);
    });

    expect(result).toBe(false);
  });

  it("isFavourite returns true after include", async () => {
    const result = await testWeb(async () => {
      const Output = await import(
        "~/components/configurator/output/element.js"
      );
      const Favourites = await import(
        "~/components/orchestrator/favourites/element.js"
      );
      const { tracks } = await import("~/testing/sample/tracks.js");

      const output = new Output.CLASS();
      output.id = "test-output";
      document.body.append(output);

      const fav = new Favourites.CLASS();
      fav.setAttribute("output-selector", "#test-output");
      document.body.append(fav);

      await customElements.whenDefined(output.localName);
      await customElements.whenDefined(fav.localName);

      await fav.include(tracks[0]);
      return fav.isFavourite(tracks[0]);
    });

    expect(result).toBe(true);
  });

  it("isFavourite returns false after expel", async () => {
    const result = await testWeb(async () => {
      const Output = await import(
        "~/components/configurator/output/element.js"
      );
      const Favourites = await import(
        "~/components/orchestrator/favourites/element.js"
      );
      const { tracks } = await import("~/testing/sample/tracks.js");

      const output = new Output.CLASS();
      output.id = "test-output";
      document.body.append(output);

      const fav = new Favourites.CLASS();
      fav.setAttribute("output-selector", "#test-output");
      document.body.append(fav);

      await customElements.whenDefined(output.localName);
      await customElements.whenDefined(fav.localName);

      await fav.include(tracks[0]);
      await fav.expel(tracks[0]);
      return fav.isFavourite(tracks[0]);
    });

    expect(result).toBe(false);
  });

  it("toggles mixed tracks (some already favourited)", async () => {
    const favourites = await testWeb(async () => {
      const Output = await import(
        "~/components/configurator/output/element.js"
      );
      const Favourites = await import(
        "~/components/orchestrator/favourites/element.js"
      );
      const { tracks } = await import("~/testing/sample/tracks.js");

      const output = new Output.CLASS();
      output.id = "test-output";
      document.body.append(output);

      const fav = new Favourites.CLASS();
      fav.setAttribute("output-selector", "#test-output");
      document.body.append(fav);

      await customElements.whenDefined(output.localName);
      await customElements.whenDefined(fav.localName);

      // Add first track
      await fav.include(tracks[0]);

      // Toggle both — should remove first, add second
      await fav.toggle(tracks);

      return fav.playlistItems();
    });

    expect(favourites.length).toBe(1);
    expect(favourites[0].criteria[0].value).toBe(tracks[1].tags?.artist);
    expect(favourites[0].criteria[1].value).toBe(tracks[1].tags?.title);
  });
});
