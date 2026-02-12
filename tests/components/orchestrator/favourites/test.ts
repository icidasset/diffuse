import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { testWeb } from "@tests/common/index.ts";
import { tracks } from "@testing/sample/tracks.js";

describe("components/orchestrator/favourites", () => {
  it("includes tracks", async () => {
    const favourites = await testWeb(async () => {
      const Output = await import(
        "@components/configurator/output/element.js"
      );
      const Favourites = await import(
        "@components/orchestrator/favourites/element.js"
      );
      const { tracks } = await import("@testing/sample/tracks.js");

      const output = new Output.CLASS();
      output.id = "test-output";
      document.body.append(output);

      const fav = new Favourites.CLASS();
      fav.setAttribute("output-selector", "#test-output");
      document.body.append(fav);

      await customElements.whenDefined(output.localName);
      await customElements.whenDefined(fav.localName);

      await fav.include(tracks[0]);

      return fav.playlist();
    });

    expect(favourites.items.length).toBe(1);
    expect(favourites.items[0].criteria[0].value).toBe(tracks[0].id);
  });

  it("includes multiple tracks", async () => {
    const favourites = await testWeb(async () => {
      const Output = await import(
        "@components/configurator/output/element.js"
      );
      const Favourites = await import(
        "@components/orchestrator/favourites/element.js"
      );
      const { tracks } = await import("@testing/sample/tracks.js");

      const output = new Output.CLASS();
      output.id = "test-output";
      document.body.append(output);

      const fav = new Favourites.CLASS();
      fav.setAttribute("output-selector", "#test-output");
      document.body.append(fav);

      await customElements.whenDefined(output.localName);
      await customElements.whenDefined(fav.localName);

      await fav.include(tracks);

      return fav.playlist();
    });

    expect(favourites.items.length).toBe(2);
    expect(favourites.items[0].criteria[0].value).toBe(tracks[0].id);
    expect(favourites.items[1].criteria[0].value).toBe(tracks[1].id);
  });

  it("does not include duplicate tracks", async () => {
    const favourites = await testWeb(async () => {
      const Output = await import(
        "@components/configurator/output/element.js"
      );
      const Favourites = await import(
        "@components/orchestrator/favourites/element.js"
      );
      const { tracks } = await import("@testing/sample/tracks.js");

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

      return fav.playlist();
    });

    expect(favourites.items.length).toBe(1);
  });

  it("expels tracks", async () => {
    const favourites = await testWeb(async () => {
      const Output = await import(
        "@components/configurator/output/element.js"
      );
      const Favourites = await import(
        "@components/orchestrator/favourites/element.js"
      );
      const { tracks } = await import("@testing/sample/tracks.js");

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

      return fav.playlist();
    });

    expect(favourites.items.length).toBe(1);
    expect(favourites.items[0].criteria[0].value).toBe(tracks[1].id);
  });

  it("toggles tracks", async () => {
    const result = await testWeb(async () => {
      const Output = await import(
        "@components/configurator/output/element.js"
      );
      const Favourites = await import(
        "@components/orchestrator/favourites/element.js"
      );
      const { tracks } = await import("@testing/sample/tracks.js");

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
      const afterAdd = fav.playlist();

      // Toggle off
      await fav.toggle(tracks[0]);
      const afterRemove = fav.playlist();

      return {
        afterAddCount: afterAdd.items.length,
        afterRemoveCount: afterRemove.items.length,
      };
    });

    expect(result.afterAddCount).toBe(1);
    expect(result.afterRemoveCount).toBe(0);
  });

  it("toggles mixed tracks (some already favourited)", async () => {
    const favourites = await testWeb(async () => {
      const Output = await import(
        "@components/configurator/output/element.js"
      );
      const Favourites = await import(
        "@components/orchestrator/favourites/element.js"
      );
      const { tracks } = await import("@testing/sample/tracks.js");

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

      return fav.playlist();
    });

    expect(favourites.items.length).toBe(1);
    expect(favourites.items[0].criteria[0].value).toBe(tracks[1].id);
  });
});
