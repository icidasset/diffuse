import { describe, it } from "@std/testing/bdd";
import { expect } from "@std/expect";

import { buildTileCar } from "../../../tasks/tile-car.ts";
import { parseCar, resolveRoot } from "~/common/tiles.js";

// Round-trips the build-time tile CAR generator against the loader's CAR parser.
describe("build-time tile CAR", () => {
  it("produces a CAR the loader can parse, rooted at `/`", async () => {
    const html = '<link rel="stylesheet" href="/style.css">\n<div id="app"></div>';
    const js = 'import foundation from "~/common/foundation.js";';
    const css = "#app { color: red }";

    const car = await buildTileCar(
      {
        "/": { content: html },
        "/facet.js": { content: js },
        "/style.css": { content: css },
      },
      { name: "Test Tile" },
    );

    // Loader reads the CAR header as the manifest + blocks.
    const { manifest, blocks } = parseCar(car);

    // The manifest's `/` is the index; it resolves back to the html.
    const root = resolveRoot(manifest.resources, blocks);
    expect(root?.html).toBe(html);
    expect(manifest.name).toBe("Test Tile");

    // Every listed absolute resource is present as a block, and its content
    // round-trips exactly (so `/facet.js` really is embedded in the CAR).
    for (const path of ["/facet.js", "/style.css"]) {
      const resource = manifest.resources?.[path];
      expect(resource).toBeDefined();
      const src = resource && resource.src;
      const cid = src && (typeof src === "string" ? src : src.$link);
      expect(cid).toBeDefined();
      const bytes = cid && blocks.get(String(cid));
      expect(bytes).toBeDefined();
      const expected = path === "/facet.js" ? js : css;
      expect(new TextDecoder().decode(bytes)).toBe(expected);
    }

    // Shared/relative assets are not shipped as resources.
    expect(manifest.resources?.["/styles/base.css"]).toBeUndefined();
  });
});