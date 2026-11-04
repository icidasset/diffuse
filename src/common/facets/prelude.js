/**
 * @import {Facet} from "~/definitions/types.d.ts"
 */

import { linkTileResources, resolveFacetTile } from "../loader.js";

/**
 * @param {Facet[]} facets
 * @param {HTMLElement} [container]
 */
export async function insertPreludes(facets, container) {
  container ??= document.body;

  const range = document.createRange();
  range.selectNode(container);

  const preludes = facets
    .filter((f) => f.kind === "prelude" && f.enabled !== false)
    .sort((a, b) => a.name.localeCompare(b.name));

  // Resolve tile resources in parallel. Tile fetches are independent and the
  // dashboard waits for preludes before rendering; sequential fetches made
  // startup latency scale with the number of enabled preludes.
  const resolved = await Promise.all(
    preludes.map(async (prelude) => {
      try {
        return { prelude, tile: await resolveFacetTile(prelude) };
      } catch (err) {
        console.error(
          `Failed to resolve prelude '${prelude.name}':`,
          err,
        );
        return { prelude, tile: undefined };
      }
    }),
  );

  for (const { tile } of resolved) {
    if (!tile?.html) continue;

    const fragment = range.createContextualFragment(tile.html);
    // Always run linking: it rewrites tile resources to Blob URLs and makes any
    // relative URLs absolute against the Diffuse build root. If linking fails,
    // still inject the prelude rather than skipping it.
    try {
      linkTileResources(fragment, tile.resources, tile.blocks);
    } catch (err) {
      console.error("Failed to link prelude resources", err);
    }
    container.append(fragment);
  }
}
