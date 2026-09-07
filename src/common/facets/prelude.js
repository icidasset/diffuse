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

  for (const prelude of preludes) {
    const tile = await resolveFacetTile(prelude);
    if (!tile?.html) continue;

    const fragment = range.createContextualFragment(tile.html);
    if (tile.resources && Object.keys(tile.resources).length) {
      // Rewrite absolute `/…` resource URLs to Blob URLs on the detached
      // fragment BEFORE inserting it, so the browser never requests the raw
      // (un-rewritten) path from the build root.
      linkTileResources(fragment, tile.resources, tile.blocks);
    }
    container.append(fragment);
  }
}
