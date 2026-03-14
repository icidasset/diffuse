/**
 * @import {Facet} from "~/definitions/types.d.ts"
 */

import { loadURI } from "../loader.js";

/**
 * @param {Facet[]} facets
 * @param {HTMLElement} [container]
 */
export async function insertPreludes(facets, container) {
  container ??= document.body;

  const range = document.createRange();
  range.selectNode(container);

  const preludes = facets
    .filter((f) => f.kind === "prelude")
    .sort((a, b) => a.name.localeCompare(b.name));

  for (const prelude of preludes) {
    const html = prelude.html ??
      (prelude.uri ? await loadURI(prelude.uri) : "");
    if (!html) continue;
    const preludeFragment = range.createContextualFragment(html);
    container.append(preludeFragment);
  }
}
