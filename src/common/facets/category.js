/**
 * @import { Facet } from "~/definitions/types.d.ts";
 */

/**
 * @param {Facet} facet
 * @returns {string}
 *
 * @example Returns accent-twist-4 for prelude kind and accent-twist-2 for others
 * ```js
 * import { color } from "~/common/facets/category.js";
 *
 * const prelude = color({ $type: "sh.diffuse.output.facet", id: "1", name: "x", kind: "prelude" });
 * if (prelude !== "var(--accent-twist-4)") throw new Error("prelude should return accent-twist-4");
 *
 * const interactive = color({ $type: "sh.diffuse.output.facet", id: "1", name: "x", kind: "interactive" });
 * if (interactive !== "var(--accent-twist-2)") throw new Error("interactive should return accent-twist-2");
 *
 * const undef = color({ $type: "sh.diffuse.output.facet", id: "1", name: "x" });
 * if (undef !== "var(--accent-twist-2)") throw new Error("undefined kind should return accent-twist-2");
 * ```
 */
export function color(facet) {
  switch (facet.kind) {
    case "prelude":
      return "var(--accent-twist-4)";
    default:
      return "var(--accent-twist-2)";
  }
}

/**
 * @param {Facet} facet
 * @returns {string}
 *
 * @example Returns 'feature' for prelude kind and 'interface' for others
 * ```js
 * import { name } from "~/common/facets/category.js";
 *
 * if (name({ $type: "sh.diffuse.output.facet", id: "1", name: "x", kind: "prelude" }) !== "feature") throw new Error("prelude should return 'feature'");
 * if (name({ $type: "sh.diffuse.output.facet", id: "1", name: "x", kind: "interactive" }) !== "interface") throw new Error("interactive should return 'interface'");
 * if (name({ $type: "sh.diffuse.output.facet", id: "1", name: "x" }) !== "interface") throw new Error("undefined kind should return 'interface'");
 * ```
 */
export function name(facet) {
  // return facet.kind ?? "interactive";
  switch (facet.kind) {
    case "prelude":
      return "feature";
    default:
      return "interface";
  }
}
