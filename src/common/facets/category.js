/**
 * @import { Facet } from "~/definitions/types.d.ts";
 */

/**
 * @param {Facet} facet
 * @returns {string}
 */
export function color(facet) {
  switch (facet.kind) {
    case "prelude":
      return "var(--accent)";
    default:
      return "var(--accent-twist-2)";
  }
}

/**
 * @param {Facet} facet
 * @returns {string}
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
