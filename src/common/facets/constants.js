import facets from "../../_data/facets.json" with {
  type: "json",
};

/**
 * @import {Facet} from "~/definitions/types.d.ts"
 */

export const TYPE = /** @type {const} */ ("sh.diffuse.output.facet");

/** @type {Facet[]} */
export const STARTING_SET = facets.flatMap((facet) => {
  const properties = {
    $type: TYPE,
    description: facet.desc,
    kind: facet.kind === "prelude"
      ? /** @type {const} */ ("prelude")
      : /** @type {const} */ ("interactive"),
    name: facet.title,
    uri: "diffuse://" + facet.url,
  };

  switch (facet.url) {
    case "facets/data/input-bundle/index.html":
      return [{
        ...properties,
        id: "defaults/input-bundle",
      }];
    case "facets/data/output-bundle/index.html":
      return [{
        ...properties,
        id: "defaults/output-bundle",
      }];
    case "facets/data/process-tracks/index.html":
      return [{
        ...properties,
        id: "defaults/process-tracks",
      }];
    default:
      return [];
  }
});
