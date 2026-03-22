import * as TID from "@atcute/tid";
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

  if (
    [
      "facets/data/input-bundle/index.html",
      "facets/data/output-bundle/index.html",
      "facets/data/process-tracks/prelude/index.html",
    ].includes(facet.url)
  ) {
    return [{
      ...properties,
      id: TID.now(),
    }];
  }

  return [];
});
