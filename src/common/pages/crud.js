import * as Output from "~/common/output.js";
import { output } from "./output.js";

/**
 * @import {Facet} from "~/definitions/types.d.ts"
 */

/**
 * @param {{ id: string }} _
 */
export function deleteFacet({ id }) {
  return async () => {
    const c = confirm("Are you sure you want to delete this facet?");
    if (!c) return;

    const out = await output();
    const col = await Output.data(out.facets);

    out.facets.save(col.filter((c) => !(c.id === id)));
  };
}

/**
 * @param {Facet} facet
 */
export async function saveFacet(facet) {
  const out = await output();
  const col = await Output.data(out.facets);
  const colWithoutId = col.filter((c) => c.id !== facet.id);
  await out.facets.save([...colWithoutId, {
    ...facet,
    updatedAt: new Date().toISOString(),
  }]);
}
