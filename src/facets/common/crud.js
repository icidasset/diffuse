import * as Output from "~/common/output.js";
import foundation from "~/common/facets/foundation.js";

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

    const output = await foundation.orchestrator.output();
    const col = await Output.data(output.facets);

    output.facets.save(col.filter((c) => !(c.id === id)));
  };
}

/**
 * @param {Facet} facet
 */
export async function saveFacet(facet) {
  const output = await foundation.orchestrator.output();
  const col = await Output.data(output.facets);
  const colWithoutId = col.filter((c) => c.id !== facet.id);
  await output.facets.save([...colWithoutId, {
    ...facet,
    updatedAt: new Date().toISOString(),
  }]);
}
