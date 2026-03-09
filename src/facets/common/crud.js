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

    const output = foundation.orchestrator.output();
    await Output.waitUntilLoaded(output.facets);

    output.facets.save(
      output.facets.collection().filter((c) => !(c.id === id)),
    );
  };
}

/**
 * @param {Facet} facet
 */
export async function saveFacet(facet) {
  const output = foundation.orchestrator.output();
  await Output.waitUntilLoaded(output.facets);

  const col = output.facets.collection();
  const colWithoutId = col.filter((c) => c.id !== facet.id);
  await output.facets.save([...colWithoutId, {
    ...facet,
    updatedAt: new Date().toISOString(),
  }]);
}
