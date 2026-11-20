/**
 * Helpers for reading the local (default) output.
 *
 * The default output (`dtos-json`, `#do-output__dc-output__local`) stores each
 * collection as a JSON string and is the layer that decodes those strings.
 * Reading the raw IndexedDB output (`#do-output__dop-indexed-db__json`) would
 * return the encoded string instead of the records.
 *
 * @import { CollectionName } from "~/common/self-describing.js"
 */

/** Selector of the default output element in the output orchestrator. */
const LOCAL_OUTPUT_SELECTOR = "#do-output__dc-output__local";

/**
 * Wait for a collection of the default/local output to load and return its
 * records. Returns an empty array when the output is missing, the collection
 * errors, or the load times out.
 *
 * @param {CollectionName} name
 * @returns {Promise<any[]>}
 */
export async function readLocalCollection(name) {
  const local = /** @type {any} */ (
    document.querySelector(LOCAL_OUTPUT_SELECTOR)
  );
  if (!local?.[name]) return [];

  const deadline = Date.now() + 5_000;

  while (Date.now() < deadline) {
    const col = local[name].collection();
    if (col.state === "loaded") {
      return Array.isArray(col.data) ? col.data : [];
    }
    if (col.state === "error") return [];
    await new Promise((resolve) => setTimeout(resolve, 100));
  }

  return [];
}
