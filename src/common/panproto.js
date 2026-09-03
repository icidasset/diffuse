/**
 * panproto (`@panproto/core`) connection point.
 *
 * The pure-JS migration engine (`~/common/lens.js`) projects records between
 * schema NSIDs by interpreting an authored lens document's `steps` — sufficient
 * for the additive/rename migrations diffuse ships. panproto's WASM engine is the
 * *complement* path: it produces and consumes the opaque `complement` so that
 * fields a migration discarded can be written back losslessly (see design doc
 * open items).
 *
 * This module loads `@panproto/core` lazily (its WASM is only needed when the
 * complement path actually runs), and exposes helpers for the flat-record JSON
 * call shape confirmed in the design spike:
 *
 * - include the record's `$type` and root at the record's object/body vertex;
 * - `chain.instantiate(schema)` needs only the *target* schema;
 * - `getJson` produces `{ view, complement }`; `putJson(view, complement)`
 *   reconstructs the source losslessly.
 *
 * @import {LensDocument} from "~/common/lens-registry.js"
 */

let promise = null;

/**
 * Lazily obtain a panproto instance.
 *
 * @returns {Promise<any>}
 */
async function panproto() {
  promise ??= import("@panproto/core").then(
    /** @param {any} m */
    async (m) => m.Panproto.init(),
  );
  return promise;
}

/**
 * Compile an authored lens document and instantiate it against the target schema.
 *
 * @param {LensDocument} doc
 * @param {unknown} schema - a `BuiltSchema` from `parseLexicon`
 * @returns {Promise<any>} an instantiated `LensHandle`
 *
 * @example Compiles a lens and round-trips a record losslessly
 * ```ts
 * import { compileLens, parseLexicon, get } from "~/common/panproto.js";
 *
 * const NEW_LEXICON = {
 *   lexicon: 1, id: "sh.diffuse.output.facet2",
 *   defs: { main: { type: "record", record: { type: "object", properties: {
 *     $type: { type: "string" }, id: { type: "string" }, name: { type: "string" },
 *     starred: { type: "boolean" }, description: { type: "string" },
 *   } } } },
 * };
 * const schema = await parseLexicon(NEW_LEXICON);
 * const doc = {
 *   id: "f-to-f2", source: "sh.diffuse.output.facet", target: "sh.diffuse.output.facet2",
 *   steps: [{ rename_field: { old: "favourite", new: "starred" } }],
 * };
 * await compileLens(doc, schema);
 * const record = { $type: "sh.diffuse.output.facet", id: "a", name: "x", favourite: true };
 * const { view, complement } = await get(doc, schema, record);
 * if (view === undefined) throw new Error("expected a projected view");
 * if (!(complement instanceof Uint8Array) || complement.length === 0) throw new Error("expected a complement");
 * ```
 */
export async function compileLens(doc, schema) {
  const pp = await panproto();
  const chain = pp.compileLensDocument(doc, bodyVertex(doc), "json");
  return chain.instantiate(schema);
}

/**
 * Parse an atproto lexicon (a diffuse `lexicons/output/*.json` object) into a
 * `BuiltSchema` usable for instantiating a lens.
 *
 * @param {object | string} lexicon
 * @returns {Promise<any>} a `BuiltSchema`
 */
export async function parseLexicon(lexicon) {
  const pp = await panproto();
  return pp.parseLexicon(lexicon);
}

/**
 * Forward projection: extract the view + complement from a source record.
 *
 * @param {LensDocument} doc
 * @param {unknown} schema
 * @param {unknown} record
 * @returns {Promise<{ view: unknown; complement: Uint8Array }>}
 */
export async function get(doc, schema, record) {
  const lens = await compileLens(doc, schema);
  return lens.getJson(record, bodyVertex(doc));
}

/**
 * Backward put: reconstruct a source record from an edited view + complement,
 * losslessly (GetPut).
 *
 * @param {LensDocument} doc
 * @param {unknown} schema
 * @param {unknown} view
 * @param {Uint8Array} complement
 * @returns {Promise<unknown>}
 */
export async function put(doc, schema, view, complement) {
  const lens = await compileLens(doc, schema);
  return lens.putJson(view, complement, bodyVertex(doc));
}

/**
 * The record's object/body vertex of the target schema the lens projects to
 * (used as the root for `getJson`/`putJson`). Field steps attach to this vertex.
 *
 * @param {LensDocument} doc
 * @returns {string}
 */
function bodyVertex(doc) {
  return `${doc.target ?? doc.source}:body`;
}