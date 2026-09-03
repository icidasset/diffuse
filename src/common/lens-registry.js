/**
 * Lens registry: authored lens documents that migrate diffuse data from one
 * lexicon NSID to another.
 *
 * Lens documents are deliberately authored (per schema change) and schema
 * independent: each is an object carrying `id`, `source`, `target`, and `steps`,
 * which `@panproto/core`'s `compileLensDocument` accepts. A build's bundled
 * registry resolves an `(sourceNsid, targetNsid)` transition to a lens document.
 * When a payload's `$schemaHistory` embeds its own lens for a segment the reader
 * doesn't know, the embedded document takes precedence for that segment (so old
 * apps can read newer data).
 *
 * In keeping with atproto's lexicon model, the NSID is the schema identity, so
 * `source`/`target` are NSIDs. Compatible evolution keeps the same NSID (new
 * optional fields, no lens needed); breaking changes use a NEW NSID and an
 * authored lens from the old NSID to the new one.
 */

/**
 * A lens document that maps one lexicon NSID to another.
 *
 * @typedef {{
 *   id: string;
 *   source: string;
 *   target: string;
 *   steps: unknown[];
 * }} LensDocument
 */

/** @type {Map<string, LensDocument>} */
const registry = new Map();

/**
 * Register an authored lens document.
 *
 * @param {LensDocument} doc
 *
 * @example Registers and looks up a lens by its transition
 * ```js
 * import { register, resolve } from "~/common/lens-registry.js";
 *
 * register({ id: "track-v1-to-v2", source: "sh.diffuse.output.track", target: "sh.diffuse.output.track2", steps: [] });
 * const doc = resolve("sh.diffuse.output.track", "sh.diffuse.output.track2");
 * if (doc?.id !== "track-v1-to-v2") throw new Error("expected registered lens");
 * ```
 */
export function register(doc) {
  registry.set(key(doc.source, doc.target), doc);
}

/**
 * Resolve a lens document for a transition. The bundled registry is consulted
 * first; then the `history` supplied with the payload (embedded lenses), which
 * lets an older app traverse segments it doesn't have in its own registry.
 *
 * @param {string} from - The source lexicon NSID
 * @param {string} to - The target lexicon NSID
 * @param {{ history?: import("./self-describing.js").HistoryEntry<LensDocument | null>[] }} [options]
 * @returns {LensDocument | null}
 *
 * @example Finds a bundled-registry lens before any embedded one
 * ```js
 * import { register, resolve } from "~/common/lens-registry.js";
 *
 * register({ id: "a-to-b", source: "s", target: "t", steps: [] });
 * const doc = resolve("s", "t");
 * if (!doc) throw new Error("expected a lens");
 * ```
 *
 * @example Falls back to an embedded lens for a segment not in the registry
 * ```js
 * import { resolve } from "~/common/lens-registry.js";
 *
 * const embedded = { id: "old-segment", source: "s", target: "t", steps: [{ rename_field: { old: "a", new: "b" } }] };
 * const doc = resolve("s", "t", {
 *   history: [{
 *     from: "s", to: "t", lens: embedded, complement: null,
 *   }],
 * });
 * if (doc?.id !== "old-segment") throw new Error("expected embedded lens");
 * ```
 */
export function resolve(from, to, options = {}) {
  const bundled = registry.get(key(from, to));
  if (bundled) return bundled;

  const entry = options.history?.find(
    (h) => h.from === from && h.to === to,
  );
  return entry?.lens ?? null;
}

/**
 * @param {string} source
 * @param {string} target
 */
function key(source, target) {
  return `${source}@${target}`;
}