import { ostiary, rpc } from "~/common/worker.js";

import {
  changeDocumentBatched,
  changesSince,
  headsOf,
  loadDocument,
} from "./doc.js";

/**
 * Apply a large collection save off the main thread.
 *
 * The document has to cross the worker boundary (Automerge docs are not
 * transferable), so the main thread sends its bytes. What comes back is not the
 * whole document but **one delta per batch**: the caller applies them to its
 * own copy, yielding between them, so even the result doesn't block the page.
 *
 * @param {{ bytes: Uint8Array; items: any[]; schema?: string }} args
 * @returns {Promise<{ changed: boolean; deltas?: Uint8Array[] }>}
 */
async function applyLargeChange({ bytes, items, schema }) {
  if (!(bytes instanceof Uint8Array)) {
    throw new TypeError("Expected the document bytes");
  }

  const doc = loadDocument(bytes);
  if (!doc) throw new Error("Could not load the document");

  /** @type {Uint8Array[]} */
  const deltas = [];
  let previous = headsOf(doc);

  // No yielding is needed here: the worker is already off the main thread.
  const { changed } = await changeDocumentBatched(
    doc,
    items,
    { schema },
    async () => {},
    (next) => {
      deltas.push(changesSince(next, previous));
      previous = headsOf(next);
    },
  );

  if (!changed) return { changed: false };

  return { changed: true, deltas };
}

////////////////////////////////////////////
// 🚀
////////////////////////////////////////////

ostiary((context) => {
  rpc(context, { applyLargeChange });
});
