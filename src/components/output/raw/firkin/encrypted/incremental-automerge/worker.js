import { ostiary, rpc } from "~/common/worker.js";

import {
  changeDocument,
  changeDocumentBatched,
  changesSince,
  encodeDocument,
  hasRecords,
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
 * @param {{ bytes: Uint8Array; items: any[]; schema?: string; versioned?: boolean; pushHeads?: string[]; snapshot?: boolean }} args
 * @returns {Promise<{ changed: boolean; deltas?: Uint8Array[]; docBytes?: Uint8Array; pushBytes?: Uint8Array; heads?: string[] }>}
 */
async function applyLargeChange(
  { bytes, items, schema, versioned, pushHeads, snapshot = false },
) {
  if (!(bytes instanceof Uint8Array)) {
    throw new TypeError("Expected the document bytes");
  }

  const doc = loadDocument(bytes);
  if (!doc) throw new Error("Could not load the document");

  /** @type {Uint8Array[]} */
  const deltas = [];
  let previous = headsOf(doc);

  // The first import into a document is written as one change: `changeDocument`
  // assigns the whole record map at once, which is several times faster than
  // writing keys one by one and needs no batching. Later saves are patches of
  // an existing map, where batching is what keeps the cost down.
  if (!hasRecords(doc)) {
    const result = changeDocument(doc, items, { schema, versioned });

    if (!result.changed) return { changed: false };

    const next = result.doc;
    const docBytes = encodeDocument(next);
    const pushBytes = snapshot ? docBytes : changesSince(next, pushHeads ?? []);

    return {
      changed: true,
      deltas: [changesSince(next, previous)],
      docBytes,
      pushBytes,
      heads: headsOf(next),
    };
  }

  // No yielding is needed here: the worker is already off the main thread.
  const { doc: next, changed } = await changeDocumentBatched(
    doc,
    items,
    { schema, versioned },
    async () => {},
    (batch) => {
      deltas.push(changesSince(batch, previous));
      previous = headsOf(batch);
    },
  );

  if (!changed) return { changed: false };

  // The whole document is needed for the compact local cache; when the push
  // cadence calls for a snapshot, the same bytes serve as the log entry.
  const docBytes = encodeDocument(next);
  const pushBytes = snapshot
    ? docBytes
    : changesSince(next, pushHeads ?? []);

  return {
    changed: true,
    deltas,
    docBytes,
    pushBytes,
    heads: headsOf(next),
  };
}

////////////////////////////////////////////
// 🚀
////////////////////////////////////////////

ostiary((context) => {
  rpc(context, { applyLargeChange });
});
