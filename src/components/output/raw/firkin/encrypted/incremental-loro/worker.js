import { ostiary, rpc } from "~/common/worker.js";

import {
  changeDocumentBatched,
  changesSince,
  encodeDocument,
  headsOf,
  loadDocument,
} from "./doc.js";

/**
 * Apply a large collection save off the main thread.
 *
 * A document can't cross the worker boundary, so the main thread sends its
 * snapshot bytes. What comes back is an update plus the document's snapshot and
 * the entry to push, so the caller doesn't serialise anything twice.
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

  // `changeDocumentBatched` commits the whole collection in one transaction;
  // the callback hands the caller one update. No yielding is needed here: the
  // worker is already off the main thread.
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
