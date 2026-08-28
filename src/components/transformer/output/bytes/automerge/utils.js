import * as Automerge from "@automerge/automerge";
import { base64 } from "iso-base/rfc4648";

/**
 * Generate a new collection document.
 */
export function initCollectionDoc() {
  const doc = Automerge.change(Automerge.init(), (doc) => {
    doc.collection = [];
  });

  const bytes = Automerge.save(doc);
  return base64.encode(bytes);
}
