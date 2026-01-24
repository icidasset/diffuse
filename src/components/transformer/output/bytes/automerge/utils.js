import * as Automerge from "@automerge/automerge";
import { base64 } from "iso-base/rfc4648";

/**
 * Generate a new tracks document to put in the `INITIAL_TRACKS_DOCUMENT` constant.
 */
export function initTracksDoc() {
  const doc = Automerge.change(Automerge.init(), (doc) => {
    doc.collection = [];
  });

  const bytes = Automerge.save(doc);
  return base64.encode(bytes);
}
