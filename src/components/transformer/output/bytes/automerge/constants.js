import * as Automerge from "@automerge/automerge";
import { base64 } from "iso-base/rfc4648";

/**
 * @import { FacetsDocument, PlaylistItemsDocument, SettingsDocument, TracksDocument } from "./types.d.ts";
 */

/** @type {Automerge.Doc<FacetsDocument>} */
export const INITIAL_FACETS_DOCUMENT = Automerge.load(
  base64.decode(
    "hW9Kg1fmLLcAeAEQZ2dnbV0O8AUftAeEXT1QVQHsmc9lhGh1ysz4ECa3pEUgzSZG2aF8hob78u7FqeybBQYBAgMCEwIjBkACVgIHFQwhAiMCNAFCAlYCgAECfwB/AX8Bf8KTqcwGfwB/B38KY29sbGVjdGlvbn8AfwEBfwJ/AH8AAA",
  ),
);

/** @type {Automerge.Doc<PlaylistItemsDocument>} */
export const INITIAL_PLAYLIST_ITEMS_DOCUMENT = Automerge.load(
  base64.decode(
    "hW9Kg5IPZcsAeAEQIyp0LRYp0l9bpZKWJXTPlgGtUD/lrIatFjiIwoUdtJhh/sBQFIcpPppxduoIp1ArXwYBAgMCEwIjBkACVgIHFQwhAiMCNAFCAlYCgAECfwB/AX8Bf8eTqcwGfwB/B38KY29sbGVjdGlvbn8AfwEBfwJ/AH8AAA",
  ),
);

/** @type {Automerge.Doc<SettingsDocument>} */
export const INITIAL_SETTINGS_DOCUMENT = Automerge.from({ collection: [] });

/** @type {Automerge.Doc<TracksDocument>} */
export const INITIAL_TRACKS_DOCUMENT = Automerge.load(
  base64.decode(
    "hW9Kg+NQ56QAeAEQxkdUqSHSfh9+5TiM1gnDswH+Nh6PA98q+0oB013slgls/ARH8Fi5NvI9jhK2RZ1DTgYBAgMCEwIjBkACVgIHFQwhAiMCNAFCAlYCgAECfwB/AX8Bf5eSqcwGfwB/B38KY29sbGVjdGlvbn8AfwEBfwJ/AH8AAA",
  ),
);
