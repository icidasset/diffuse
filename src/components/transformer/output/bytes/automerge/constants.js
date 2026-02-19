import * as Automerge from "@automerge/automerge";
import { base64 } from "iso-base/rfc4648";

/**
 * @import { FacetsDocument, PlaylistItemsDocument, ThemesDocument, TracksDocument } from "./types.d.ts";
 */

/** @type {Automerge.Doc<FacetsDocument>} */
export const INITIAL_FACETS_DOCUMENT = Automerge.load(
  base64.decode(
    "hW9Kg1fmLLcAeAEQZ2dnbV0O8AUftAeEXT1QVQHsmc9lhGh1ysz4ECa3pEUgzSZG2aF8hob78u7FqeybBQYBAgMCEwIjBkACVgIHFQwhAiMCNAFCAlYCgAECfwB/AX8Bf8KTqcwGfwB/B38KY29sbGVjdGlvbn8AfwEBfwJ/AH8AAA",
  ),
);

/** @type {Automerge.Doc<PlaylistItemsDocument>} */
export const INITIAL_PLAYLISTS_DOCUMENT = Automerge.load(
  base64.decode(
    "hW9Kg5IPZcsAeAEQIyp0LRYp0l9bpZKWJXTPlgGtUD/lrIatFjiIwoUdtJhh/sBQFIcpPppxduoIp1ArXwYBAgMCEwIjBkACVgIHFQwhAiMCNAFCAlYCgAECfwB/AX8Bf8eTqcwGfwB/B38KY29sbGVjdGlvbn8AfwEBfwJ/AH8AAA",
  ),
);

/** @type {Automerge.Doc<ThemesDocument>} */
export const INITIAL_THEMES_DOCUMENT = Automerge.load(
  base64.decode(
    "hW9Kgw5i4LcAeAEQzljXzJAwgqwMkIT3CseCywF4jHbKg9Q2XqVU26bSDj0GtjkQq1HyriZedXU+vUt5wAYBAgMCEwIjBkACVgIHFQwhAiMCNAFCAlYCgAECfwB/AX8Bf8iTqcwGfwB/B38KY29sbGVjdGlvbn8AfwEBfwJ/AH8AAA",
  ),
);

/** @type {Automerge.Doc<TracksDocument>} */
export const INITIAL_TRACKS_DOCUMENT = Automerge.load(
  base64.decode(
    "hW9Kg+NQ56QAeAEQxkdUqSHSfh9+5TiM1gnDswH+Nh6PA98q+0oB013slgls/ARH8Fi5NvI9jhK2RZ1DTgYBAgMCEwIjBkACVgIHFQwhAiMCNAFCAlYCgAECfwB/AX8Bf5eSqcwGfwB/B38KY29sbGVjdGlvbn8AfwEBfwJ/AH8AAA",
  ),
);
