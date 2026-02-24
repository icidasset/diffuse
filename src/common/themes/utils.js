import * as TID from "@atcute/tid";

import { loadURI } from "../loader.js";
import * as CID from "../cid.js";

/**
 * @import {Theme} from "@definitions/types.d.ts"
 */

/**
 * @param {{ name: string; uri: string }} _args
 * @param {{ fetchHTML: boolean }} options
 */
export async function themeFromURI({ name, uri }, { fetchHTML }) {
  const html = fetchHTML ? await loadURI(uri) : undefined;
  const cid = html
    ? await CID.create(0x55, new TextEncoder().encode(html))
    : undefined;
  const timestamp = new Date().toISOString();

  /** @type {Theme} */
  const theme = {
    $type: "sh.diffuse.output.theme",
    createdAt: timestamp,
    id: TID.now(),
    cid,
    html,
    name,
    updatedAt: timestamp,
    uri,
  };

  return theme;
}
