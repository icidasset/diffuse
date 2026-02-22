import * as CID from "../cid.js";

/**
 * @import {Theme} from "@definitions/types.d.ts"
 */

/**
 * @param {{ name: string; url: string }} _args
 * @param {{ fetchHTML: boolean }} options
 */
export async function themeFromUrl({ name, url }, { fetchHTML }) {
  const html = fetchHTML
    ? await fetch(url).then((res) => res.text())
    : undefined;
  const cid = html
    ? await CID.create(0x55, new TextEncoder().encode(html))
    : undefined;
  const timestamp = new Date().toISOString();

  /** @type {Theme} */
  const theme = {
    $type: "sh.diffuse.output.theme",
    createdAt: timestamp,
    id: crypto.randomUUID(),
    cid,
    html,
    name,
    updatedAt: timestamp,
    url,
  };

  return theme;
}
