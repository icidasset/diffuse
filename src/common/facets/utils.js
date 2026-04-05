import * as TID from "@atcute/tid";

import { loadURI } from "../loader.js";
import * as CID from "../cid.js";

/**
 * @import {Facet} from "~/definitions/types.d.ts"
 */

/**
 * @param {{ description?: string; kind: string | undefined; name: string; tags?: string[]; uri: string }} _args
 * @param {{ fetchHTML: boolean }} options
 */
export async function facetFromURI(
  { description, kind, name, tags, uri },
  { fetchHTML },
) {
  const html = fetchHTML ? await loadURI(uri) : undefined;
  const cid = html
    ? await CID.create(0x55, new TextEncoder().encode(html))
    : undefined;
  const timestamp = new Date().toISOString();

  /** @type {Facet} */
  const facet = {
    $type: "sh.diffuse.output.facet",
    createdAt: timestamp,
    id: TID.now(),
    cid,
    description,
    html,
    name,
    kind: kind === "interactive" || kind === "prelude" ? kind : undefined,
    tags: tags?.length ? tags : undefined,
    updatedAt: timestamp,
    uri,
  };

  return facet;
}
