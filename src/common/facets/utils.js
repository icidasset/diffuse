import * as TID from "@atcute/tid";

import { loadURI } from "../loader.js";
import * as CID from "../cid.js";

/**
 * @import {Facet} from "~/definitions/types.d.ts"
 */

/**
 * @param {{ description?: string; kind: string | undefined; name: string; tags?: string[]; uri: string }} _args
 * @param {{ fetchHTML: boolean }} options
 *
 * @example Creates a facet with correct $type, name, uri, id, and timestamps
 * ```js
 * import { facetFromURI } from "~/common/facets/utils.js";
 *
 * const facet = await facetFromURI({ name: "My Facet", uri: "facets/test/index.html", kind: undefined, description: undefined }, { fetchHTML: false });
 *
 * if (facet.$type !== "sh.diffuse.output.facet") throw new Error("$type should be sh.diffuse.output.facet");
 * if (facet.name !== "My Facet") throw new Error("name should be preserved");
 * if (facet.uri !== "facets/test/index.html") throw new Error("uri should be preserved");
 * if (typeof facet.id !== "string" || facet.id.length === 0) throw new Error("id should be a non-empty string");
 * if (!facet.createdAt) throw new Error("createdAt should be set");
 * if (facet.createdAt !== facet.updatedAt) throw new Error("createdAt and updatedAt should match");
 * if (new Date(facet.createdAt).toISOString() !== facet.createdAt) throw new Error("createdAt should be a valid ISO string");
 * ```
 *
 * @example fetchHTML false leaves html and cid undefined; kind is validated
 * ```js
 * import { facetFromURI } from "~/common/facets/utils.js";
 *
 * const base = { name: "Test", uri: "test.html", description: undefined };
 *
 * const noHtml = await facetFromURI({ ...base, kind: undefined }, { fetchHTML: false });
 * if (noHtml.html !== undefined) throw new Error("html should be undefined when fetchHTML is false");
 * if (noHtml.cid !== undefined) throw new Error("cid should be undefined when fetchHTML is false");
 *
 * const prelude = await facetFromURI({ ...base, kind: "prelude" }, { fetchHTML: false });
 * if (prelude.kind !== "prelude") throw new Error("prelude kind should be preserved");
 *
 * const interactive = await facetFromURI({ ...base, kind: "interactive" }, { fetchHTML: false });
 * if (interactive.kind !== "interactive") throw new Error("interactive kind should be preserved");
 *
 * const unknown = await facetFromURI({ ...base, kind: "unknown" }, { fetchHTML: false });
 * if (unknown.kind !== undefined) throw new Error("unrecognised kind should be set to undefined");
 * ```
 *
 * @example Generates unique ids across calls
 * ```js
 * import { facetFromURI } from "~/common/facets/utils.js";
 *
 * const a = await facetFromURI({ name: "A", uri: "a.html", kind: undefined, description: undefined }, { fetchHTML: false });
 * const b = await facetFromURI({ name: "B", uri: "b.html", kind: undefined, description: undefined }, { fetchHTML: false });
 * if (a.id === b.id) throw new Error("ids should be unique across calls");
 * ```
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
