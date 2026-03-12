// import { fragments, serializeFragments } from "@fcrozatier/htmlcrunch";

import * as TID from "@atcute/tid";

import { loadURI } from "../loader.js";
import * as CID from "../cid.js";

/**
 * @import {Facet} from "~/definitions/types.d.ts"
 */

/**
 * @param {{ description?: string; kind: string | undefined; name: string; uri: string }} _args
 * @param {{ fetchHTML: boolean }} options
 */
export async function facetFromURI(
  { description, kind, name, uri },
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
    updatedAt: timestamp,
    uri,
  };

  return facet;
}

// /**
//  * @param {string} html
//  */
// export async function inlineModuleScripts(html) {
//   const docPromises = fragments.parseOrThrow(html).map(async (frag) => {
//     if ("tagName" in frag && frag.tagName === "script") {
//       const isModScript = frag.attributes.find((a) =>
//         a[0] === "type"
//       )?.[1] === "module";
//       if (!isModScript) return frag;
//
//       const src = frag.attributes.find((a) => a[0] === "src")?.[1];
//       if (!src) return frag;
//
//       const scriptContents = await fetch(src).then((r) => r.text()).catch(() =>
//         null
//       );
//
//       if (!scriptContents) return frag;
//
//       /**
//        * @type {import("@fcrozatier/htmlcrunch").MTextNode}
//        */
//       const child = {
//         kind: "TEXT",
//         text: "\n" + scriptContents.split("\n").map((l) =>
//           `  ${l}`
//         ).join("\n") + "\n",
//       };
//
//       return {
//         ...frag,
//         attributes: frag.attributes.filter((a) => a[0] !== "src"),
//         children: [child],
//       };
//     }
//
//     return frag;
//   });
//
//   const doc = await Promise.all(docPromises);
//   return serializeFragments(doc);
// }
