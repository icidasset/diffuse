// import { fragments, serializeFragments } from "@fcrozatier/htmlcrunch";

import { Temporal } from "@js-temporal/polyfill";
import * as CID from "../cid.js";

/**
 * @import {Facet} from "@definitions/types.d.ts"
 */

/**
 * @param {{ name: string; url: string }} _args
 * @param {{ fetchHTML: boolean }} options
 */
export async function facetFromUrl({ name, url }, { fetchHTML }) {
  const html = fetchHTML
    ? await fetch(url).then((res) => res.text())
    : undefined;
  const cid = html
    ? await CID.create(0x55, new TextEncoder().encode(html))
    : undefined;
  const timestamp = Temporal.Now.zonedDateTimeISO().toString();

  /** @type {Facet} */
  const facet = {
    $type: "sh.diffuse.output.facet",
    createdAt: timestamp,
    id: crypto.randomUUID(),
    cid,
    html,
    name,
    updatedAt: timestamp,
    url,
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
