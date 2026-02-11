import * as CID from "@atcute/cid";

import foundation from "@common/facets/foundation.js";
import { effect } from "@common/signal.js";

/**
 * @import {Facet} from "@definitions/types.d.ts"
 */

////////////////////////////////////////////
// OUTPUT
////////////////////////////////////////////

const output = foundation.orchestrator.output();

////////////////////////////////////////////
// URL PARAMS
////////////////////////////////////////////

const docUrl = new URL(document.location.href);

const cid = docUrl.searchParams.get("cid");
const name = docUrl.searchParams.get("name");
const url = docUrl.searchParams.get("url");

////////////////////////////////////////////
// LOAD
////////////////////////////////////////////

const containerNull = document.querySelector("#container");
if (!containerNull) throw new Error("Container not found");

const container = /** @type {HTMLDivElement} */ (containerNull);

effect(async () => {
  const collection = output.facets.collection();
  if (output.facets.state() !== "loaded") return;

  let facet;

  if (cid) {
    facet = collection.find((c) => c.cid === cid);
  } else if (name) {
    facet = collection.find((c) => c.name === name);
  } else if (url) {
    const html = await fetch(url).then((res) => res.text());
    const cid = await CID.create(0x55, new TextEncoder().encode(html));
    const name = "tryout";

    /** @type {Facet} */
    const c = {
      $type: "sh.diffuse.output.facet",
      cid: CID.toString(cid),
      html,
      name,
    };

    facet = c;
  }

  // TODO: Message that facet was not found
  if (!facet) return;

  loadIntoContainer(facet);
});

/**
 * @param {Facet} facet
 */
function loadIntoContainer(facet) {
  // TODO: Validate if CID matches HTML

  const range = document.createRange();
  range.selectNode(container);
  const documentFragment = range.createContextualFragment(facet.html);

  container.innerHTML = "";
  container.append(documentFragment);
}
