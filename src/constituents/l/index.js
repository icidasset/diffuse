import * as CID from "@atcute/cid";

import foundation from "@common/constituents/foundation.js";
import { effect } from "@common/signal.js";

/**
 * @import {Constituent} from "@definitions/types.d.ts"
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
  const collection = output.constituents.collection();
  if (output.constituents.state() !== "loaded") return;

  let constituent;

  if (cid) {
    constituent = collection.find((c) => c.cid === cid);
  } else if (name) {
    constituent = collection.find((c) => c.name === name);
  } else if (url) {
    const html = await fetch(url).then((res) => res.text());
    const cid = await CID.create(0x55, new TextEncoder().encode(html));
    const name = "tryout";

    /** @type {Constituent} */
    const c = {
      $type: "sh.diffuse.output.constituent",
      cid: CID.toString(cid),
      html,
      name,
    };

    constituent = c;
  }

  // TODO: Message that constituent was not found
  if (!constituent) return;

  loadIntoContainer(constituent);
});

/**
 * @param {Constituent} constituent
 */
function loadIntoContainer(constituent) {
  // TODO: Validate if CID matches HTML

  const range = document.createRange();
  range.selectNode(container);
  const documentFragment = range.createContextualFragment(constituent.html);

  container.innerHTML = "";
  container.append(documentFragment);
}
