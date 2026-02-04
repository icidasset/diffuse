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

const url = new URL(document.location.href);

const cid = url.searchParams.get("cid");
const name = url.searchParams.get("name");

////////////////////////////////////////////
// LOAD
////////////////////////////////////////////

const containerNull = document.querySelector("#container");
if (!containerNull) throw new Error("Container not found");

const container = /** @type {HTMLDivElement} */ (containerNull);

effect(() => {
  const collection = output.constituents.collection();
  if (output.constituents.state() !== "loaded") return;

  let constituent;

  if (cid) {
    constituent = collection.find((c) => c.cid === cid);
  } else if (name) {
    constituent = collection.find((c) => c.name === name);
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
