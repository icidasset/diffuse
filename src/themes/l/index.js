import * as CID from "@common/cid.js";
import foundation from "@common/facets/foundation.js";
import { effect } from "@common/signal.js";

/**
 * @import {Theme} from "@definitions/types.d.ts"
 */

////////////////////////////////////////////
// OUTPUT
////////////////////////////////////////////

const output = foundation.orchestrator.output();

////////////////////////////////////////////
// URL PARAMS
////////////////////////////////////////////

const docUrl = new URL(document.location.href);

const id = docUrl.searchParams.get("id");
const cid = docUrl.searchParams.get("cid");
const name = docUrl.searchParams.get("name");
const url = docUrl.searchParams.get("url");

////////////////////////////////////////////
// LOAD
////////////////////////////////////////////

effect(async () => {
  const collection = output.themes.collection();
  if (output.themes.state() !== "loaded") return;

  let theme;

  if (id) {
    theme = collection.find((t) => t.id === id);
  } else if (cid) {
    theme = collection.find((t) => t.cid === cid);
  } else if (name) {
    theme = collection.find((t) => t.name === name);
  } else if (url) {
    /** @type {Theme} */
    const t = {
      $type: "sh.diffuse.output.theme",
      id: crypto.randomUUID(),
      name: "tryout",
      url,
    };

    theme = t;
  }

  // TODO: Message that theme was not found
  if (!theme) return;

  // Make sure HTML is loaded
  // TODO: Handle URL loading error
  if (!theme.html && theme.url) {
    const html = await fetch(theme.url).then((res) => res.text());
    const cid = await CID.create(0x55, new TextEncoder().encode(html));

    theme.html = html;
    theme.cid = cid;
  }

  loadIntoContainer(theme);
});

/**
 * @param {Theme} theme
 */
function loadIntoContainer(theme) {
  // TODO: Validate if CID matches HTML

  const iframe = document.createElement("iframe")
  iframe.srcdoc = theme.html ?? ""

  document.body.innerHTML = ""
  document.body.append(iframe)
}
