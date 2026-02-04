import * as CID from "@atcute/cid";
import { html, render } from "lit-html";

import foundation from "@common/constituents/foundation.js";
import { effect } from "@common/signal.js";

/**
 * @import {Constituent} from "@definitions/types.d.ts"
 */

////////////////////////////////////////////
// LIST
////////////////////////////////////////////

/** @type {HTMLElement | null} */
const listEl = document.querySelector("#list");
if (!listEl) throw new Error("List element not found");

const output = foundation.orchestrator.output();

effect(() => {
  const col = output.constituents.collection();

  const h = col.length
    ? html`
      <ul>
        ${col.map((c) =>
          html`
            <li>
              <a href="themes/loader/constituent/s/?cid=${c.cid}">
                ${c.name}
              </a>
            </li>
          `
        )}
      </ul>
    `
    : output.constituents.state() === "loaded"
    ? emptyConstituentsList
    : html`
      <i class="ph-bold ph-spinner-gap"></i>
    `;

  render(h, listEl);
});

const emptyConstituentsList = html`
  <p style="margin-bottom: 0;">
    <i class="ph-fill ph-info"></i> You have not added any constituents yet. Add
    or create some using the tools below.
  </p>
`;

////////////////////////////////////////////
// BUILD
////////////////////////////////////////////

document.querySelector("#build-form")?.addEventListener(
  "submit",
  onBuildSubmit,
);

/**
 * @param {Event} event
 */
async function onBuildSubmit(event) {
  event.preventDefault();

  const htmlEl =
    /** @type {HTMLTextAreaElement | null} */ (document.querySelector(
      "#html-input",
    ));
  const nameEl = /** @type {HTMLInputElement | null} */ (document.querySelector(
    "#name-input",
  ));

  const html = htmlEl?.value ?? "";
  const cid = await CID.create(0x55, new TextEncoder().encode(html));
  const name = nameEl?.value ?? "nameless";

  /** @type {Constituent} */
  const constituent = {
    $type: "sh.diffuse.output.constituent",
    cid: CID.toString(cid),
    html,
    name,
  };

  switch (/** @type {any} */ (event).submitter.name) {
    case "load-example": {
      /** @type {HTMLSelectElement | null} */
      const selected = document.body.querySelector("#example-select");

      if (htmlEl && selected?.value) {
        htmlEl.value = await fetch(
          `themes/loader/constituent/examples/${selected.value}`,
        ).then((r) => r.text());
      }
      break;
    }
    case "save":
      await output.constituents.save([constituent]);
      break;
    case "save+open":
      await output.constituents.save([constituent]);
      window.open(`${location.href}s/?cid=${constituent.cid}`, "blank");
      break;
  }
}
